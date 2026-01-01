use std::{borrow::Cow, cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    lexer::Lexer,
    native::{
        NativeError, NativeResult, add, and, apply, boundp, car, cdr, cond, cons, defmacro, div,
        equal, error, eval, if_native, lambda, less, let_native, list, macroexpand, mul, or, print,
        progn, quote, setq, sub,
    },
    parser::Parser,
    values::{ConsCell, MacroValue, NativeFunction, NativeFunctionValue, Scope, Symbol, Value},
};

pub struct Interpreter {
    active_scope_stack: Vec<Rc<RefCell<Box<Scope>>>>,
    native_functions: HashMap<Cow<'static, str>, NativeFunction>,
}

impl Interpreter {
    pub fn new() -> Self {
        let root_scope = Rc::new(RefCell::new(Box::new(Scope::default())));
        Self {
            active_scope_stack: vec![root_scope],
            native_functions: build_native_function_map(),
        }
    }

    pub fn evaluate(&mut self, value: Value) -> NativeResult<Value> {
        let result = match value {
            Value::ConsCell(cell) => self.evaluate_list(cell)?,
            Value::Symbol(symbol) => self
                .read_value(&symbol)
                .ok_or_else(|| NativeError::UndefinedVariable(symbol))?,
            _ => value,
        };

        Ok(result)
    }

    pub fn evaluate_list(&mut self, cell: ConsCell) -> NativeResult<Value> {
        let ConsCell { value, rest: next } = cell;

        let head = match *value {
            Value::Symbol(symbol) => self.read_value(&symbol).unwrap_or(Value::Symbol(symbol)),
            head => head,
        };

        let result = match head {
            Value::Symbol(value) => Err(NativeError::UndefinedVariable(value))?,
            Value::Function(function) => apply(
                self,
                Value::ConsCell(ConsCell::new(Value::Function(function)).with_rest(
                    Value::ConsCell(ConsCell::new(Value::ConsCell(
                        ConsCell::new(Value::Symbol(Symbol::new("list"))).with_rest(*next),
                    ))),
                )),
            )?,
            Value::Macro(MacroValue { params, body }) => {
                self.evaluate_macro(*next, params, body)?
            }
            Value::NativeFunction(NativeFunctionValue { func, .. }) => func(self, *next)?,
            _ => Err(NativeError::InvalidFunctionExpression(head))?,
        };

        Ok(result)
    }

    /// First expands a macro into a value and then evaluates it.
    fn evaluate_macro(
        &mut self,
        next: Value,
        params: Vec<Symbol>,
        body: ConsCell,
    ) -> NativeResult<Value> {
        // validate & prepare
        let arguments = next.into_iter().map(|value| value).collect::<Vec<_>>();
        if arguments.len() != params.len() {
            return Err(NativeError::MismatchedArgumentAndParameterCount {
                name: "macro".into(),
                params: params.len(),
                arguments: arguments.len(),
            });
        }

        let mut param_map = HashMap::new();
        for (param, argument) in params.into_iter().zip(arguments) {
            param_map.insert(param, argument);
        }

        let expanded = self.expand_macro(&param_map, Value::ConsCell(body));

        self.evaluate(expanded)
    }

    pub(crate) fn expand_macro(
        &mut self,
        param_map: &HashMap<Symbol, Value>,
        value: Value,
    ) -> Value {
        match value {
            Value::ConsCell(cell) => Value::join(
                cell.into_iter()
                    .map(|value| self.expand_macro(param_map, value)),
            ),
            Value::Symbol(symbol) => {
                if let Some(value) = param_map.get(&symbol) {
                    value.clone()
                } else {
                    Value::Symbol(symbol)
                }
            }
            _ => value,
        }
    }

    pub(crate) fn read_value(&self, symbol: &Symbol) -> Option<Value> {
        let mut current_scope = Some(self.active_scope());

        // Look for values in scope
        while let Some(scope) = current_scope {
            let scope = scope.borrow();
            if let Some(value) = scope.get(symbol) {
                return Some(value.clone()); // Huge footgun, this should not be cloned...
            }
            current_scope = scope.parent_scope();
        }

        // Look for native functions
        if let Some(native_func) = self.native_functions.get(&symbol.as_cow()) {
            return Some(Value::NativeFunction(NativeFunctionValue {
                name: symbol.as_cow(),
                func: *native_func,
            }));
        }

        None
    }

    pub fn load_std(&mut self) {
        let lexer = Lexer::new(include_str!("std.lisp"));
        let tokens = lexer.lex();
        let parser = Parser::new(tokens);
        let statements = parser
            .parse()
            .expect("Interpreter should not load with issue");
        for (i, value) in statements.into_iter().enumerate() {
            self.evaluate(value)
                .expect(&format!("Statement {} of std failed", i + 1));
        }
    }

    pub(crate) fn set_value(&mut self, symbol: Symbol, value: Value) {
        let mut current_scope = Some(self.active_scope());

        while let Some(scope) = current_scope {
            let mut scope = scope.borrow_mut();
            if scope.get(&symbol).is_some() {
                scope.insert(symbol, value);
                return;
            }
            current_scope = scope.parent_scope();
        }

        self.set_global_value(symbol, value);
    }

    pub(crate) fn set_global_value(&mut self, symbol: Symbol, value: Value) {
        let scope = self
            .active_scope_stack
            .first()
            .expect("There should always be at least one scope");
        let mut scope = scope.borrow_mut();
        scope.insert(symbol, value);
    }

    /// Returns the closest scope to the active form/expression.
    pub(crate) fn active_scope(&self) -> Rc<RefCell<Box<Scope>>> {
        self.active_scope_stack
            .last()
            .expect("It should never be possible to pop the root scope")
            .clone()
    }

    pub(crate) fn pop_active_scope(&mut self) -> Rc<RefCell<Box<Scope>>> {
        let result = self
            .active_scope_stack
            .pop()
            .expect("It should never be possible to pop the root active scope");
        result
    }

    pub(crate) fn push_active_scope(&mut self, scope: Rc<RefCell<Box<Scope>>>) {
        self.active_scope_stack.push(scope);
    }
}

fn build_native_function_map() -> HashMap<Cow<'static, str>, NativeFunction> {
    let mut map: HashMap<Cow<'static, str>, NativeFunction> = HashMap::new();
    map.insert("+".into(), add);
    map.insert("*".into(), mul);
    map.insert("-".into(), sub);
    map.insert("/".into(), div);
    map.insert("=".into(), equal);
    map.insert("<".into(), less);
    map.insert("and".into(), and);
    map.insert("apply".into(), apply);
    map.insert("boundp".into(), boundp);
    map.insert("car".into(), car);
    map.insert("cdr".into(), cdr);
    map.insert("cond".into(), cond);
    map.insert("cons".into(), cons);
    map.insert("defmacro".into(), defmacro);
    map.insert("error".into(), error);
    map.insert("eval".into(), eval);
    map.insert("if".into(), if_native);
    map.insert("lambda".into(), lambda);
    map.insert("let".into(), let_native);
    map.insert("list".into(), list);
    map.insert("macroexpand".into(), macroexpand);
    map.insert("or".into(), or);
    map.insert("print".into(), print);
    map.insert("progn".into(), progn);
    map.insert("quote".into(), quote);
    map.insert("setq".into(), setq);
    map
}
