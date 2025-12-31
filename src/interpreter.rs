use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{ConsCell, MacroValue, Scope, Symbol, Value},
    lexer::Lexer,
    native::{
        NativeError, NativeResult, add, and, apply, boundp, car, cdr, cond, cons, defmacro, div,
        equal, error, eval, if_native, lambda, less, let_native, list, macroexpand, mul, or, print,
        progn, quote, setq, sub,
    },
    parser::Parser,
};

pub struct Interpreter {
    active_scope_stack: Vec<Rc<RefCell<Box<Scope>>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let root_scope = Rc::new(RefCell::new(Box::new(Scope::default())));
        Self {
            active_scope_stack: vec![root_scope],
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
            Value::Symbol(value) => self.evaluate_function(value, *next)?,
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

    fn evaluate_function(&mut self, function: Symbol, next: Value) -> NativeResult<Value> {
        match function.as_str() {
            "+" => add(self, next),
            "*" => mul(self, next),
            "-" => sub(self, next),
            "/" => div(self, next),
            "=" => equal(self, next),
            "<" => less(self, next),
            "and" => and(self, next),
            "apply" => apply(self, next),
            "boundp" => boundp(self, next),
            "car" => car(self, next),
            "cdr" => cdr(self, next),
            "cond" => cond(self, next),
            "cons" => cons(self, next),
            "defmacro" => defmacro(self, next),
            "error" => error(self, next),
            "eval" => eval(self, next),
            "if" => if_native(self, next),
            "lambda" => lambda(self, next),
            "let" => let_native(self, next),
            "list" => list(self, next),
            "macroexpand" => macroexpand(self, next),
            "or" => or(self, next),
            "print" => print(self, next),
            "progn" => progn(self, next),
            "quote" => quote(self, next),
            "setq" => setq(self, next),
            _ => Err(NativeError::UnresolvedFunction(function)),
        }
    }

    pub(crate) fn read_value(&self, symbol: &Symbol) -> Option<Value> {
        let mut current_scope = Some(self.active_scope());

        while let Some(scope) = current_scope {
            let scope = scope.borrow();
            if let Some(value) = scope.get(symbol) {
                return Some(value.clone()); // Huge footgun, this should not be cloned...
            }
            current_scope = scope.parent_scope();
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
