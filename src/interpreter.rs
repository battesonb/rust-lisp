use std::{borrow::Cow, cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use thiserror::Error;

use crate::{
    lexer::Lexer,
    native::{
        NativeError, NativeResult, add, and, apply, apply_internal, boundp, car, cdr, cond, cons,
        defmacro, div, equal, error, eval, gethash, if_native, lambda, less, let_native, list,
        macroexpand, make_hash_table, mul, or, print, progn, quote, sethash, setq, sub, type_of,
    },
    parser::Parser,
    values::{ConsCell, MacroValue, NativeFunction, NativeFunctionValue, Scope, Symbol, Value},
};

#[derive(Clone, Debug, Error)]
pub struct InterpreterError {
    pub stack: Vec<Value>,
    pub message: String,
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ERROR: {}", self.message)?;
        if self.stack.len() > 1 {
            writeln!(f, "TRACE:")?;
            for (i, context) in self.stack.iter().enumerate() {
                writeln!(f, "{}. {}", i + 1, context)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Error)]
pub struct InterpreterErrorEntry {
    /// A quoted, not evaluated, value which provides the context for a function name.
    pub context: Option<Value>,
    pub message: String,
}

impl Display for InterpreterErrorEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.context {
            Some(context) => write!(f, "{}: {}", context, self.message),
            None => write!(f, "{}", self.message),
        }
    }
}

pub type InterpreterResult<T> = Result<T, InterpreterError>;

pub trait IntoInterpreterResult<T> {
    fn into_interpreter_result(self, interpreter: &Interpreter) -> InterpreterResult<T>;
}

pub struct Interpreter {
    context_stack: Vec<Value>,
    active_scope_stack: Vec<Rc<RefCell<Box<Scope>>>>,
    native_functions: HashMap<Cow<'static, str>, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        let root_scope = Rc::new(RefCell::new(Box::new(Scope::default())));
        Self {
            context_stack: Vec::new(),
            active_scope_stack: vec![root_scope],
            native_functions: build_native_function_map(),
        }
    }

    pub fn evaluate(&mut self, value: Value) -> InterpreterResult<Value> {
        let result = match value {
            Value::ConsCell(cell) => self.evaluate_list(cell)?,
            Value::Symbol(symbol) => self
                .read_value(&symbol)
                .ok_or_else(|| NativeError::UndefinedVariable(symbol))
                .into_interpreter_result(&self)?,
            _ => value,
        };

        Ok(result)
    }

    pub fn evaluate_list(&mut self, cell: ConsCell) -> InterpreterResult<Value> {
        let ConsCell { value, rest: next } = cell;

        let head = self.evaluate(*value)?;

        // Potentially quite expensive... Values should likely always be Rc<RefCell<...>> types.
        self.context_stack.push(head.clone());

        let result = match head {
            Value::Symbol(value) => {
                Err(NativeError::UndefinedVariable(value)).into_interpreter_result(&self)
            }
            Value::Function(function) => {
                // Why is this provided as follows?
                //
                // 1. We want to use the same apply implementation as is exposed in the Lisp, but
                //    we don't want the same indirection. There is no need to coerce the
                //    interpreter to perform these operations as we can just stay in the native
                //    world.
                // 2. We don't want the internal `apply` and `list` calls to be visible in the
                //    error stack, as it's just extra noise and an internal implementation detail.
                list(self, *next)
                    .map(|rest| apply_internal(self, Value::Function(function), rest))
                    .flatten()
            }
            Value::Macro(MacroValue { params, body, .. }) => {
                self.evaluate_macro(*next, params, body)
            }
            Value::NativeFunction(NativeFunctionValue { func, .. }) => func(self, *next),
            _ => Err(NativeError::InvalidFunctionExpression(head)).into_interpreter_result(&self),
        };

        self.context_stack.pop();

        result
    }

    /// First expands a macro into a value and then evaluates it.
    fn evaluate_macro(
        &mut self,
        next: Value,
        params: Vec<Symbol>,
        body: ConsCell,
    ) -> InterpreterResult<Value> {
        // validate & prepare
        let arguments = next.into_iter().map(|value| value).collect::<Vec<_>>();
        if arguments.len() != params.len() {
            return Err(NativeError::MismatchedArgumentAndParameterCount {
                name: "macro".into(),
                params: params.len(),
                arguments: arguments.len(),
            })
            .into_interpreter_result(&self);
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
                return Some(value.clone());
            }
            current_scope = scope.parent_scope();
        }

        // Look for native functions
        if let Some(value) = self.native_functions.get(&symbol.as_cow()) {
            return Some(value.clone());
        }

        None
    }

    pub fn load_std(&mut self) {
        let lexer = Lexer::new(include_str!("std.lisp"));
        let tokens = lexer.lex().expect("Failed to lex std");
        let parser = Parser::new(tokens);
        let statements = parser
            .parse()
            .expect("Interpreter should not load with issue");
        for (i, value) in statements.into_iter().enumerate() {
            self.evaluate(value)
                .expect(&format!("Statement {} of std failed", i + 1));
        }
    }

    pub(crate) fn set_value(&mut self, symbol: Symbol, value: Value) -> NativeResult<()> {
        let mut current_scope = Some(self.active_scope());

        if self.native_functions.contains_key(&symbol.as_cow()) {
            return Err(NativeError::Generic(
                "Symbol representing native function may not be set".into(),
            ));
        }

        while let Some(scope) = current_scope {
            let mut scope = scope.borrow_mut();
            if scope.get(&symbol).is_some() {
                scope.insert(symbol, value);
                return Ok(());
            }
            current_scope = scope.parent_scope();
        }

        self.set_global_value(symbol, value);

        Ok(())
    }

    fn set_global_value(&mut self, symbol: Symbol, value: Value) {
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

    pub(crate) fn context_stack(&self) -> &Vec<Value> {
        &self.context_stack
    }
}

fn build_native_function_map() -> HashMap<Cow<'static, str>, Value> {
    let mut map: HashMap<Cow<'static, str>, Value> = HashMap::new();
    let native_functions: [(Cow<'static, str>, NativeFunction); _] = [
        ("+".into(), add),
        ("*".into(), mul),
        ("-".into(), sub),
        ("/".into(), div),
        ("=".into(), equal),
        ("<".into(), less),
        ("and".into(), and),
        ("apply".into(), apply),
        ("boundp".into(), boundp),
        ("car".into(), car),
        ("cdr".into(), cdr),
        ("cond".into(), cond),
        ("cons".into(), cons),
        ("defmacro".into(), defmacro),
        ("error".into(), error),
        ("eval".into(), eval),
        ("if".into(), if_native),
        ("lambda".into(), lambda),
        ("let".into(), let_native),
        ("list".into(), list),
        ("macroexpand".into(), macroexpand),
        ("or".into(), or),
        ("print".into(), print),
        ("progn".into(), progn),
        ("quote".into(), quote),
        ("setq".into(), setq),
        ("make-hash-table".into(), make_hash_table),
        ("gethash".into(), gethash),
        ("sethash".into(), sethash),
        ("type-of".into(), type_of),
    ];

    for (name, func) in native_functions {
        map.insert(
            name.clone(),
            Value::NativeFunction(NativeFunctionValue { name, func }),
        );
    }

    map
}
