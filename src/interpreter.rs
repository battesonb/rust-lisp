use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{Link, List, MacroValue, Scope, Symbol, Value},
    lexer::Lexer,
    native::{
        add, and, apply, car, cdr, defmacro, div, equal, error, eval, if_native, lambda, less,
        let_native, list, macroexpand, mul, or, print, progn, quote, setq, sub,
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

    pub fn evaluate(&mut self, value: Value) -> Value {
        match value {
            Value::List(list) => self.evaluate_list(list),
            Value::Symbol(symbol) => self.read_value(&symbol).unwrap_or(Value::Symbol(symbol)),
            _ => value,
        }
    }

    pub fn evaluate_list(&mut self, list: Box<List>) -> Value {
        let Some(head) = list.head else {
            return Value::List(list);
        };

        let Link { value, next } = *head;

        // In case the function name is encoded in code, not actually common in implementations of Lisp.
        let value = self.evaluate(value);

        match value {
            Value::Symbol(value) => self.evaluate_function(value, next),
            Value::Function(function) => apply(
                self,
                Some(Box::new(Link::new_with_link(
                    Value::Function(function),
                    Some(Box::new(Link::new(Value::List(Box::new(List::new(
                        Box::new(
                            Link::new(Value::Symbol(Symbol::new("list".to_string())))
                                .with_next(next),
                        ),
                    )))))),
                ))),
            ),
            Value::Macro(MacroValue { params, body }) => self.evaluate_macro(next, params, body),
            _ => Value::Error(format!(
                "Expected list to start with symbol (function name), received: {value}"
            )),
        }
    }

    /// First expands a macro into a value and then evaluates it.
    fn evaluate_macro(
        &mut self,
        next: Option<Box<Link>>,
        params: Vec<Symbol>,
        body: Box<List>,
    ) -> Value {
        let Some(next) = next else {
            return Value::Error("macro expects 3 arguments".to_string());
        };

        // validate & prepare
        let arguments = next.into_iter().map(|value| value).collect::<Vec<_>>();
        if arguments.len() != params.len() {
            return Value::Error(format!(
                "macro param count {} does not match argument count {}",
                params.len(),
                arguments.len()
            ));
        }

        let mut param_map = HashMap::new();
        for (param, argument) in params.into_iter().zip(arguments) {
            param_map.insert(param, argument);
        }

        let Some(body) = body.head else {
            return Value::Error("macro expects 3 arguments".to_string());
        };

        if body.next.is_some() {
            return Value::Error("macro expects 3 arguments".into());
        }

        let expanded = self.expand_macro(&param_map, body.value);

        // evaluate
        self.evaluate(expanded)
    }

    pub(crate) fn expand_macro(
        &mut self,
        param_map: &HashMap<Symbol, Value>,
        value: Value,
    ) -> Value {
        match value {
            Value::List(list) => {
                let Some(link) = list.head else {
                    return Value::default();
                };

                let links = Value::join(
                    link.into_iter()
                        .map(|value| self.expand_macro(param_map, value)),
                );

                Value::List(Box::new(List::new_option(links)))
            }
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

    fn evaluate_function(&mut self, function: Symbol, links: Option<Box<Link>>) -> Value {
        match function.as_str() {
            "+" => add(self, links),
            "*" => mul(self, links),
            "-" => sub(self, links),
            "/" => div(self, links),
            "=" => equal(self, links),
            "<" => less(self, links),
            "and" => and(self, links),
            "or" => or(self, links),
            "apply" => apply(self, links),
            "car" => car(self, links),
            "cdr" => cdr(self, links),
            "error" => error(self, links),
            "eval" => eval(self, links),
            "if" => if_native(self, links),
            "lambda" => lambda(self, links),
            "let" => let_native(self, links),
            "list" => list(self, links),
            "progn" => progn(self, links),
            "setq" => setq(self, links),
            "defmacro" => defmacro(self, links),
            "macroexpand" => macroexpand(self, links),
            "quote" => quote(self, links),
            "print" => print(self, links),
            _ => Value::Error(format!("Unresolved function {function}")),
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
        for value in statements {
            self.evaluate(value);
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
