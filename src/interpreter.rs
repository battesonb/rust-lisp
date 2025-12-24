use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Link, List, Scope, Symbol, Value},
    lexer::Lexer,
    native::{
        add, and, apply, car, cdr, div, equal, error, eval, if_native, lambda, less, list, print,
        more, mul, not, or, progn, quote, setq, sub,
    },
    parser::Parser,
};

pub struct Interpreter {
    scopes: Vec<Rc<RefCell<Scope>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scopes: vec![Rc::new(RefCell::new(Scope::default()))],
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
            _ => Value::Error(format!(
                "Expected list to start with symbol (function name), received: {value}"
            )),
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
            ">" => more(self, links),
            "not" => not(self, links),
            "and" => and(self, links),
            "or" => or(self, links),
            "apply" => apply(self, links),
            "car" => car(self, links),
            "cdr" => cdr(self, links),
            "error" => error(self, links),
            "eval" => eval(self, links),
            "if" => if_native(self, links),
            "lambda" => lambda(self, links),
            "list" => list(self, links),
            "progn" => progn(self, links),
            "setq" => setq(self, links),
            "quote" => quote(self, links),
            "print" => print(self, links),
            _ => Value::Error(format!("Unresolved function {function}")),
        }
    }

    pub(crate) fn read_value(&self, symbol: &Symbol) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            let scope = scope.borrow();
            if let Some(value) = scope.get(symbol) {
                return Some(value.clone()); // Huge footgun, this should not be cloned...
            }
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

    pub(crate) fn set_global_value(&mut self, symbol: Symbol, value: Value) {
        let scope = self
            .scopes
            .first_mut()
            .expect("There should always be at least one scope");
        let mut scope = scope.borrow_mut();
        scope.insert(symbol, value);
    }

    pub(crate) fn pop_scope(&mut self) -> Scope {
        let Some(result) = self.scopes.pop() else {
            panic!("It should never be possible to pop the root scope");
        };
        result.take()
    }

    pub(crate) fn push_scope(&mut self, scope: Rc<RefCell<Scope>>) {
        self.scopes.push(scope);
    }
}
