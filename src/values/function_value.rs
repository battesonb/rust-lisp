use std::{cell::RefCell, rc::Rc};

use crate::values::{ConsCell, Scope, Symbol};

/// TODO: Support this as an enum of lisp functions and native functions, so that both can be
/// referenced in code as values.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionValue {
    pub parent_scope: Rc<RefCell<Box<Scope>>>,
    pub params: Vec<Symbol>,
    pub body: ConsCell,
}

impl FunctionValue {
    pub fn new(parent_scope: Rc<RefCell<Box<Scope>>>, params: Vec<Symbol>, body: ConsCell) -> Self {
        Self {
            parent_scope,
            params,
            body,
        }
    }
}
