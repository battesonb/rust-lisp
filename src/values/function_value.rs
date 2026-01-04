use std::{cell::RefCell, hash::Hash, rc::Rc};

use crate::values::{ConsCell, Scope, Symbol};

/// TODO: Support this as an enum of lisp functions and native functions, so that both can be
/// referenced in code as values.
#[derive(Clone, Debug)]
pub struct FunctionValue {
    pub parent_scope: Rc<RefCell<Box<Scope>>>,
    pub params: Vec<Symbol>,
    pub rest_argument: Option<Symbol>,
    pub body: ConsCell,
}

impl PartialEq for FunctionValue {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Eq for FunctionValue {}

impl Hash for FunctionValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let address = (self) as *const FunctionValue as usize;
        address.hash(state);
    }
}

impl FunctionValue {
    pub fn new(
        parent_scope: Rc<RefCell<Box<Scope>>>,
        params: Vec<Symbol>,
        body: ConsCell,
        rest_argument: Option<Symbol>,
    ) -> Self {
        Self {
            parent_scope,
            params,
            rest_argument,
            body,
        }
    }
}
