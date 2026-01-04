use crate::values::{ConsCell, Symbol};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MacroValue {
    pub params: Vec<Symbol>,
    pub body: ConsCell,
}

impl MacroValue {
    pub fn new(params: Vec<Symbol>, body: ConsCell) -> Self {
        Self { params, body }
    }
}
