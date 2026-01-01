use core::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::values::{Symbol, Value};

#[derive(Clone, PartialEq, Default)]
pub struct Scope {
    parent_scope: Option<Rc<RefCell<Box<Scope>>>>,
    map: HashMap<Symbol, Value>,
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "=== Scope ===")?;
        for (k, v) in self.map.iter() {
            writeln!(f, "{k}: {v}")?;
        }
        if let Some(scope) = &self.parent_scope {
            fmt::Debug::fmt(scope.borrow().as_ref(), f)?;
        };
        writeln!(f, "=============")?;
        Ok(())
    }
}

impl Scope {
    pub fn new(parent_scope: Option<Rc<RefCell<Box<Scope>>>>) -> Self {
        Self {
            parent_scope,
            map: HashMap::default(),
        }
    }

    pub fn parent_scope(&self) -> Option<Rc<RefCell<Box<Scope>>>> {
        self.parent_scope.clone()
    }

    pub fn insert(&mut self, key: Symbol, value: Value) {
        self.map.insert(key, value);
    }

    pub fn get(&self, key: &Symbol) -> Option<&Value> {
        self.map.get(key)
    }
}

