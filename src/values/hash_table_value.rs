use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

use crate::values::Value;

#[derive(Clone, Debug, Default)]
pub struct HashTableValue {
    map: Rc<RefCell<HashMap<Value, Value>>>,
}

impl PartialEq for HashTableValue {
    fn eq(&self, other: &Self) -> bool {
        (self as *const HashTableValue) == (other as *const HashTableValue)
    }
}

impl Eq for HashTableValue {}

impl Hash for HashTableValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let address = (self) as *const HashTableValue as usize;
        address.hash(state);
    }
}

impl HashTableValue {
    pub fn get(&self, key: &Value) -> Option<Value> {
        let map = self.map.borrow();
        map.get(key).cloned()
    }

    pub fn insert(&mut self, key: Value, value: Value) {
        let mut map = self.map.borrow_mut();
        map.insert(key, value);
    }
}
