use std::{borrow::Cow, fmt::Debug, hash::Hash};

use crate::{interpreter::Interpreter, native::NativeResult, values::Value};

pub type NativeFunction = fn(&mut Interpreter, Value) -> NativeResult<Value>;

#[derive(Debug, Clone)]
pub struct NativeFunctionValue {
    pub name: Cow<'static, str>,
    pub func: NativeFunction,
}

impl PartialEq for NativeFunctionValue {
    fn eq(&self, other: &Self) -> bool {
        // We assume that no two native functions have the same name.
        self.name == other.name
    }
}

impl Eq for NativeFunctionValue {}

impl Hash for NativeFunctionValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let address = (self) as *const NativeFunctionValue as usize;
        address.hash(state);
    }
}

impl NativeFunctionValue {
    pub fn new<S>(name: S, func: NativeFunction) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self {
            name: name.into(),
            func,
        }
    }
}
