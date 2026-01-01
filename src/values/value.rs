use std::{borrow::Cow, fmt::Display};

use thiserror::Error;

use crate::values::{
    FunctionValue, MacroValue, NumberValue, Symbol,
    cons_cell::ConsCell,
    native_function_value::{NativeFunction, NativeFunctionValue},
};

#[derive(Debug, Clone, Error)]
pub enum ValueExpectError {
    #[error("Expected non-empty list, received: {0}")]
    ExpectedNonEmptyList(Value),
    #[error("Expected symbol, received: {0}")]
    ExpectedSymbol(Value),
    #[error("Expected macro reference, received: {0}")]
    ExpectedMacro(Value),
    #[error("Expected function reference, received: {0}")]
    ExpectedFunction(Value),
}

pub type ValueExpectResult<T> = Result<T, ValueExpectError>;

#[derive(Debug, Default, Clone, PartialEq)]
pub enum Value {
    #[default]
    Nil,
    ConsCell(ConsCell),
    Symbol(Symbol),
    Number(NumberValue),
    Function(FunctionValue),
    NativeFunction(NativeFunctionValue),
    Macro(MacroValue),
}

impl Value {
    pub fn symbol<S>(symbol: S) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self::Symbol(Symbol::new(symbol))
    }

    /// Returns either the `t` Symbol or the empty list (nil) depending on the provided condition.
    pub fn cond(cond: bool) -> Self {
        if cond { Self::t() } else { Self::nil() }
    }

    /// Returns the symbol representing "true".
    pub fn t() -> Self {
        Value::Symbol(Symbol::t())
    }

    /// Returns the nil value.
    pub fn nil() -> Self {
        Value::default()
    }

    pub fn is_true(&self) -> bool {
        !self.is_nil()
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            _ => false,
        }
    }

    pub fn expect_cons_cell(self) -> ValueExpectResult<ConsCell> {
        match self {
            Value::ConsCell(cons_cell) => Ok(cons_cell),
            _ => Err(ValueExpectError::ExpectedNonEmptyList(self)),
        }
    }

    pub fn expect_cons_cell_mut(&mut self) -> ValueExpectResult<&mut ConsCell> {
        match self {
            Value::ConsCell(cons_cell) => Ok(cons_cell),
            _ => Err(ValueExpectError::ExpectedNonEmptyList(self.clone())),
        }
    }

    pub fn expect_symbol(self) -> ValueExpectResult<Symbol> {
        match self {
            Value::Symbol(symbol) => Ok(symbol),
            _ => Err(ValueExpectError::ExpectedSymbol(self)),
        }
    }

    pub fn expect_function(self) -> ValueExpectResult<FunctionValue> {
        match self {
            Value::Function(function) => Ok(function),
            _ => Err(ValueExpectError::ExpectedFunction(self)),
        }
    }

    pub fn expect_macro(self) -> ValueExpectResult<MacroValue> {
        match self {
            Value::Macro(macro_value) => Ok(macro_value),
            _ => Err(ValueExpectError::ExpectedMacro(self)),
        }
    }

    pub fn iter(&self) -> ValueIter<'_> {
        ValueIter { value: Some(self) }
    }

    pub fn join<T>(iterator: T) -> Value
    where
        T: IntoIterator<Item = Value>,
    {
        let cells = iterator.into_iter().map(|value| ConsCell::new(value));
        match ConsCell::join(cells) {
            Some(cell) => Value::ConsCell(cell),
            None => Value::nil(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Symbol(symbol) => symbol.fmt(f),
            Value::ConsCell(cell) => cell.fmt(f),
            Value::Number(value) => write!(f, "{value}"),
            Value::Function(FunctionValue { params, body, .. }) => write!(
                f,
                "<FUNCTION ({}) {}>",
                params
                    .iter()
                    .map(Symbol::as_str)
                    .collect::<Vec<_>>()
                    .join(" "),
                body
            ),
            Value::Macro(MacroValue { params, body }) => write!(
                f,
                "<MACRO ({}) {}>",
                params
                    .iter()
                    .map(Symbol::as_str)
                    .collect::<Vec<_>>()
                    .join(" "),
                body
            ),
            Value::NativeFunction(NativeFunctionValue { name, .. }) => {
                write!(f, "<NATIVE_FUNCTION {}>", name)
            }
        }
    }
}

impl IntoIterator for Value {
    type Item = Value;

    type IntoIter = ValueIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        ValueIntoIter { value: Some(self) }
    }
}

pub struct ValueIntoIter {
    pub value: Option<Value>,
}

impl Iterator for ValueIntoIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(value) = self.value.take() {
            let cell = match value {
                Value::ConsCell(cell) => cell,
                Value::Nil => {
                    self.value = None;
                    return None;
                }
                _ => {
                    return Some(value);
                }
            };

            let ConsCell { value, rest } = cell;

            self.value = Some(*rest);
            return Some(*value);
        }
        None
    }
}

pub struct ValueIter<'value> {
    pub value: Option<&'value Value>,
}

impl<'value> Iterator for ValueIter<'value> {
    type Item = &'value Value;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(value) = self.value.take() {
            let cell = match value {
                Value::ConsCell(cell) => cell,
                Value::Nil => {
                    self.value = None;
                    return None;
                }
                _ => {
                    return Some(value);
                }
            };

            let ConsCell { value, rest: next } = cell;

            self.value = Some(next.as_ref());
            return Some(value.as_ref());
        }
        None
    }
}
