use core::fmt;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Display, Write},
    rc::Rc,
};

use thiserror::Error;

#[derive(Clone, Debug, PartialEq)]
pub struct ConsCell {
    pub value: Box<Value>,
    pub rest: Box<Value>,
}

impl ConsCell {
    pub fn new(value: Value) -> Self {
        Self::new_with_boxed_value(Box::new(value))
    }

    pub fn new_with_boxed_value(value: Box<Value>) -> Self {
        ConsCell {
            value,
            rest: Box::new(Value::nil()),
        }
    }

    pub fn with_boxed_value(mut self, value: Box<Value>) -> Self {
        self.value = value;
        self
    }

    pub fn with_value(self, value: Value) -> Self {
        self.with_boxed_value(Box::new(value))
    }

    pub fn with_boxed_rest(mut self, rest: Box<Value>) -> Self {
        self.rest = rest;
        self
    }

    pub fn with_rest_option(mut self, rest: Option<Box<Value>>) -> Self {
        self.rest = rest.unwrap_or_default();
        self
    }

    pub fn with_rest(self, rest: Value) -> Self {
        // TODO: Should this panic if set without a value? I.e., an invalid state.
        self.with_boxed_rest(Box::new(rest))
    }

    pub fn next_is_nil(&self) -> bool {
        match self.rest.as_ref() {
            Value::Nil => true,
            _ => false,
        }
    }

    fn fmt_inner(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ConsCell { value, rest } = self;
        value.fmt(f)?;

        if rest.is_nil() {
            return Ok(());
        }

        write!(f, " ")?;
        match self.rest.as_ref() {
            Value::ConsCell(cell) => cell.fmt_inner(f),
            rest => rest.fmt(f),
        }
    }

    pub fn join<T>(cells: T) -> Option<ConsCell>
    where
        T: IntoIterator<Item = ConsCell>,
    {
        // TODO: Prefer not to collect into a Vec. We want to build it backwards so we return the
        // first cons cell.
        cells
            .into_iter()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .reduce(|acc, mut curr| {
                curr.rest = Box::new(Value::ConsCell(acc));
                curr
            })
    }
}

impl IntoIterator for Value {
    type Item = Value;

    type IntoIter = ValueIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        ValueIntoIter { value: Some(self) }
    }
}

impl IntoIterator for ConsCell {
    type Item = Value;

    type IntoIter = ValueIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        ValueIntoIter {
            value: Some(Value::ConsCell(self)),
        }
    }
}

impl Display for ConsCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('(')?;
        self.fmt_inner(f)?;
        f.write_char(')')?;
        Ok(())
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

#[derive(Debug, Default, Clone, PartialEq)]
pub enum Value {
    #[default]
    Nil,
    ConsCell(ConsCell),
    Symbol(Symbol),
    Integer(i64),
    Float(f64),
    Error(String),
    Function(FunctionValue),
    Macro(MacroValue),
}

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

impl Value {
    pub fn symbol(symbol: String) -> Self {
        Self::Symbol(Symbol::new(symbol))
    }

    /// Returns either the `t` Symbol or the empty list (nil) depending on the provided condition.
    pub fn cond(cond: bool) -> Self {
        if cond { Self::t() } else { Self::default() }
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

    pub fn number(value: f64) -> Value {
        if value.trunc() == value {
            Value::Integer(value as i64)
        } else {
            Value::Float(value)
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::Error(_) => true,
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
            Value::Integer(value) => write!(f, "{value}"),
            Value::Float(value) => write!(f, "{value}"),
            Value::Error(message) => write!(f, "<ERROR: {message}>"),
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
        }
    }
}

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

#[derive(Clone, Debug, PartialEq)]
pub struct MacroValue {
    pub params: Vec<Symbol>,
    pub body: ConsCell,
}

impl MacroValue {
    pub fn new(params: Vec<Symbol>, body: ConsCell) -> Self {
        Self { params, body }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
    value: String,
}

impl Symbol {
    pub fn new(value: String) -> Self {
        Self { value }
    }

    /// Returns the symbol representing "true".
    pub fn t() -> Self {
        Symbol::new("t".into())
    }

    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)?;
        Ok(())
    }
}
