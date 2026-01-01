use std::fmt::{Display, Write};

use crate::values::value::{Value, ValueIntoIter};

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
            rest => {
                write!(f, ". ")?;
                rest.fmt(f)
            }
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

