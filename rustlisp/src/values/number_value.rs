use std::{fmt::Display, hash::Hash};

#[derive(Debug, Clone, PartialEq)]
pub enum NumberValue {
    Integer(i64),
    Float(f64),
}

impl Hash for NumberValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            NumberValue::Integer(v) => v.hash(state),
            NumberValue::Float(_) => {
                panic!("Hash is not supported for float values");
            }
        }
    }
}

impl Eq for NumberValue {}

impl Default for NumberValue {
    fn default() -> Self {
        Self::Integer(0)
    }
}

impl NumberValue {
    pub fn from_f64(value: f64) -> Self {
        if value.trunc() == value {
            NumberValue::Integer(value as i64)
        } else {
            NumberValue::Float(value)
        }
    }

    pub fn add(&self, other: &Self) -> Self {
        match (self, other) {
            (NumberValue::Integer(a), NumberValue::Integer(b)) => NumberValue::Integer(a + b),
            (NumberValue::Integer(a), NumberValue::Float(b)) => {
                NumberValue::from_f64((*a as f64) + b)
            }
            (NumberValue::Float(a), NumberValue::Integer(b)) => {
                NumberValue::from_f64(a + (*b as f64))
            }
            (NumberValue::Float(a), NumberValue::Float(b)) => NumberValue::from_f64(a + b),
        }
    }

    pub fn sub(&self, other: &Self) -> Self {
        match (self, other) {
            (NumberValue::Integer(a), NumberValue::Integer(b)) => NumberValue::Integer(a - b),
            (NumberValue::Integer(a), NumberValue::Float(b)) => {
                NumberValue::from_f64((*a as f64) - b)
            }
            (NumberValue::Float(a), NumberValue::Integer(b)) => {
                NumberValue::from_f64(a - (*b as f64))
            }
            (NumberValue::Float(a), NumberValue::Float(b)) => NumberValue::from_f64(a - b),
        }
    }

    pub fn mul(&self, other: &Self) -> Self {
        match (self, other) {
            (NumberValue::Integer(a), NumberValue::Integer(b)) => NumberValue::Integer(a * b),
            (NumberValue::Integer(a), NumberValue::Float(b)) => {
                NumberValue::from_f64((*a as f64) * b)
            }
            (NumberValue::Float(a), NumberValue::Integer(b)) => {
                NumberValue::from_f64(a * (*b as f64))
            }
            (NumberValue::Float(a), NumberValue::Float(b)) => NumberValue::from_f64(a * b),
        }
    }

    pub fn div(&self, other: &Self) -> Self {
        match (self, other) {
            (NumberValue::Integer(a), NumberValue::Integer(b)) => {
                NumberValue::from_f64((*a as f64) / (*b as f64))
            }
            (NumberValue::Integer(a), NumberValue::Float(b)) => {
                NumberValue::from_f64((*a as f64) / b)
            }
            (NumberValue::Float(a), NumberValue::Integer(b)) => {
                NumberValue::from_f64(a / (*b as f64))
            }
            (NumberValue::Float(a), NumberValue::Float(b)) => NumberValue::from_f64(a / b),
        }
    }

    pub fn less(&self, other: &Self) -> bool {
        match (self, other) {
            (NumberValue::Integer(a), NumberValue::Integer(b)) => a < b,
            (NumberValue::Integer(a), NumberValue::Float(b)) => (*a as f64) < *b,
            (NumberValue::Float(a), NumberValue::Integer(b)) => *a < (*b as f64),
            (NumberValue::Float(a), NumberValue::Float(b)) => a < b,
        }
    }

    pub fn neg(&self) -> Self {
        match self {
            NumberValue::Integer(value) => NumberValue::Integer(-value),
            NumberValue::Float(value) => NumberValue::Float(-value),
        }
    }
}

impl Display for NumberValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberValue::Integer(value) => write!(f, "{value}"),
            NumberValue::Float(value) => write!(f, "{value}"),
        }
    }
}
