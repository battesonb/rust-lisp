use std::{borrow::Cow, fmt::Display};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
    value: Cow<'static, str>,
}

impl Symbol {
    pub fn new<S>(value: S) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self {
            value: value.into(),
        }
    }

    /// Returns the symbol representing "true".
    pub fn t() -> Self {
        Symbol::new("t")
    }

    pub fn as_str(&self) -> &str {
        self.value.as_ref()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)?;
        Ok(())
    }
}
