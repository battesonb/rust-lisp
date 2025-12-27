use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Display, Write},
    rc::Rc,
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct List {
    pub head: Option<Box<Link>>,
}

impl List {
    pub fn new(head: Box<Link>) -> Self {
        Self { head: Some(head) }
    }

    pub fn new_option(head: Option<Box<Link>>) -> Self {
        Self { head }
    }
}

impl List {
    pub fn iter(&self) -> LinkIter<'_> {
        LinkIter {
            link: self.head.as_ref(),
        }
    }
}

impl IntoIterator for Box<List> {
    type Item = Value;

    type IntoIter = LinkIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        LinkIntoIter { link: self.head }
    }
}

impl IntoIterator for List {
    type Item = Value;

    type IntoIter = LinkIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self).into_iter()
    }
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('(')?;
        if let Some(head) = &self.head {
            head.fmt(f)?;
        }
        f.write_char(')')?;
        Ok(())
    }
}

/// In a regular lisp, the next value could technically be any value Lisp value, not just a link.
/// Another way of thinking of this is that a linked list should terminate in a link to an empty
/// linked list (indicating `nil`). For now, we'll leave this as-is.
#[derive(Clone, Debug, PartialEq)]
pub struct Link {
    pub value: Value,
    pub next: Option<Box<Link>>,
}

impl Link {
    pub fn new(value: Value) -> Self {
        Self { value, next: None }
    }

    pub fn new_with_link(value: Value, link: Option<Box<Link>>) -> Self {
        Self { value, next: link }
    }

    pub fn with_next(mut self, next: Option<Box<Self>>) -> Self {
        self.next = next;
        self
    }

    pub fn iter(link: &Box<Self>) -> LinkIter<'_> {
        LinkIter { link: Some(link) }
    }

    pub fn join<T>(links: T) -> Option<Box<Link>>
    where
        T: IntoIterator<Item = Box<Link>>,
    {
        let mut links = links.into_iter();
        let Some(mut first_link) = links.next() else {
            return None;
        };

        let mut last_link: &mut Box<Link> = &mut first_link;

        while let Some(next) = links.next() {
            last_link.next = Some(next);
            last_link = last_link.next.as_mut().unwrap();
        }

        Some(first_link)
    }
}

impl IntoIterator for Box<Link> {
    type Item = Value;

    type IntoIter = LinkIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        LinkIntoIter { link: Some(self) }
    }
}

impl IntoIterator for Link {
    type Item = Value;

    type IntoIter = LinkIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self).into_iter()
    }
}

pub struct LinkIntoIter {
    pub link: Option<Box<Link>>,
}

impl Iterator for LinkIntoIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(link) = self.link.take() {
            let Link { value, next } = *link;
            self.link = next;
            return Some(value);
        }
        None
    }
}

pub struct LinkIter<'list> {
    pub link: Option<&'list Box<Link>>,
}

impl<'list> Iterator for LinkIter<'list> {
    type Item = &'list Value;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(link) = self.link.take() {
            self.link = link.next.as_ref();
            return Some(&link.value);
        }
        None
    }
}

impl Display for Link {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)?;
        if let Some(next) = &self.next {
            f.write_char(' ')?;
            next.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    List(Box<List>),
    Symbol(Symbol),
    Integer(i64),
    Float(f64),
    Error(String),
    Function(FunctionValue),
    Macro(MacroValue),
}

impl Value {
    pub fn list(list: List) -> Self {
        Self::List(Box::new(list))
    }

    pub fn symbol(symbol: String) -> Self {
        Self::Symbol(Symbol::new(symbol))
    }

    pub fn is_true(&self) -> bool {
        match self {
            Value::List(list) => list.head.is_some(),
            Value::Error(_) => false,
            _ => true,
        }
    }

    pub fn join<T>(iterator: T) -> Option<Box<Link>>
    where
        T: IntoIterator<Item = Value>,
    {
        let links = iterator.into_iter().map(|value| Box::new(Link::new(value)));
        Link::join(links)
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::List(Box::new(List::default()))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Symbol(symbol) => symbol.fmt(f),
            Value::List(list) => {
                list.fmt(f)?;
                Ok(())
            }
            Value::Integer(value) => write!(f, "{value}"),
            Value::Float(value) => write!(f, "{value}"),
            Value::Error(message) => write!(f, "<ERROR:\n{message}>"),
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
    pub scope: Rc<RefCell<Scope>>,
    pub params: Vec<Symbol>,
    pub body: Box<List>,
}

impl FunctionValue {
    pub fn new(params: Vec<Symbol>, body: Box<List>) -> Self {
        Self {
            scope: Rc::new(RefCell::new(Scope::default())),
            params,
            body,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Scope {
    map: HashMap<Symbol, Value>,
}

impl Scope {
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
    pub body: Box<List>,
}

impl MacroValue {
    pub fn new(params: Vec<Symbol>, body: Box<List>) -> Self {
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
