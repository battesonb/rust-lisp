#[derive(Clone, Debug)]
pub enum Token {
    /// From the perspective of the lexer, anything separated by white space is a "symbol". The
    /// parser is responsible for providing semantic meaning such as integer, symbol, etc.
    Symbol(String),
    /// Strings are modelled separately since they can have whitespace, unlike symbols.
    String(String),
    RParen,
    LParen,
}
