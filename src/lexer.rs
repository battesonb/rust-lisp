use std::{iter::Peekable, str::Chars};

use thiserror::Error;

use crate::token::Token;

#[derive(Debug, Clone, Error)]
pub enum LexerError {
    #[error("Failed to parse string, missing closing `\"`")]
    StringMissingClosingQuote,
    #[error("Failed to parse string, incomplete escape sequence")]
    StringIncompleteEscapeSequence,
}

pub type LexerResult<T> = Result<T, LexerError>;

pub struct Lexer<'source> {
    chars: Peekable<Chars<'source>>,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            chars: source.chars().peekable(),
        }
    }

    pub fn lex(mut self) -> LexerResult<Vec<Token>> {
        let mut tokens = Vec::new();

        while let Some(c) = self.chars.peek() {
            match c {
                ';' => {
                    // this is a comment, take until the end of the line.
                    self.chars.next();
                    while self.chars.peek().is_some_and(|v| *v != '\n') {
                        self.chars.next();
                    }
                }
                '"' => {
                    tokens.push(self.lex_string()?);
                    continue;
                }
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                ' ' | '\r' | '\n' => {
                    // skip white space
                }
                _ => {
                    tokens.push(self.lex_symbol());
                    continue;
                }
            }
            self.chars.next();
        }

        Ok(tokens)
    }

    fn lex_symbol(&mut self) -> Token {
        let mut symbol = String::new();

        while let Some(c) = self.chars.peek() {
            match c {
                ' ' | '\r' | '\n' | '(' | ')' => break,
                _ => symbol.push(*c),
            }
            self.chars.next();
        }

        Token::Symbol(symbol)
    }

    fn lex_string(&mut self) -> LexerResult<Token> {
        let mut string = String::new();

        // Skip first `"`.
        self.chars.next();

        while let Some(c) = self.chars.peek() {
            match c {
                '\\' => {
                    self.chars.next();
                    let Some(next) = self.chars.peek() else {
                        return Err(LexerError::StringIncompleteEscapeSequence);
                    };
                    match next {
                        '\\' => string.push('\\'),
                        'n' => string.push_str("\n"),
                        _ => string.push(*next),
                    }
                }
                '"' => break,
                _ => string.push(*c),
            }
            self.chars.next();
        }

        if !self.chars.peek().is_some_and(|c| *c == '"') {
            return Err(LexerError::StringMissingClosingQuote);
        }

        // Skip last `"`.
        self.chars.next();

        Ok(Token::String(string))
    }
}
