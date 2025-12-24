use std::{iter::Peekable, str::Chars};

use crate::token::Token;

pub struct Lexer<'source> {
    chars: Peekable<Chars<'source>>,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            chars: source.chars().peekable(),
        }
    }

    pub fn lex(mut self) -> Vec<Token> {
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

        tokens
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
}
