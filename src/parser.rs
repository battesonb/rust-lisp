use std::{iter::Peekable, vec::IntoIter};

use thiserror::Error;

use crate::{
    ast::{Link, List, Symbol, Value},
    token::Token,
};

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unexpected ')', expected an expression (list or symbol)")]
    UnexpectedRParen,
    #[error("Expected ')' symbol to end list, received {0:?}")]
    ExpectedRParen(Option<Token>),
    #[error("Expected '(' symbol to end list, received {0:?}")]
    ExpectedLParen(Option<Token>),
}

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Value>, ParseError> {
        let mut lists = Vec::new();

        while let Some(token) = self.tokens.peek() {
            match token {
                Token::LParen => {
                    let list = self.parse_list()?;
                    lists.push(Value::List(Box::new(list)));
                }
                Token::Symbol(symbol) => {
                    lists.push(Value::Symbol(Symbol::new(symbol.clone())));
                    self.tokens.next();
                }
                Token::RParen => {
                    return Err(ParseError::UnexpectedRParen);
                }
            }
        }

        Ok(lists)
    }

    pub fn parse_list(&mut self) -> Result<List, ParseError> {
        let mut list = List::default();

        let Some(token) = self.tokens.next() else {
            return Err(ParseError::ExpectedLParen(None));
        };

        if !matches!(token, Token::LParen) {
            return Err(ParseError::ExpectedLParen(Some(token)));
        }

        let mut prev_link: Option<&mut Box<Link>> = None;

        while let Some(token) = self.tokens.peek() {
            match token {
                Token::LParen => {
                    let result_list = self.parse_list()?;
                    let result_list = Box::new(result_list);

                    let new_link = Box::new(Link::new(Value::List(result_list)));

                    if let Some(prev) = prev_link {
                        prev.next = Some(new_link);
                        prev_link = Some(prev.next.as_mut().unwrap());
                    } else {
                        list.head = Some(new_link);
                        prev_link = list.head.as_mut();
                    }
                }
                Token::Symbol(symbol) => {
                    let value = if let Ok(value) = symbol.parse::<i64>() {
                        Value::Integer(value)
                    } else if let Ok(value) = symbol.parse::<f64>() {
                        Value::Float(value)
                    } else {
                        Value::symbol(symbol.clone())
                    };

                    let new_link = Box::new(Link::new(value));

                    if let Some(prev) = prev_link {
                        prev.next = Some(new_link);
                        prev_link = Some(prev.next.as_mut().unwrap());
                    } else {
                        list.head = Some(new_link);
                        prev_link = list.head.as_mut();
                    }

                    // Drop the symbol
                    self.tokens.next();
                }
                Token::RParen => {
                    break;
                }
            }
        }

        let Some(token) = self.tokens.next() else {
            return Err(ParseError::ExpectedRParen(None));
        };

        if !matches!(token, Token::RParen) {
            return Err(ParseError::ExpectedRParen(Some(token)));
        }

        Ok(list)
    }
}
