use std::{iter::Peekable, vec::IntoIter};

use thiserror::Error;

use crate::{
    token::Token,
    values::{ConsCell, NumberValue, Symbol, Value, ValueExpectError},
};

#[derive(Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    ValueExpectError(#[from] ValueExpectError),
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
                    lists.push(list);
                }
                Token::Symbol(symbol) => {
                    lists.push(Value::Symbol(Symbol::new(symbol.clone())));
                    self.tokens.next();
                }
                Token::RParen => {
                    return Err(ParseError::UnexpectedRParen);
                }
                Token::String(value) => {
                    lists.push(Value::String(value.clone()));
                    self.tokens.next();
                },
            }
        }

        Ok(lists)
    }

    pub fn parse_list(&mut self) -> Result<Value, ParseError> {
        let Some(token) = self.tokens.next() else {
            return Err(ParseError::ExpectedLParen(None));
        };

        if !matches!(token, Token::LParen) {
            return Err(ParseError::ExpectedLParen(Some(token)));
        }

        let mut list: Option<ConsCell> = None;
        let mut prev_cell: Option<&mut ConsCell> = None;

        while let Some(token) = self.tokens.peek() {
            match token {
                Token::LParen => {
                    let result_list = self.parse_list()?;
                    let new_cell = ConsCell::new(result_list);

                    if let Some(prev) = prev_cell {
                        prev.rest = Box::new(Value::ConsCell(new_cell));
                        prev_cell = Some(prev.rest.expect_cons_cell_mut()?);
                    } else {
                        list = Some(new_cell);
                        prev_cell = list.as_mut();
                    }
                }
                Token::String(value) => {
                    let value = Value::String(value.clone());

                    let new_cell = ConsCell::new(value);

                    if let Some(prev) = prev_cell {
                        prev.rest = Box::new(Value::ConsCell(new_cell));
                        prev_cell = Some(prev.rest.expect_cons_cell_mut()?);
                    } else {
                        list = Some(new_cell);
                        prev_cell = list.as_mut();
                    }

                    // Drop the string
                    self.tokens.next();
                },
                Token::Symbol(symbol) => {
                    let value = if let Ok(value) = symbol.parse::<i64>() {
                        Value::Number(NumberValue::Integer(value))
                    } else if let Ok(value) = symbol.parse::<f64>() {
                        Value::Number(NumberValue::Float(value))
                    } else {
                        Value::symbol(symbol.clone())
                    };

                    let new_cell = ConsCell::new(value);

                    if let Some(prev) = prev_cell {
                        prev.rest = Box::new(Value::ConsCell(new_cell));
                        prev_cell = Some(prev.rest.expect_cons_cell_mut()?);
                    } else {
                        list = Some(new_cell);
                        prev_cell = list.as_mut();
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

        Ok(if let Some(list) = list {
            Value::ConsCell(list)
        } else {
            Value::Nil
        })
    }
}
