use std::fmt::Display;

use crate::{
    error::{Error, Result},
    lexer::Token,
};

use super::Expression;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Int(i64);

impl Int {
    pub fn parse(token: Token) -> Result<Self> {
        let literal = token.literal.parse::<i64>().map_err(Error::from)?;
        Ok(Self(literal))
    }
}

impl From<i64> for Int {
    fn from(value: i64) -> Self {
        Self(value)
    }
}

impl From<Int> for i64 {
    fn from(value: Int) -> Self {
        value.0
    }
}

impl From<Int> for Expression {
    fn from(value: Int) -> Self {
        Expression::IntLiteral(value)
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
