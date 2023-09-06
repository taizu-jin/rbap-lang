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

#[derive(Debug, PartialEq, Clone)]
pub struct String(std::string::String);

impl String {
    pub fn parse(token: Token) -> Result<Self> {
        Ok(Self(token.literal.to_string()))
    }
}

impl AsRef<str> for String {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<std::string::String> for String {
    fn from(value: std::string::String) -> Self {
        Self(value)
    }
}

impl From<&str> for String {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl From<String> for std::string::String {
    fn from(value: String) -> Self {
        value.0
    }
}

impl From<String> for Expression {
    fn from(value: String) -> Self {
        Expression::StringLiteral(value)
    }
}

impl Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
