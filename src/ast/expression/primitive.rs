use std::fmt::Display;

use crate::{
    error::{Error, Result},
    lexer::{Token, TokenKind},
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

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(std::string::String);

impl Identifier {
    pub fn parse(token: Token) -> Result<Self> {
        Ok(Self(token.literal.to_string()))
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<std::string::String> for Identifier {
    fn from(value: std::string::String) -> Self {
        Self(value)
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl From<Identifier> for std::string::String {
    fn from(value: Identifier) -> Self {
        value.0
    }
}

impl From<Identifier> for Expression {
    fn from(value: Identifier) -> Self {
        Expression::Ident(value)
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Bool(bool);

impl Bool {
    pub fn parse(token: Token) -> Result<Self> {
        match token.kind {
            TokenKind::True => Ok(Self(true)),
            TokenKind::False => Ok(Self(false)),
            k => {
                return Err(Error::expected_token(
                    Some(k),
                    [TokenKind::True, TokenKind::False].as_slice().into(),
                ))
            }
        }
    }
}

impl From<bool> for Bool {
    fn from(value: bool) -> Self {
        Self(value)
    }
}

impl From<Bool> for bool {
    fn from(value: Bool) -> Self {
        value.0
    }
}

impl From<Bool> for Expression {
    fn from(value: Bool) -> Self {
        Expression::BoolLiteral(value)
    }
}

impl Display for Bool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 {
            write!(f, "rbap_true")
        } else {
            write!(f, "rbap_false")
        }
    }
}
