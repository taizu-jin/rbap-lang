use std::fmt::Display;

use crate::{
    ast::{primitive, Expression},
    error::Result,
    lexer::TokenKind,
    parser::{context::PeekToken, Carriage},
};

use super::Statement;

#[derive(Debug, PartialEq)]
pub struct Write(Vec<Expression>);

impl Write {
    pub fn parse(carriage: &mut Carriage, PeekToken(peek_token): PeekToken) -> Result<Self> {
        let mut expressions = Vec::new();

        match peek_token.kind == TokenKind::Colon {
            true => {
                carriage.next_token()?;

                loop {
                    if carriage.is_peek_token(TokenKind::Slash) {
                        expressions.push(primitive::String::from("\n").into());
                        carriage.next_token()?;
                    }
                    expressions.push(Expression::expect_and_parse(carriage)?);

                    if carriage.is_peek_token(TokenKind::Comma) {
                        carriage.next_token()?;
                    } else {
                        break;
                    }
                }
            }
            false => {
                if carriage.is_peek_token(TokenKind::Slash) {
                    expressions.push(primitive::String::from("\n").into());
                    carriage.next_token()?;
                }
                expressions.push(Expression::expect_and_parse(carriage)?)
            }
        }

        Ok(Self(expressions))
    }
}

impl From<Vec<Expression>> for Write {
    fn from(value: Vec<Expression>) -> Self {
        Self(value)
    }
}

impl From<Write> for Vec<Expression> {
    fn from(value: Write) -> Self {
        value.0
    }
}

impl From<Write> for Statement {
    fn from(value: Write) -> Self {
        Statement::Write(value)
    }
}

impl AsRef<Vec<Expression>> for Write {
    fn as_ref(&self) -> &Vec<Expression> {
        &self.0
    }
}

impl Display for Write {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WRITE:")?;
        for s in &self.0 {
            match s {
                Expression::StringLiteral(s) if s.as_ref() == "\n" => write!(f, " /")?,
                e => write!(f, " {}", e)?,
            }
        }
        write!(f, ".")
    }
}
