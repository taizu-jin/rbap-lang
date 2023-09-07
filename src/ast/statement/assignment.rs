use std::fmt::Display;

use crate::{
    ast::Expression,
    error::Error,
    error::Result,
    lexer::{Token, TokenKind},
    parser::{
        context::{CurrentToken, PeekToken},
        Carriage,
    },
};

use super::Statement;

#[derive(Debug, PartialEq)]
pub struct Assignment {
    pub ident: String,
    pub value: Expression,
}

impl Assignment {
    pub fn parse(
        carriage: &mut Carriage,
        CurrentToken(current): CurrentToken,
        PeekToken(peek): PeekToken,
    ) -> Result<Self> {
        let ident = match (current, peek) {
            (
                Token {
                    literal,
                    kind: TokenKind::Ident,
                },
                Token {
                    kind: TokenKind::Assign,
                    ..
                },
            ) => literal.to_string(),

            (current, _) => {
                return Err(Error::parse_data_assign(
                    current.kind,
                    carriage.peek_token()?.kind,
                ))
            }
        };

        carriage.expect_tokens(&[TokenKind::Assign])?;
        let value = Statement::expect_and_parse_expression(carriage)?;

        Ok(Assignment { ident, value })
    }
}

impl From<Assignment> for Statement {
    fn from(value: Assignment) -> Self {
        Statement::Assignment(value)
    }
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.ident, self.value)
    }
}
