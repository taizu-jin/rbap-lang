use std::fmt::Display;

use crate::{error::Result, lexer::TokenKind, parser::Carriage};

use super::{Block, Data, Statement};

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Data>,
    pub returns: Vec<Data>,
    pub body: Block,
}

impl Function {
    pub fn parse(carriage: &mut Carriage) -> Result<Self> {
        let ident = carriage.expect_tokens(&[TokenKind::Ident])?;

        let name = ident.literal.to_string();

        let mut parameters = Vec::new();
        let mut returns = Vec::new();

        if carriage.is_peek_token(TokenKind::Importing) {
            carriage.next_token()?;

            while !carriage.is_peek_token(TokenKind::Period)
                && !carriage.is_peek_token(TokenKind::Returning)
            {
                let ddecl = Data::parse(carriage)?;
                parameters.push(ddecl);
            }
        }

        if carriage.is_peek_token(TokenKind::Returning) {
            carriage.next_token()?;

            while !carriage.is_peek_token(TokenKind::Period) {
                let ddecl = Data::parse(carriage)?;
                returns.push(ddecl);
            }
        }

        carriage.expect_tokens(&[TokenKind::Period])?;
        let body = Block::parse(carriage)?;
        carriage.expect_tokens(&[TokenKind::EndMethod])?;

        let function = Function {
            name,
            parameters,
            returns,
            body,
        };

        Ok(function)
    }
}

impl From<Function> for Statement {
    fn from(value: Function) -> Self {
        Statement::Function(value)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "METHOD {}", self.name)?;
        if !self.parameters.is_empty() {
            write!(f, " IMPORTING ")?;
            let mut iter = self.parameters.iter().peekable();
            while let Some(dd) = iter.next() {
                write!(f, "{} TYPE {}", dd.ident, dd.ty)?;
                if iter.peek().is_some() {
                    writeln!(f)?;
                }
            }
        }
        write!(f, ".")?;

        writeln!(f, "{}", self.body)?;
        writeln!(f, "ENDMETHOD.")
    }
}
