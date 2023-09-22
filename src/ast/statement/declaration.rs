use std::fmt::Display;

use crate::{
    error::Result,
    lexer::TokenKind,
    parser::{context::PeekToken, Carriage},
};

use super::Statement;

#[derive(Debug, PartialEq)]
pub struct Declaration(Vec<Data>);

impl Declaration {
    pub fn parse(carriage: &mut Carriage, PeekToken(peek_token): PeekToken) -> Result<Self> {
        let mut declarations = Vec::new();

        match peek_token.kind == TokenKind::Colon {
            true => {
                carriage.next_token()?;

                loop {
                    let declaration = Data::parse(carriage)?;
                    declarations.push(declaration);

                    if carriage.is_peek_token(TokenKind::Comma) {
                        carriage.next_token()?;
                    } else {
                        break;
                    }
                }
            }
            false => {
                let declaration = Data::parse(carriage)?;
                declarations.push(declaration);
            }
        }

        Ok(Self(declarations))
    }
}

impl From<Vec<Data>> for Declaration {
    fn from(value: Vec<Data>) -> Self {
        Self(value)
    }
}

impl From<Declaration> for Vec<Data> {
    fn from(value: Declaration) -> Self {
        value.0
    }
}

impl From<Declaration> for Statement {
    fn from(value: Declaration) -> Self {
        Statement::Declaration(value)
    }
}

impl AsRef<Vec<Data>> for Declaration {
    fn as_ref(&self) -> &Vec<Data> {
        &self.0
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DATA: ")?;
        if self.0.len() == 1 {
            write!(f, "{}", self.0[0])?;
        } else {
            for d in &self.0 {
                writeln!(f, "{}", d)?;
            }
        }
        write!(f, ".")
    }
}

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
pub enum DataType {
    String,
    Int,
    Bool,
    None,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::String => write!(f, "string"),
            DataType::Int => write!(f, "i"),
            DataType::Bool => write!(f, "rbap_bool"),
            DataType::None => write!(f, "null"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Data {
    pub ident: String,
    pub ty: DataType,
}

impl Data {
    pub fn parse(carriage: &mut Carriage) -> Result<Data> {
        let token = carriage.expect_tokens(&[TokenKind::Ident])?;

        let ident = if let TokenKind::Ident = token.kind {
            token.literal.to_string()
        } else {
            unreachable!("current token must be Identifier kind")
        };

        carriage.expect_tokens(&[TokenKind::Type])?;
        let token =
            carriage.expect_tokens(&[TokenKind::String, TokenKind::Int, TokenKind::Bool])?;

        let ty = match token.kind {
            TokenKind::Int => DataType::Int,
            TokenKind::String => DataType::String,
            TokenKind::Bool => DataType::Bool,
            _ => unreachable!("current token must be either Int, String or Bool kind"),
        };

        Ok(Data { ident, ty })
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} TYPE {}", self.ident, self.ty)
    }
}
