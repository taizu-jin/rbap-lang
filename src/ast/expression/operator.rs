use std::fmt::Display;

use crate::{error::Error, lexer::TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Add,
    Div,
    Mul,
    Sub,
    GreaterThan,
    LesserThan,
    Equal,
    NotEqual,
    Not,
}

impl Operator {
    pub fn is_boolean(&self) -> bool {
        matches!(
            self,
            Self::GreaterThan | Self::LesserThan | Self::Equal | Self::NotEqual | Self::Not
        )
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal: &'static str = Into::<&'static str>::into(*self);
        write!(f, "{}", literal)
    }
}

impl From<Operator> for &'static str {
    fn from(value: Operator) -> Self {
        match value {
            Operator::Add => "+",
            Operator::Div => "/",
            Operator::Mul => "*",
            Operator::Sub => "-",
            Operator::GreaterThan => ">",
            Operator::LesserThan => "<",
            Operator::Equal => "==",
            Operator::NotEqual => "<>",
            Operator::Not => "NOT ",
        }
    }
}

impl TryFrom<TokenKind> for Operator {
    type Error = Error;

    fn try_from(value: TokenKind) -> std::result::Result<Self, Self::Error> {
        let operator = match value {
            TokenKind::Plus => Operator::Add,
            TokenKind::Minus => Operator::Sub,
            TokenKind::Asterisk => Operator::Mul,
            TokenKind::Slash => Operator::Div,
            TokenKind::GreaterThan => Operator::GreaterThan,
            TokenKind::LesserThan => Operator::LesserThan,
            TokenKind::Equals => Operator::Equal,
            TokenKind::NotEquals => Operator::NotEqual,
            TokenKind::Not => Operator::Not,
            _ => return Err(Error::unknown_operator(value)),
        };

        Ok(operator)
    }
}
