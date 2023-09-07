use std::fmt::Display;

use crate::{
    ast::Expression,
    error::Result,
    lexer::TokenKind,
    parser::{parse, Carriage, Context},
};

use super::{Block, Statement};

#[derive(Debug, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub consequence: Block,
    pub alternative: Option<Block>,
}

impl IfStatement {
    pub fn parse(carriage: &mut Carriage) -> Result<Self> {
        let context = Context::from_carriage(carriage)?;

        let condition = parse(carriage, &context, Expression::parse)?;

        carriage.expect_tokens(&[TokenKind::Period])?;

        let consequence = Block::parse(carriage)?;

        let alternative = if carriage.is_peek_token(TokenKind::Else) {
            carriage.expect_tokens(&[TokenKind::Else])?;
            carriage.expect_tokens(&[TokenKind::Period])?;

            Some(Block::parse(carriage)?)
        } else {
            None
        };

        carriage.expect_tokens(&[TokenKind::EndIf])?;

        let statement = IfStatement {
            condition,
            consequence,
            alternative,
        };

        Ok(statement)
    }
}

impl From<IfStatement> for Statement {
    fn from(value: IfStatement) -> Self {
        Statement::If(value)
    }
}

impl Display for IfStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "IF {}.", self.condition)?;
        writeln!(f, "{}", self.consequence)?;
        if let Some(alternative) = &self.alternative {
            writeln!(f, "ELSE.")?;
            writeln!(f, "{}", alternative)?;
        }
        write!(f, "ENDIF.")
    }
}
