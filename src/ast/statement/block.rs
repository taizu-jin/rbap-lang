use std::fmt::Display;

use crate::{ast::Node, error::Result, lexer::TokenKind, parser::Carriage};

use super::Statement;

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn parse(carriage: &mut Carriage) -> Result<Self> {
        let mut statements = Vec::new();

        while !(carriage.is_peek_token(TokenKind::EndIf)
            || carriage.is_peek_token(TokenKind::Else)
            || carriage.is_peek_token(TokenKind::EndMethod))
        {
            let statement = Statement::parse(carriage)?;
            statements.push(statement);
        }

        let block = Block { statements };

        Ok(block)
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s)?;
        }

        Ok(())
    }
}

impl From<Block> for Node {
    fn from(value: Block) -> Self {
        Node::Statement(Statement::Block(value))
    }
}
