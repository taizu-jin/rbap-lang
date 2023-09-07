mod assignment;
mod block;
mod declaration;
mod function;
mod if_statement;
mod write;

use crate::{
    error::Result,
    lexer::TokenKind,
    parser::{parse, Carriage, Context},
};

use std::fmt::Display;

pub use self::{
    assignment::Assignment,
    block::Block,
    declaration::{Data, DataType, Declaration},
    function::Function,
    if_statement::IfStatement,
    write::Write,
};

use super::Expression;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Declaration(Declaration),
    Write(Write),
    Assignment(Assignment),
    Block(Block),
    If(IfStatement),
    Function(Function),
}

impl Statement {
    pub fn parse(carriage: &mut Carriage) -> Result<Self> {
        let context = Context::from_carriage(carriage)?;

        let statement = match context.current_token.kind {
            TokenKind::Data => parse(carriage, &context, Declaration::parse)?.into(),
            TokenKind::Ident if context.peek_token.kind == TokenKind::Assign => {
                parse(carriage, &context, Assignment::parse)?.into()
            }
            TokenKind::Write => parse(carriage, &context, Write::parse)?.into(),
            TokenKind::If => parse(carriage, &context, IfStatement::parse)?.into(),
            TokenKind::Method => parse(carriage, &context, Function::parse)?.into(),
            _ => Statement::Expression(parse(carriage, &context, Expression::parse)?),
        };

        carriage.expect_tokens(&[TokenKind::Period])?;

        Ok(statement)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expression(e) => write!(f, "{}.", e),
            Statement::Function(func) => write!(f, "{}", func),
            Statement::Block(b) => write!(f, "{}", b),
            Statement::If(s) => write!(f, "{}", s),
            Statement::Assignment(d) => write!(f, "{}", d),
            Statement::Declaration(d) => write!(f, "{}", d),
            Statement::Write(w) => write!(f, "{}", w),
        }
    }
}
