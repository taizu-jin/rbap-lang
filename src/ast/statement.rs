mod assignment;
mod block;
mod declaration;
mod function;
mod if_statement;

use crate::{
    error::Result,
    lexer::TokenKind,
    parser::{context::PeekToken, parse, Carriage, Context},
};

use std::fmt::Display;

pub use self::{
    assignment::Assignment,
    block::Block,
    declaration::{Data, DataType, Declaration},
    function::Function,
    if_statement::IfStatement,
};

use super::{primitive, Expression};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Write(Vec<Expression>),
    Declaration(Declaration),
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
            TokenKind::Write => parse(carriage, &context, Self::parse_write_statement)?,
            TokenKind::If => parse(carriage, &context, IfStatement::parse)?.into(),
            TokenKind::Method => parse(carriage, &context, Function::parse)?.into(),
            _ => Statement::Expression(parse(carriage, &context, Expression::parse)?),
        };

        carriage.expect_tokens(&[TokenKind::Period])?;

        Ok(statement)

    fn parse_write_statement(
        carriage: &mut Carriage,
        PeekToken(peek_token): PeekToken,
    ) -> Result<Self> {
        let mut expressions = Vec::new();

        match peek_token.kind == TokenKind::Colon {
            true => {
                carriage.next_token()?;

                loop {
                    if carriage.is_peek_token(TokenKind::Slash) {
                        expressions.push(primitive::String::from("\n").into());
                        carriage.next_token()?;
                    }

                    expressions.push(Self::expect_and_parse_expression(carriage)?);

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
                expressions.push(Self::expect_and_parse_expression(carriage)?)
            }
        }

        Ok(Statement::Write(expressions))
    }

    fn expect_and_parse_expression(carriage: &mut Carriage) -> Result<Expression> {
        let token = carriage.expect_tokens(&[
            TokenKind::Ident,
            TokenKind::IntLiteral,
            TokenKind::StringLiteral,
            TokenKind::VSlash,
        ])?;

        let context = Context::new(token, carriage.peek_token()?.clone());

        parse(carriage, &context, Expression::parse)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expression(e) => write!(f, "{}.", e),
            Statement::Function(func) => write!(f, "{}", func),
            Statement::Block(b) => write!(f, "{}", b),
            Statement::Write(strings) => {
                write!(f, "WRITE:")?;
                for s in strings {
                    match s {
                        Expression::StringLiteral(s) if s.as_ref() == "\n" => write!(f, " /")?,
                        e => write!(f, " {}", e)?,
                    }
                }
                write!(f, ".")
            }
            Statement::If(s) => write!(f, "{}", s),
            Statement::Assignment(d) => write!(f, "{}", d),
            Statement::Declaration(d) => write!(f, "{}", d),
            Statement::Write(w) => write!(f, "{}", w),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum DataType {
    String,
    Int,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::String => write!(f, "string"),
            DataType::Int => write!(f, "i"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct DataDeclaration {
    pub ident: String,
    pub ty: DataType,
}

impl Display for DataDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} TYPE {}", self.ident, self.ty)
    }
}
