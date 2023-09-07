use std::fmt::Display;
mod block;
mod if_statement;

use crate::{
    error::{Error, Result},
    lexer::{Token, TokenKind},
    parser::{context::CurrentToken, context::PeekToken, parse, Carriage, Context},
};

pub use self::{block::Block, if_statement::IfStatement};

use super::{primitive, Expression};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    DataDeclaration(Vec<DataDeclaration>),
    Write(Vec<Expression>),
    Data(Data),
    Block(Block),
    If(IfStatement),
    Function(Function),
}

impl Statement {
    pub fn parse(carriage: &mut Carriage) -> Result<Self> {
        let context = Context::from_carriage(carriage)?;

        let statement = match context.current_token.kind {
            TokenKind::Data => parse(carriage, &context, Self::parse_data_declaration_statement)?,
            TokenKind::Ident if context.peek_token.kind == TokenKind::Assign => {
                parse(carriage, &context, Self::parse_data_assignment_statement)?
            }
            TokenKind::Write => parse(carriage, &context, Self::parse_write_statement)?,
            TokenKind::If => parse(carriage, &context, IfStatement::parse)?.into(),
            TokenKind::Method => parse(carriage, &context, Self::parse_function_statement)?,
            _ => Statement::Expression(parse(carriage, &context, Expression::parse)?),
        };

        carriage.expect_tokens(&[TokenKind::Period])?;

        Ok(statement)
    }

    fn parse_function_statement(carriage: &mut Carriage) -> Result<Self> {
        let ident = carriage.expect_tokens(&[TokenKind::Ident])?;

        let name = ident.literal.to_string();

        let mut parameters = Vec::new();
        let mut returns = Vec::new();

        if carriage.is_peek_token(TokenKind::Importing) {
            carriage.next_token()?;

            while !carriage.is_peek_token(TokenKind::Period)
                && !carriage.is_peek_token(TokenKind::Returning)
            {
                let ddecl = Self::parse_data_declaration(carriage)?;
                parameters.push(ddecl);
            }
        }

        if carriage.is_peek_token(TokenKind::Returning) {
            carriage.next_token()?;

            while !carriage.is_peek_token(TokenKind::Period) {
                let ddecl = Self::parse_data_declaration(carriage)?;
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

        Ok(Statement::Function(function))
    }

    fn parse_data_declaration_statement(
        carriage: &mut Carriage,
        PeekToken(peek_token): PeekToken,
    ) -> Result<Self> {
        let mut declarations = Vec::new();

        match peek_token.kind == TokenKind::Colon {
            true => {
                carriage.next_token()?;

                loop {
                    let declaration = Self::parse_data_declaration(carriage)?;
                    declarations.push(declaration);

                    if carriage.is_peek_token(TokenKind::Comma) {
                        carriage.next_token()?;
                    } else {
                        break;
                    }
                }
            }
            false => {
                let declaration = Self::parse_data_declaration(carriage)?;
                declarations.push(declaration);
            }
        }

        Ok(Statement::DataDeclaration(declarations))
    }

    fn parse_data_declaration(carriage: &mut Carriage) -> Result<DataDeclaration> {
        let token = carriage.expect_tokens(&[TokenKind::Ident])?;

        let ident = if let TokenKind::Ident = token.kind {
            token.literal.to_string()
        } else {
            unreachable!("current token must be Identifier kind")
        };

        carriage.expect_tokens(&[TokenKind::Type])?;
        let token = carriage.expect_tokens(&[TokenKind::String, TokenKind::Int])?;

        let ty = match token.kind {
            TokenKind::Int => DataType::Int,
            TokenKind::String => DataType::String,
            _ => unreachable!("current token must be either Int or String kind"),
        };

        Ok(DataDeclaration { ident, ty })
    }

    fn parse_data_assignment_statement(
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
        let value = Self::expect_and_parse_expression(carriage)?;

        Ok(Statement::Data(Data { ident, value }))
    }

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
            Statement::Function(func) => write!(f, "{}", func),
            Statement::Block(b) => write!(f, "{}", b),
            Statement::Expression(e) => write!(f, "{}.", e),
            Statement::Data(d) => write!(f, "{}.", d),
            Statement::DataDeclaration(dd) => {
                write!(f, "DATA: ")?;
                if dd.len() == 1 {
                    write!(f, "{}", dd[0])?;
                } else {
                    for d in dd {
                        writeln!(f, "{}", d)?;
                    }
                }
                write!(f, ".")
            }
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
            Statement::If(s) => write!(f, "{}.", s),
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
pub struct Data {
    pub ident: String,
    pub value: Expression,
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.ident, self.value)
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

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<DataDeclaration>,
    pub returns: Vec<DataDeclaration>,
    pub body: Block,
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
