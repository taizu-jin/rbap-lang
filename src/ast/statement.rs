use std::fmt::Display;

use crate::{
    error::{Error, Result},
    lexer::{Token, TokenKind},
    parser::{context::CurrentToken, context::PeekToken, parse, Carriage, Context},
};

use super::{Block, Data, DataDeclaration, DataType, Expression};

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    DataDeclaration(Vec<DataDeclaration>),
    Write(Vec<Expression>),
    Data(Data),
    Block(Block),
}

impl Statement {
    pub fn parse(carriage: &mut Carriage) -> Result<Statement> {
        let context = Context::from_carriage(carriage)?;

        let statement = match context.current_token.kind {
            TokenKind::Data => parse(carriage, &context, Self::parse_data_declaration_statement)?,
            TokenKind::DataInline => {
                parse(carriage, &context, Self::parse_data_assignment_statement)?
            }
            TokenKind::Ident if context.peek_token.kind == TokenKind::Assign => {
                parse(carriage, &context, Self::parse_data_assignment_statement)?
            }
            TokenKind::Write => parse(carriage, &context, Self::parse_write_statement)?,
            _ => Statement::Expression(parse(carriage, &context, Expression::parse)?),
        };

        carriage.expect_tokens(&[TokenKind::Period])?;

        Ok(statement)
    }

    fn parse_data_declaration_statement(
        carriage: &mut Carriage,
        PeekToken(peek_token): PeekToken,
    ) -> Result<Statement> {
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
    ) -> Result<Statement> {
        let ident = match (current, peek) {
            (
                Token {
                    literal,
                    kind: TokenKind::DataInline,
                },
                _,
            ) => literal.to_string(),

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
    ) -> Result<Statement> {
        let mut expressions = Vec::new();

        match peek_token.kind == TokenKind::Colon {
            true => {
                carriage.next_token()?;

                loop {
                    if carriage.is_peek_token(TokenKind::Slash) {
                        expressions.push(Expression::StringLiteral("\n".to_string()));
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
                    expressions.push(Expression::StringLiteral("\n".to_string()));
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
            Statement::Block(b) => write!(f, "{}", b),
            Statement::Expression(e) => write!(f, "{}.", e),
            Statement::Data(d) => write!(f, "{}.", d),
            Statement::DataDeclaration(dd) => {
                write!(f, "DATA:")?;
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
                        Expression::StringLiteral(s) if s == "\n" => write!(f, " /")?,
                        e => write!(f, " {}", e)?,
                    }
                }
                write!(f, ".")
            }
        }
    }
}
