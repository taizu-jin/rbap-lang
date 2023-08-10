use crate::{
    lexer::{Token, TokenKind},
    parser::{context::Current, context::Peek, parse, Carriage, Context, Error, Result},
};

use super::{Data, DataDeclaration, DataType, Expression};

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    DataDeclaration(Vec<DataDeclaration>),
    Write(Vec<Expression>),
    Data(Data),
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
        Peek(peek_token): Peek,
    ) -> Result<Statement> {
        let mut declarations = Vec::new();

        match peek_token.kind == TokenKind::Colon {
            true => {
                carriage.next_token()?;

                loop {
                    let declaration = Self::parse_data_declaration(carriage)?;
                    declarations.push(declaration);

                    if carriage.is_peek_token(&TokenKind::Comma) {
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
        Current(current): Current,
        Peek(peek): Peek,
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
                return Err(Error::ParseDataAssign {
                    current: current.kind.to_owned(),
                    peek: carriage.peek_token()?.kind.to_owned(),
                })
            }
        };

        carriage.expect_tokens(&[TokenKind::Assign])?;
        let value = Self::expect_and_parse_expression(carriage)?;

        Ok(Statement::Data(Data { ident, value }))
    }

    fn parse_write_statement(carriage: &mut Carriage, Peek(peek_token): Peek) -> Result<Statement> {
        let mut expressions = Vec::new();

        match peek_token.kind == TokenKind::Colon {
            true => {
                carriage.next_token()?;

                loop {
                    if carriage.is_peek_token(&TokenKind::Slash) {
                        expressions.push(Expression::StringLiteral("\n".to_string()));
                        carriage.next_token()?;
                    }

                    expressions.push(Self::expect_and_parse_expression(carriage)?);

                    if carriage.is_peek_token(&TokenKind::Comma) {
                        carriage.next_token()?;
                    } else {
                        break;
                    }
                }
            }
            false => {
                if carriage.is_peek_token(&TokenKind::Slash) {
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