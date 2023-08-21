use std::borrow::Cow;

use crate::{
    lexer::{Token, TokenKind},
    parser::{context::Current, context::Peek, parse, Carriage, Context, Error, Result},
};

#[derive(Debug, PartialEq)]
pub struct InfixExpression<'a> {
    pub left: Box<Expression<'a>>,
    pub operator: Cow<'a, str>,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression<'a> {
    pub operator: Cow<'a, str>,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    IntLiteral(i64),
    StringLiteral(String),
    Ident(String),
    StringTemplate(Vec<Expression<'a>>),
    InfixExpression(InfixExpression<'a>),
    PrefixExpression(PrefixExpression<'a>),
}

impl<'a> Expression<'a> {
    pub fn parse(
        carriage: &mut Carriage,
        Current(current): Current,
        Peek(peek): Peek,
    ) -> Result<Self> {
        let expression = match current.kind {
            TokenKind::IntLiteral => Self::parse_int_literal_expression(current)?,
            TokenKind::StringLiteral => {
                Expression::StringLiteral(Self::parse_string_expression(current))
            }
            TokenKind::Ident => Expression::Ident(Self::parse_string_expression(current)),
            TokenKind::VSlash => Self::parse_string_template_expression(carriage, peek)?,
            token => unimplemented!("can't parse expression '{}({:?})", token, token),
        };

        Ok(expression)
    }

    fn parse_int_literal_expression(token: Token) -> Result<Self> {
        let literal = token.literal.parse::<i64>().map_err(Error::ParseInt)?;
        Ok(Expression::IntLiteral(literal))
    }

    fn parse_string_expression(token: Token) -> String {
        token.literal.to_string()
    }

    fn parse_string_template_expression(carriage: &mut Carriage, peek: Token) -> Result<Self> {
        let tokens = &[
            TokenKind::StringLiteral,
            TokenKind::LSquirly,
            TokenKind::VSlash,
        ];

        if !tokens.contains(&peek.kind) {
            return Err(Error::ExpectToken {
                got: None,
                expected: tokens.as_slice().into(),
            });
        }

        let mut expressions = Vec::new();

        while let Some(expression) = Self::parse_string_template(carriage) {
            let expression = expression?;

            match expression {
                Expression::StringLiteral(literal) if literal.is_empty() => continue,
                expression => expressions.push(expression),
            }
        }

        Ok(Expression::StringTemplate(expressions))
    }

    fn parse_string_template(carriage: &mut Carriage) -> Option<Result<Expression<'a>>> {
        let context = match Context::from_carriage(carriage) {
            Ok(context) => context,
            Err(err) => return Some(Err(err)),
        };

        match context.current_token.kind {
            TokenKind::StringLiteral => {
                let expression = match parse(carriage, &context, Expression::parse) {
                    Ok(expression) => expression,
                    Err(err) => return Some(Err(err)),
                };

                Some(Ok(expression))
            }
            TokenKind::LSquirly => {
                let context = match Context::from_carriage(carriage) {
                    Ok(context) => context,
                    Err(err) => return Some(Err(err)),
                };

                let expression = match parse(carriage, &context, Expression::parse) {
                    Ok(expression) => expression,
                    Err(e) => return Some(Err(e)),
                };

                match carriage.expect_tokens(&[TokenKind::RSquirly]) {
                    Ok(_) => (),
                    Err(err) => return Some(Err(err)),
                };

                Some(Ok(expression))
            }
            TokenKind::VSlash => None,
            _ => Some(Err(Error::ParseStringTemplate {
                kind: context.current_token.kind,
                literal: context.current_token.literal.to_string(),
            })),
        }
    }
}
