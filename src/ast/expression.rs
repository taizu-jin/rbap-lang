mod call;
mod infix;
mod operator;
mod prefix;
pub mod primitive;
mod string_template;

use std::fmt::Display;

use crate::{
    error::{Error, ErrorKind, ParseInfixError, Result},
    lexer::TokenKind,
    parser::{
        context::{CurrentToken, PeekToken},
        parse, Carriage, Context, Precedence,
    },
};

pub use self::{
    call::Call, infix::Infix, operator::Operator, prefix::Prefix, string_template::StringTemplate,
};

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(primitive::Int),
    StringLiteral(primitive::String),
    Ident(primitive::Identifier),
    BoolLiteral(primitive::Bool),
    StringTemplate(StringTemplate),
    InfixExpression(Infix),
    PrefixExpression(Prefix),
    CallExpression(Call),
}

impl Expression {
    pub fn parse(
        carriage: &mut Carriage,
        current: CurrentToken,
        peek: PeekToken,
        precedence: Precedence,
    ) -> Result<Self> {
        let mut expression = Self::parse_prefix(carriage, current, peek.clone())?;

        while !carriage.is_peek_token(TokenKind::Period)
            && precedence < carriage.peek_precedence()?
        {
            let peek_kind = carriage.peek_token()?.kind;
            expression = match Self::parse_infix(carriage, peek_kind, expression) {
                Ok(expression) => expression,
                Err(e) if e.kind() == ErrorKind::ParseInfixUnexpectedToken => {
                    return Ok(e
                        .expression()
                        .expect("ParseInfixUnexpectedToken always carries an expression"))
                }
                Err(e) => return Err(e),
            };
        }

        Ok(expression)
    }

    fn parse_prefix(
        carriage: &mut Carriage,
        CurrentToken(current): CurrentToken,
        PeekToken(peek): PeekToken,
    ) -> Result<Self> {
        match current.kind {
            TokenKind::IntLiteral => Ok(primitive::Int::parse(current)?.into()),
            TokenKind::StringLiteral => Ok(primitive::String::parse(current)?.into()),
            TokenKind::LParen => Self::parse_grouped_expression(carriage),
            TokenKind::True | TokenKind::False => Ok(primitive::Bool::parse(current)?.into()),
            TokenKind::Ident => Ok(primitive::Identifier::parse(current)?.into()),
            TokenKind::VSlash => Ok(StringTemplate::parse(carriage, peek)?.into()),
            TokenKind::Minus | TokenKind::Not => Ok(Prefix::parse(carriage, current)?.into()),
            _ => Err(Error::parse_expression(&current)),
        }
    }

    fn get_expression_tokens<'a>() -> &'a [TokenKind] {
        &[
            TokenKind::Ident,
            TokenKind::IntLiteral,
            TokenKind::StringLiteral,
            TokenKind::VSlash,
            TokenKind::Minus,
            TokenKind::True,
            TokenKind::False,
            TokenKind::LParen,
        ]
    }

    fn parse_infix(
        carriage: &mut Carriage,
        peek_kind: TokenKind,
        expression: Expression,
    ) -> Result<Self> {
        if !Self::is_infix_token(peek_kind) {
            return Err(Error::from(ParseInfixError::UnexpectedToken {
                token: peek_kind,
                expression,
            }));
        }

        let mut context = Context::from_carriage(carriage)?;
        context.set_expression(expression);
        let expression = match &context.current_token.kind {
            TokenKind::LParen => parse(carriage, &context, Call::parse)?.into(),
            _ => parse(carriage, &context, Infix::parse)?.into(),
        };

        Ok(expression)
    }

    fn is_infix_token(kind: TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Slash
                | TokenKind::Asterisk
                | TokenKind::GreaterThan
                | TokenKind::LesserThan
                | TokenKind::Equals
                | TokenKind::NotEquals
                | TokenKind::LParen
                | TokenKind::And
                | TokenKind::Or
        )
    }

    fn parse_grouped_expression(carriage: &mut Carriage) -> Result<Self> {
        let context = Context::from_carriage(carriage)?;

        let expression = parse(carriage, &context, Self::parse)?;

        carriage.expect_tokens(&[TokenKind::RParen])?;

        Ok(expression)
    }

    fn parse_expression_list(carriage: &mut Carriage) -> Result<Vec<Expression>> {
        let mut list = Vec::new();

        if carriage.is_peek_token(TokenKind::RParen) {
            carriage.next_token()?;
            return Ok(list);
        }

        let context = Context::from_carriage(carriage)?;
        let expression = parse(carriage, &context, Self::parse)?;
        list.push(expression);

        while carriage.is_peek_token(TokenKind::Comma) {
            carriage.next_token()?;
            let context = Context::from_carriage(carriage)?;
            let expression = parse(carriage, &context, Self::parse)?;
            list.push(expression);
        }

        carriage.expect_tokens(&[TokenKind::RParen])?;

        Ok(list)
    }

    pub fn expect_and_parse(carriage: &mut Carriage) -> Result<Expression> {
        let token = carriage.expect_tokens(Self::get_expression_tokens())?;

        let context = Context::new(token, carriage.peek_token()?.clone());

        parse(carriage, &context, Expression::parse)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IntLiteral(il) => write!(f, "{}", il),
            Expression::StringLiteral(sl) => write!(f, "{}", sl),
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::InfixExpression(ie) => write!(f, "{}", ie),
            Expression::StringTemplate(st) => write!(f, "{}", st),
            Expression::BoolLiteral(b) => write!(f, "{}", b),
            Expression::PrefixExpression(pe) => write!(f, "{}", pe),
            Expression::CallExpression(ce) => write!(f, "{}", ce),
        }
    }
}

impl From<i64> for Expression {
    fn from(value: i64) -> Self {
        Expression::IntLiteral(value.into())
    }
}
