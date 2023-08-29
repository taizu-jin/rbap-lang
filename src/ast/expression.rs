use std::fmt::Display;

use crate::{
    error::{Error, ErrorKind, ParseInfixError, Result},
    lexer::{Token, TokenKind},
    parser::{
        context::{CurrentToken, PeekToken},
        parse, Carriage, Context, Precedence,
    },
};

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: Operator,
    pub right: Box<Expression>,
}

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
            Operator::Equal => "=",
            Operator::NotEqual => "<>",
        }
    }
}

impl TryFrom<&str> for Operator {
    type Error = Error;

    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        let operator = match value {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            ">" => Operator::GreaterThan,
            "<" => Operator::LesserThan,
            "=" => Operator::Equal,
            "<>" => Operator::NotEqual,
            _ => return Err(Error::unknown_operator(value.to_string())),
        };

        Ok(operator)
    }
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(i64),
    StringLiteral(String),
    Ident(String),
    StringTemplate(Vec<Expression>),
    InfixExpression(InfixExpression),
    PrefixExpression(PrefixExpression),
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
            TokenKind::IntLiteral => Self::parse_int_literal_expression(current),
            TokenKind::StringLiteral => Self::parse_string_expression(current),
            TokenKind::Ident => Self::parse_ident_expression(current),
            TokenKind::VSlash => Self::parse_string_template_expression(carriage, peek),
            TokenKind::Minus => Self::parse_prefix_expression(carriage, current),
            _ => Err(Error::parse_expression(&current)),
        }
    }

    fn parse_prefix_expression(carriage: &mut Carriage, current: Token) -> Result<Self> {
        let mut context = Context::from_carriage(carriage)?;
        context.precedence = Precedence::Perfix;

        let expression = parse(carriage, &context, Self::parse)?;

        let expression = PrefixExpression {
            operator: Operator::try_from(current.literal.as_ref())?,
            right: Box::new(expression),
        };

        Ok(Expression::PrefixExpression(expression))
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
        let expression = parse(carriage, &context, Self::parse_infix_expression)?;
        Ok(expression)
    }

    fn is_infix_token(kind: TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Plus | TokenKind::Minus | TokenKind::Slash | TokenKind::Asterisk
        )
    }

    fn parse_infix_expression(
        carriage: &mut Carriage,
        CurrentToken(current): CurrentToken,
        left: Option<Expression>,
    ) -> Result<Expression> {
        let left = if let Some(left) = left {
            left
        } else {
            return Err(Error::from(ParseInfixError::LeftExpression));
        };

        let mut context = Context::from_carriage(carriage)?;
        context.precedence = Precedence::from(&current);

        let right = parse(carriage, &context, Expression::parse)?;

        let expression = InfixExpression {
            left: Box::new(left),
            operator: Operator::try_from(current.literal.as_ref())?,
            right: Box::new(right),
        };

        Ok(Expression::InfixExpression(expression))
    }

    fn parse_int_literal_expression(token: Token) -> Result<Self> {
        let literal = token.literal.parse::<i64>().map_err(Error::from)?;
        Ok(Expression::IntLiteral(literal))
    }

    fn parse_string_expression(token: Token) -> Result<Self> {
        Ok(Expression::StringLiteral(token.literal.to_string()))
    }

    fn parse_ident_expression(token: Token) -> Result<Self> {
        Ok(Expression::Ident(token.literal.to_string()))
    }

    fn parse_string_template_expression(carriage: &mut Carriage, peek: Token) -> Result<Self> {
        let tokens = &[
            TokenKind::StringLiteral,
            TokenKind::LSquirly,
            TokenKind::VSlash,
        ];

        if !tokens.contains(&peek.kind) {
            return Err(Error::expected_token(None, tokens.as_slice().into()));
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

    fn parse_string_template(carriage: &mut Carriage) -> Option<Result<Expression>> {
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
            _ => Some(Err(Error::parse_string_template(&context.current_token))),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IntLiteral(il) => write!(f, "{}", il),
            Expression::StringLiteral(sl) => write!(f, "{}", sl),
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::StringTemplate(st) => {
                write!(f, "|")?;
                for s in st {
                    write!(f, "{}", s)?;
                }
                write!(f, "|")
            }
            Expression::InfixExpression(ie) => {
                write!(f, "({} {} {})", ie.left, ie.operator, ie.right)
            }
            Expression::PrefixExpression(pe) => write!(f, "({}{})", pe.operator, pe.right),
        }
    }
}
