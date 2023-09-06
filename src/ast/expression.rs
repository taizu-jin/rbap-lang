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
pub struct Infix {
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
    Not,
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
            Operator::Equal => "==",
            Operator::NotEqual => "<>",
            Operator::Not => "NOT ",
        }
    }
}

impl TryFrom<TokenKind> for Operator {
    type Error = Error;

    fn try_from(value: TokenKind) -> std::result::Result<Self, Self::Error> {
        let operator = match value {
            TokenKind::Plus => Operator::Add,
            TokenKind::Minus => Operator::Sub,
            TokenKind::Asterisk => Operator::Mul,
            TokenKind::Slash => Operator::Div,
            TokenKind::GreaterThan => Operator::GreaterThan,
            TokenKind::LesserThan => Operator::LesserThan,
            TokenKind::Equals => Operator::Equal,
            TokenKind::NotEquals => Operator::NotEqual,
            TokenKind::Not => Operator::Not,
            _ => return Err(Error::unknown_operator(value)),
        };

        Ok(operator)
    }
}

#[derive(Debug, PartialEq)]
pub struct Prefix {
    pub operator: Operator,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(i64),
    StringLiteral(String),
    Ident(String),
    BoolLiteral(bool),
    StringTemplate(Vec<Expression>),
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
            TokenKind::IntLiteral => Self::parse_int_literal_expression(current),
            TokenKind::StringLiteral => Self::parse_string_expression(current),
            TokenKind::LParen => Self::parse_grouped_expression(carriage),
            TokenKind::True | TokenKind::False => Self::parse_bool_literal_expression(current),
            TokenKind::Ident => Self::parse_ident_expression(current),
            TokenKind::VSlash => Self::parse_string_template_expression(carriage, peek),
            TokenKind::Minus | TokenKind::Not => Self::parse_prefix_expression(carriage, current),
            _ => Err(Error::parse_expression(&current)),
        }
    }

    fn parse_grouped_expression(carriage: &mut Carriage) -> Result<Self> {
        let context = Context::from_carriage(carriage)?;

        let expression = parse(carriage, &context, Self::parse)?;

        carriage.expect_tokens(&[TokenKind::RParen])?;

        Ok(expression)
    }

    fn parse_prefix_expression(carriage: &mut Carriage, current: Token) -> Result<Self> {
        let mut context = Context::from_carriage(carriage)?;
        context.precedence = Precedence::Prefix;

        let expression = parse(carriage, &context, Self::parse)?;

        let expression = Prefix {
            operator: Operator::try_from(current.kind)?,
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
        let expression = match &context.current_token.kind {
            TokenKind::LParen => parse(carriage, &context, Self::parse_call_expression)?,
            _ => parse(carriage, &context, Self::parse_infix_expression)?,
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
        )
    }

    fn parse_infix_expression(
        carriage: &mut Carriage,
        CurrentToken(current): CurrentToken,
        left: Expression,
    ) -> Result<Expression> {
        let mut context = Context::from_carriage(carriage)?;
        context.precedence = Precedence::from(&current);

        let right = parse(carriage, &context, Expression::parse)?;

        let expression = Infix {
            left: Box::new(left),
            operator: Operator::try_from(current.kind)?,
            right: Box::new(right),
        };

        Ok(Expression::InfixExpression(expression))
    }

    fn parse_call_expression(_carriage: &mut Carriage, _function: Expression) -> Result<Self> {
        todo!("call expression")
    }

    fn parse_int_literal_expression(token: Token) -> Result<Self> {
        let literal = token.literal.parse::<i64>().map_err(Error::from)?;
        Ok(Expression::IntLiteral(literal))
    }

    fn parse_bool_literal_expression(token: Token) -> Result<Self> {
        match token.kind {
            TokenKind::True => Ok(Expression::BoolLiteral(true)),
            TokenKind::False => Ok(Expression::BoolLiteral(false)),
            k => {
                return Err(Error::expected_token(
                    Some(k),
                    [TokenKind::True, TokenKind::False].as_slice().into(),
                ))
            }
        }
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
            Expression::BoolLiteral(b) => {
                if *b {
                    write!(f, "rbap_true")
                } else {
                    write!(f, "rbap_false")
                }
            }
            Expression::InfixExpression(ie) => {
                write!(f, "({} {} {})", ie.left, ie.operator, ie.right)
            }
            Expression::PrefixExpression(pe) => write!(f, "({}{})", pe.operator, pe.right),
            Expression::CallExpression(ce) => write!(f, "{}", ce),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub function: String,
    pub arguments: Vec<Expression>,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.function)?;

        let mut iter = self.arguments.iter().peekable();

        while let Some(exp) = iter.next() {
            if iter.peek().is_some() {
                write!(f, "{}, ", exp)?;
            } else {
                write!(f, "{}", exp)?;
            }
        }
        write!(f, ")")
    }
}
