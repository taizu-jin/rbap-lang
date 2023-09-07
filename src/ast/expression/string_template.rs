use std::fmt::Display;

use crate::{
    error::{Error, Result},
    lexer::{Token, TokenKind},
    parser::{parse, Carriage, Context},
};

use super::Expression;

#[derive(Debug, PartialEq)]
pub struct StringTemplate(Vec<Expression>);

impl StringTemplate {
    pub fn parse(carriage: &mut Carriage, peek: Token) -> Result<Self> {
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
                Expression::StringLiteral(literal) if literal.as_ref().is_empty() => continue,
                expression => expressions.push(expression),
            }
        }

        Ok(Self(expressions))
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

impl From<Vec<Expression>> for StringTemplate {
    fn from(value: Vec<Expression>) -> Self {
        Self(value)
    }
}

impl From<StringTemplate> for Vec<Expression> {
    fn from(value: StringTemplate) -> Self {
        value.0
    }
}

impl From<StringTemplate> for Expression {
    fn from(value: StringTemplate) -> Self {
        Expression::StringTemplate(value)
    }
}

impl AsRef<Vec<Expression>> for StringTemplate {
    fn as_ref(&self) -> &Vec<Expression> {
        &self.0
    }
}

impl Display for StringTemplate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "|")?;
        for s in &self.0 {
            write!(f, "{}", s)?;
        }
        write!(f, "|")
    }
}
