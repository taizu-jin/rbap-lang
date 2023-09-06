use crate::{
    error::Result,
    lexer::Token,
    parser::{parse, Carriage, Context, Precedence},
};

use super::{operator::Operator, Expression};

#[derive(Debug, PartialEq)]
pub struct Prefix {
    pub operator: Operator,
    pub right: Box<Expression>,
}

impl Prefix {
    pub fn parse(carriage: &mut Carriage, current: Token) -> Result<Self> {
        let mut context = Context::from_carriage(carriage)?;
        context.precedence = Precedence::Prefix;

        let expression = parse(carriage, &context, Expression::parse)?;

        let expression = Prefix {
            operator: Operator::try_from(current.kind)?,
            right: Box::new(expression),
        };

        Ok(expression)
    }
}

impl From<Prefix> for Expression {
    fn from(value: Prefix) -> Self {
        Expression::PrefixExpression(value)
    }
}
