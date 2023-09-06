use std::fmt::Display;

use crate::{
    error::Result,
    parser::{context::CurrentToken, parse, Carriage, Context, Precedence},
};

use super::{Expression, Operator};

#[derive(Debug, PartialEq)]
pub struct Infix {
    pub left: Box<Expression>,
    pub operator: Operator,
    pub right: Box<Expression>,
}

impl Infix {
    pub fn parse(
        carriage: &mut Carriage,
        CurrentToken(current): CurrentToken,
        left: Expression,
    ) -> Result<Self> {
        let mut context = Context::from_carriage(carriage)?;
        context.precedence = Precedence::from(&current);

        let right = parse(carriage, &context, Expression::parse)?;

        let expression = Infix {
            left: Box::new(left),
            operator: Operator::try_from(current.kind)?,
            right: Box::new(right),
        };

        Ok(expression)
    }
}

impl From<Infix> for Expression {
    fn from(value: Infix) -> Self {
        Expression::InfixExpression(value)
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}
