use std::fmt::Display;

use crate::{
    error::{Error, Result},
    parser::Carriage,
};

use super::{primitive::Identifier, Expression};

#[derive(Debug, PartialEq)]
pub struct Call {
    pub function: Identifier,
    pub arguments: Vec<Expression>,
}

impl Call {
    pub fn parse(carriage: &mut Carriage, function: Expression) -> Result<Self> {
        let function = if let Expression::Ident(function) = function {
            function
        } else {
            return Err(Error::unexpected_expression(function));
        };

        let arguments = Expression::parse_expression_list(carriage)?;

        let call = Call {
            function,
            arguments,
        };

        Ok(call)
    }
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

impl From<Call> for Expression {
    fn from(value: Call) -> Self {
        Expression::CallExpression(value)
    }
}
