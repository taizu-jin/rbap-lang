mod expression;
mod statement;

use std::fmt::Display;

pub use expression::Expression;
pub use statement::Statement;

#[derive(Debug, PartialEq)]
pub enum DataType {
    String,
    Int,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::String => write!(f, "string"),
            DataType::Int => write!(f, "i"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Data<'a> {
    pub ident: String,
    pub value: Expression<'a>,
}

impl Display for Data<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.ident, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct DataDeclaration {
    pub ident: String,
    pub ty: DataType,
}

impl Display for DataDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} TYPE {}", self.ident, self.ty)
    }
}

#[derive(Debug)]
pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl Program<'_> {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }
}
