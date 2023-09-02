mod expression;
mod statement;

use std::fmt::Display;

pub use expression::{Expression, Operator};
pub use statement::Statement;

#[derive(Debug, PartialEq, Copy, Clone)]
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

#[derive(Debug)]
pub struct IfStatement {
    condition: Expression,
    consequence: Block,
    alternative: Option<Block>,
}

impl Display for IfStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "IF {}.", self.condition)?;
        writeln!(f, "{}", self.consequence)?;
        if let Some(alternative) = &self.alternative {
            writeln!(f, "ELSE.")?;
            writeln!(f, "{}", alternative)?;
        }
        write!(f, "ENDIF.")
    }
}

#[derive(Debug)]
pub struct Block {
    statements: Vec<Statement>,
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Data {
    pub ident: String,
    pub value: Expression,
}

impl Display for Data {
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
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }
}

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl From<Program> for Node {
    fn from(value: Program) -> Self {
        Node::Program(value)
    }
}

impl From<Statement> for Node {
    fn from(value: Statement) -> Self {
        Node::Statement(value)
    }
}

impl From<Expression> for Node {
    fn from(value: Expression) -> Self {
        Node::Expression(value)
    }
}
