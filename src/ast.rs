mod expression;
mod statement;

pub use expression::{primitive, Call, Expression, Infix, Operator, Prefix, StringTemplate};
pub use statement::{Assignment, Block, Data, DataType, Declaration, Function, Statement, Write};

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
