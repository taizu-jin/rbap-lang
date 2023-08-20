mod expression;
mod statement;

pub use expression::Expression;
pub use statement::Statement;

#[derive(Debug, PartialEq)]
pub enum DataType {
    String,
    Int,
}

#[derive(Debug, PartialEq)]
pub struct Data {
    pub ident: String,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct DataDeclaration {
    pub ident: String,
    pub ty: DataType,
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
