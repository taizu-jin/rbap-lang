use crate::lexer::Token;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    DataDeclaration(DataDeclaration),
}

#[derive(Debug)]
pub enum Expression {
    IntLiteral { value: i64, token: Token },
    StringLiteral { value: String, token: Token },
}

#[derive(Debug)]
pub struct DataDeclaration {
    pub ident: String,
    pub ty: DataType,
}

#[derive(Debug, PartialEq)]
pub enum DataType {
    String,
    Int,
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
