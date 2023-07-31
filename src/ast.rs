#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    DataDeclaration(Vec<DataDeclaration>),
    Write(Vec<Expression>),
    Data(Data),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(i64),
    StringLiteral(String),
    Ident(String),
    StringTemplate(Vec<Expression>),
}

#[derive(Debug, PartialEq)]
pub struct DataDeclaration {
    pub ident: String,
    pub ty: DataType,
}

#[derive(Debug, PartialEq)]
pub struct Data {
    pub ident: String,
    pub value: Expression,
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
