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
pub struct Data<'a> {
    pub ident: String,
    pub value: Expression<'a>,
}

#[derive(Debug, PartialEq)]
pub struct DataDeclaration {
    pub ident: String,
    pub ty: DataType,
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
