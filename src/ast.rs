use crate::{
    lexer::{Token, TokenKind},
    parser::{Carriage, Error, Parse, Result},
};

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

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    DataDeclaration(Vec<DataDeclaration>),
    Write(Vec<Expression>),
    Data(Data),
}




impl Parse<Vec<DataDeclaration>> for Statement {
    fn parse(carriage: &mut Carriage) -> Result<Vec<DataDeclaration>> {
        carriage.next_token()?;

        let mut declarations = Vec::new();

        match carriage.is_peek_token(&TokenKind::Colon) {
            true => {
                carriage.next_token()?;

                loop {
                    let declaration = DataDeclaration::parse(carriage)?;
                    declarations.push(declaration);

                    if carriage.is_peek_token(&TokenKind::Comma) {
                        carriage.next_token()?;
                    } else {
                        break;
                    }
                }
            }
            false => {
                let declaration = DataDeclaration::parse(carriage)?;
                declarations.push(declaration);
            }
        }

        Ok(declarations)
    }
}

#[derive(Debug, PartialEq)]
pub struct DataDeclaration {
    pub ident: String,
    pub ty: DataType,
}

#[derive(Debug, PartialEq)]
pub enum DataType {
    String,
    Int,
}

impl Parse<DataDeclaration> for DataDeclaration {
    fn parse(carriage: &mut Carriage) -> Result<Self> {
        let token = carriage.expect_tokens(&[TokenKind::Ident])?;

        let ident = if let TokenKind::Ident = token.kind() {
            token.literal().to_owned()
        } else {
            unreachable!("current token must be Identifier kind")
        };

        carriage.expect_tokens(&[TokenKind::Type])?;
        let token = carriage.expect_tokens(&[TokenKind::String, TokenKind::Int])?;

        let ty = match token.kind() {
            TokenKind::Int => DataType::Int,
            TokenKind::String => DataType::String,
            _ => unreachable!("current token must be either Int or String kind"),
        };

        Ok(DataDeclaration { ident, ty })
    }
}
#[derive(Debug, PartialEq)]
pub struct Data {
    pub ident: String,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(i64),
    StringLiteral(String),
    Ident(String),
    StringTemplate(Vec<Expression>),
}

}

impl Parse<Vec<Expression>> for Expression {
    fn parse(carriage: &mut Carriage) -> Result<Vec<Expression>> {
        carriage.next_token()?;

        let peeked_kind = carriage.peek_token()?.kind();
        let tokens = &[
            TokenKind::StringLiteral,
            TokenKind::LSquirly,
            TokenKind::VSlash,
        ];

        if !tokens.contains(peeked_kind) {
            return Err(Error::ExpectToken {
                got: None,
                expected: tokens.as_slice().into(),
            });
        }

        let mut expressions = Vec::new();

        while let Some(expression) = parse_string_template(carriage) {
            let expression = expression?;

            match expression {
                Expression::StringLiteral(literal) if literal.is_empty() => continue,
                expression => expressions.push(expression),
            }
        }

        Ok(expressions)
    }
}

fn parse_string_template(carriage: &mut Carriage) -> Option<Result<Expression>> {
    let token = match carriage.next_token() {
        Ok(token) => token,
        Err(err) => return Some(Err(err)),
    };

    match token.kind() {
        TokenKind::StringLiteral => {
            let expression = match Expression::parse(carriage) {
                Ok(expression) => expression,
                Err(err) => return Some(Err(err)),
            };

            match carriage.next_token() {
                Ok(_) => (),
                Err(err) => return Some(Err(err)),
            };
            Some(Ok(expression))
        }
        TokenKind::LSquirly => {
            match carriage.next_token() {
                Ok(_) => (),
                Err(e) => return Some(Err(e)),
            };
            let expression = match Expression::parse(carriage) {
                Ok(expression) => expression,
                Err(e) => return Some(Err(e)),
            };

            match carriage.expect_tokens(&[TokenKind::RSquirly]) {
                Ok(_) => match carriage.next_token() {
                    Ok(_) => (),
                    Err(e) => return Some(Err(e)),
                },
                Err(err) => return Some(Err(err)),
            };

            Some(Ok(expression))
        }
        TokenKind::VSlash => None,
        _ => Some(Err(Error::ParseStringTemplate {
            kind: token.kind().to_owned(),
            literal: token.literal().to_owned(),
        })),
    }
}
