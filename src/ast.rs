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

impl Parse<Data> for Statement {
    fn parse(carriage: &mut Carriage) -> Result<Data> {
        let ident = match carriage.next_token()? {
            Token {
                literal,
                kind: TokenKind::DataInline,
            } => literal.to_string(),

            Token {
                literal,
                kind: TokenKind::Ident,
            } if carriage.is_peek_token(&TokenKind::Assign) => literal.to_string(),

            current => {
                return Err(Error::ParseDataAssign {
                    current: current.kind().to_owned(),
                    peek: carriage.peek_token()?.kind().to_owned(),
                })
            }
        };

        carriage.expect_tokens(&[TokenKind::Assign])?;
        let value = expect_and_parse_expression(carriage)?;

        Ok(Data { ident, value })
    }
}

fn expect_and_parse_expression(carriage: &mut Carriage) -> Result<Expression> {
    carriage.expect_tokens(&[
        TokenKind::Ident,
        TokenKind::IntLiteral,
        TokenKind::StringLiteral,
        TokenKind::VSlash,
    ])?;
    Expression::parse(carriage)
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(i64),
    StringLiteral(String),
    Ident(String),
    StringTemplate(Vec<Expression>),
}

impl Parse<Expression> for Expression {
    fn parse(carriage: &mut Carriage) -> Result<Self> {
        let expression = match carriage.peek_token()?.kind() {
            TokenKind::IntLiteral => Expression::IntLiteral(Expression::parse(carriage)?),
            TokenKind::StringLiteral => Expression::StringLiteral(Expression::parse(carriage)?),
            TokenKind::Ident => Expression::Ident(Expression::parse(carriage)?),
            TokenKind::VSlash => Expression::StringTemplate(Expression::parse(carriage)?),
            token => unimplemented!("can't parse expression '{}({:?})", token, token),
        };

        Ok(expression)
    }
}

impl Parse<i64> for Expression {
    fn parse(carriage: &mut Carriage) -> Result<i64> {
        let token = carriage.next_token()?;
        token.literal().parse::<i64>().map_err(|e| e.into())
    }
}

impl Parse<String> for Expression {
    fn parse(carriage: &mut Carriage) -> Result<String> {
        let token = carriage.next_token()?;
        Ok(token.literal().to_owned())
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
