mod error;

use crate::{
    ast::{Data, DataDeclaration, DataType, Expression, Program, Statement},
    lexer::{LexerIter, Token, TokenKind},
};
use std::iter::Peekable;

pub use self::error::{Error, Result};

pub trait Parse<T: Sized> {
    fn parse(carriage: &mut Carriage) -> Result<T>;
}

pub struct Carriage<'t, 's: 't> {
    iter: Peekable<LexerIter<'t, 's>>,
    errors: Vec<String>,
}

impl<'t, 's: 't> Carriage<'t, 's> {
    fn new(iter: LexerIter<'t, 's>) -> Self {
        Self {
            iter: iter.peekable(),
            errors: Vec::new(),
        }
    }
}

impl<'t, 's: 't> Carriage<'t, 's> {
    fn next(&mut self) -> Option<Token<'t>> {
        self.iter.next()
    }

    pub fn next_token(&mut self) -> Result<Token<'t>> {
        match self.iter.next() {
            Some(token) => Ok(token),
            None => Err(Error::Eof),
        }
    }

    pub fn peek_token(&mut self) -> Result<&Token<'t>> {
        match self.iter.peek() {
            Some(token) => Ok(token),
            None => Err(Error::Eof),
        }
    }

    pub fn expect_tokens(&mut self, tokens: &[TokenKind]) -> Result<Token<'t>> {
        let peeked = match self.iter.peek() {
            Some(peeked) => peeked,
            None => {
                return Err(Error::ExpectToken {
                    got: None,
                    expected: tokens.into(),
                })
            }
        };

        for token in tokens {
            if peeked.kind() == token {
                return self.next_token();
            }
        }

        Err(Error::ExpectToken {
            got: Some(peeked.kind().to_owned()),
            expected: tokens.into(),
        })
    }

    pub fn is_peek_token(&mut self, token: &TokenKind) -> bool {
        match self.iter.peek() {
            Some(peeked) => peeked.kind() == token,
            None => false,
        }
    }
}

pub struct Parser<'t, 's: 't> {
    carriage: Carriage<'t, 's>,
}

impl<'t, 's: 't> Parser<'t, 's> {
    pub fn new(iter: LexerIter<'t, 's>) -> Self {
        let carriage = Carriage::new(iter);

        Self { carriage }
    }

    fn parse_statement(&mut self, token: Token<'t>) -> Result<Statement> {
        let statement = match token.kind() {
            TokenKind::Data => self.parse_data_declaration_statement()?,
            TokenKind::DataInline => self.parse_data_statement(token.literal().to_string())?,
            TokenKind::Ident if self.carriage.is_peek_token(&TokenKind::Assign) => {
                self.parse_data_statement(token.literal().to_string())?
            }
            TokenKind::Write => self.parse_write_statement()?,
            _ => Statement::Expression(self.parse_expression(&token)?),
        };

        self.carriage.expect_tokens(&[TokenKind::Period])?;

        Ok(statement)
    }

    fn parse_data_declaration_statement(&mut self) -> Result<Statement> {
        let mut declarations = Vec::new();

        match self.carriage.is_peek_token(&TokenKind::Colon) {
            true => {
                self.carriage.next_token()?;

                loop {
                    let declaration = self.parse_data_declaration()?;
                    declarations.push(declaration);

                    if self.carriage.is_peek_token(&TokenKind::Comma) {
                        self.carriage.next_token()?;
                    } else {
                        break;
                    }
                }
            }
            false => {
                let declaration = self.parse_data_declaration()?;
                declarations.push(declaration);
            }
        }

        Ok(Statement::DataDeclaration(declarations))
    }

    fn parse_data_declaration(&mut self) -> Result<DataDeclaration> {
        let token = self.carriage.expect_tokens(&[TokenKind::Ident])?;

        let ident = if let TokenKind::Ident = token.kind() {
            token.literal().to_owned()
        } else {
            unreachable!("current token must be Identifier type")
        };

        self.carriage.expect_tokens(&[TokenKind::Type])?;
        let token = self
            .carriage
            .expect_tokens(&[TokenKind::String, TokenKind::Int])?;

        let ty = match token.kind() {
            TokenKind::Int => DataType::Int,
            TokenKind::String => DataType::String,
            _ => unreachable!("current token must be either Int or String type"),
        };

        Ok(DataDeclaration { ident, ty })
    }

    fn expect_and_parse_expression(&mut self) -> Result<Expression> {
        let token = self.carriage.expect_tokens(&[
            TokenKind::Ident,
            TokenKind::IntLiteral,
            TokenKind::StringLiteral,
            TokenKind::VSlash,
        ])?;

        self.parse_expression(&token)
    }

    fn parse_expression(&mut self, token: &Token<'t>) -> Result<Expression> {
        let expression = match token.kind() {
            TokenKind::IntLiteral => Self::parse_integer_literal(token.literal())?,
            TokenKind::StringLiteral => Self::parse_string_literal(token.literal()),
            TokenKind::Ident => Expression::Ident(token.literal().to_owned()),
            TokenKind::VSlash => self.parse_string_template_expression()?,
            _ => unimplemented!("can't parse expression '{}({:?})'", token, token),
        };

        Ok(expression)
    }

    fn parse_string_template_expression(&mut self) -> Result<Expression> {
        let mut token = self.carriage.expect_tokens(&[
            TokenKind::StringLiteral,
            TokenKind::LSquirly,
            TokenKind::VSlash,
        ])?;

        let mut expressions = Vec::new();

        while let Some(expression) = self.parse_string_template(&token) {
            let (expression, t) = expression?;
            token = t;
            match expression {
                Expression::StringLiteral(literal) if literal.is_empty() => continue,
                expression => expressions.push(expression),
            }
        }

        Ok(Expression::StringTemplate(expressions))
    }

    fn parse_string_template(
        &mut self,
        token: &Token<'t>,
    ) -> Option<Result<(Expression, Token<'t>)>> {
        match token.kind() {
            TokenKind::StringLiteral => {
                let expression = match self.parse_expression(token) {
                    Ok(expression) => expression,
                    Err(e) => return Some(Err(e)),
                };
                let token = match self.carriage.next_token() {
                    Ok(token) => token,
                    Err(e) => return Some(Err(e)),
                };

                Some(Ok((expression, token)))
            }
            TokenKind::LSquirly => {
                let token = match self.carriage.next_token() {
                    Ok(token) => token,
                    Err(e) => return Some(Err(e)),
                };
                let expression = match self.parse_expression(&token) {
                    Ok(expression) => expression,
                    Err(e) => return Some(Err(e)),
                };

                let token = match self.carriage.expect_tokens(&[TokenKind::RSquirly]) {
                    Ok(_) => match self.carriage.next_token() {
                        Ok(token) => token,
                        Err(e) => return Some(Err(e)),
                    },
                    Err(err) => return Some(Err(err)),
                };

                Some(Ok((expression, token)))
            }
            TokenKind::VSlash => None,
            _ => Some(Err(Error::ParseStringTemplate {
                kind: token.kind().to_owned(),
                literal: token.literal().to_owned(),
            })),
        }
    }
}

impl Parser<'_, '_> {
    pub fn errors(&self) -> &Vec<String> {
        &self.carriage.errors
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();

        while let Some(token) = self.carriage.next() {
            match self.parse_statement(token) {
                Ok(statement) => program.statements.push(statement),
                Err(error) => self.carriage.errors.push(error.to_string()),
            }
        }

        program
    }

    fn parse_string_literal(value: &str) -> Expression {
        Expression::StringLiteral(value.to_string())
    }

    fn parse_integer_literal(literal: &str) -> Result<Expression> {
        match literal.parse::<i64>() {
            Ok(value) => Ok(Expression::IntLiteral(value)),
            Err(err) => Err(Error::ParseInt(err)),
        }
    }

    fn parse_data_statement(&mut self, ident: String) -> Result<Statement> {
        self.carriage.expect_tokens(&[TokenKind::Assign])?;

        let expression = self.expect_and_parse_expression()?;

        Ok(Statement::Data(Data {
            ident: ident.to_string(),
            value: expression,
        }))
    }

    fn parse_write_statement(&mut self) -> Result<Statement> {
        let mut expressions = Vec::new();

        match self.carriage.is_peek_token(&TokenKind::Colon) {
            true => {
                self.carriage.next_token()?;

                loop {
                    if self.carriage.is_peek_token(&TokenKind::Slash) {
                        expressions.push(Expression::StringLiteral("\n".to_string()));
                        self.carriage.next_token()?;
                    }

                    expressions.push(self.expect_and_parse_expression()?);

                    if self.carriage.is_peek_token(&TokenKind::Comma) {
                        self.carriage.next_token()?;
                    } else {
                        break;
                    }
                }
            }
            false => {
                if self.carriage.is_peek_token(&TokenKind::Slash) {
                    expressions.push(Expression::StringLiteral("\n".to_string()));
                    self.carriage.next_token()?;
                }
                expressions.push(self.expect_and_parse_expression()?)
            }
        }

        Ok(Statement::Write(expressions))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Data, DataDeclaration, DataType},
        lexer::Lexer,
    };

    use super::*;
    use std::fmt::Write;

    #[test]
    fn test_integer_literal_expression() {
        let input = String::from("5.");
        let program = parse_program(input);

        assert_eq!(
            1,
            program.statements.len(),
            "program has not enough statements. got={}",
            program.statements.len()
        );

        if let Statement::Expression(expression) = &program.statements[0] {
            match expression {
                Expression::IntLiteral(value) => {
                    assert_eq!(5, *value, "value is not {}, got={}", 5, *value);
                }
                _ => panic!("expression is not IntLiteral. got={:?}", expression),
            }
        } else {
            panic!(
                "program.statements[0] is not an Statement::Expression. got={:?}",
                program.statements[0]
            )
        }
    }

    fn parse_program(input: String) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer.iter());
        let program = parser.parse();

        check_parser_errors(parser);

        program
    }

    fn check_parser_errors(parser: Parser) {
        if parser.carriage.errors.is_empty() {
            return;
        }

        let mut message = String::new();
        writeln!(
            message,
            "parsers has {} errors",
            parser.carriage.errors.len()
        )
        .unwrap();
        for msg in parser.carriage.errors {
            writeln!(message, "\tparser error: {}", msg).unwrap();
        }

        panic!("{}", message)
    }

    #[test]
    fn test_string_literal_expression() {
        let input = String::from("'5'.");
        let program = parse_program(input);

        assert_eq!(
            1,
            program.statements.len(),
            "program has not enough statements. got={}",
            program.statements.len()
        );

        if let Statement::Expression(expression) = &program.statements[0] {
            match expression {
                Expression::StringLiteral(value) => {
                    assert_eq!("5", value, "value is not {}, got={}", "5", value);
                }
                _ => panic!("expression is not StringLiteral. got={:?}", expression),
            }
        } else {
            panic!(
                "program.statements[0] is not an Statement::Expression. got={:?}",
                program.statements[0]
            )
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = String::from("lv_string.");
        let program = parse_program(input);

        assert_eq!(
            1,
            program.statements.len(),
            "program has not enough statements. got={}",
            program.statements.len()
        );

        if let Statement::Expression(expression) = &program.statements[0] {
            match expression {
                Expression::Ident(ident) => {
                    assert_eq!("lv_string", ident, "value is not {}, got={}", "5", ident);
                }
                _ => panic!("expression is not StringLiteral. got={:?}", expression),
            }
        } else {
            panic!(
                "program.statements[0] is not an Statement::Expression. got={:?}",
                program.statements[0]
            )
        }
    }

    #[test]
    fn test_data_declaration_statement() {
        struct TestCase {
            input: &'static str,
            expected: Vec<DataDeclaration>,
        }

        let tests = vec![
            TestCase {
                input: "DATA lv_string TYPE string.",
                expected: vec![DataDeclaration {
                    ident: "lv_string".into(),
                    ty: DataType::String,
                }],
            },
            TestCase {
                input: "DATA lv_int type i.",
                expected: vec![DataDeclaration {
                    ident: "lv_int".into(),
                    ty: DataType::Int,
                }],
            },
            TestCase {
                input: "DATA: lv_int type i.",
                expected: vec![DataDeclaration {
                    ident: "lv_int".into(),
                    ty: DataType::Int,
                }],
            },
            TestCase {
                input: "DATA: lv_int type i,
lv_int2 TYPE i,
lv_string TYPE string,
lv_string2 TYPE string.",
                expected: vec![
                    DataDeclaration {
                        ident: "lv_int".into(),
                        ty: DataType::Int,
                    },
                    DataDeclaration {
                        ident: "lv_int2".into(),
                        ty: DataType::Int,
                    },
                    DataDeclaration {
                        ident: "lv_string".into(),
                        ty: DataType::String,
                    },
                    DataDeclaration {
                        ident: "lv_string2".into(),
                        ty: DataType::String,
                    },
                ],
            },
        ];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::DataDeclaration(data_declaration) = &program.statements[0] {
                for (i, dd) in data_declaration.iter().enumerate() {
                    let expected = &test.expected[i];

                    assert_eq!(
                        expected.ident, dd.ident,
                        "identifier is not '{}'. got={}",
                        expected.ident, dd.ident
                    );
                    assert_eq!(
                        expected.ty, dd.ty,
                        "type is not '{:?}'. got={:?}",
                        expected.ty, dd.ty
                    );
                }
            } else {
                panic!(
                    "program.statements[0] is not an Statement::DataDeclaration. got={:?}",
                    program.statements[0]
                )
            }
        }
    }

    #[test]
    fn test_data_statement() {
        struct TestCase {
            input: &'static str,
            expected: Data,
        }

        let tests = vec![
            TestCase {
                input: "DATA(lv_string) = '5'.",
                expected: Data {
                    ident: "lv_string".into(),
                    value: Expression::StringLiteral("5".into()),
                },
            },
            TestCase {
                input: "DATA(lv_int) = 1.",
                expected: Data {
                    ident: "lv_int".into(),
                    value: Expression::IntLiteral(1),
                },
            },
            TestCase {
                input: "lv_int = 1.",
                expected: Data {
                    ident: "lv_int".into(),
                    value: Expression::IntLiteral(1),
                },
            },
            TestCase {
                input: "lv_int = |some { lv_string } literal|.",
                expected: Data {
                    ident: "lv_int".into(),
                    value: Expression::StringTemplate(vec![
                        Expression::StringLiteral("some ".into()),
                        Expression::Ident("lv_string".into()),
                        Expression::StringLiteral(" literal".into()),
                    ]),
                },
            },
        ];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::Data(data) = &program.statements[0] {
                assert_eq!(
                    test.expected.ident, data.ident,
                    "identifier is not '{}'. got={}",
                    test.expected.ident, data.ident
                );
                assert_eq!(
                    test.expected.value, data.value,
                    "type is not '{:?}'. got={:?}",
                    test.expected.value, data.value,
                );
            } else {
                panic!(
                    "program.statements[0] is not an Statement::DataDeclaration. got={:?}",
                    program.statements[0]
                )
            }
        }
    }

    #[test]
    fn test_write_statement() {
        struct TestCase {
            input: &'static str,
            expected: Vec<Expression>,
        }

        let tests = vec![
            TestCase {
                input: "WRITE '5'.",
                expected: vec![Expression::StringLiteral("5".to_string())],
            },
            TestCase {
                input: "WRITE 1.",
                expected: vec![Expression::IntLiteral(1)],
            },
            TestCase {
                input: "WRITE lv_string.",
                expected: vec![Expression::Ident("lv_string".to_string())],
            },
            TestCase {
                input: "WRITE: lv_string.",
                expected: vec![Expression::Ident("lv_string".to_string())],
            },
            TestCase {
                input: "WRITE: lv_string, 5, '5'.",
                expected: vec![
                    Expression::Ident("lv_string".to_string()),
                    Expression::IntLiteral(5),
                    Expression::StringLiteral("5".to_string()),
                ],
            },
            TestCase {
                input: "WRITE: / lv_string, / 5, / '5'.",
                expected: vec![
                    Expression::StringLiteral("\n".to_string()),
                    Expression::Ident("lv_string".to_string()),
                    Expression::StringLiteral("\n".to_string()),
                    Expression::IntLiteral(5),
                    Expression::StringLiteral("\n".to_string()),
                    Expression::StringLiteral("5".to_string()),
                ],
            },
            TestCase {
                input: "WRITE: / lv_string.",
                expected: vec![
                    Expression::StringLiteral("\n".to_string()),
                    Expression::Ident("lv_string".to_string()),
                ],
            },
            TestCase {
                input: "WRITE/ lv_string.",
                expected: vec![
                    Expression::StringLiteral("\n".to_string()),
                    Expression::Ident("lv_string".to_string()),
                ],
            },
        ];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::Write(expressions) = &program.statements[0] {
                assert_eq!(
                    &test.expected, expressions,
                    "expression is not '{:?}'. got={:?}",
                    test.expected, expressions
                );
            } else {
                panic!(
                    "program.statements[0] is not an Statement::Write. got={:?}",
                    program.statements[0]
                )
            }
        }
    }

    #[test]
    fn test_string_template_statement() {
        struct TestCase {
            input: &'static str,
            expected: Vec<Expression>,
        }

        let tests = vec![
            TestCase {
                input: "|this { lv_that } those|.",
                expected: vec![
                    Expression::StringLiteral("this ".to_string()),
                    Expression::Ident("lv_that".to_string()),
                    Expression::StringLiteral(" those".to_string()),
                ],
            },
            TestCase {
                input: "||.",
                expected: vec![],
            },
            TestCase {
                input: "|this{ | is | }{ |a { nested } string| } template|.",
                expected: vec![
                    Expression::StringLiteral("this".to_string()),
                    Expression::StringTemplate(vec![Expression::StringLiteral(" is ".to_string())]),
                    Expression::StringTemplate(vec![
                        Expression::StringLiteral("a ".to_string()),
                        Expression::Ident("nested".to_string()),
                        Expression::StringLiteral(" string".to_string()),
                    ]),
                    Expression::StringLiteral(" template".to_string()),
                ],
            },
        ];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::Expression(Expression::StringTemplate(expressions)) =
                &program.statements[0]
            {
                assert_eq!(
                    &test.expected, expressions,
                    "expression is not '{:?}'. got={:?}",
                    test.expected, expressions
                );
            } else {
                panic!(
                    "program.statements[0] is not an Statement::Expression(Expression::StringTemplate). got={:?}",
                    program.statements[0]
                )
            }
        }
    }
}
