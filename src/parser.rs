mod carriage;
pub mod context;
mod error;

use crate::{
    ast::{Program, Statement},
    lexer::LexerIter,
};

pub use carriage::Carriage;
pub use context::Context;
pub use error::{Error, Result};

pub use context::Handler;

pub fn parse<'t, T, R, H>(
    carriage: &'t mut Carriage,
    context: &'t Context<'t>,
    handler: H,
) -> Result<R>
where
    H: Handler<'t, T, R>,
{
    handler.call(carriage, context)
}

pub struct Parser<'t, 's: 't> {
    carriage: Carriage<'t, 's>,
}

impl<'t, 's: 't> Parser<'t, 's> {
    pub fn new(iter: LexerIter<'t, 's>) -> Self {
        let carriage = Carriage::new(iter);

        Self { carriage }
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();

        loop {
            match Statement::parse(&mut self.carriage) {
                Ok(statement) => program.statements.push(statement),
                Err(Error::Eof) => break,
                Err(e) => self.carriage.errors.push(e.to_string()),
            }
        }

        program
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.carriage.errors
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Data, DataDeclaration, DataType, Expression},
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
