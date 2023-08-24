mod carriage;
pub mod context;
mod error;

use crate::{
    ast::{Program, Statement},
    lexer::{LexerIter, Token, TokenKind},
};

pub use carriage::Carriage;
pub use context::Context;
pub use error::{Error, Result};

pub use context::Handler;

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    _Equals,
    _LessGreater,
    Sum,
    Product,
    _Perfix,
    _Call,
}

impl From<&Token<'_>> for Precedence {
    fn from(value: &Token<'_>) -> Self {
        match value.kind {
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::Asterisk | TokenKind::Slash => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

pub fn parse<'t, 's: 't, 'e, T, R, H>(
    carriage: &'_ mut Carriage<'t, 's>,
    context: &'_ Context<'t, 'e>,
    handler: H,
) -> Result<R>
where
    H: Handler<'t, 's, 'e, T, R>,
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

    pub fn parse<'a>(&mut self) -> Program<'a> {
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
    use super::*;
    use crate::{
        ast::{Data, DataDeclaration, DataType, Expression::*},
        lexer::Lexer,
    };

    use std::{borrow::Cow, fmt::Write};

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
                IntLiteral(value) => {
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

    fn parse_program<'a>(input: String) -> Program<'a> {
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
                StringLiteral(value) => {
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
                Ident(ident) => {
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

    struct TestCaseDataDeclaration {
        input: &'static str,
        expected: Vec<DataDeclaration>,
    }

    macro_rules! def_case_ddecl {
        ($input:expr, $($kind:ident:$literal:literal),*) => {
            TestCaseDataDeclaration {
                input: $input,
                expected: vec![$(
                    DataDeclaration{
                        ident: $literal.into(),
                        ty: DataType::$kind,
                    }
                    ),*]
            }
        };
    }

    #[test]
    fn test_data_declaration_statement() {
        let tests = vec![
            def_case_ddecl!("DATA lv_string TYPE string.",String:"lv_string"),
            def_case_ddecl!("DATA lv_int TYPE i.",Int:"lv_int"),
            def_case_ddecl!("DATA: lv_int TYPE i.",Int:"lv_int"),
            def_case_ddecl!("DATA: lv_int type i,
                                   lv_int2 TYPE i,
                                   lv_string TYPE string,
                                   lv_string2 TYPE string.",
                            Int:"lv_int",
                            Int:"lv_int2",
                            String:"lv_string",
                            String:"lv_string2"),
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

    struct TestCaseData<'a> {
        input: &'static str,
        expected: Data<'a>,
    }

    macro_rules! def_case_data {
        ($input:expr,$ident:literal,$expected:expr) => {
            TestCaseData {
                input: $input,
                expected: Data {
                    ident: $ident.into(),
                    value: $expected,
                },
            }
        };
    }

    #[test]
    fn test_data_statement() {
        let tests = vec![
            def_case_data!(
                "DATA(lv_string) = '5'.",
                "lv_string",
                StringLiteral("5".into())
            ),
            def_case_data!("DATA(lv_int) = 1.", "lv_int", IntLiteral(1)),
            def_case_data!("lv_int = 1.", "lv_int", IntLiteral(1)),
            def_case_data!(
                "lv_int = |some { lv_string } literal|.",
                "lv_int",
                StringTemplate(vec![
                    StringLiteral("some ".into()),
                    Ident("lv_string".into()),
                    StringLiteral(" literal".into()),
                ])
            ),
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

    struct TestCaseExpression<'a> {
        input: &'static str,
        expected: Vec<crate::ast::Expression<'a>>,
    }

    macro_rules! def_case_expr {
        ($input:expr, $($expected:expr),*) => {
            TestCaseExpression {
                input: $input,
                expected: vec![$($expected),*]
            }
        };
    }

    #[test]
    fn test_write_statement() {
        let tests = vec![
            def_case_expr!("WRITE '5'.", StringLiteral("5".into())),
            def_case_expr!("WRITE 1.", IntLiteral(1)),
            def_case_expr!("WRITE lv_string.", Ident("lv_string".into())),
            def_case_expr!(
                "WRITE: lv_string, 5, '5'.",
                Ident("lv_string".into()),
                IntLiteral(5),
                StringLiteral("5".into())
            ),
            def_case_expr!(
                "WRITE: / lv_string, / 5, / '5'.",
                StringLiteral("\n".into()),
                Ident("lv_string".into()),
                StringLiteral("\n".into()),
                IntLiteral(5),
                StringLiteral("\n".into()),
                StringLiteral("5".into())
            ),
            def_case_expr!(
                "WRITE: / lv_string.",
                StringLiteral("\n".into()),
                Ident("lv_string".into())
            ),
            def_case_expr!(
                "WRITE/ lv_string.",
                StringLiteral("\n".into()),
                Ident("lv_string".into())
            ),
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
        let tests = vec![
            def_case_expr!(
                "|this { lv_that } those|.",
                StringLiteral("this ".into()),
                Ident("lv_that".into()),
                StringLiteral(" those".into())
            ),
            def_case_expr!("||.",),
            def_case_expr!(
                "|this{ | is | }{ |a { nested } string| } template|.",
                StringLiteral("this".into()),
                StringTemplate(vec![StringLiteral(" is ".into())]),
                StringTemplate(vec![
                    StringLiteral("a ".into()),
                    Ident("nested".into()),
                    StringLiteral(" string".into()),
                ]),
                StringLiteral(" template".into())
            ),
        ];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::Expression(StringTemplate(expressions)) = &program.statements[0] {
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

    struct TestCaseInfix<'a> {
        input: &'static str,
        operator: Cow<'a, str>,
        left_value: crate::ast::Expression<'a>,
        right_value: crate::ast::Expression<'a>,
    }

    macro_rules! def_case_infix {
        ($input:expr,$operator:literal,$left:expr,$right:expr) => {
            TestCaseInfix {
                input: $input,
                operator: $operator.into(),
                left_value: $left,
                right_value: $right,
            }
        };
    }

    #[test]
    fn test_infix_expression() {
        let tests = vec![
            def_case_infix!("5 + 5.", "+", IntLiteral(5), IntLiteral(5)),
            def_case_infix!("5 - 5.", "-", IntLiteral(5), IntLiteral(5)),
            def_case_infix!("5 * 5.", "*", IntLiteral(5), IntLiteral(5)),
            def_case_infix!("5 / 5.", "/", IntLiteral(5), IntLiteral(5)),
            def_case_infix!(
                "foobar + barfoo.",
                "+",
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "foobar - barfoo.",
                "-",
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "foobar * barfoo.",
                "*",
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "foobar / barfoo.",
                "/",
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
        ];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::Expression(InfixExpression(infix)) = &program.statements[0] {
                assert_eq!(
                    &test.operator, &infix.operator,
                    "operator does not match. want={}, got={}",
                    test.operator, &infix.operator,
                );

                assert_eq!(
                    test.left_value, *infix.left,
                    "operator does not match. want={:?}, got={:?}",
                    test.left_value, *infix.left,
                );
            } else {
                panic!(
                    "program.statements[0] is not an Statement::InfixExpression. got={:?}",
                    program.statements[0]
                )
            }
        }
    }
}
