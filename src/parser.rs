mod carriage;
pub mod context;

use crate::{
    ast::{Program, Statement},
    error::{Error, ErrorKind, Result},
    lexer::{LexerIter, Token, TokenKind},
};

pub use carriage::Carriage;
pub use context::Context;

pub use context::Handler;

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    Lowest,
    AndOr,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl From<&Token<'_>> for Precedence {
    fn from(value: &Token<'_>) -> Self {
        let kind = &value.kind;
        Precedence::from(*kind)
    }
}

impl From<TokenKind> for Precedence {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::Asterisk | TokenKind::Slash => Precedence::Product,
            TokenKind::Equals | TokenKind::NotEquals => Precedence::Equals,
            TokenKind::LesserThan | TokenKind::GreaterThan => Precedence::LessGreater,
            TokenKind::Or | TokenKind::And => Precedence::AndOr,
            TokenKind::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

pub fn parse<'t, 's: 't, 'e, T, R, H>(
    carriage: &mut Carriage<'t, 's>,
    context: &Context<'t>,
    handler: H,
) -> Result<R>
where
    H: Handler<'t, 's, T, R>,
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
                Err(e) if e.kind() == ErrorKind::Eof => break,
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
        ast::{
            Assignment, Data, DataType,
            Expression::{self, *},
            Infix, Operator,
        },
        lexer::Lexer,
    };

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
                IntLiteral(value) => {
                    assert_eq!(5i64, (*value).into(), "value is not {}, got={}", 5, *value);
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
                StringLiteral(value) => {
                    assert_eq!("5", value.as_ref(), "value is not {}, got={}", "5", value);
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
                    assert_eq!(
                        "lv_string",
                        ident.as_ref(),
                        "value is not {}, got={}",
                        "5",
                        ident
                    );
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
        expected: Vec<Data>,
    }

    macro_rules! def_ddecl {
        ($kind:ident,$literal:literal) => {
            Data {
                ident: $literal.into(),
                ty: DataType::$kind,
            }
        };
    }

    macro_rules! def_case_ddecl {
        ($input:expr, $($kind:ident:$literal:literal),*) => {
            TestCaseDataDeclaration {
                input: $input,
                expected: vec![$(def_ddecl!($kind, $literal)),*]
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
                                   lv_string2 TYPE string,
                                   lv_bool TYPE rbap_bool.",
                            Int:"lv_int",
                            Int:"lv_int2",
                            String:"lv_string",
                            String:"lv_string2",
                            Bool:"lv_bool"),
        ];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::Declaration(data_declaration) = &program.statements[0] {
                for (i, dd) in data_declaration.as_ref().iter().enumerate() {
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

    struct TestCaseData {
        input: &'static str,
        expected: Assignment,
    }

    macro_rules! def_case_data {
        ($input:expr,$ident:literal,$expected:expr) => {
            TestCaseData {
                input: $input,
                expected: Assignment {
                    ident: $ident.into(),
                    value: $expected,
                },
            }
        };
    }

    #[test]
    fn test_data_statement() {
        let tests = vec![
            def_case_data!("lv_string = '5'.", "lv_string", StringLiteral("5".into())),
            def_case_data!("lv_int = 1.", "lv_int", 1.into()),
            def_case_data!(
                "lv_int = |some { lv_string } literal|.",
                "lv_int",
                StringTemplate(
                    vec![
                        StringLiteral("some ".into()),
                        Ident("lv_string".into()),
                        StringLiteral(" literal".into()),
                    ]
                    .into()
                )
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

            if let Statement::Assignment(data) = &program.statements[0] {
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

    struct TestCaseExpression {
        input: &'static str,
        expected: Vec<crate::ast::Expression>,
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
            def_case_expr!("WRITE 1.", 1.into()),
            def_case_expr!("WRITE lv_string.", Ident("lv_string".into())),
            def_case_expr!(
                "WRITE: lv_string, 5, '5'.",
                Ident("lv_string".into()),
                5.into(),
                StringLiteral("5".into())
            ),
            def_case_expr!(
                "WRITE: / lv_string, / 5, / '5'.",
                StringLiteral("\n".into()),
                Ident("lv_string".into()),
                StringLiteral("\n".into()),
                5.into(),
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
                    &test.expected,
                    expressions.as_ref(),
                    "expression is not '{:?}'. got={:?}",
                    test.expected,
                    expressions
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
                StringTemplate(vec![StringLiteral(" is ".into())].into()),
                StringTemplate(
                    vec![
                        StringLiteral("a ".into()),
                        Ident("nested".into()),
                        StringLiteral(" string".into()),
                    ]
                    .into()
                ),
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
                    &test.expected,
                    expressions.as_ref(),
                    "expression is not '{:?}'. got={:?}",
                    test.expected,
                    expressions
                );
            } else {
                panic!(
                    "program.statements[0] is not an Statement::Expression(Expression::StringTemplate). got={:?}",
                    program.statements[0]
                )
            }
        }
    }

    struct TestCasePrefix {
        input: &'static str,
        operator: TokenKind,
        right_value: crate::ast::Expression,
    }

    macro_rules! def_case_prefix {
        ($input:expr,$operator:ident,$right:expr) => {
            TestCasePrefix {
                input: $input,
                operator: TokenKind::$operator,
                right_value: $right,
            }
        };
    }

    #[test]
    fn test_prefix_expression() -> Result<()> {
        let tests = vec![
            def_case_prefix!("-15.", Minus, 15.into()),
            def_case_prefix!("-foobar.", Minus, Ident("foobar".into())),
            def_case_prefix!("NOT rbap_true.", Not, BoolLiteral(true.into())),
            def_case_prefix!("NOT rbap_false.", Not, BoolLiteral(false.into())),
            def_case_prefix!("NOT foobar.", Not, Ident("foobar".into())),
        ];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::Expression(PrefixExpression(infix)) = &program.statements[0] {
                assert_eq!(
                    Operator::try_from(test.operator)?,
                    infix.operator,
                    "operator does not match. want={}, got={}",
                    test.operator,
                    &infix.operator,
                );

                assert_eq!(
                    test.right_value, *infix.right,
                    "operator does not match. want={:?}, got={:?}",
                    test.right_value, *infix.right,
                );
            } else {
                panic!(
                    "program.statements[0] is not an Statement::PrefixExpression. got={:?}",
                    program.statements[0]
                )
            }
        }

        Ok(())
    }

    struct TestCaseInfix {
        input: &'static str,
        operator: TokenKind,
        left_value: crate::ast::Expression,
        right_value: crate::ast::Expression,
    }

    macro_rules! def_case_infix {
        ($input:expr,$operator:ident,$left:expr,$right:expr) => {
            TestCaseInfix {
                input: $input,
                operator: TokenKind::$operator,
                left_value: $left,
                right_value: $right,
            }
        };
    }

    #[test]
    fn test_infix_expression() -> Result<()> {
        let tests = vec![
            def_case_infix!("5 + 5.", Plus, 5.into(), 5.into()),
            def_case_infix!("5 - 5.", Minus, 5.into(), 5.into()),
            def_case_infix!("5 * 5.", Asterisk, 5.into(), 5.into()),
            def_case_infix!("5 / 5.", Slash, 5.into(), 5.into()),
            def_case_infix!("5 > 5.", GreaterThan, 5.into(), 5.into()),
            def_case_infix!("5 < 5.", LesserThan, 5.into(), 5.into()),
            def_case_infix!("5 == 5.", Equals, 5.into(), 5.into()),
            def_case_infix!("5 <> 5.", NotEquals, 5.into(), 5.into()),
            def_case_infix!(
                "foobar + barfoo.",
                Plus,
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "foobar - barfoo.",
                Minus,
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "foobar * barfoo.",
                Asterisk,
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "foobar / barfoo.",
                Slash,
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "foobar > barfoo.",
                GreaterThan,
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "foobar < barfoo.",
                LesserThan,
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "foobar == barfoo.",
                Equals,
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "foobar <> barfoo.",
                NotEquals,
                Ident("foobar".into()),
                Ident("barfoo".into())
            ),
            def_case_infix!(
                "rbap_true == rbap_true.",
                Equals,
                BoolLiteral(true.into()),
                BoolLiteral(true.into())
            ),
            def_case_infix!(
                "rbap_true <> rbap_false.",
                NotEquals,
                BoolLiteral(true.into()),
                BoolLiteral(false.into())
            ),
            def_case_infix!(
                "rbap_false == rbap_false.",
                Equals,
                BoolLiteral(false.into()),
                BoolLiteral(false.into())
            ),
            def_case_infix!(
                "rbap_true AND rbap_true.",
                And,
                BoolLiteral(true.into()),
                BoolLiteral(true.into())
            ),
            def_case_infix!(
                "rbap_true OR rbap_false.",
                Or,
                BoolLiteral(true.into()),
                BoolLiteral(false.into())
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
                    Operator::try_from(test.operator)?,
                    infix.operator,
                    "operator does not match. want={}, got={}",
                    test.operator,
                    &infix.operator,
                );

                assert_eq!(
                    test.left_value, *infix.left,
                    "operator does not match. want={:?}, got={:?}",
                    test.left_value, *infix.left,
                );

                assert_eq!(
                    test.right_value, *infix.right,
                    "operator does not match. want={:?}, got={:?}",
                    test.right_value, *infix.right,
                );
            } else {
                panic!(
                    "program.statements[0] is not an Statement::InfixExpression. got={:?}",
                    program.statements[0]
                )
            }
        }

        Ok(())
    }

    struct TestCasePrecedence {
        input: &'static str,
        expected: &'static str,
    }

    macro_rules! define_case {
            ($($input:expr,$expected:expr),+) => {
                vec![$(TestCasePrecedence{
                    input: $input,
                    expected: $expected,
                }),+]
            };
        }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = define_case!(
            "-a * b.",
            "((-a) * b).",
            "NOT rbap_true.",
            "(NOT rbap_true).",
            "NOT NOT rbap_true.",
            "(NOT (NOT rbap_true)).",
            "a + b + c.",
            "((a + b) + c).",
            "a + b - c.",
            "((a + b) - c).",
            "a * b * c.",
            "((a * b) * c).",
            "a * b / c.",
            "((a * b) / c).",
            "a + b / c.",
            "(a + (b / c)).",
            "a + b * c + d / e - f.",
            "(((a + (b * c)) + (d / e)) - f).",
            "3 + 4. -5 * 5.",
            "(3 + 4).((-5) * 5).",
            "1 + (2 + 3) + 4.",
            "((1 + (2 + 3)) + 4).",
            "(5 + 5) * 2.",
            "((5 + 5) * 2).",
            "2 / (5 + 5).",
            "(2 / (5 + 5)).",
            "(5 + 5) * 2 * (5 + 5).",
            "(((5 + 5) * 2) * (5 + 5)).",
            "-(5 + 5).",
            "(-(5 + 5)).",
            "rbap_true.",
            "rbap_true.",
            "rbap_false.",
            "rbap_false.",
            "5 > 4 == 3 < 4.",
            "((5 > 4) == (3 < 4)).",
            "5 < 4 <> 3 > 4.",
            "((5 < 4) <> (3 > 4)).",
            "3 > 5 == rbap_false.",
            "((3 > 5) == rbap_false).",
            "3 < 5 == rbap_true.",
            "((3 < 5) == rbap_true).",
            "NOT (true == true).",
            "(NOT (true == true))."
        );

        for test in tests {
            let program = parse_program(test.input.to_string());

            let got = program
                .statements
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .concat();

            assert_eq!(
                test.expected, got,
                "\nexpected={}\ngot={}",
                test.expected, got
            )
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "IF x < y.
x.
ENDIF.";
        let program = parse_program(input.into());

        assert_eq!(
            1,
            program.statements.len(),
            "program has not enough statements. got={}",
            program.statements.len()
        );

        if let Statement::If(statement) = &program.statements[0] {
            match &statement.condition {
                InfixExpression(ie) => {
                    assert_eq!(*ie.left, Ident("x".into()));
                    assert_eq!(ie.operator, Operator::LesserThan);
                    assert_eq!(*ie.right, Ident("y".into()));
                }
                cond => panic!(
                    "condition is not an Expression::InfixExpression. got={:?}",
                    cond
                ),
            };

            for (a, e) in statement
                .consequence
                .statements
                .iter()
                .zip(vec![Statement::Expression(Ident("x".into()))])
            {
                assert_eq!(
                    a, &e,
                    "statement within a block is not as expected.\n\tgot={}\n\twant={}",
                    a, e
                );
            }

            assert_eq!(statement.alternative, None);
        } else {
            panic!(
                "program.statements[0] is not an Statement::IfStatement. got={:?}",
                program.statements[0]
            )
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "IF x < y.
x.
ELSE.
y.
ENDIF.";
        let program = parse_program(input.into());

        assert_eq!(
            1,
            program.statements.len(),
            "program has not enough statements. got={}",
            program.statements.len()
        );

        if let Statement::If(statement) = &program.statements[0] {
            match &statement.condition {
                InfixExpression(ie) => {
                    assert_eq!(*ie.left, Ident("x".into()));
                    assert_eq!(ie.operator, Operator::LesserThan);
                    assert_eq!(*ie.right, Ident("y".into()));
                }
                cond => panic!(
                    "condition is not an Expression::InfixExpression. got={:?}",
                    cond
                ),
            };

            for (a, e) in statement
                .consequence
                .statements
                .iter()
                .zip(vec![Statement::Expression(Ident("x".into()))])
            {
                assert_eq!(
                    a, &e,
                    "statement within a block is not as expected.\n\tgot={}\n\twant={}",
                    a, e
                );
            }

            for (a, e) in statement
                .alternative
                .as_ref()
                .expect("should have parsed an alernative block")
                .statements
                .iter()
                .zip(vec![Statement::Expression(Ident("y".into()))])
            {
                assert_eq!(
                    a, &e,
                    "statement within a block is not as expected.\n\tgot={}\n\twant={}",
                    a, e
                );
            }
        } else {
            panic!(
                "program.statements[0] is not an Statement::IfStatement. got={:?}",
                program.statements[0]
            )
        }
    }

    struct TestCaseFunction {
        input: &'static str,
        name: &'static str,
        parameters: Vec<Data>,
        returns: Vec<Data>,
    }

    macro_rules! def_case_function {
            ($input:expr;$name:expr;$($par_ident:literal,$par_ty:ident),+;$($ret_ident:literal,$ret_ty:ident),+) => {
                TestCaseFunction{
                    input: $input,
                    name: $name.into(),
                    parameters: vec![$(def_ddecl!($par_ty, $par_ident)),+],
                    returns: vec![$(def_ddecl!($ret_ty, $ret_ident)),+],
                }
            };
        }

    #[test]
    fn test_function_statement_name_and_parameter_parsing() {
        let tests = vec![def_case_function!(
            "METHOD sum IMPORTING iv_x TYPE i iv_y TYPE i RETURNING rv_sum TYPE i.
rv_sum = iv_x + iv_y.
ENDMETHOD.";
            "sum";
            "iv_x", Int, "iv_y", Int;
            "rv_sum", Int
        )];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::Function(statement) = &program.statements[0] {
                assert_eq!(
                    statement.name, test.name,
                    "function names do not match.\n\tgot={}\n\twant={}",
                    statement.name, test.name
                );

                assert_eq!(
                    statement.parameters.len(),
                    test.parameters.len(),
                    "parameter count does not match.\n\tgot={}\n\twant={}",
                    statement.parameters.len(),
                    test.parameters.len()
                );

                for (g, w) in statement.parameters.iter().zip(test.parameters.iter()) {
                    assert_eq!(g, w, "parameters do not match.\n\tgot={}\n\twant={}", g, w);
                }

                assert_eq!(
                    statement.returns.len(),
                    test.returns.len(),
                    "return parameter count does not match.\n\tgot={}\n\twant={}",
                    statement.returns.len(),
                    test.returns.len()
                );

                for (g, w) in statement.returns.iter().zip(test.returns.iter()) {
                    assert_eq!(g, w, "parameters do not match.\n\tgot={}\n\twant={}", g, w);
                }
            } else {
                panic!(
                    "program.statements[0] is not an Statement::Function. got={:?}",
                    program.statements[0]
                )
            }
        }
    }

    #[test]
    fn test_function_statement_body_parsing() {
        let input = "METHOD sum IMPORTING iv_x TYPE i iv_y TYPE i RETURNING rv_sum TYPE i.
DATA: lv_sum TYPE i.

lv_sum = iv_x + iv_y.
rv_sum = lv_sum.
ENDMETHOD.";

        let program = parse_program(input.to_string());

        assert_eq!(
            1,
            program.statements.len(),
            "program has not enough statements. got={}",
            program.statements.len()
        );
        if let Statement::Function(statement) = &program.statements[0] {
            for (a, e) in statement
                .body
                .statements
                .iter()
                .map(|s| s.to_string())
                .zip(vec![
                    "DATA: lv_sum TYPE i.",
                    "lv_sum = (iv_x + iv_y).",
                    "rv_sum = lv_sum.",
                ])
            {
                assert_eq!(
                    a, e,
                    "statement within a block is not as expected.\n\tgot={}\n\twant={}",
                    a, e
                );
            }
        } else {
            panic!(
                "program.statements[0] is not an Statement::Function. got={:?}",
                program.statements[0]
            )
        }
    }

    struct TestCaseCall {
        input: &'static str,
        function: &'static str,
        arguments: Vec<Expression>,
    }

    macro_rules! def_case_call{
        ($input:literal, $function:literal, $($argument:expr),+) => {
            TestCaseCall{
                input: $input.into(),
                function: $function.into(),
                arguments: vec![$($argument),+],
            }
        }
    }

    #[test]
    fn test_call_expression() {
        let tests = vec![def_case_call!(
            "add(1, 2 * 3, 4 + 5).",
            "add",
            1.into(),
            InfixExpression(Infix {
                left: Expression::from(2).into(),
                operator: Operator::Mul,
                right: Expression::from(3).into(),
            }),
            InfixExpression(Infix {
                left: Expression::from(4).into(),
                operator: Operator::Add,
                right: Expression::from(5).into(),
            })
        )];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::Expression(CallExpression(ce)) = &program.statements[0] {
                assert_eq!(ce.function.as_ref(), test.function);

                assert_eq!(
                    ce.arguments.len(),
                    test.arguments.len(),
                    "argument count does not match.\n\tgot={}\n\twant={}",
                    ce.arguments.len(),
                    test.arguments.len(),
                );

                for (g, w) in ce.arguments.iter().zip(test.arguments.iter()) {
                    assert_eq!(g, w, "expression do not match.\n\tgot={}\n\twant={}", g, w);
                }
            } else {
                panic!(
                "program.statements[0] is not an Statement::Expression(Expression::CallExpression). got={:?}",
                program.statements[0]
            )
            }
        }
    }

    struct TestCaseCallArg {
        input: &'static str,
        function: &'static str,
        arguments: Vec<&'static str>,
    }

    macro_rules! def_case_callarg{
        ($input:literal, $function:literal, $($argument:literal),*) => {
            TestCaseCallArg{
                input: $input.into(),
                function: $function.into(),
                arguments: vec![$($argument),*],
            }
        }
    }

    #[test]
    fn test_call_expression_argument_parsing() {
        let tests = vec![
            def_case_callarg!("add().", "add",),
            def_case_callarg!("add(1).", "add", "1"),
            def_case_callarg!("add(1, 2 * 3, 4 + 5).", "add", "1", "(2 * 3)", "(4 + 5)"),
        ];

        for test in tests {
            let program = parse_program(test.input.to_string());

            assert_eq!(
                1,
                program.statements.len(),
                "program has not enough statements. got={}",
                program.statements.len()
            );

            if let Statement::Expression(CallExpression(ce)) = &program.statements[0] {
                assert_eq!(ce.function.as_ref(), test.function);

                assert_eq!(
                    ce.arguments.len(),
                    test.arguments.len(),
                    "argument count does not match.\n\tgot={}\n\twant={}",
                    ce.arguments.len(),
                    test.arguments.len(),
                );

                let got = ce.arguments.iter().map(|arg| arg.to_string());

                for (g, w) in got.zip(test.arguments.iter()) {
                    assert_eq!(g, *w, "arguments do not match.\n\tgot={}\n\twant={}", g, w);
                }
            } else {
                panic!(
                "program.statements[0] is not an Statement::Expression(Expression::CallExpression). got={:?}",
                program.statements[0]
            )
            }
        }
    }
}
