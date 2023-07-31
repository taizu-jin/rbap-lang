use crate::{
    ast::{Data, DataDeclaration, DataType, Expression, Program, Statement},
    lexer::{Lexer, Token},
};
use std::fmt::Write;

pub struct Carriage {
    lexer: Lexer,
    peek_token: Token,
    cur_token: Token,
    errors: Vec<String>,
}

impl Carriage {
    fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Self {
            lexer,
            cur_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_tokens(&mut self, tokens: &[Token]) -> bool {
        let mut expected = String::from("expected next token to be one of the following:");

        for token in tokens {
            if self.is_peek_token(token) {
                self.next_token();
                return true;
            } else {
                write!(expected, " {:?}", token).unwrap();
            }
        }

        self.errors
            .push(format!("{}\ngot {:?} instead", expected, self.peek_token));

        false
    }

    fn is_cur_token(&self, token: &Token) -> bool {
        &self.cur_token == token
    }

    fn is_peek_token(&self, token: &Token) -> bool {
        &self.peek_token == token
    }
}

pub struct Parser {
    carriage: Carriage,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let carriage = Carriage::new(lexer);

        Self { carriage }
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while !self.carriage.is_cur_token(&Token::Eof) {
            match self.parse_statement() {
                Some(statement) => {
                    self.carriage.next_token();
                    program.statements.push(statement)
                }
                None => {
                    self.carriage.next_token();
                    continue;
                }
            }
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let statement = match &self.carriage.cur_token {
            Token::Data => self.parse_data_declaration_statement(),
            Token::DataInline(ident) => self.parse_data_statement(ident.to_string()),
            Token::Ident(ident) if self.carriage.is_peek_token(&Token::Assign) => {
                self.parse_data_statement(ident.to_string())
            }
            Token::Write => self.parse_write_statement(),
            _ => Some(Statement::Expression(self.parse_expression()?)),
        };

        if !self.carriage.expect_tokens(&[Token::Period]) {
            return None;
        }

        statement
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        match &self.carriage.cur_token {
            Token::IntLiteral(literal) => {
                Self::parse_integer_literal(literal.as_str(), &mut self.carriage.errors)
            }
            Token::StringLiteral(literal) => Some(Self::parse_string_literal(literal.as_str())),
            Token::Ident(ident) => Some(Expression::Ident(ident.to_owned())),
            _ => unimplemented!(
                "can't parse expression '{}({:?})'",
                self.carriage.cur_token,
                self.carriage.cur_token
            ),
        }
    }

    fn parse_integer_literal(literal: &str, errors: &mut Vec<String>) -> Option<Expression> {
        match literal.parse::<i64>() {
            Ok(value) => Some(Expression::IntLiteral(value)),
            Err(_) => {
                errors.push(format!("can't not parse {} as integer", literal));
                None
            }
        }
    }

    fn parse_string_literal(value: &str) -> Expression {
        Expression::StringLiteral(value.to_string())
    }

    fn parse_data_declaration_statement(&mut self) -> Option<Statement> {
        let mut declarations = Vec::new();

        match self.carriage.is_peek_token(&Token::Colon) {
            true => {
                self.carriage.next_token();

                loop {
                    let declaration = self.parse_data_declaration()?;
                    declarations.push(declaration);

                    if self.carriage.is_peek_token(&Token::Comma) {
                        self.carriage.next_token();
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

        Some(Statement::DataDeclaration(declarations))
    }

    fn parse_data_declaration(&mut self) -> Option<DataDeclaration> {
        // Tokens are equal, if they are of same type, no matter what is the actual literal
        if !self.carriage.expect_tokens(&[Token::Ident("Dummy".into())]) {
            return None;
        }

        let ident = if let Token::Ident(ref ident) = self.carriage.cur_token {
            ident.clone()
        } else {
            unreachable!("current token must be Identifier type")
        };

        if !self.carriage.expect_tokens(&[Token::Type])
            || !self.carriage.expect_tokens(&[Token::String, Token::Int])
        {
            return None;
        }

        let ty = match self.carriage.cur_token {
            Token::Int => DataType::Int,
            Token::String => DataType::String,
            _ => unreachable!("current token must be either Int or String type"),
        };

        Some(DataDeclaration { ident, ty })
    }

    fn parse_data_statement(&mut self, ident: String) -> Option<Statement> {
        if !self.carriage.expect_tokens(&[Token::Assign]) {
            return None;
        }

        let expression = match self.expect_and_parse_expression() {
            Some(expression) => expression,
            None => {
                self.carriage
                    .errors
                    .push("expected an expression after '='".to_string());
                return None;
            }
        };

        Some(Statement::Data(Data {
            ident: ident.to_string(),
            value: expression,
        }))
    }

    fn parse_write_statement(&mut self) -> Option<Statement> {
        let mut expressions = Vec::new();

        match self.carriage.is_peek_token(&Token::Colon) {
            true => {
                self.carriage.next_token();

                loop {
                    if self.carriage.is_peek_token(&Token::Slash) {
                        expressions.push(Expression::StringLiteral("\n".to_string()));
                        self.carriage.next_token();
                    }

                    expressions.push(self.expect_and_parse_expression()?);

                    if self.carriage.is_peek_token(&Token::Comma) {
                        self.carriage.next_token();
                    } else {
                        break;
                    }
                }
            }
            false => {
                if self.carriage.is_peek_token(&Token::Slash) {
                    expressions.push(Expression::StringLiteral("\n".to_string()));
                    self.carriage.next_token();
                }
                expressions.push(self.expect_and_parse_expression()?)
            }
        }
        Some(Statement::Write(expressions))
    }

    fn expect_and_parse_expression(&mut self) -> Option<Expression> {
        if !self.carriage.expect_tokens(&[
            Token::Ident("Dummy".into()),
            Token::IntLiteral("Dummy".into()),
            Token::StringLiteral("Dummy".into()),
        ]) {
            return None;
        }

        self.parse_expression()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Data, DataDeclaration, DataType};

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
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

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
}
