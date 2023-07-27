use crate::{
    ast::{DataDeclaration, DataType, Expression, Program, Statement},
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
        match self.carriage.cur_token {
            Token::Data => self.parse_data_declaration_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let statement = Statement::Expression(self.parse_expression()?);

        if self.carriage.is_peek_token(&Token::Period) {
            self.carriage.next_token()
        }

        Some(statement)
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        match &self.carriage.cur_token {
            Token::IntLiteral(literal) => {
                Self::parse_integer_literal(literal.as_str(), &mut self.carriage.errors)
            }
            Token::StringLiteral(literal) => Some(Self::parse_string_literal(literal.as_str())),
            _ => unimplemented!("{:?}", self.carriage.cur_token),
        }
    }

    fn parse_integer_literal(literal: &str, errors: &mut Vec<String>) -> Option<Expression> {
        match literal.parse::<i64>() {
            Ok(value) => Some(Expression::IntLiteral(value)),
            Err(_) => {
                errors.push(format!("could not parse {} as integer", literal));
                None
            }
        }
    }

    fn parse_string_literal(value: &str) -> Expression {
        Expression::StringLiteral(value.to_string())
    }

    fn parse_data_declaration_statement(&mut self) -> Option<Statement> {
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

        if !self.carriage.expect_tokens(&[Token::Period]) {
            return None;
        }

        Some(Statement::DataDeclaration(vec![DataDeclaration {
            ident,
            ty,
        }]))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{DataDeclaration, DataType};

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
                program.statements[1]
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
                program.statements[1]
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
                    program.statements[1]
                )
            }
        }
    }
}
