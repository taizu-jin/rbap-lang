use crate::{
    ast::{Expression, Program, Statement},
    lexer::{Lexer, Token},
};

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
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

    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while !self.is_cur_token(&Token::Eof) {
            program.statements.push(self.parse_statement());
            self.next_token();
        }

        program
    }

    fn is_cur_token(&self, token: &Token) -> bool {
        &self.cur_token == token
    }
    fn parse_statement(&mut self) -> Statement {
        match self.cur_token {
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let statement = Statement::Expression(self.parse_expression());

        if self.is_peek_token(&Token::Period) {
            self.next_token()
        }

        statement
    }

    fn is_peek_token(&self, token: &Token) -> bool {
        &self.peek_token == token
    }

    fn parse_expression(&mut self) -> Expression {
        let token = self.cur_token.clone();

        match &self.cur_token {
            Token::IntLiteral(literal) => Self::parse_integer_literal(literal, token).unwrap(),
            Token::StringLiteral(literal) => Self::parse_string_literal(literal, token).unwrap(),
            _ => unimplemented!("{:?}", self.cur_token),
        }
    }

    fn parse_integer_literal(literal: &str, token: Token) -> Result<Expression, String> {
        match literal.parse::<i64>() {
            Ok(value) => Ok(Expression::IntLiteral { value, token }),
            Err(_) => Err(format!("could not parse {} as integer", literal)),
        }
    }

    fn parse_string_literal(value: &str, token: Token) -> Result<Expression, String> {
        Ok(Expression::StringLiteral {
            value: value.to_string(),
            token,
        })
    }
}

#[cfg(test)]
mod tests {
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
                Expression::IntLiteral {
                    value,
                    token: Token::IntLiteral(literal),
                } => {
                    assert_eq!(5, *value, "value is not {}, got={}", 5, *value);
                    assert_eq!("5", literal, "literal is not {}, got={}", "5", literal);
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
        if parser.errors.is_empty() {
            return;
        }

        let mut message = String::new();
        writeln!(message, "parsers has {} errors", parser.errors.len()).unwrap();
        for msg in parser.errors {
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
                Expression::StringLiteral {
                    value,
                    token: Token::StringLiteral(literal),
                } => {
                    assert_eq!("5", value, "value is not {}, got={}", "5", value);
                    assert_eq!("5", literal, "literal is not {}, got={}", "5", literal);
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

}
