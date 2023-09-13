use std::num::ParseIntError;

use thiserror::Error;

use crate::{
    ast::{DataType, Expression, Operator},
    lexer::{Token, TokenKind, TokenKinds},
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum ErrorKind {
    ParseInt,
    ExpectToken,
    ParseDataAssign,
    ParseStringTemplate,
    Eof,
    ParseExpression,
    UnexpectedExpression,
    ContextExpression,
    ParseInfixUnexpectedToken,
    ParseInfixUnusupportedOperator,
    ParsePrefixUnusupportedOperator,
    UnknownOperator,
    CompilerUndefinedVariable,
    CompilerExpectedDataType,
    CompilerUndefinedOpcode,
}

impl From<&ErrorRepr> for ErrorKind {
    fn from(value: &ErrorRepr) -> Self {
        match value {
            ErrorRepr::ParseInt(_) => Self::ParseInt,
            ErrorRepr::ExpectToken { .. } => Self::ExpectToken,
            ErrorRepr::ParseDataAssign { .. } => Self::ParseDataAssign,
            ErrorRepr::ParseStringTemplate { .. } => Self::ParseStringTemplate,
            ErrorRepr::ParseExpression { .. } => Self::ParseExpression,
            ErrorRepr::Eof => Self::Eof,
            ErrorRepr::ParseInfixError(e) => e.into(),
            ErrorRepr::ParsePrefixError(e) => e.into(),
            ErrorRepr::ContextError(e) => e.into(),
            ErrorRepr::CompilerError(e) => e.into(),
            ErrorRepr::UnknownOperator(_) => Self::UnknownOperator,
            ErrorRepr::UnexpectedExpression(_) => Self::UnexpectedExpression,
        }
    }
}

impl From<&ParseInfixError> for ErrorKind {
    fn from(value: &ParseInfixError) -> Self {
        match value {
            ParseInfixError::UnsupportedOperator(_) => Self::ParseInfixUnusupportedOperator,
            ParseInfixError::UnexpectedToken { .. } => Self::ParseInfixUnexpectedToken,
        }
    }
}

impl From<&ParsePrefixError> for ErrorKind {
    fn from(value: &ParsePrefixError) -> Self {
        match value {
            ParsePrefixError::UnsupportedOperator(_) => Self::ParsePrefixUnusupportedOperator,
        }
    }
}

impl From<&ContextError> for ErrorKind {
    fn from(value: &ContextError) -> Self {
        match value {
            ContextError::Expression => Self::ContextExpression,
        }
    }
}

impl From<&CompilerError> for ErrorKind {
    fn from(value: &CompilerError) -> Self {
        match value {
            CompilerError::UndefinedVariable(_) => Self::CompilerUndefinedVariable,
            CompilerError::ExpectedDataType { .. } => Self::CompilerExpectedDataType,
            CompilerError::UndefinedOpcode(_) => Self::CompilerUndefinedOpcode,
        }
    }
}

#[derive(Error, Debug)]
#[error("{repr}")]
pub struct Error {
    kind: ErrorKind,
    #[source]
    repr: ErrorRepr,
}

impl Error {
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    pub fn expression(self) -> Option<Expression> {
        match self {
            Self {
                repr:
                    ErrorRepr::ParseInfixError(ParseInfixError::UnexpectedToken { expression, .. }),
                ..
            } => Some(expression),
            _ => None,
        }
    }

    pub fn eof() -> Self {
        Self {
            kind: ErrorKind::Eof,
            repr: ErrorRepr::Eof,
        }
    }

    pub fn expected_token(got: Option<TokenKind>, expected: TokenKinds) -> Self {
        Self {
            kind: ErrorKind::ExpectToken,
            repr: ErrorRepr::ExpectToken { got, expected },
        }
    }

    pub fn parse_data_assign(current: TokenKind, peek: TokenKind) -> Self {
        Self {
            kind: ErrorKind::ParseDataAssign,
            repr: ErrorRepr::ParseDataAssign { current, peek },
        }
    }

    pub fn parse_string_template(token: &Token) -> Self {
        Self {
            kind: ErrorKind::ParseStringTemplate,
            repr: ErrorRepr::ParseStringTemplate {
                kind: token.kind,
                literal: token.literal.to_string(),
            },
        }
    }

    pub fn parse_expression(token: &Token) -> Self {
        Self {
            kind: ErrorKind::ParseExpression,
            repr: ErrorRepr::ParseExpression {
                kind: token.kind,
                literal: token.literal.to_string(),
            },
        }
    }

    pub fn unknown_operator(kind: TokenKind) -> Self {
        Self {
            kind: ErrorKind::UnknownOperator,
            repr: ErrorRepr::UnknownOperator(kind),
        }
    }

    pub fn unexpected_expression(expression: Expression) -> Self {
        Self {
            kind: ErrorKind::UnexpectedExpression,
            repr: ErrorRepr::UnexpectedExpression(expression),
        }
    }
}

impl From<ErrorRepr> for Error {
    fn from(value: ErrorRepr) -> Self {
        Error {
            kind: ErrorKind::from(&value),
            repr: value,
        }
    }
}

impl From<ParseIntError> for Error {
    fn from(value: ParseIntError) -> Self {
        Self {
            kind: ErrorKind::ParseInt,
            repr: value.into(),
        }
    }
}

impl From<ParseInfixError> for Error {
    fn from(value: ParseInfixError) -> Self {
        Self {
            kind: ErrorKind::from(&value),
            repr: value.into(),
        }
    }
}

impl From<ParsePrefixError> for Error {
    fn from(value: ParsePrefixError) -> Self {
        Self {
            kind: ErrorKind::from(&value),
            repr: value.into(),
        }
    }
}

impl From<ContextError> for Error {
    fn from(value: ContextError) -> Self {
        Self {
            kind: ErrorKind::from(&value),
            repr: value.into(),
        }
    }
}

impl From<CompilerError> for Error {
    fn from(value: CompilerError) -> Self {
        Self {
            kind: ErrorKind::from(&value),
            repr: value.into(),
        }
    }
}

#[derive(Debug, Error)]
enum ErrorRepr {
    #[error(transparent)]
    ParseInt(#[from] ParseIntError),
    #[error("expected tokens: {expected}. got={got:?}")]
    ExpectToken {
        got: Option<TokenKind>,
        expected: TokenKinds,
    },
    #[error("Failed to parse data assingment.\n\tcurrent token kind - {current}\n\tpeek token kind - {peek}")]
    ParseDataAssign { current: TokenKind, peek: TokenKind },
    #[error("unrecognized string template token: \n\tkind - {kind}\n\tliteral - {literal}")]
    ParseStringTemplate { kind: TokenKind, literal: String },
    #[error("expected a token, but reached EOF")]
    Eof,
    #[error("can't parse expression '{literal}({kind})")]
    ParseExpression { literal: String, kind: TokenKind },
    #[error(transparent)]
    ParseInfixError(#[from] ParseInfixError),
    #[error(transparent)]
    ParsePrefixError(#[from] ParsePrefixError),
    #[error("Unexpected expression {0}")]
    UnexpectedExpression(Expression),
    #[error("unknown operator {0}")]
    UnknownOperator(TokenKind),
    #[error(transparent)]
    ContextError(#[from] ContextError),
    #[error(transparent)]
    CompilerError(#[from] CompilerError),
}

#[derive(Debug, Error)]
pub enum ContextError {
    #[error("Context has expression missing")]
    Expression,
}

#[derive(Debug, Error)]
pub enum ParseInfixError {
    #[error("Unsupported operator `{0}` for an infix expression")]
    UnsupportedOperator(Operator),
    #[error("Can't parse infix expression for token '{token}'")]
    UnexpectedToken {
        token: TokenKind,
        expression: Expression,
    },
}

#[derive(Debug, Error)]
pub enum ParsePrefixError {
    #[error("Unsupported operator `{0}` for an prefix expression")]
    UnsupportedOperator(Operator),
}

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("undefined variable {0}")]
    UndefinedVariable(String),
    #[error("Expected data type {expected}. Got {got}")]
    ExpectedDataType { got: DataType, expected: DataType },
    #[error("opcode {0} is undefined")]
    UndefinedOpcode(u8),
}

impl From<(DataType, DataType)> for CompilerError {
    fn from(value: (DataType, DataType)) -> Self {
        Self::ExpectedDataType {
            expected: value.0,
            got: value.1,
        }
    }
}
