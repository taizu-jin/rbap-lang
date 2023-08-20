use std::num::ParseIntError;

use thiserror::Error;

use crate::{
    ast::Expression,
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
    ParseInfixLeftExpressionMissing,
    ParseInfixUnexpectedToken,
    UndefinedOpcode,
    UnknownOperator,
}

impl From<&ErrorRepr> for ErrorKind {
    fn from(value: &ErrorRepr) -> Self {
        match value {
            ErrorRepr::ParseInt(_) => Self::ParseInt,
            ErrorRepr::ExpectToken { .. } => Self::ExpectToken,
            ErrorRepr::ParseDataAssign { .. } => Self::ParseDataAssign,
            ErrorRepr::ParseStringTemplate { .. } => Self::ParseStringTemplate,
            ErrorRepr::UndefinedOpcode(_) => Self::UndefinedOpcode,
            ErrorRepr::ParseExpression { .. } => Self::ParseExpression,
            ErrorRepr::Eof => Self::Eof,
            ErrorRepr::ParseInfixError(e) => e.into(),
            ErrorRepr::UnknownOperator(_) => Self::UnknownOperator,
            ErrorRepr::UndefinedOpcode(_) => Self::UndefinedOpcode,
        }
    }
}

impl From<&ParseInfixError> for ErrorKind {
    fn from(value: &ParseInfixError) -> Self {
        match value {
            ParseInfixError::LeftExpression => Self::ParseInfixLeftExpressionMissing,
            ParseInfixError::UnexpectedToken { .. } => Self::ParseInfixUnexpectedToken,
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

    pub fn undefined_opcode(opcode: u8) -> Self {
        Self {
            kind: ErrorKind::UndefinedOpcode,
            repr: ErrorRepr::UndefinedOpcode(opcode),
        }
    }
    pub fn unknown_operator(operator: String) -> Self {
        Self {
            kind: ErrorKind::UnknownOperator,
            repr: ErrorRepr::UnknownOperator(operator),
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
    #[error("opcode {0} is undefined")]
    UndefinedOpcode(u8),
    #[error("can't parse expression '{literal}({kind})")]
    ParseExpression { literal: String, kind: TokenKind },
    #[error(transparent)]
    ParseInfixError(#[from] ParseInfixError),
    #[error("unknown operator {0}")]
    UnknownOperator(String),
    #[error("opcode {0} is undefined")]
    UndefinedOpcode(u8),
}

#[derive(Debug, Error)]
pub enum ParseInfixError {
    #[error("Context doesn't carry a left expression to parse infix expression")]
    LeftExpression,
    #[error("Can't parse infix expression for token '{token}'")]
    UnexpectedToken {
        token: TokenKind,
        expression: Expression,
    },
}
