use std::num::ParseIntError;

use thiserror::Error;

use crate::lexer::{Token, TokenKind, TokenKinds};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
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
}

impl From<Token<'_>> for Error {
    fn from(value: Token<'_>) -> Self {
        Error::ParseExpression {
            literal: value.literal.to_string(),
            kind: value.kind,
        }
    }
}
