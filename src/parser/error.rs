use std::num::ParseIntError;

use thiserror::Error;

use crate::lexer::{TokenKind, TokenKinds};

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
    #[error("unrecognized string template token: \n\tkind - {kind}\n\tliteral - {literal}")]
    ParseStringTemplate { kind: TokenKind, literal: String },
    #[error("expected a token, but reached EOF")]
    Eof,
}
