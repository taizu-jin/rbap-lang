use std::num::ParseIntError;

use thiserror::Error;

use crate::lexer::Token;

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

#[derive(Debug, Error)]
pub enum Error<'a> {
    #[error(transparent)]
    ParseInt(#[from] ParseIntError),
    #[error("unexpected token: `{0}`")]
    ExpectToken(Token<'a>),
    #[error("expected an expression")]
    ParseDataAssingment,
    #[error("Unrecognized string template token: {0}")]
    ParseStringTemplate(Token<'a>),
}
