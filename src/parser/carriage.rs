use std::iter::Peekable;

use crate::lexer::{LexerIter, Token, TokenKind};

use super::{Error, Precedence, Result};

pub struct Carriage<'t, 's: 't> {
    iter: Peekable<LexerIter<'t, 's>>,
    pub errors: Vec<String>,
}

impl<'t, 's: 't> Carriage<'t, 's> {
    pub fn new(iter: LexerIter<'t, 's>) -> Self {
        Self {
            iter: iter.peekable(),
            errors: Vec::new(),
        }
    }
}

impl<'t, 's: 't> Carriage<'t, 's> {
    pub fn next_token(&mut self) -> Result<Token<'t>> {
        match self.iter.next() {
            Some(token) => Ok(token),
            None => Err(Error::eof()),
        }
    }

    pub fn peek_token(&mut self) -> Result<&Token<'t>> {
        match self.iter.peek() {
            Some(token) => Ok(token),
            None => Err(Error::eof()),
        }
    }

    pub fn peek_precedence(&mut self) -> Result<Precedence> {
        Ok(Precedence::from(self.peek_token()?))
    }

    pub fn expect_tokens(&mut self, tokens: &[TokenKind]) -> Result<Token<'t>> {
        let peek = match self.iter.peek() {
            Some(peek) => peek,
            None => return Err(Error::expected_token(None, tokens.into())),
        };

        for token in tokens {
            if &peek.kind == token {
                return self.next_token();
            }
        }

        Err(Error::expected_token(Some(peek.kind), tokens.into()))
    }

    pub fn is_peek_token(&mut self, token: TokenKind) -> bool {
        match self.iter.peek() {
            Some(peek) => peek.kind == token,
            None => false,
        }
    }
}
