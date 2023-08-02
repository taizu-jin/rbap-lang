mod token;

use std::borrow::Cow;

pub use token::{Token, TokenKind};

pub struct Lexer {
    input: Vec<u8>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input: input.to_lowercase().into_bytes(),
        }
    }

    pub fn iter(&self) -> LexerIter {
        let mut iter = LexerIter {
            input: &self.input,
            position: 0,
            read_position: 0,
            ch: 0,
            strategy: Strategy::Token,
        };

        iter.read_char();
        iter
    }
}
pub(super) enum Strategy<'a> {
    Ident,
    String,
    Int,
    StrTemplate,
    Token,
    Single(Token<'a>),
}

pub struct LexerIter<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    ch: u8,
    strategy: Strategy<'a>,
}

impl<'a> LexerIter<'a> {
    fn next_token(&mut self) -> Option<Token<'a>> {
        match self.strategy {
            Strategy::Ident => Some(self.get_ident()),
            Strategy::String => Some(self.get_string()),
            Strategy::Int => Some(self.get_int()),
            Strategy::StrTemplate => Some(self.get_string_template()),
            Strategy::Token => self.get_token(),
            Strategy::Single(ref token) => Some(self.get_single_token(token.clone())),
        }
    }

    fn get_single_token(&mut self, token: Token<'a>) -> Token<'a> {
        self.read_char();
        self.switch_strategy(Strategy::Token);
        token
    }

    fn get_string_template(&mut self) -> Token<'a> {
        let pos = self.position;

        loop {
            match self.ch {
                b'{' => {
                    self.switch_strategy(Strategy::Token);
                    break;
                }
                b'|' => {
                    let literal =
                        String::from_utf8_lossy(&self.input[self.position..self.position + 1]);
                    let token = Token::new(literal, TokenKind::VSlash);
                    self.switch_strategy(Strategy::Single(token));
                    break;
                }
                0 => {
                    break;
                }
                _ => (),
            }
            self.read_char();
        }

        let literal = String::from_utf8_lossy(&self.input[pos..self.position]);

        Token::new(literal, TokenKind::StringLiteral)
    }

    fn get_token(&mut self) -> Option<Token<'a>> {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => self.tokenize_char(TokenKind::Assign),
            b'+' => self.tokenize_char(TokenKind::Plus),
            b'-' => self.tokenize_char(TokenKind::Minus),
            b'/' => self.tokenize_char(TokenKind::Slash),
            b'*' => self.tokenize_char(TokenKind::Asterisk),
            b'(' => self.tokenize_char(TokenKind::LParen),
            b')' => self.tokenize_char(TokenKind::RParen),
            b'.' => self.tokenize_char(TokenKind::Period),
            b':' => self.tokenize_char(TokenKind::Colon),
            b',' => self.tokenize_char(TokenKind::Comma),
            b'|' => {
                self.switch_strategy(Strategy::StrTemplate);
                self.tokenize_char(TokenKind::VSlash)
            }
            b'{' => self.tokenize_char(TokenKind::LSquirly),
            b'}' => {
                self.switch_strategy(Strategy::StrTemplate);
                self.tokenize_char(TokenKind::RSquirly)
            }
            b'\'' => {
                self.switch_strategy(Strategy::String);
                self.next_token()?
            }
            ch if ch.is_ascii_alphabetic() || ch == b'_' => {
                self.switch_strategy(Strategy::Ident);
                return self.next_token();
            }
            ch if ch.is_ascii_digit() => {
                self.switch_strategy(Strategy::Int);
                return self.next_token();
            }
            0 => return None,
            _ => self.tokenize_char(TokenKind::Illegal),
        };

        self.read_char();

        Some(token)
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char()
        }
    }

    fn switch_strategy(&mut self, strategy: Strategy<'a>) {
        self.strategy = strategy
    }

    fn tokenize_char(&mut self, kind: TokenKind) -> Token<'a> {
        let literal = String::from_utf8_lossy(&self.input[self.position..self.position + 1]);
        Token::new(literal, kind)
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn get_ident(&mut self) -> Token<'a> {
        let literal = self.read_ident();

        let token = if literal == "data" && self.ch == b'(' {
            let token = Token::new(literal, TokenKind::Data);

            self.get_data_inline(token)
        } else {
            let kind = match TokenKind::from(&literal) {
                Some(kind) => kind,
                None => TokenKind::Ident,
            };

            Token::new(literal, kind)
        };

        self.switch_strategy(Strategy::Token);
        token
    }

    fn read_ident(&mut self) -> Cow<'a, str> {
        let pos = self.position;

        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char()
        }

        String::from_utf8_lossy(&self.input[pos..self.position])
    }

    fn get_int(&mut self) -> Token<'a> {
        self.switch_strategy(Strategy::Token);
        let literal = self.read_int();

        Token::new(literal, TokenKind::IntLiteral)
    }

    fn read_int(&mut self) -> Cow<'a, str> {
        let pos = self.position;

        while self.ch.is_ascii_digit() || self.ch == b'_' {
            self.read_char()
        }

        String::from_utf8_lossy(&self.input[pos..self.position])
    }

    fn get_string(&mut self) -> Token<'a> {
        self.switch_strategy(Strategy::Token);
        let literal = self.read_string();

        Token::new(literal, TokenKind::StringLiteral)
    }

    fn read_string(&mut self) -> Cow<'a, str> {
        let pos = self.position + 1;

        loop {
            self.read_char();
            if self.ch == b'\'' || self.ch == 0 {
                break;
            }
        }

        String::from_utf8_lossy(&self.input[pos..self.position])
    }

    fn get_data_inline(&mut self, token: Token<'a>) -> Token<'a> {
        let position = self.position;
        let read_position = self.read_position;
        let ch = self.ch;

        self.read_char();

        let literal = self.read_ident();
        if self.ch != b')' {
            self.position = position;
            self.read_position = read_position;
            self.ch = ch;

            return token;
        }

        self.read_char();

        Token::new(literal, TokenKind::DataInline)
    }
}

impl<'a> Iterator for LexerIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = r#"DATA: lv_string TYPE string,
      lv_int TYPE i.

lv_string = '(ツ)'.
WRITE lv_string.

DATA(lv_template_string) = |\_{ lv_string }_/|.
WRITE: / ' ', lv_template_string, ' '.

WRITE: / lv_int.
lv_int = 5 + 10.
WRITE: / lv_int.
WRITE: / 'answer to life:', / (lv_int - 2 * 5) * (16 - 32 / 4) + 2.
WRITE: | |, | { } |, |a{ |b| }c|.
WRITE: |{}|.
}abc|.
WRITE: |nested { | string | } templates|.
WRITE: |nested { | { 'string' } | } templates|.

DATA lv_string2 TYPE string.
DATA( lv_test) = '5'."#;

        let tokens = vec![
            Token::new("data".into(), TokenKind::Data),
            Token::new(":".into(), TokenKind::Colon),
            Token::new("lv_string".into(), TokenKind::Ident),
            Token::new("type".into(), TokenKind::Type),
            Token::new("string".into(), TokenKind::String),
            Token::new(",".into(), TokenKind::Comma),
            Token::new("lv_int".into(), TokenKind::Ident),
            Token::new("type".into(), TokenKind::Type),
            Token::new("i".into(), TokenKind::Int),
            Token::new(".".into(), TokenKind::Period),
            Token::new("lv_string".into(), TokenKind::Ident),
            Token::new("=".into(), TokenKind::Assign),
            Token::new("(ツ)".into(), TokenKind::StringLiteral),
            Token::new(".".into(), TokenKind::Period),
            Token::new("write".into(), TokenKind::Write),
            Token::new("lv_string".into(), TokenKind::Ident),
            Token::new(".".into(), TokenKind::Period),
            Token::new("lv_template_string".into(), TokenKind::DataInline),
            Token::new("=".into(), TokenKind::Assign),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new("\\_".into(), TokenKind::StringLiteral),
            Token::new("{".into(), TokenKind::LSquirly),
            Token::new("lv_string".into(), TokenKind::Ident),
            Token::new("}".into(), TokenKind::RSquirly),
            Token::new("_/".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(".".into(), TokenKind::Period),
            Token::new("write".into(), TokenKind::Write),
            Token::new(":".into(), TokenKind::Colon),
            Token::new("/".into(), TokenKind::Slash),
            Token::new(" ".into(), TokenKind::StringLiteral),
            Token::new(",".into(), TokenKind::Comma),
            Token::new("lv_template_string".into(), TokenKind::Ident),
            Token::new(",".into(), TokenKind::Comma),
            Token::new(" ".into(), TokenKind::StringLiteral),
            Token::new(".".into(), TokenKind::Period),
            Token::new("write".into(), TokenKind::Write),
            Token::new(":".into(), TokenKind::Colon),
            Token::new("/".into(), TokenKind::Slash),
            Token::new("lv_int".into(), TokenKind::Ident),
            Token::new(".".into(), TokenKind::Period),
            Token::new("lv_int".into(), TokenKind::Ident),
            Token::new("=".into(), TokenKind::Assign),
            Token::new("5".into(), TokenKind::IntLiteral),
            Token::new("+".into(), TokenKind::Plus),
            Token::new("10".into(), TokenKind::IntLiteral),
            Token::new(".".into(), TokenKind::Period),
            Token::new("write".into(), TokenKind::Write),
            Token::new(":".into(), TokenKind::Colon),
            Token::new("/".into(), TokenKind::Slash),
            Token::new("lv_int".into(), TokenKind::Ident),
            Token::new(".".into(), TokenKind::Period),
            Token::new("write".into(), TokenKind::Write),
            Token::new(":".into(), TokenKind::Colon),
            Token::new("/".into(), TokenKind::Slash),
            Token::new("answer to life:".into(), TokenKind::StringLiteral),
            Token::new(",".into(), TokenKind::Comma),
            Token::new("/".into(), TokenKind::Slash),
            Token::new("(".into(), TokenKind::LParen),
            Token::new("lv_int".into(), TokenKind::Ident),
            Token::new("-".into(), TokenKind::Minus),
            Token::new("2".into(), TokenKind::IntLiteral),
            Token::new("*".into(), TokenKind::Asterisk),
            Token::new("5".into(), TokenKind::IntLiteral),
            Token::new(")".into(), TokenKind::RParen),
            Token::new("*".into(), TokenKind::Asterisk),
            Token::new("(".into(), TokenKind::LParen),
            Token::new("16".into(), TokenKind::IntLiteral),
            Token::new("-".into(), TokenKind::Minus),
            Token::new("32".into(), TokenKind::IntLiteral),
            Token::new("/".into(), TokenKind::Slash),
            Token::new("4".into(), TokenKind::IntLiteral),
            Token::new(")".into(), TokenKind::RParen),
            Token::new("+".into(), TokenKind::Plus),
            Token::new("2".into(), TokenKind::IntLiteral),
            Token::new(".".into(), TokenKind::Period),
            Token::new("write".into(), TokenKind::Write),
            Token::new(":".into(), TokenKind::Colon),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(" ".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(",".into(), TokenKind::Comma),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(" ".into(), TokenKind::StringLiteral),
            Token::new("{".into(), TokenKind::LSquirly),
            Token::new("}".into(), TokenKind::RSquirly),
            Token::new(" ".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(",".into(), TokenKind::Comma),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new("a".into(), TokenKind::StringLiteral),
            Token::new("{".into(), TokenKind::LSquirly),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new("b".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new("}".into(), TokenKind::RSquirly),
            Token::new("c".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(".".into(), TokenKind::Period),
            Token::new("write".into(), TokenKind::Write),
            Token::new(":".into(), TokenKind::Colon),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new("".into(), TokenKind::StringLiteral),
            Token::new("{".into(), TokenKind::LSquirly),
            Token::new("}".into(), TokenKind::RSquirly),
            Token::new("".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(".".into(), TokenKind::Period),
            Token::new("}".into(), TokenKind::RSquirly),
            Token::new("abc".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(".".into(), TokenKind::Period),
            Token::new("write".into(), TokenKind::Write),
            Token::new(":".into(), TokenKind::Colon),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new("nested ".into(), TokenKind::StringLiteral),
            Token::new("{".into(), TokenKind::LSquirly),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(" string ".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new("}".into(), TokenKind::RSquirly),
            Token::new(" templates".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(".".into(), TokenKind::Period),
            Token::new("write".into(), TokenKind::Write),
            Token::new(":".into(), TokenKind::Colon),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new("nested ".into(), TokenKind::StringLiteral),
            Token::new("{".into(), TokenKind::LSquirly),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(" ".into(), TokenKind::StringLiteral),
            Token::new("{".into(), TokenKind::LSquirly),
            Token::new("string".into(), TokenKind::StringLiteral),
            Token::new("}".into(), TokenKind::RSquirly),
            Token::new(" ".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new("}".into(), TokenKind::RSquirly),
            Token::new(" templates".into(), TokenKind::StringLiteral),
            Token::new("|".into(), TokenKind::VSlash),
            Token::new(".".into(), TokenKind::Period),
            Token::new("data".into(), TokenKind::Data),
            Token::new("lv_string2".into(), TokenKind::Ident),
            Token::new("type".into(), TokenKind::Type),
            Token::new("string".into(), TokenKind::String),
            Token::new(".".into(), TokenKind::Period),
            Token::new("data".into(), TokenKind::Data),
            Token::new("(".into(), TokenKind::LParen),
            Token::new("lv_test".into(), TokenKind::Ident),
            Token::new(")".into(), TokenKind::RParen),
            Token::new("=".into(), TokenKind::Assign),
            Token::new("5".into(), TokenKind::StringLiteral),
            Token::new(".".into(), TokenKind::Period),
        ];

        let lexer = Lexer::new(input.into());
        let mut iter = lexer.iter();

        for token in tokens {
            let next_token = match iter.next() {
                Some(token) => token,
                None => break,
            };

            println!("expected={}, got={}", token, next_token);
            assert_eq!(token, next_token)
        }
    }
}
