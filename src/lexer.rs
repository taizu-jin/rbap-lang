mod token;

use std::borrow::Cow;

pub use token::{Token, TokenKind, TokenKinds};

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
    LesserThan,
    Equals,
    Single(Token<'a>),
}

pub struct LexerIter<'t, 's: 't> {
    input: &'s [u8],
    position: usize,
    read_position: usize,
    ch: u8,
    strategy: Strategy<'t>,
}

impl<'t, 's: 't> LexerIter<'t, 's> {
    fn next_token(&mut self) -> Option<Token<'t>> {
        match self.strategy {
            Strategy::Ident => Some(self.get_ident()),
            Strategy::String => Some(self.get_string()),
            Strategy::Int => Some(self.get_int()),
            Strategy::StrTemplate => Some(self.get_string_template()),
            Strategy::Token => self.get_token(),
            Strategy::LesserThan => Some(self.get_lesser_than()),
            Strategy::Equals => Some(self.get_equals()),
            Strategy::Single(ref token) => Some(self.get_single_token(token.clone())),
        }
    }

    fn get_single_token(&mut self, token: Token<'t>) -> Token<'t> {
        self.read_char();
        self.switch_strategy(Strategy::Token);
        token
    }

    fn get_string_template(&mut self) -> Token<'t> {
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

    fn get_equals(&mut self) -> Token<'t> {
        self.read_char();

        self.switch_strategy(Strategy::Token);

        if self.ch != b'=' {
            Token::new("=".into(), TokenKind::Assign)
        } else {
            self.read_char();
            Token::new("==".into(), TokenKind::Equals)
        }
    }

    fn get_lesser_than(&mut self) -> Token<'t> {
        self.read_char();

        self.switch_strategy(Strategy::Token);

        if self.ch != b'>' {
            Token::new("<".into(), TokenKind::LesserThan)
        } else {
            self.read_char();
            Token::new("<>".into(), TokenKind::NotEquals)
        }
    }

    fn get_token(&mut self) -> Option<Token<'t>> {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => {
                self.switch_strategy(Strategy::Equals);
                return self.next_token();
            }
            b'+' => self.tokenize_char(TokenKind::Plus),
            b'-' => self.tokenize_char(TokenKind::Minus),
            b'/' => self.tokenize_char(TokenKind::Slash),
            b'*' => self.tokenize_char(TokenKind::Asterisk),
            b'(' => self.tokenize_char(TokenKind::LParen),
            b')' => self.tokenize_char(TokenKind::RParen),
            b'.' => self.tokenize_char(TokenKind::Period),
            b':' => self.tokenize_char(TokenKind::Colon),
            b',' => self.tokenize_char(TokenKind::Comma),
            b'>' => self.tokenize_char(TokenKind::GreaterThan),
            b'<' => {
                self.switch_strategy(Strategy::LesserThan);
                return self.next_token();
            }
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

    fn switch_strategy(&mut self, strategy: Strategy<'t>) {
        self.strategy = strategy
    }

    fn tokenize_char(&mut self, kind: TokenKind) -> Token<'t> {
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

    fn get_ident(&mut self) -> Token<'t> {
        let literal = self.read_ident();

        let kind = match TokenKind::from(&literal) {
            Some(kind) => kind,
            None => TokenKind::Ident,
        };

        let token = Token::new(literal, kind);

        self.switch_strategy(Strategy::Token);
        token
    }

    fn read_ident(&mut self) -> Cow<'t, str> {
        let pos = self.position;

        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char()
        }

        String::from_utf8_lossy(&self.input[pos..self.position])
    }

    fn get_int(&mut self) -> Token<'t> {
        self.switch_strategy(Strategy::Token);
        let literal = self.read_int();

        Token::new(literal, TokenKind::IntLiteral)
    }

    fn read_int(&mut self) -> Cow<'t, str> {
        let pos = self.position;

        while self.ch.is_ascii_digit() || self.ch == b'_' {
            self.read_char()
        }

        String::from_utf8_lossy(&self.input[pos..self.position])
    }

    fn get_string(&mut self) -> Token<'t> {
        self.switch_strategy(Strategy::Token);
        let literal = self.read_string();

        Token::new(literal, TokenKind::StringLiteral)
    }

    fn read_string(&mut self) -> Cow<'t, str> {
        let pos = self.position + 1;

        loop {
            self.read_char();
            if self.ch == b'\'' || self.ch == 0 {
                break;
            }
        }

        String::from_utf8_lossy(&self.input[pos..self.position])
    }
}

impl<'t, 's: 't> Iterator for LexerIter<'t, 's> {
    type Item = Token<'t>;

    fn next(&mut self) -> Option<Token<'t>> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! define_cases {
            ($($literal:literal:$token:ident),+) => {
                vec![$(Token::new(
                    $literal.into(),
                    TokenKind::$token,
                )),+]
            };
        }

    #[test]
    fn test_next_token() {
        let input = r"DATA: lv_string TYPE string,
      lv_int TYPE i,
      lv_bool TYPE rbap_bool.

lv_string = '(ツ)'.
WRITE lv_string.

lv_template_string = |\_{ lv_string }_/|.
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
DATA( lv_test) = '5'.

IF lv_int == 15 AND 5 < 10 OR 15 > 5 OR 5 <> 10 OR NOT 5 <> 5 OR NOT rbap_false AND rbap_true.
    WRITE: 'TRUE'.
ENDIF.

METHOD sum IMPORTING iv_x TYPE i
                     iv_y TYPE i 
           RETURNING rv_sum TYPE i.
  rv_sum = iv_x + iv_y.
ENDMETHOD.";
        let tokens = define_cases!(
            "data":Data,
            ":":Colon,
            "lv_string":Ident,
            "type":Type,
            "string":String,
            ",":Comma,
            "lv_int":Ident,
            "type":Type,
            "i":Int,
            ",":Comma,
            "lv_bool":Ident,
            "type":Type,
            "rbap_bool":Bool,
            ".":Period,
            "lv_string":Ident,
            "=":Assign,
            "(ツ)":StringLiteral,
            ".":Period,
            "write":Write,
            "lv_string":Ident,
            ".":Period,
            "lv_template_string":Ident,
            "=":Assign,
            "|":VSlash,
            "\\_":StringLiteral,
            "{":LSquirly,
            "lv_string":Ident,
            "}":RSquirly,
            "_/":StringLiteral,
            "|":VSlash,
            ".":Period,
            "write":Write,
            ":":Colon,
            "/":Slash,
            " ":StringLiteral,
            ",":Comma,
            "lv_template_string":Ident,
            ",":Comma,
            " ":StringLiteral,
            ".":Period,
            "write":Write,
            ":":Colon,
            "/":Slash,
            "lv_int":Ident,
            ".":Period,
            "lv_int":Ident,
            "=":Assign,
            "5":IntLiteral,
            "+":Plus,
            "10":IntLiteral,
            ".":Period,
            "write":Write,
            ":":Colon,
            "/":Slash,
            "lv_int":Ident,
            ".":Period,
            "write":Write,
            ":":Colon,
            "/":Slash,
            "answer to life:":StringLiteral,
            ",":Comma,
            "/":Slash,
            "(":LParen,
            "lv_int":Ident,
            "-":Minus,
            "2":IntLiteral,
            "*":Asterisk,
            "5":IntLiteral,
            ")":RParen,
            "*":Asterisk,
            "(":LParen,
            "16":IntLiteral,
            "-":Minus,
            "32":IntLiteral,
            "/":Slash,
            "4":IntLiteral,
            ")":RParen,
            "+":Plus,
            "2":IntLiteral,
            ".":Period,
            "write":Write,
            ":":Colon,
            "|":VSlash,
            " ":StringLiteral,
            "|":VSlash,
            ",":Comma,
            "|":VSlash,
            " ":StringLiteral,
            "{":LSquirly,
            "}":RSquirly,
            " ":StringLiteral,
            "|":VSlash,
            ",":Comma,
            "|":VSlash,
            "a":StringLiteral,
            "{":LSquirly,
            "|":VSlash,
            "b":StringLiteral,
            "|":VSlash,
            "}":RSquirly,
            "c":StringLiteral,
            "|":VSlash,
            ".":Period,
            "write":Write,
            ":":Colon,
            "|":VSlash,
            "":StringLiteral,
            "{":LSquirly,
            "}":RSquirly,
            "":StringLiteral,
            "|":VSlash,
            ".":Period,
            "}":RSquirly,
            "abc":StringLiteral,
            "|":VSlash,
            ".":Period,
            "write":Write,
            ":":Colon,
            "|":VSlash,
            "nested ":StringLiteral,
            "{":LSquirly,
            "|":VSlash,
            " string ":StringLiteral,
            "|":VSlash,
            "}":RSquirly,
            " templates":StringLiteral,
            "|":VSlash,
            ".":Period,
            "write":Write,
            ":":Colon,
            "|":VSlash,
            "nested ":StringLiteral,
            "{":LSquirly,
            "|":VSlash,
            " ":StringLiteral,
            "{":LSquirly,
            "string":StringLiteral,
            "}":RSquirly,
            " ":StringLiteral,
            "|":VSlash,
            "}":RSquirly,
            " templates":StringLiteral,
            "|":VSlash,
            ".":Period,
            "data":Data,
            "lv_string2":Ident,
            "type":Type,
            "string":String,
            ".":Period,
            "data":Data,
            "(":LParen,
            "lv_test":Ident,
            ")":RParen,
            "=":Assign,
            "5":StringLiteral,
            ".":Period,
            "if":If,
            "lv_int":Ident,
            "==":Equals,
            "15":IntLiteral,
            "and":And,
            "5":IntLiteral,
            "<":LesserThan,
            "10":IntLiteral,
            "or":Or,
            "15":IntLiteral,
            ">":GreaterThan,
            "5":IntLiteral,
            "or": Or,
            "5":IntLiteral,
            "<>":NotEquals,
            "10":IntLiteral,
            "or": Or,
            "not": Not,
            "5":IntLiteral,
            "<>":NotEquals,
            "5":IntLiteral,
            "or": Or,
            "not": Not,
            "rbap_false": False,
            "and": And,
            "rbap_true": True,
            ".":Period,
            "write":Write,
            ":":Colon,
            "true":StringLiteral,
            ".":Period,
            "endif":EndIf,
            ".":Period,
            "method":Method,
            "sum":Ident,
            "importing":Importing,
            "iv_x":Ident,
            "type":Type,
            "i":Int,
            "iv_y":Ident,
            "type":Type,
            "i":Int,
            "returning":Returning,
            "rv_sum":Ident,
            "type":Type,
            "i":Int,
            ".":Period,
            "rv_sum":Ident,
            "=":Assign,
            "iv_x":Ident,
            "+":Plus,
            "iv_y":Ident,
            ".":Period,
            "endmethod":EndMethod,
            ".":Period
        );

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
