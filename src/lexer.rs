mod token;

pub use token::Token;

pub(super) enum Strategy {
    Ident,
    String,
    Int,
    StrTemplate,
    Token,
    Single(Token),
}

pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
    strategy: Strategy,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Self {
            input: input.to_lowercase().into_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
            strategy: Strategy::Token,
        };
        lexer.read_char();
        lexer
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

    fn _peek_char(lexer: &mut Lexer) -> u8 {
        if lexer.read_position >= lexer.input.len() {
            0
        } else {
            lexer.input[lexer.read_position]
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char()
        }
    }

    pub fn next_token(&mut self) -> Token {
        match self.strategy {
            Strategy::Ident => self.get_ident(),
            Strategy::Int => self.get_int(),
            Strategy::String => self.get_string(),
            Strategy::StrTemplate => self.get_str_template(),
            Strategy::Token => self.get_token(),
            Strategy::Single(ref token) => self.get_single_token(token.clone()),
        }
    }

    fn get_ident(&mut self) -> Token {
        let literal = self.read_ident();

        let token = match Token::from_keyword(&literal) {
            Some(token) => token,
            None => Token::Ident(literal),
        };

        self.switch_strategy(Strategy::Token);
        token
    }

    fn read_ident(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char()
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn get_int(&mut self) -> Token {
        let literal = self.read_int();

        let token = Token::IntLiteral(literal);
        self.switch_strategy(Strategy::Token);
        token
    }

    fn read_int(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_ascii_digit() || self.ch == b'_' {
            self.read_char()
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn get_string(&mut self) -> Token {
        let token = Token::StringLiteral(self.read_string());
        self.switch_strategy(Strategy::Token);
        token
    }

    fn read_string(&mut self) -> String {
        let pos = self.position + 1;
        loop {
            self.read_char();
            if self.ch == b'\'' || self.ch == 0 {
                break;
            }
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn get_str_template(&mut self) -> Token {
        let pos = self.position;

        loop {
            match self.ch {
                b'{' => {
                    self.switch_strategy(Strategy::Token);
                    break;
                }
                b'|' => {
                    self.switch_strategy(Strategy::Single(Token::VSlash));
                    break;
                }
                0 => {
                    break;
                }
                _ => (),
            }
            self.read_char();
        }

        let literal = String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
        Token::StringLiteral(literal)
    }

    fn get_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => Token::Assign,
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'/' => Token::Slash,
            b'|' => {
                self.switch_strategy(Strategy::StrTemplate);
                Token::VSlash
            }
            b'*' => Token::Asterisk,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LSquirly,
            b'}' => {
                self.switch_strategy(Strategy::StrTemplate);
                Token::RSquirly
            }
            b'.' => Token::Period,
            b':' => Token::Colon,
            b',' => Token::Comma,
            b'\'' => {
                self.switch_strategy(Strategy::String);
                self.next_token()
            }
            ch if ch.is_ascii_alphabetic() || ch == b'_' => {
                self.switch_strategy(Strategy::Ident);
                return self.next_token();
            }
            ch if ch.is_ascii_digit() => {
                self.switch_strategy(Strategy::Int);
                return self.next_token();
            }
            0 => Token::Eof,
            _ => unreachable!(
                "( ⚆ _ ⚆) Sorry, we're regressive here, so we don't recognize such tokens '{}'",
                String::from_utf8_lossy(&[self.ch])
            ),
        };

        self.read_char();
        token
    }

    fn get_single_token(&mut self, token: Token) -> Token {
        self.read_char();
        self.switch_strategy(Strategy::Token);
        token
    }

    fn switch_strategy(&mut self, scope: Strategy) {
        self.strategy = scope
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
WRITE: | |, | { } |, |a{ |b| }c|."#;

        let tokens = vec![
            Token::Data,
            Token::Colon,
            Token::Ident("lv_string".into()),
            Token::Type,
            Token::String,
            Token::Comma,
            Token::Ident("lv_int".into()),
            Token::Type,
            Token::Int,
            Token::Period,
            Token::Ident("lv_string".into()),
            Token::Assign,
            Token::StringLiteral("(ツ)".into()),
            Token::Period,
            Token::Write,
            Token::Ident("lv_string".into()),
            Token::Period,
            Token::Data,
            Token::LParen,
            Token::Ident("lv_template_string".into()),
            Token::RParen,
            Token::Assign,
            Token::VSlash,
            Token::StringLiteral("\\_".into()),
            Token::LSquirly,
            Token::Ident("lv_string".into()),
            Token::RSquirly,
            Token::StringLiteral("_/".into()),
            Token::VSlash,
            Token::Period,
            Token::Write,
            Token::Colon,
            Token::Slash,
            Token::StringLiteral(" ".into()),
            Token::Comma,
            Token::Ident("lv_template_string".into()),
            Token::Comma,
            Token::StringLiteral(" ".into()),
            Token::Period,
            Token::Write,
            Token::Colon,
            Token::Slash,
            Token::Ident("lv_int".into()),
            Token::Period,
            Token::Ident("lv_int".into()),
            Token::Assign,
            Token::IntLiteral("5".into()),
            Token::Plus,
            Token::IntLiteral("10".into()),
            Token::Period,
            Token::Write,
            Token::Colon,
            Token::Slash,
            Token::Ident("lv_int".into()),
            Token::Period,
            Token::Write,
            Token::Colon,
            Token::Slash,
            Token::StringLiteral("answer to life:".into()),
            Token::Comma,
            Token::Slash,
            Token::LParen,
            Token::Ident("lv_int".into()),
            Token::Minus,
            Token::IntLiteral("2".into()),
            Token::Asterisk,
            Token::IntLiteral("5".into()),
            Token::RParen,
            Token::Asterisk,
            Token::LParen,
            Token::IntLiteral("16".into()),
            Token::Minus,
            Token::IntLiteral("32".into()),
            Token::Slash,
            Token::IntLiteral("4".into()),
            Token::RParen,
            Token::Plus,
            Token::IntLiteral("2".into()),
            Token::Period,
            Token::Write,
            Token::Colon,
            Token::VSlash,
            Token::StringLiteral(" ".into()),
            Token::VSlash,
            Token::Comma,
            Token::VSlash,
            Token::StringLiteral(" ".into()),
            Token::LSquirly,
            Token::RSquirly,
            Token::StringLiteral(" ".into()),
            Token::VSlash,
            Token::Comma,
            Token::VSlash,
            Token::StringLiteral("a".into()),
            Token::LSquirly,
            Token::VSlash,
            Token::StringLiteral("b".into()),
            Token::VSlash,
            Token::RSquirly,
            Token::StringLiteral("c".into()),
            Token::VSlash,
            Token::Period,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input.into());

        for token in tokens {
            let next_token = lexer.next_token();
            println!("expected={}, got={}", token, next_token);
            assert_eq!(token, next_token)
        }
    }
}
