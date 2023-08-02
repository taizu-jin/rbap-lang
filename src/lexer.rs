mod token;

pub use token::{Token, TokenKind};

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
    pub fn new(input: String) -> Self {
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
        let token = Token::from(literal);

        let token = match token.kind() {
            TokenKind::Data if self.ch == b'(' => self.get_data_inline(),
            _ => token,
        };

        self.switch_strategy(Strategy::Token);
        token
    }

    fn read_ident(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char()
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn get_data_inline(&mut self) -> Token {
        let position = self.position;
        let read_position = self.read_position;
        let ch = self.ch;

        self.read_char();

        let literal = self.read_ident();

        if self.ch != b')' {
            self.position = position;
            self.read_position = read_position;
            self.ch = ch;

            return Token::data();
        }

        self.read_char();

        Token::data_inline(literal.into())
    }

    fn get_int(&mut self) -> Token {
        let literal = self.read_int();

        let token = Token::int_literal(literal.into());
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
        let token = Token::string_literal(self.read_string().into());
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
                    self.switch_strategy(Strategy::Single(Token::vertical_slash()));
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
        Token::string_literal(literal.into())
    }

    fn get_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = Token::from(self.ch);

        let token = match token.kind() {
            TokenKind::VSlash | TokenKind::RSquirly => {
                self.switch_strategy(Strategy::StrTemplate);
                token
            }
            TokenKind::Illegal => match self.ch {
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
                ch => Token::illegal(ch.to_string().into()),
            },
            _ => token,
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
WRITE: | |, | { } |, |a{ |b| }c|.
WRITE: |{}|.
}abc|.
WRITE: |nested { | string | } templates|.
WRITE: |nested { | { 'string' } | } templates|.

DATA lv_string2 TYPE string.
DATA( lv_test) = '5'."#;

        let tokens = vec![
            Token::data(),
            Token::colon(),
            Token::ident("lv_string".into()),
            Token::ty(),
            Token::string(),
            Token::comma(),
            Token::ident("lv_int".into()),
            Token::ty(),
            Token::int(),
            Token::period(),
            Token::ident("lv_string".into()),
            Token::assign(),
            Token::string_literal("(ツ)".into()),
            Token::period(),
            Token::write(),
            Token::ident("lv_string".into()),
            Token::period(),
            Token::data_inline("lv_template_string".into()),
            Token::assign(),
            Token::vertical_slash(),
            Token::string_literal("\\_".into()),
            Token::left_squirly(),
            Token::ident("lv_string".into()),
            Token::right_squirly(),
            Token::string_literal("_/".into()),
            Token::vertical_slash(),
            Token::period(),
            Token::write(),
            Token::colon(),
            Token::slash(),
            Token::string_literal(" ".into()),
            Token::comma(),
            Token::ident("lv_template_string".into()),
            Token::comma(),
            Token::string_literal(" ".into()),
            Token::period(),
            Token::write(),
            Token::colon(),
            Token::slash(),
            Token::ident("lv_int".into()),
            Token::period(),
            Token::ident("lv_int".into()),
            Token::assign(),
            Token::int_literal("5".into()),
            Token::plus(),
            Token::int_literal("10".into()),
            Token::period(),
            Token::write(),
            Token::colon(),
            Token::slash(),
            Token::ident("lv_int".into()),
            Token::period(),
            Token::write(),
            Token::colon(),
            Token::slash(),
            Token::string_literal("answer to life:".into()),
            Token::comma(),
            Token::slash(),
            Token::left_paren(),
            Token::ident("lv_int".into()),
            Token::minus(),
            Token::int_literal("2".into()),
            Token::asterisk(),
            Token::int_literal("5".into()),
            Token::right_paren(),
            Token::asterisk(),
            Token::left_paren(),
            Token::int_literal("16".into()),
            Token::minus(),
            Token::int_literal("32".into()),
            Token::slash(),
            Token::int_literal("4".into()),
            Token::right_paren(),
            Token::plus(),
            Token::int_literal("2".into()),
            Token::period(),
            Token::write(),
            Token::colon(),
            Token::vertical_slash(),
            Token::string_literal(" ".into()),
            Token::vertical_slash(),
            Token::comma(),
            Token::vertical_slash(),
            Token::string_literal(" ".into()),
            Token::left_squirly(),
            Token::right_squirly(),
            Token::string_literal(" ".into()),
            Token::vertical_slash(),
            Token::comma(),
            Token::vertical_slash(),
            Token::string_literal("a".into()),
            Token::left_squirly(),
            Token::vertical_slash(),
            Token::string_literal("b".into()),
            Token::vertical_slash(),
            Token::right_squirly(),
            Token::string_literal("c".into()),
            Token::vertical_slash(),
            Token::period(),
            Token::write(),
            Token::colon(),
            Token::vertical_slash(),
            Token::string_literal("".into()),
            Token::left_squirly(),
            Token::right_squirly(),
            Token::string_literal("".into()),
            Token::vertical_slash(),
            Token::period(),
            Token::right_squirly(),
            Token::string_literal("abc".into()),
            Token::vertical_slash(),
            Token::period(),
            Token::write(),
            Token::colon(),
            Token::vertical_slash(),
            Token::string_literal("nested ".into()),
            Token::left_squirly(),
            Token::vertical_slash(),
            Token::string_literal(" string ".into()),
            Token::vertical_slash(),
            Token::right_squirly(),
            Token::string_literal(" templates".into()),
            Token::vertical_slash(),
            Token::period(),
            Token::write(),
            Token::colon(),
            Token::vertical_slash(),
            Token::string_literal("nested ".into()),
            Token::left_squirly(),
            Token::vertical_slash(),
            Token::string_literal(" ".into()),
            Token::left_squirly(),
            Token::string_literal("string".into()),
            Token::right_squirly(),
            Token::string_literal(" ".into()),
            Token::vertical_slash(),
            Token::right_squirly(),
            Token::string_literal(" templates".into()),
            Token::vertical_slash(),
            Token::period(),
            Token::data(),
            Token::ident("lv_string2".into()),
            Token::ty(),
            Token::string(),
            Token::period(),
            Token::data(),
            Token::left_paren(),
            Token::ident("lv_test".into()),
            Token::right_paren(),
            Token::assign(),
            Token::string_literal("5".into()),
            Token::period(),
            Token::eof(),
        ];

        let mut lexer = Lexer::new(input.into());

        for token in tokens {
            let next_token = lexer.next_token();
            println!("expected={}, got={}", token, next_token);
            assert_eq!(token, next_token)
        }
    }
}
