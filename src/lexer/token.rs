use std::{borrow::Cow, fmt::Display};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Eof,
    Illegal,

    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    VSlash,

    Ident,
    IntLiteral,
    StringLiteral,

    Comma,
    Colon,
    Period,

    LParen,
    RParen,
    LSquirly,
    RSquirly,

    Data,
    DataInline,
    Type,
    Write,
    String,
    Int,
    True,
    False,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    literal: Cow<'static, str>,
    kind: TokenKind,
}

impl Token {
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn literal(&self) -> &str {
        &self.literal
    }
}

impl Token {
    pub fn eof() -> Self {
        Token {
            kind: TokenKind::Eof,
            literal: "EOF".into(),
        }
    }
    pub fn illegal(literal: Cow<'static, str>) -> Self {
        Token {
            kind: TokenKind::Illegal,
            literal,
        }
    }
    pub fn assign() -> Self {
        Token {
            kind: TokenKind::Assign,
            literal: "=".into(),
        }
    }
    pub fn plus() -> Self {
        Token {
            kind: TokenKind::Plus,
            literal: "+".into(),
        }
    }
    pub fn minus() -> Self {
        Token {
            kind: TokenKind::Minus,
            literal: "-".into(),
        }
    }
    pub fn asterisk() -> Self {
        Token {
            kind: TokenKind::Asterisk,
            literal: "*".into(),
        }
    }
    pub fn slash() -> Self {
        Token {
            kind: TokenKind::Slash,
            literal: "/".into(),
        }
    }
    pub fn vertical_slash() -> Self {
        Token {
            kind: TokenKind::VSlash,
            literal: "|".into(),
        }
    }
    pub fn ident(literal: Cow<'static, str>) -> Self {
        Token {
            kind: TokenKind::Ident,
            literal,
        }
    }
    pub fn int_literal(literal: Cow<'static, str>) -> Self {
        Token {
            kind: TokenKind::IntLiteral,
            literal,
        }
    }
    pub fn string_literal(literal: Cow<'static, str>) -> Self {
        Token {
            kind: TokenKind::StringLiteral,
            literal,
        }
    }
    pub fn rbap_true() -> Self {
        Token {
            kind: TokenKind::True,
            literal: "rbap_true".into(),
        }
    }
    pub fn rbap_false() -> Self {
        Token {
            kind: TokenKind::False,
            literal: "rbap_false".into(),
        }
    }
    pub fn comma() -> Self {
        Token {
            kind: TokenKind::Comma,
            literal: ",".into(),
        }
    }
    pub fn colon() -> Self {
        Token {
            kind: TokenKind::Colon,
            literal: ":".into(),
        }
    }
    pub fn period() -> Self {
        Token {
            kind: TokenKind::Period,
            literal: ".".into(),
        }
    }
    pub fn left_paren() -> Self {
        Token {
            kind: TokenKind::LParen,
            literal: "(".into(),
        }
    }
    pub fn right_paren() -> Self {
        Token {
            kind: TokenKind::RParen,
            literal: ")".into(),
        }
    }
    pub fn left_squirly() -> Self {
        Token {
            kind: TokenKind::LSquirly,
            literal: "{".into(),
        }
    }
    pub fn right_squirly() -> Self {
        Token {
            kind: TokenKind::RSquirly,
            literal: "}".into(),
        }
    }
    pub fn data() -> Self {
        Token {
            kind: TokenKind::Data,
            literal: "data".into(),
        }
    }
    pub fn data_inline(literal: Cow<'static, str>) -> Self {
        Token {
            kind: TokenKind::DataInline,
            literal,
        }
    }
    pub fn ty() -> Self {
        Token {
            kind: TokenKind::Type,
            literal: "TYPE".into(),
        }
    }
    pub fn write() -> Self {
        Token {
            kind: TokenKind::Write,
            literal: "WRITE".into(),
        }
    }
    pub fn string() -> Self {
        Token {
            kind: TokenKind::String,
            literal: "string".into(),
        }
    }
    pub fn int() -> Self {
        Token {
            kind: TokenKind::Int,
            literal: "i".into(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenKind::Eof => write!(f, "EOF"),
            TokenKind::Illegal => write!(f, "Illegal({})", self.literal),
            TokenKind::Assign => write!(f, "Assign({})", self.literal),
            TokenKind::Plus => write!(f, "Plus({})", self.literal),
            TokenKind::Minus => write!(f, "Minus({})", self.literal),
            TokenKind::Asterisk => write!(f, "Asterisk({})", self.literal),
            TokenKind::Slash => write!(f, "Slash({})", self.literal),
            TokenKind::VSlash => write!(f, "VSlash({})", self.literal),
            TokenKind::Ident => write!(f, "Ident({})", self.literal),
            TokenKind::IntLiteral => write!(f, "IntLiteral({})", self.literal),
            TokenKind::StringLiteral => write!(f, "StringLiteral({})", self.literal),
            TokenKind::True => write!(f, "True({})", self.literal),
            TokenKind::False => write!(f, "False({})", self.literal),
            TokenKind::Comma => write!(f, "Comma({})", self.literal),
            TokenKind::Colon => write!(f, "Colon({})", self.literal),
            TokenKind::Period => write!(f, "Period({})", self.literal),
            TokenKind::LParen => write!(f, "LParen({})", self.literal),
            TokenKind::RParen => write!(f, "RParen({})", self.literal),
            TokenKind::LSquirly => write!(f, "LSquirly({})", self.literal),
            TokenKind::RSquirly => write!(f, "RSquirly({})", self.literal),
            TokenKind::Data => write!(f, "Data({})", self.literal),
            TokenKind::DataInline => write!(f, "DataInline({})", self.literal),
            TokenKind::Type => write!(f, "Type({})", self.literal),
            TokenKind::Write => write!(f, "Write({})", self.literal),
            TokenKind::String => write!(f, "String({})", self.literal),
            TokenKind::Int => write!(f, "Int({})", self.literal),
        }
    }
}

impl From<u8> for Token {
    fn from(value: u8) -> Self {
        match value {
            b'=' => Token::assign(),
            b'+' => Token::plus(),
            b'-' => Token::minus(),
            b'/' => Token::slash(),
            b'|' => Token::vertical_slash(),
            b'*' => Token::asterisk(),
            b'(' => Token::left_paren(),
            b')' => Token::right_paren(),
            b'{' => Token::left_squirly(),
            b'}' => Token::right_squirly(),
            b'.' => Token::period(),
            b':' => Token::colon(),
            b',' => Token::comma(),
            0 => Token::eof(),
            ch => Token::illegal(String::from_utf8_lossy(&[ch]).to_string().into()),
        }
    }
}

impl From<&str> for Token {
    fn from(value: &str) -> Self {
        match value {
            "type" => Token::ty(),
            "data" => Token::data(),
            "write" => Token::write(),
            "string" => Token::string(),
            "i" => Token::int(),
            "rbap_true" => Token::rbap_true(),
            "rbap_false" => Token::rbap_false(),
            value => Token::ident(value.to_string().into()),
        }
    }
}

impl From<String> for Token {
    fn from(value: String) -> Self {
        match value.as_str() {
            "type" => Token::ty(),
            "data" => Token::data(),
            "write" => Token::write(),
            "string" => Token::string(),
            "i" => Token::int(),
            "rbap_true" => Token::rbap_true(),
            "rbap_false" => Token::rbap_false(),
            _ => Token::ident(value.into()),
        }
    }
}
