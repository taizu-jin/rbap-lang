use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Eof,
    _Illegal,

    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    VSlash,

    Ident(String),
    IntLiteral(String),
    StringLiteral(String),
    _True,
    _False,

    Comma,
    Colon,
    Period,

    LParen,
    RParen,
    LSquirly,
    RSquirly,

    Data,
    Type,
    Write,
    String,
    Int,
}

impl Token {
    pub fn from_keyword(keyword: &str) -> Option<Token> {
        match keyword {
            "type" => Some(Token::Type),
            "data" => Some(Token::Data),
            "write" => Some(Token::Write),
            "string" => Some(Token::String),
            "i" => Some(Token::Int),
            _ => None,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Eof => write!(f, "Eof"),
            Token::_Illegal => write!(f, "Illegal"),
            Token::Assign => write!(f, "Assign"),
            Token::Plus => write!(f, "Plus"),
            Token::Minus => write!(f, "Minus"),
            Token::Asterisk => write!(f, "Asterisk"),
            Token::Slash => write!(f, "Slash"),
            Token::VSlash => write!(f, "VSlash"),
            Token::Ident(l) => write!(f, "Ident({})", l),
            Token::IntLiteral(l) => write!(f, "Int({})", l),
            Token::StringLiteral(l) => write!(f, "String({})", l),
            Token::Comma => write!(f, "Comma"),
            Token::Colon => write!(f, "Colon"),
            Token::LParen => write!(f, "LParen"),
            Token::RParen => write!(f, "RParen"),
            Token::LSquirly => write!(f, "LSquirly"),
            Token::RSquirly => write!(f, "RSquirly"),
            Token::Data => write!(f, "Data"),
            Token::Type => write!(f, "Type"),
            Token::Write => write!(f, "Write"),
            Token::String => write!(f, "String"),
            Token::Int => write!(f, "Int"),
            Token::Period => write!(f, "Period"),
            Token::_True => write!(f, "True"),
            Token::_False => write!(f, "False"),
        }
    }
}
