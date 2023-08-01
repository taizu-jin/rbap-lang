use std::fmt::Display;

#[derive(Debug, PartialEq)]
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
    DataInline,
    Type,
    Write,
    String,
    Int,
}

#[derive(Debug, PartialEq)]
pub struct TokenNew<'a> {
    literal: &'a str,
    kind: TokenKind,
}

impl<'a> TokenNew<'a> {
    pub fn new(kind: TokenKind, literal: &'a str) -> TokenNew {
        Self { literal, kind }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn is_keyword(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Type
                | TokenKind::Data
                | TokenKind::Write
                | TokenKind::String
                | TokenKind::Int
        )
    }
}

impl<'a> Display for TokenNew<'a> {
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
            TokenKind::_True => write!(f, "True({})", self.literal),
            TokenKind::_False => write!(f, "False({})", self.literal),
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

#[derive(Debug, Clone)]
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
    DataInline(String),
    Type,
    Write,
    String,
    Int,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Token::Eof, Token::Eof)
                | (Token::_Illegal, Token::_Illegal)
                | (Token::Assign, Token::Assign)
                | (Token::Plus, Token::Plus)
                | (Token::Minus, Token::Minus)
                | (Token::Asterisk, Token::Asterisk)
                | (Token::Slash, Token::Slash)
                | (Token::VSlash, Token::VSlash)
                | (Token::Ident(_), Token::Ident(_))
                | (Token::IntLiteral(_), Token::IntLiteral(_))
                | (Token::StringLiteral(_), Token::StringLiteral(_))
                | (Token::_True, Token::_True)
                | (Token::_False, Token::_False)
                | (Token::Comma, Token::Comma)
                | (Token::Colon, Token::Colon)
                | (Token::Period, Token::Period)
                | (Token::LParen, Token::LParen)
                | (Token::RParen, Token::RParen)
                | (Token::LSquirly, Token::LSquirly)
                | (Token::RSquirly, Token::RSquirly)
                | (Token::Data, Token::Data)
                | (Token::Type, Token::Type)
                | (Token::Write, Token::Write)
                | (Token::String, Token::String)
                | (Token::Int, Token::Int)
                | (Token::DataInline(_), Token::DataInline(_))
        )
    }
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
            Token::DataInline(l) => write!(f, "Data({})", l),
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
