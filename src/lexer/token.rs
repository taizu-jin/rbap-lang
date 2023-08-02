use std::{borrow::Cow, fmt::Display};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
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

impl TokenKind {
    pub fn from(literal: &str) -> Option<TokenKind> {
        let kind = match literal {
            "type" => TokenKind::Type,
            "data" => TokenKind::Data,
            "write" => TokenKind::Write,
            "string" => TokenKind::String,
            "i" => TokenKind::Int,
            "rbap_true" => TokenKind::True,
            "rbap_false" => TokenKind::False,
            _ => return None,
        };

        Some(kind)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    literal: Cow<'a, str>,
    kind: TokenKind,
}

impl<'a> Token<'a> {
    pub fn new(literal: Cow<'a, str>, kind: TokenKind) -> Self {
        Self { literal, kind }
    }
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn literal(&self) -> &str {
        &self.literal
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
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

impl<'a> From<Cow<'a, str>> for Token<'a> {
    fn from(literal: Cow<'a, str>) -> Self {
        match literal.as_ref() {
            "=" => Token {
                literal,
                kind: TokenKind::Assign,
            },
            "+" => Token {
                literal,
                kind: TokenKind::Plus,
            },
            "-" => Token {
                literal,
                kind: TokenKind::Minus,
            },
            "/" => Token {
                literal,
                kind: TokenKind::Slash,
            },
            "|" => Token {
                literal,
                kind: TokenKind::VSlash,
            },
            "*" => Token {
                literal,
                kind: TokenKind::Asterisk,
            },
            "(" => Token {
                literal,
                kind: TokenKind::LParen,
            },
            ")" => Token {
                literal,
                kind: TokenKind::RParen,
            },
            "{" => Token {
                literal,
                kind: TokenKind::LSquirly,
            },
            "}" => Token {
                literal,
                kind: TokenKind::RSquirly,
            },
            "." => Token {
                literal,
                kind: TokenKind::Period,
            },
            ":" => Token {
                literal,
                kind: TokenKind::Colon,
            },
            "," => Token {
                literal,
                kind: TokenKind::Comma,
            },
            "type" => Token {
                literal,
                kind: TokenKind::Type,
            },
            "data" => Token {
                literal,
                kind: TokenKind::Data,
            },
            "write" => Token {
                literal,
                kind: TokenKind::Write,
            },
            "string" => Token {
                literal,
                kind: TokenKind::String,
            },
            "i" => Token {
                literal,
                kind: TokenKind::Int,
            },
            "rbap_true" => Token {
                literal,
                kind: TokenKind::True,
            },
            "rbap_false" => Token {
                literal,
                kind: TokenKind::False,
            },
            value if value.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') => Token {
                literal,
                kind: TokenKind::Ident,
            },
            value if value.chars().all(|c| c.is_ascii_digit() || c == '_') => Token {
                literal,
                kind: TokenKind::IntLiteral,
            },
            _ => Token {
                literal,
                kind: TokenKind::Illegal,
            },
        }
    }
}
