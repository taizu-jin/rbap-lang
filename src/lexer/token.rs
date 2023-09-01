use std::{borrow::Cow, fmt::Display};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Illegal,

    Plus,
    Minus,
    Asterisk,
    Slash,

    Ident,
    IntLiteral,
    StringLiteral,

    VSlash,
    Comma,
    Colon,
    Period,
    Assign,
    NotEquals,
    GreaterThan,
    LesserThan,

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
    If,
    EndIf,
    And,
    Or,
    Not,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Illegal => write!(f, "Illegal"),
            TokenKind::Assign => write!(f, "Assign"),
            TokenKind::Plus => write!(f, "Plus"),
            TokenKind::Minus => write!(f, "Minus"),
            TokenKind::Asterisk => write!(f, "Asterisk"),
            TokenKind::Slash => write!(f, "Slash"),
            TokenKind::VSlash => write!(f, "VSlash"),
            TokenKind::Ident => write!(f, "Ident"),
            TokenKind::IntLiteral => write!(f, "IntLiteral"),
            TokenKind::StringLiteral => write!(f, "StringLiteral"),
            TokenKind::Comma => write!(f, "Comma"),
            TokenKind::Colon => write!(f, "Colon"),
            TokenKind::Period => write!(f, "Period"),
            TokenKind::LParen => write!(f, "LParen"),
            TokenKind::RParen => write!(f, "RParen"),
            TokenKind::LSquirly => write!(f, "LSquirly"),
            TokenKind::RSquirly => write!(f, "RSquirly"),
            TokenKind::Data => write!(f, "Data"),
            TokenKind::DataInline => write!(f, "DataInline"),
            TokenKind::Type => write!(f, "Type"),
            TokenKind::Write => write!(f, "Write"),
            TokenKind::String => write!(f, "String"),
            TokenKind::Int => write!(f, "Int"),
            TokenKind::True => write!(f, "True"),
            TokenKind::False => write!(f, "False"),
            TokenKind::If => write!(f, "If"),
            TokenKind::EndIf => write!(f, "Endif"),
            TokenKind::NotEquals => write!(f, "NotEquals"),
            TokenKind::GreaterThan => write!(f, "GreaterThan"),
            TokenKind::LesserThan => write!(f, "LesserThan"),
            TokenKind::And => write!(f, "And"),
            TokenKind::Or => write!(f, "Or"),
            TokenKind::Not => write!(f, "Not"),
        }
    }
}

#[derive(Debug)]
pub struct TokenKinds(Vec<TokenKind>);

impl<'a> From<&'a [TokenKind]> for TokenKinds {
    fn from(value: &'a [TokenKind]) -> Self {
        Self(value.to_owned())
    }
}

impl Display for TokenKinds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter().peekable();

        while let Some(kind) = iter.next() {
            if iter.peek().is_some() {
                write!(f, "{}, ", kind)?
            } else {
                write!(f, "{}", kind)?
            };
        }

        Ok(())
    }
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
            "endif" => TokenKind::EndIf,
            "if" => TokenKind::If,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            _ => return None,
        };

        Some(kind)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    pub literal: Cow<'a, str>,
    pub kind: TokenKind,
}

impl<'a> Token<'a> {
    pub fn new(literal: Cow<'a, str>, kind: TokenKind) -> Self {
        Self { literal, kind }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.kind, self.literal)
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
            "if" => Token {
                literal,
                kind: TokenKind::If,
            },
            "endif" => Token {
                literal,
                kind: TokenKind::EndIf,
            },
            "<>" => Token {
                literal,
                kind: TokenKind::NotEquals,
            },
            ">" => Token {
                literal,
                kind: TokenKind::GreaterThan,
            },
            "<" => Token {
                literal,
                kind: TokenKind::LesserThan,
            },
            "and" => Token {
                literal,
                kind: TokenKind::And,
            },
            "or" => Token {
                literal,
                kind: TokenKind::Or,
            },
            "not" => Token {
                literal,
                kind: TokenKind::Not,
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
