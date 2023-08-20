use std::{fmt::Display, fs::write};

pub enum Object {
    String(String),
    Int(i64),
    Bool(bool),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Int(i) => write!(f, "{}", i),
            Object::Bool(b) => write!(f, "{}", b),
        }
    }
}
