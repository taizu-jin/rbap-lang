#![allow(dead_code)]

use std::fmt::Display;

use crate::code::Instructions;

#[derive(Debug, PartialEq)]
pub enum Object {
    String(String),
    Int(i64),
    Bool(bool),
    Function(CompiledFunction),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Int(i) => write!(f, "{}", i),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Function(func) => write!(f, "{}", func),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct CompiledFunction {
    pub instructions: Instructions,
    pub num_locals: usize,
    pub num_parameters: usize,
}

impl Display for CompiledFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CompiledFunction[{:?}](", std::ptr::addr_of!(self))?;
        for ins in self.instructions.as_ref() {
            write!(f, "{:#02x},", ins)?;
        }

        write!(f, ")")
    }
}

impl From<CompiledFunction> for Object {
    fn from(value: CompiledFunction) -> Self {
        Object::Function(value)
    }
}
