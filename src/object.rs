use std::fmt::Display;

use serde::{Deserialize, Serialize};

use crate::{ast::DataType, code::Instructions};

#[derive(Debug, PartialEq, Clone, PartialOrd, Serialize, Deserialize)]
pub enum Object {
    String(String),
    Int(i64),
    Bool(bool),
    Function(CompiledFunction),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Int(i) => write!(f, "{}", i),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Function(func) => write!(f, "{}", func),
            Object::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize)]
pub struct CompiledFunction {
    pub instructions: Instructions,
    pub num_locals: usize,
    pub num_parameters: usize,
    pub ty: DataType,
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

impl From<Object> for DataType {
    fn from(value: Object) -> Self {
        match value {
            Object::String(_) => DataType::String,
            Object::Int(_) => DataType::Int,
            Object::Bool(_) => DataType::Bool,
            Object::Null => DataType::None,
            Object::Function(f) => f.ty,
        }
    }
}
