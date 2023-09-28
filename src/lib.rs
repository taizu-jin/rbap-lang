mod ast;
pub mod cli;
pub mod code;
mod compiler;
mod error;
mod lexer;
mod object;
mod parser;
mod repl;
mod vm;

pub use {clap::Parser, compiler::Compiler, error::Result, repl::start, vm::VM};
