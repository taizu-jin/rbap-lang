use std::io::{self, Write};

use crate::{
    compiler::{Compiler, SymbolTable},
    lexer::Lexer,
    object::Object,
    parser::Parser,
    vm::{StdoutWriter, GLOBAL_SIZE, VM},
};

pub fn start() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

    let mut constants = Vec::new();
    let mut symbol_table = SymbolTable::new();
    let mut globals: Box<_> = vec![Object::Null; GLOBAL_SIZE].try_into().unwrap();

    loop {
        write!(stdout, ">>")?;
        stdout.flush()?;
        let mut input = String::new();
        let _ = stdin.read_line(&mut input)?;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer.iter());
        let program = parser.parse();

        for error in parser.errors() {
            write!(stderr, "{}", error)?;
        }

        let mut compiler = Compiler::with_state(constants, symbol_table);
        if let Err(e) = compiler.compile(program) {
            writeln!(stderr, "{}", e)?;
        }

        let mut vm = VM::with_state(compiler.bytecode(), StdoutWriter, globals);
        if let Err(e) = vm.run() {
            writeln!(stderr, "{}", e)?;
        }

        let last_popped = vm.last_popped_stack_elem();
        writeln!(stdout, "{}", last_popped)?;

        stderr.flush()?;
        stdout.flush()?;

        (constants, symbol_table) = compiler.consume();
        globals = vm.consume();
    }
}
