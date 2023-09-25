use std::io::{self, Write};

use crate::{
    compiler::Compiler,
    lexer::Lexer,
    parser::Parser,
    vm::{StdoutWriter, VM},
};

pub fn start() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

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

        let mut compiler = Compiler::new();
        if let Err(e) = compiler.compile(program) {
            writeln!(stderr, "{}", e)?;
        }

        let mut vm = VM::new(compiler.bytecode(), StdoutWriter);
        if let Err(e) = vm.run() {
            writeln!(stderr, "{}", e)?;
        }

        let last_popped = vm.last_popped_stack_elem();
        writeln!(stdout, "{}", last_popped)?;

        stderr.flush()?;
        stdout.flush()?;
    }
}
