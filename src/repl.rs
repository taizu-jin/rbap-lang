use std::io::{self, Write};

use crate::{lexer::Lexer, parser::Parser};

pub fn start() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        write!(stdout, ">>")?;
        stdout.flush()?;
        let mut input = String::new();
        let _ = stdin.read_line(&mut input)?;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer.iter());
        let program = parser.parse();

        for error in parser.errors() {
            writeln!(stdout, "{}", error)?;
        }

        for statement in program.statements {
            writeln!(stdout, "{:#?}", statement)?;
        }
        stdout.flush()?;
    }
}
