use std::io::{self, Write};

use crate::lexer::{Lexer, Token};

pub fn start() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        write!(stdout, ">>")?;
        stdout.flush()?;
        let mut input = String::new();
        let _ = stdin.read_line(&mut input)?;

        let mut lexer = Lexer::new(input);

        loop {
            let token = lexer.next_token();

            writeln!(stdout, "{}", token)?;
            stdout.flush()?;

            if token == Token::Eof {
                break;
            }
        }
    }
}
