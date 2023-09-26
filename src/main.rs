use std::io;

use rbap_lang::{cli::Cli, start, Parser};

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Some(commands) => match commands {
            rbap_lang::cli::Commands::Compile(_) => todo!(),
            rbap_lang::cli::Commands::Run(_) => todo!(),
        },
        None => start()?,
    }

    Ok(())
}
