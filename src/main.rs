use std::path::PathBuf;

use rbap_lang::{cli::Cli, start, Compiler, Parser, Result};

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Some(commands) => match commands {
            rbap_lang::cli::Commands::Compile(c) => {
                let src = PathBuf::from(&c.src);
                let dest = c.dest.map_or_else(
                    || {
                        let mut dest = src.clone();
                        dest.set_extension("rbap");
                        dest
                    },
                    |dest| PathBuf::from(&dest),
                );

                let compiler = Compiler::compile(src)?;
                compiler.into_file(dest)?;
            }
            rbap_lang::cli::Commands::Run(_) => todo!(),
        },
        None => start()?,
    }

    Ok(())
}
