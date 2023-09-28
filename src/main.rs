use std::path::PathBuf;

use rbap_lang::{
    cli::{Cli, Commands},
    start, Compiler, Parser, Result, VM,
};

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Some(commands) => match commands {
            Commands::Compile(c) => {
                let dest = c.dest.map_or_else(
                    || {
                        let mut dest = PathBuf::from(&c.src);
                        dest.set_extension("rbap");
                        dest
                    },
                    |dest| PathBuf::from(&dest),
                );

                let compiler = Compiler::compile(c.src)?;
                compiler.into_file(dest)?;
            }
            Commands::Run(f) => VM::run(f.target)?,
            Commands::Execute(f) => VM::execute(f.target)?,
        },
        None => start()?,
    }

    Ok(())
}
