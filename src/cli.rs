use clap::{Args, Parser, Subcommand};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
/// Running the tool without any options or commands will enter REPL
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Compile source file
    Compile(ArgsOutput),
    /// Run source file
    Run(ArgsTarget),
    /// Execute compiled file
    Execute(ArgsTarget),
}

#[derive(Args)]
pub struct ArgsOutput {
    /// Source file
    pub src: String,
    /// Optional destination file
    ///
    /// If no destination file is provided, compiled file is placed in the current directory
    /// with the following naming convention: <name>.rbap
    pub dest: Option<String>,
}

#[derive(Args)]
pub struct ArgsTarget {
    /// Target file
    pub target: String,
}
