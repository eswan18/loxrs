use clap::Parser;
use loxrs::{run_file, run_prompt};

#[derive(Parser)]
struct Cli {
    argument: Option<String>,
}

fn main() -> Result<(), std::io::Error> {
    let args = Cli::parse();
    match args.argument {
        None => run_prompt(),
        Some(arg) => run_file(&arg),
    }
}
