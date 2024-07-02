use clap::Parser;
use env_logger;
use log::info;
use loxrs::{run_file, run_repl};

#[derive(Parser)]
struct Cli {
    argument: Option<String>,
}

fn main() {
    let args = Cli::parse();
    env_logger::init();

    info!("Starting LoxRS");
    let result = match args.argument {
        None => run_repl(),
        Some(arg) => run_file(&arg),
    };
    if let Err(code) = result {
        std::process::exit(code);
    }
}
