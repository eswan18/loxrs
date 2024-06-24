use clap::Parser;

#[derive(Parser)]
struct Cli {
    argument: Option<String>,
}

fn main() {
    let args = Cli::parse();
    match args.argument {
        None => println!("No argument passed"),
        Some(arg) => println!("Argument passed: {}", arg),
    }
}
