use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

mod ast;
mod error;
mod interpret;
mod parse;
mod run;
mod scan;
mod token;
mod value;

pub fn run_file(filename: &str) -> Result<(), i32> {
    // Open the file and get its contents.
    let contents = match fs::read_to_string(filename) {
        Ok(contents) => contents,
        Err(_) => {
            eprintln!("Error reading file '{}'", filename);
            return Err(1);
        }
    };
    match run::run_code(&contents) {
        Ok(_) => Ok(()),
        Err(e) => {
            eprintln!("{}", e);
            Err(e.exit_code)
        }
    }
}

pub fn run_repl() -> Result<(), i32> {
    let mut buffer = String::new();

    loop {
        // Repeatedly read input from the user and execute it.
        let bytes_read = prompt_for_code(&mut buffer).map_err(|e| {
            eprintln!("Error reading input: {}", e);
            1
        })?;
        if bytes_read == 0 {
            // Allows us to exit on ctrl-D.
            break;
        }
        if let Err(e) = run::run_code(&buffer) {
            // If an error is encountered, print it to stderr.
            eprintln!("{}", e);
        }
        buffer.clear();
    }
    Ok(())
}

fn prompt_for_code(buffer: &mut String) -> io::Result<usize> {
    let input = io::stdin();
    let mut handle = input.lock();
    print!("> ");
    io::stdout().flush()?;
    handle.read_line(buffer)
}
