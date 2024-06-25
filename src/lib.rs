use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

mod error;
mod parse;
mod run;
mod scan;
mod expr;
mod token;
mod interpret;

pub fn run_file(filename: &str) -> Result<(), i32> {
    // Open the file and get its contents.
    let contents = match fs::read_to_string(filename) {
        Ok(contents) => contents,
        Err(_) => {
            eprintln!("Error reading file '{}'", filename);
            return Err(1);
        }
    };
    run::run_code(&contents)
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
        // We intentionally suppress errors here because we want the REPL to keep running.
        let _ = run::run_code(&buffer);
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
