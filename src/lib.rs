use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

mod error;
mod run;
mod scan;

pub fn run_file(filename: &str) -> Result<(), std::io::Error> {
    // Open the file and get its contents.
    let contents = fs::read_to_string(filename)?;
    run::run_code(&contents);
    Ok(())
}

pub fn run_prompt() -> Result<(), std::io::Error> {
    let mut buffer = String::new();
    let input = io::stdin();
    let mut handle = input.lock();

    loop {
        // Repeatedly read input from the user and execute it.
        print!("> ");
        io::stdout().flush()?;
        buffer.clear();
        let bytes_read = handle.read_line(&mut buffer)?;
        if bytes_read == 0 {
            // Allows us to exit on ctrl-D.
            break;
        }
        run::run_code(&buffer);
    }
    Ok(())
}
