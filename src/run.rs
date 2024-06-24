use crate::error;
use crate::scan;

pub fn run_code(code: &str) -> Result<(), Vec<error::Error>> {
    println!("{}", code);
    let tokens = match scan::scan(code.to_string()) {
        Ok(tokens) => tokens,
        Err(scan_errors) => {
            let errors: Vec<error::Error> = scan_errors
                .into_iter()
                .map(|scan_error| error::Error::ScanError(scan_error))
                .collect();
            return Err(errors);
        }
    };
    for token in tokens {
        println!("{:?}", token);
    }
    Ok(())
}

fn error(line: usize, message: &str) {
    report(line, "", message);
}

fn report(line: usize, location: &str, message: &str) {
    eprintln!("[line {}] Error {}: {}", line, location, message);
}
