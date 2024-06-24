use crate::scan;

pub fn run_code(code: &str) -> Result<(), std::io::Error> {
    println!("{}", code);
    scan::scan();
    Ok(())
}

fn error(line: usize, message: &str) {
    report(line, "", message);
}

fn report(line: usize, location: &str, message: &str) {
    eprintln!("[line {}] Error {}: {}", line, location, message);
}