use crate::scan;

pub fn run_code(code: &str) -> Result<(), std::io::Error> {
    println!("{}", code);
    scan::scan();
    Ok(())
}