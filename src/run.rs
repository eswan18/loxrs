use crate::scan;

pub fn run_code(code: &str) -> Result<(), i32> {
    let tokens = match scan::scan(code.to_string()) {
        Ok(tokens) => tokens,
        Err(scan_errors) => {
            eprintln!("Scanning failed:");
            for error in scan_errors {
                eprintln!("{}", error);
            }
            return Err(65);
        }
    };
    for token in tokens {
        println!("{:?}", token);
    }
    Ok(())
}
