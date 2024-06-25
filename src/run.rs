use crate::parse;
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
    let ast = match parse::parse(tokens) {
        Ok(ast) => ast,
        Err(parse_error) => {
            eprintln!("Parsing failed:");
            eprintln!("{}", parse_error);
            return Err(65);
        }
    };
    println!("{}", ast);
    Ok(())
}
