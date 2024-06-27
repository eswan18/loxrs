use crate::error::LoxError;
use crate::interpret;
use crate::parse;
use crate::scan;
use crate::value::LoxValue;

pub fn run_code(code: &str) -> Result<LoxValue, LoxError> {
    let tokens = match scan::scan(code.to_string()) {
        Ok(tokens) => tokens,
        Err(scan_errors) => {
            match scan_errors.len() {
                0 => panic!("No scan errors but scan failed"),
                1 => {
                    return Err(LoxError {
                        exit_code: 65,
                        text: scan_errors[0].to_string(),
                    });
                }
                _ => {
                    // Join the errors into a single string, separated by newlines.
                    let err_text = scan_errors
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join("\n");
                    return Err(LoxError {
                        exit_code: 65,
                        text: err_text,
                    });
                }
            }
        }
    };
    let ast = match parse::parse(tokens) {
        Ok(ast) => ast,
        Err(parse_error) => {
            return Err(LoxError {
                exit_code: 65,
                text: parse_error.to_string(),
            });
        }
    };
    let value = match interpret::interpret(ast) {
        Ok(value) => value,
        Err(runtime_error) => {
            return Err(LoxError {
                exit_code: 70,
                text: runtime_error.to_string(),
            });
        }
    };
    Ok(value)
}
