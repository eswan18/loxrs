use crate::error::LoxError;
use crate::interpret::Interpreter;
use crate::parse;
use crate::resolve::resolve;
use crate::scan;
use std::io::Write;

pub fn run_code<W: Write>(code: &str, writer: W) -> Result<(), LoxError> {
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
        Err(parse_errors) => {
            // Join the errors into a single string, separated by newlines.
            let err_text = parse_errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n");
            return Err(LoxError {
                exit_code: 65,
                text: err_text,
            });
        }
    };
    let local_resolutions = match resolve(&ast) {
        Ok(resolutions) => resolutions,
        Err(resolve_error) => {
            return Err(LoxError {
                exit_code: 65,
                text: resolve_error.to_string(),
            });
        }
    };
    let mut interpreter = Interpreter::new(writer, local_resolutions);
    match interpreter.interpret(ast) {
        Ok(_) => Ok(()),
        Err(runtime_error) => {
            return Err(LoxError {
                exit_code: 70,
                text: runtime_error.to_string(),
            });
        }
    }
}
