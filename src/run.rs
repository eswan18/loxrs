use crate::error::LoxError;
use crate::interpret;
use crate::parse;
use crate::scan;
use crate::value::LoxValue;

pub fn run_code(code: &str) -> Result<LoxValue, LoxError> {
    let tokens = match scan::scan(code.to_string()) {
        Ok(tokens) => tokens,
        Err(scan_errors) => {
            let mut err_text = String::from("Scanning failed:\n");
            for error in scan_errors {
                err_text.push_str(&format!("{}\n", error));
            }
            return Err(LoxError {
                exit_code: 65,
                text: err_text,
            });
        }
    };
    let ast = match parse::parse(tokens) {
        Ok(ast) => ast,
        Err(parse_error) => {
            let err_text = format!("Parsing failed:\n{}", parse_error);
            return Err(LoxError {
                exit_code: 65,
                text: err_text,
            });
        }
    };
    let value = match interpret::interpret(ast) {
        Ok(value) => value,
        Err(runtime_error) => {
            let err_text = format!("Runtime error:\n{}", runtime_error);
            return Err(LoxError {
                exit_code: 70,
                text: err_text,
            });
        }
    };
    Ok(value)
}
