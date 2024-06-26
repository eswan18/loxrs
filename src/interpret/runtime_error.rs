use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeError {
    TypeError { msg: String, line: u32 },
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::TypeError { msg, line } => {
                write!(f, "TypeError (line {}): {}", line, msg)
            }
        }
    }
}
