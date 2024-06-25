use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeError {
    Generic(String),
    TypeError(String),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::Generic(msg) => {
                write!(f, "{}", msg)
            }
            RuntimeError::TypeError(msg) => {
                write!(f, "{}", msg)
            }
        }
    }
}
