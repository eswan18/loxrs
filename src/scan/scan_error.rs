use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScanError {
    UnexpectedCharacter(char),
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScanError::UnexpectedCharacter(c) => write!(f, "Unexpected character: {}", c),
        }
    }
}
