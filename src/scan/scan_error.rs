use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScanError {
    UnexpectedCharacter { char: char, line: u32 },
    UnterminatedString { line: u32 },
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScanError::UnexpectedCharacter { char, line } => {
                write!(f, "[line {}] Unexpected character: '{}'", line, char)
            }
            ScanError::UnterminatedString { line } => {
                write!(f, "[line {}] Unterminated string", line)
            }
        }
    }
}

impl std::error::Error for ScanError {}
