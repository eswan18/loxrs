use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    MissingRightParen { line: u32 },
    ExtraInput { line: u32 },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::MissingRightParen { line } => {
                write!(f, "[line {}] Missing ')'", line)
            }
            ParseError::ExtraInput { line } => {
                write!(f, "[line {}] Extra input", line)
            }
        }
    }
}
