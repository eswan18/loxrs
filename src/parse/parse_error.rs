use crate::ast::Expr;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    MissingRightParen { line: u32 },
    ExtraInput { line: u32 },
    ExpectedSemicolon { line: u32 },
    ExpectedIdentifier { line: u32 },
    ExpectedRightBrace { line: u32 },
    InvalidAssignmentTarget { expr: Expr, line: u32 },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::MissingRightParen { line } => {
                write!(f, "ParseError [line {}]: Missing ')'", line)
            }
            ParseError::ExtraInput { line } => {
                write!(f, "ParseError [line {}]: Extra input", line)
            }
            ParseError::ExpectedSemicolon { line } => {
                write!(f, "ParseError [line {}]: Expected ';'", line)
            }
            ParseError::ExpectedIdentifier { line } => {
                write!(f, "ParseError [line {}]: Expected identifier", line)
            }
            ParseError::ExpectedRightBrace { line } => {
                write!(f, "ParseError [line {}]: Expected '}}'", line)
            }
            ParseError::InvalidAssignmentTarget { line, expr } => {
                write!(
                    f,
                    "ParseError [line {}]: Invalid assignment target ({})",
                    line, expr
                )
            }
        }
    }
}
