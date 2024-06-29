use crate::ast::Expr;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    MissingRightParen { line: u32 },
    ExtraInput { line: u32 },
    ExpectedSemicolon { line: u32 },
    ExpectedVar { line: u32 },
    ExpectedIdentifier { line: u32, entity: String },
    ExpectedLeftBrace { line: u32 },
    ExpectedRightBrace { line: u32 },
    ExpectedLeftParen { line: u32 },
    ExpectedRightParen { line: u32 },
    InvalidAssignmentTarget { expr: Expr, line: u32 },
    TooManyArguments { line: u32 },
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
            ParseError::ExpectedVar { line } => {
                write!(f, "ParseError [line {}]: Expected 'var' keyword", line)
            }
            ParseError::ExpectedIdentifier { line, entity } => {
                write!(
                    f,
                    "ParseError [line {}]: Expected identifier as name for {}",
                    line, entity
                )
            }
            ParseError::ExpectedLeftBrace { line } => {
                write!(f, "ParseError [line {}]: Expected '{{'", line)
            }
            ParseError::ExpectedRightBrace { line } => {
                write!(f, "ParseError [line {}]: Expected '}}'", line)
            }
            ParseError::ExpectedLeftParen { line } => {
                write!(f, "ParseError [lint {}]: Expected '('", line)
            }
            ParseError::ExpectedRightParen { line } => {
                write!(f, "ParseError [lint {}]: Expected ')'", line)
            }
            ParseError::InvalidAssignmentTarget { line, expr } => {
                write!(
                    f,
                    "ParseError [line {}]: Invalid assignment target ({})",
                    line, expr
                )
            }
            ParseError::TooManyArguments { line } => {
                write!(f, "ParseError [line {}]: Too many arguments", line)
            }
        }
    }
}
