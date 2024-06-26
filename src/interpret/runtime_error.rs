use std::fmt;

use crate::{token::TokenType, value::LoxType};

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    UnaryOpTypeError {
        op: TokenType,
        operand: LoxType,
        line: u32,
    },
    BinaryOpTypeError {
        op: TokenType,
        left: LoxType,
        right: LoxType,
        line: u32,
    },
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::UnaryOpTypeError { op, operand, line } => {
                let err_msg = format!("Unary operator `{}` is not valid for type {}", op, operand);
                write!(f, "UnaryOpTypeError (line {}): {}", line, err_msg)
            }
            RuntimeError::BinaryOpTypeError {
                op,
                left,
                right,
                line,
            } => {
                let err_msg = format!(
                    "Binary operator `{}` is not valid for types {} and {}",
                    op, left, right
                );
                write!(f, "BinaryOpTypeError (line {}): {}", line, err_msg)
            }
        }
    }
}
