use std::fmt;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::value::LoxType;

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    UnaryOpTypeError {
        operator: UnaryOperator,
        operand: LoxType,
        line: u32,
    },
    BinaryOpTypeError {
        operator: BinaryOperator,
        left: LoxType,
        right: LoxType,
        line: u32,
    },
    IOError(std::io::Error),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::UnaryOpTypeError {
                operator,
                operand,
                line,
            } => {
                let err_msg = format!(
                    "Unary operator `{}` is not valid for type {}",
                    operator, operand
                );
                write!(f, "UnaryOpTypeError [line {}]: {}", line, err_msg)
            }
            RuntimeError::BinaryOpTypeError {
                operator,
                left,
                right,
                line,
            } => {
                let err_msg = format!(
                    "Binary operator `{}` is not valid for types {} and {}",
                    operator, left, right
                );
                write!(f, "BinaryOpTypeError [line {}]: {}", line, err_msg)
            }
        }
    }
}
