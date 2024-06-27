use std::fmt;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::value::LoxType;

#[derive(Debug)]
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

impl PartialEq for RuntimeError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                RuntimeError::UnaryOpTypeError {
                    operator: op1,
                    operand: operand1,
                    line: line1,
                },
                RuntimeError::UnaryOpTypeError {
                    operator: op2,
                    operand: operand2,
                    line: line2,
                },
            ) => op1 == op2 && operand1 == operand2 && line1 == line2,
            (
                RuntimeError::BinaryOpTypeError {
                    operator: op1,
                    left: left1,
                    right: right1,
                    line: line1,
                },
                RuntimeError::BinaryOpTypeError {
                    operator: op2,
                    left: left2,
                    right: right2,
                    line: line2,
                },
            ) => op1 == op2 && left1 == left2 && right1 == right2 && line1 == line2,
            (RuntimeError::IOError(err1), RuntimeError::IOError(err2)) => {
                err1.kind() == err2.kind()
            }
            _ => false,
        }
    }
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
            RuntimeError::IOError(err) => write!(f, "IOError: {}", err),
        }
    }
}
