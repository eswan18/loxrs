use std::fmt;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::value::{LoxType, LoxValue};

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
    CallableTypeError {
        uncallable_type: LoxType,
        line: u32,
    },
    ArityError {
        expected: usize,
        received: usize,
        line: u32,
    },
    IOError(std::io::Error),
    UndefinedVariable(String),
    InternalError(String),
    // A return call isn't an error per se, but using an error lets us elegantly propagate the return value up the stack.
    ReturnCall(LoxValue),
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
            (
                RuntimeError::CallableTypeError {
                    uncallable_type: tp1,
                    line: l1,
                },
                RuntimeError::CallableTypeError {
                    uncallable_type: tp2,
                    line: l2,
                },
            ) => tp1 == tp2 && l1 == l2,
            (
                RuntimeError::ArityError {
                    expected: e1,
                    received: r1,
                    line: l1,
                },
                RuntimeError::ArityError {
                    expected: e2,
                    received: r2,
                    line: l2,
                },
            ) => e1 == e2 && r1 == r2 && l1 == l2,
            (RuntimeError::IOError(err1), RuntimeError::IOError(err2)) => {
                err1.kind() == err2.kind()
            }
            (RuntimeError::UndefinedVariable(v1), RuntimeError::UndefinedVariable(v2)) => v1 == v2,
            (RuntimeError::ReturnCall(v1), RuntimeError::ReturnCall(v2)) => v1 == v2,
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
            RuntimeError::CallableTypeError {
                uncallable_type,
                line,
            } => {
                let err_msg = format!("Type {} is not callable", uncallable_type);
                write!(f, "CallableTypeError [line {}]: {}", line, err_msg)
            }
            RuntimeError::ArityError {
                expected,
                received,
                line,
            } => {
                let err_msg = format!("Expected {} arguments but received {}", expected, received);
                write!(f, "ArityError [line {}]: {}", line, err_msg)
            }
            RuntimeError::IOError(err) => write!(f, "IOError: {}", err),
            RuntimeError::UndefinedVariable(name) => {
                write!(f, "Undefined variable '{}'", name)
            }
            RuntimeError::ReturnCall(value) => write!(f, "ReturnCall: {}", value),
            RuntimeError::InternalError(msg) => write!(f, "InternalError: {}", msg),
        }
    }
}
