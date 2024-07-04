use crate::ast::VariableReference;
use std::fmt;

#[derive(Debug)]
pub enum ResolveError {
    ReadBeforeInitialize(VariableReference),
    Redeclaration(String),
    InvalidReturn,
    InvalidScopeOperation(String),
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        match self {
            ResolveError::ReadBeforeInitialize(VariableReference { name, id }) => {
                write!(
                    f,
                    "Variable '{}' (ID {}) is read before it is initialized",
                    name, id
                )
            }
            ResolveError::Redeclaration(name) => {
                write!(f, "Variable '{}' is already declared in this scope", name)
            }
            ResolveError::InvalidReturn => write!(f, "Invalid return statement"),
            ResolveError::InvalidScopeOperation(message) => {
                write!(f, "Invalid scope operation: {}", message)
            }
        }
    }
}
