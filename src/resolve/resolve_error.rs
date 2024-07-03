use crate::ast::VariableReference;
use std::fmt;

#[derive(Debug)]
pub enum ResolveError {
    ReadVarBeforeInitialize(VariableReference),
    InvalidScopeOperation(String),
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        match self {
            ResolveError::ReadVarBeforeInitialize(VariableReference { name, id }) => {
                write!(
                    f,
                    "Variable '{}' (ID {}) is read before it is initialized",
                    name, id
                )
            }
            ResolveError::InvalidScopeOperation(message) => {
                write!(f, "Invalid scope operation: {}", message)
            }
        }
    }
}
