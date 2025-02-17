use crate::ast::VariableReference;
use std::fmt;

#[derive(Debug)]
pub enum ResolveError {
    ReadBeforeInitialize(VariableReference),
    Redeclaration(String),
    ReturnOutsideFunction,
    ReturnWithinInitializer,
    InvalidScopeOperation(String),
    InheritanceCycle(String),
    ThisOutsideClass,
    SuperOutsideSubclass,
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
            ResolveError::ReturnOutsideFunction => write!(f, "Invalid return statement"),
            ResolveError::ReturnWithinInitializer => {
                write!(f, "Cannot return a value from an initializer")
            }
            ResolveError::InvalidScopeOperation(message) => {
                write!(f, "Invalid scope operation: {}", message)
            }
            ResolveError::InheritanceCycle(class_name) => {
                write!(f, "Inheritance cycle detected in class '{}'", class_name)
            }
            ResolveError::ThisOutsideClass => write!(f, "Cannot use 'this' outside of a class"),
            ResolveError::SuperOutsideSubclass => {
                write!(f, "Cannot use 'super' outside of a subclass")
            }
        }
    }
}
