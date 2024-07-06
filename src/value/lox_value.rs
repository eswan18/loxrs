use std::fmt;

use crate::ast::LiteralValue;
use crate::value::{Callable, Class, LoxType};

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Callable(Callable),
    Class(Class),
    Instance { class: Class },
    Nil,
}

impl LoxValue {
    pub fn new_from_literal(value: LiteralValue) -> LoxValue {
        match value {
            LiteralValue::Number(n) => LoxValue::Number(n),
            LiteralValue::String(s) => LoxValue::String(s),
            LiteralValue::Boolean(b) => LoxValue::Boolean(b),
            LiteralValue::Nil => LoxValue::Nil,
        }
    }

    /// Check if the value is "truthy". In Lox, only `false` and `nil` are falsy.
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxValue::Nil => false,
            LoxValue::Boolean(b) => *b,
            _ => true,
        }
    }

    /// Get the type of the value.
    pub fn tp(&self) -> LoxType {
        match self {
            LoxValue::Number(_) => LoxType::Number,
            LoxValue::String(_) => LoxType::String,
            LoxValue::Boolean(_) => LoxType::Boolean,
            LoxValue::Callable(_) => LoxType::Callable,
            LoxValue::Class(_) => LoxType::Class,
            LoxValue::Instance { class } => LoxType::Instance(class.clone()),
            LoxValue::Nil => LoxType::Nil,
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxValue::Number(n) => write!(f, "{}", n),
            LoxValue::String(s) => write!(f, "{}", s),
            LoxValue::Boolean(b) => write!(f, "{}", b),
            LoxValue::Callable(Callable::Native(_)) => write!(f, "<native-function>"),
            LoxValue::Callable(Callable::UserDefined(_)) => write!(f, "<function>"),
            LoxValue::Class(Class { name }) => write!(f, "<class {}>", name),
            LoxValue::Instance { class } => write!(f, "<{} instance>", class.name),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}
