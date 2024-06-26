use crate::expr::LiteralValue;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    Number(f64),
    String(String),
    Boolean(bool),
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
            LoxValue::Nil => LoxType::Nil,
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxValue::Number(n) => write!(f, "{}", n),
            LoxValue::String(s) => write!(f, "\"{}\"", s),
            LoxValue::Boolean(b) => write!(f, "{}", b),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}

pub enum LoxType {
    Number,
    String,
    Boolean,
    Nil,
}

impl fmt::Display for LoxType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxType::Number => write!(f, "Number"),
            LoxType::String => write!(f, "String"),
            LoxType::Boolean => write!(f, "Boolean"),
            LoxType::Nil => write!(f, "Nil"),
        }
    }
}
