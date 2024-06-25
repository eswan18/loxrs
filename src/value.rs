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
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxValue::Number(n) => write!(f, "{}", n),
            LoxValue::String(s) => write!(f, "{}", s),
            LoxValue::Boolean(b) => write!(f, "{}", b),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}
