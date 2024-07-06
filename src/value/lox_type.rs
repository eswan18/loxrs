use std::fmt;

use crate::value::Class;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoxType {
    Number,
    String,
    Boolean,
    Callable,
    Class,
    Instance(Class),
    Nil,
}

impl fmt::Display for LoxType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxType::Number => write!(f, "Number"),
            LoxType::String => write!(f, "String"),
            LoxType::Boolean => write!(f, "Boolean"),
            LoxType::Callable => write!(f, "Callable"),
            LoxType::Class => write!(f, "Class"),
            LoxType::Instance(class) => write!(f, "{} Instance", class.name),
            LoxType::Nil => write!(f, "Nil"),
        }
    }
}
