use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoxType {
    Number,
    String,
    Boolean,
    Callable,
    Nil,
}

impl fmt::Display for LoxType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxType::Number => write!(f, "Number"),
            LoxType::String => write!(f, "String"),
            LoxType::Boolean => write!(f, "Boolean"),
            LoxType::Callable => write!(f, "Callable"),
            LoxType::Nil => write!(f, "Nil"),
        }
    }
}
