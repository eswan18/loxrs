use core::fmt;
use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BinaryOperator {
    pub tp: BinaryOperatorType,
    pub line: u32,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.tp)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperatorType {
    Plus,
    Minus,
    Star,
    Slash,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl Display for BinaryOperatorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperatorType::Plus => write!(f, "+"),
            BinaryOperatorType::Minus => write!(f, "-"),
            BinaryOperatorType::Star => write!(f, "*"),
            BinaryOperatorType::Slash => write!(f, "/"),
            BinaryOperatorType::BangEqual => write!(f, "!="),
            BinaryOperatorType::EqualEqual => write!(f, "=="),
            BinaryOperatorType::Greater => write!(f, ">"),
            BinaryOperatorType::GreaterEqual => write!(f, ">="),
            BinaryOperatorType::Less => write!(f, "<"),
            BinaryOperatorType::LessEqual => write!(f, "<="),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct UnaryOperator {
    pub tp: UnaryOperatorType,
    pub line: u32,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.tp)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperatorType {
    Minus,
    Bang,
}

impl Display for UnaryOperatorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperatorType::Minus => write!(f, "-"),
            UnaryOperatorType::Bang => write!(f, "!"),
        }
    }
}
