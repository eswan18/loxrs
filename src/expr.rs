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

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: LiteralValue,
    },
    Unary {
        operator: UnaryOperator,
        right: Box<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                write!(f, "({} {} {})", operator, left, right)
            }
            Expr::Grouping { expression } => {
                write!(f, "({})", expression)
            }
            Expr::Literal { value } => match value {
                LiteralValue::Number(n) => write!(f, "{}", n),
                LiteralValue::String(s) => write!(f, "{}", s),
                LiteralValue::Boolean(b) => write!(f, "{}", b),
                LiteralValue::Nil => write!(f, "nil"),
            },
            Expr::Unary { operator, right } => {
                write!(f, "({} {})", operator, right)
            }
        }
    }
}
