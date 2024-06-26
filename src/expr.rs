use core::fmt;
use std::fmt::Display;

use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Plus { token: Token },
    Minus { token: Token },
    Star { token: Token },
    Slash { token: Token },
    BangEqual { token: Token },
    EqualEqual { token: Token },
    Greater { token: Token },
    GreaterEqual { token: Token },
    Less { token: Token },
    LessEqual { token: Token },
}

impl BinaryOperator {
    pub fn line_number(&self) -> u32 {
        match self {
            BinaryOperator::Plus { token } => token.line,
            BinaryOperator::Minus { token } => token.line,
            BinaryOperator::Star { token } => token.line,
            BinaryOperator::Slash { token } => token.line,
            BinaryOperator::BangEqual { token } => token.line,
            BinaryOperator::EqualEqual { token } => token.line,
            BinaryOperator::Greater { token } => token.line,
            BinaryOperator::GreaterEqual { token } => token.line,
            BinaryOperator::Less { token } => token.line,
            BinaryOperator::LessEqual { token } => token.line,
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Plus { .. } => write!(f, "+"),
            BinaryOperator::Minus { .. } => write!(f, "-"),
            BinaryOperator::Star { .. } => write!(f, "*"),
            BinaryOperator::Slash { .. } => write!(f, "/"),
            BinaryOperator::BangEqual { .. } => write!(f, "!="),
            BinaryOperator::EqualEqual { .. } => write!(f, "=="),
            BinaryOperator::Greater { .. } => write!(f, ">"),
            BinaryOperator::GreaterEqual { .. } => write!(f, ">="),
            BinaryOperator::Less { .. } => write!(f, "<"),
            BinaryOperator::LessEqual { .. } => write!(f, "<="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Minus { token: Token },
    Bang { token: Token },
}

impl UnaryOperator {
    pub fn line_number(&self) -> u32 {
        match self {
            UnaryOperator::Minus { token } => token.line,
            UnaryOperator::Bang { token } => token.line,
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Minus { .. } => write!(f, "-"),
            UnaryOperator::Bang { .. } => write!(f, "!"),
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
