use std::fmt::Display;

use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, PartialEq, Clone)]
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
    Logical {
        left: Box<Expr>,
        operator: LogicalOperator,
        right: Box<Expr>,
    },
    Unary {
        operator: UnaryOperator,
        right: Box<Expr>,
    },
    Variable {
        name: String,
    },
    Assignment {
        name: String,
        value: Box<Expr>,
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
                LiteralValue::String(s) => write!(f, "\"{}\"", s),
                LiteralValue::Boolean(b) => write!(f, "{}", b),
                LiteralValue::Nil => write!(f, "nil"),
            },
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                write!(f, "({} {} {})", operator, left, right)
            }
            Expr::Unary { operator, right } => {
                write!(f, "({} {})", operator, right)
            }
            Expr::Variable { name } => {
                write!(f, "{}", name)
            }
            Expr::Assignment { name, value } => {
                write!(f, "({} = {})", name, value)
            }
        }
    }
}
