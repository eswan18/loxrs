use std::fmt::Display;

use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct VariableReference {
    pub name: String,
    pub id: u32,
}

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
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        line: u32,
    },
    Get {
        object: Box<Expr>,
        name: String,
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
    Variable(VariableReference),
    Assignment {
        reference: VariableReference,
        value: Box<Expr>,
    },
}

impl Expr {
    /// Generate a monotonically increasing ID for each variable.
    pub fn new_id() -> u32 {
        static mut ID: u32 = 0;
        unsafe {
            let id = ID;
            ID += 1;
            id
        }
    }
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
            Expr::Call { callee, args, .. } => {
                let args = args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}({})", callee, args)
            }
            Expr::Get { object, name } => {
                write!(f, "{}.{}", object, name)
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
            Expr::Variable(var_ref) => {
                write!(f, "{}", var_ref.name)
            }
            Expr::Assignment {
                reference: VariableReference { name, .. },
                value,
            } => {
                write!(f, "({} = {})", name, value)
            }
        }
    }
}
