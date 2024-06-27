use core::fmt;
use std::fmt::Display;

use crate::ast::expr;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Print(expr::Expr),
    Expression(expr::Expr),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Print(expr) => write!(f, "Print({});", expr),
            Stmt::Expression(expr) => write!(f, "{};", expr),
        }
    }
}
