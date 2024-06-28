use core::fmt;
use std::fmt::Display;

use crate::ast::expr;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Print(expr::Expr),
    Expression(expr::Expr),
    Var {
        name: String,
        initializer: Option<expr::Expr>,
    },
    Block(Vec<Stmt>),
    If {
        condition: expr::Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Print(expr) => write!(f, "Print({});", expr),
            Stmt::Expression(expr) => write!(f, "{};", expr),
            Stmt::Var { name, initializer } => match initializer {
                Some(expr) => write!(f, "var {} = {};", name, expr),
                None => write!(f, "var {};", name),
            },
            Stmt::Block(stmts) => {
                write!(f, "{{\n")?;
                for stmt in stmts {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if ({}) {}", condition, then_branch)?;
                match else_branch {
                    Some(else_branch) => write!(f, " else {}", else_branch),
                    None => write!(f, ""),
                }
            }
        }
    }
}
