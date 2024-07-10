use core::fmt;
use std::fmt::Display;

use crate::ast::expr::Expr;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Print(Expr),
    Return(Option<Expr>),
    Expression(Expr),
    Function(FunctionDefinition),
    Var {
        name: String,
        initializer: Option<Expr>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Block(Vec<Stmt>),
    Class {
        name: String,
        methods: Vec<FunctionDefinition>,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Print(expr) => write!(f, "Print({});", expr),
            Stmt::Return(expr) => match expr {
                Some(expr) => write!(f, "Return {};", expr),
                None => write!(f, "Return;"),
            },
            Stmt::Expression(expr) => write!(f, "{};", expr),
            Stmt::Function(FunctionDefinition { name, params, body }) => {
                write!(f, "fun {}(", name)?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") {{\n")?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::Var { name, initializer } => match initializer {
                Some(expr) => write!(f, "var {} = {};", name, expr),
                None => write!(f, "var {};", name),
            },
            Stmt::While { condition, body } => write!(f, "while ({}) {}", condition, body),
            Stmt::Block(stmts) => {
                write!(f, "{{\n")?;
                for stmt in stmts {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::Class { name, methods } => {
                write!(f, "class {} {{\n", name)?;
                for method in methods {
                    write!(f, "{}\n", Stmt::Function(method.clone()))?;
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
