use crate::expr::Expr;

enum LoxValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

pub fn interpret(expr: Expr) {
    todo!();
}