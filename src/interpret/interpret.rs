use crate::expr::Expr;
use crate::interpret::RuntimeError;
use crate::value::LoxValue;

/// Interpret the given expression.
//
/// This is a thin wrapper over the non-public eval function.
pub fn interpret(expr: Expr) -> Result<LoxValue, RuntimeError> {
    eval(expr)
}

/// Evaluate the given expression and return the result.
fn eval(expr: Expr) -> Result<LoxValue, RuntimeError> {
    let evaluated = match expr {
        Expr::Literal { value } => LoxValue::new_from_literal(value),
        _ => return Err(RuntimeError::Generic("Not implemented".to_string())),
    };
    Ok(evaluated)
}
