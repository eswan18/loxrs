use crate::expr::Expr;
use crate::interpret::RuntimeError;
use crate::token::TokenType;
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
        Expr::Grouping { expression } => eval(*expression)?,
        Expr::Unary { operator, right } => {
            let right_val = eval(*right)?;
            match operator.token_type {
                TokenType::Minus => match right_val {
                    LoxValue::Number(n) => LoxValue::Number(-n),
                    _ => {
                        return Err(RuntimeError::TypeError(
                            "Operand to '-' must be a number".to_string(),
                        ))
                    }
                },
                TokenType::Bang => LoxValue::Boolean(!is_truthy(right_val)),
                _ => unreachable!("Invalid unary operator"),
            }
        }
        _ => return Err(RuntimeError::Generic("Not implemented".to_string())),
    };
    Ok(evaluated)
}

fn is_truthy(value: LoxValue) -> bool {
    match value {
        LoxValue::Nil => false,
        LoxValue::Boolean(b) => b,
        _ => true,
    }
}
