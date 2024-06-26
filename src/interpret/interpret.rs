use crate::expr::Expr;
use crate::interpret::RuntimeError;
use crate::token::TokenType;
use crate::value::LoxValue as V;

/// Interpret the given expression.
//
/// This is a thin wrapper over the non-public eval function.
pub fn interpret(expr: Expr) -> Result<V, RuntimeError> {
    eval(expr)
}

/// Evaluate the given expression and return the result.
fn eval(expr: Expr) -> Result<V, RuntimeError> {
    let evaluated = match expr {
        Expr::Literal { value } => V::new_from_literal(value),
        Expr::Grouping { expression } => eval(*expression)?,
        Expr::Unary { operator, right } => {
            let right_val = eval(*right)?;
            match operator.token_type {
                TokenType::Minus => match right_val {
                    V::Number(n) => V::Number(-n),
                    _ => {
                        return Err(RuntimeError::TypeError(
                            "Operand to '-' must be a number".to_string(),
                        ))
                    }
                },
                TokenType::Bang => V::Boolean(!right_val.is_truthy()),
                _ => unreachable!("Invalid unary operator"),
            }
        }
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let left = eval(*left)?;
            let right = eval(*right)?;
            match operator.token_type {
                // Arithmetic operators that always require two numbers.
                TokenType::Minus | TokenType::Slash | TokenType::Star => match (left, right) {
                    // If both operands are numbers, return the result.
                    (V::Number(l), V::Number(r)) => match operator.token_type {
                        TokenType::Minus => V::Number(l - r),
                        TokenType::Slash => V::Number(l / r),
                        TokenType::Star => V::Number(l * r),
                        _ => unreachable!("we already matched to one of these operators"),
                    },
                    (l, r) => {
                        // If at least one operator isn't a number, return that's invalid.
                        let err_msg = format!("Operator `-`  requires two numeric arguments. Not valid for `{}` and `{}`", l, r);
                        return Err(RuntimeError::TypeError(err_msg.to_string()));
                    }
                },
                // Plus is special, since it works on numbers or strings.
                TokenType::Plus => match (left, right) {
                    (V::Number(l), V::Number(r)) => V::Number(l + r),
                    (V::String(l), V::String(r)) => V::String(l + &r),
                    (l, r) => {
                        let err_msg = format!("Operator `+`  requires two numeric arguments or two string arguments. Not valid for `{}` and `{}`", l, r);
                        return Err(RuntimeError::TypeError(err_msg.to_string()));
                    }
                },
                _ => panic!(),
            }
        }
    };
    Ok(evaluated)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;
    use crate::scan::scan;

    fn assert_inputs_resolve(input_and_expected: Vec<(&str, V)>) {
        for (input, expected) in input_and_expected {
            let tokens = scan(input.to_string()).unwrap();
            let ast = parse(tokens).unwrap();
            let actual = interpret(ast).unwrap();
            assert_eq!(
                actual, expected,
                "input: `{}` should resolve to: `{:?}`",
                input, expected
            );
        }
    }

    #[test]
    fn literals() {
        let input_and_expected: Vec<(&str, V)> = vec![
            ("true", V::Boolean(true)),
            ("false", V::Boolean(false)),
            ("nil", V::Nil),
            ("4", V::Number(4.0)),
            ("\"abc\"", V::String("abc".to_string())),
        ];
        assert_inputs_resolve(input_and_expected);
    }

    #[test]
    fn unary_not() {
        let input_and_expected: Vec<(&str, V)> = vec![
            ("!true", V::Boolean(false)),
            ("!false", V::Boolean(true)),
            ("!4", V::Boolean(false)),
            ("!nil", V::Boolean(true)),
            ("!!nil", V::Boolean(false)),
            ("!\"abc\"", V::Boolean(false)),
        ];
        assert_inputs_resolve(input_and_expected);
    }

    #[test]
    fn test_unary_minus() {
        let input_and_expected: Vec<(&str, V)> = vec![
            ("-4", V::Number(-4.0)),
            ("-4.0", V::Number(-4.0)),
            ("--4", V::Number(4.0)),
            ("---4", V::Number(-4.0)),
        ];
        assert_inputs_resolve(input_and_expected);
    }

    #[test]
    fn groupings() {
        let input_and_expected: Vec<(&str, V)> = vec![
            ("(4)", V::Number(4.0)),
            ("-(4)", V::Number(-4.0)),
            ("(true)", V::Boolean(true)),
            ("(false)", V::Boolean(false)),
            ("(nil)", V::Nil),
            ("(\"abc\")", V::String("abc".to_string())),
        ];
        assert_inputs_resolve(input_and_expected);
    }

    #[test]
    fn binary_arithmetic() {
        let input_and_expected: Vec<(&str, V)> = vec![
            ("4 + 4", V::Number(8.0)),
            ("4 - 4", V::Number(0.0)),
            ("4 * 4", V::Number(16.0)),
            ("4 / 4", V::Number(1.0)),
        ];
        assert_inputs_resolve(input_and_expected);
    }
}
