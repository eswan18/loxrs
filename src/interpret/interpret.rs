use crate::expr::{BinaryOperator, Expr, UnaryOperator};
use crate::interpret::RuntimeError;
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
            match operator {
                UnaryOperator::Minus { .. } => match right_val {
                    V::Number(n) => V::Number(-n),
                    v => {
                        return Err(RuntimeError::UnaryOpTypeError {
                            operator: operator.clone(),
                            operand: v.tp(),
                            line: operator.line_number(),
                        })
                    }
                },
                UnaryOperator::Bang { .. } => V::Boolean(!right_val.is_truthy()),
            }
        }
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let left = eval(*left)?;
            let right = eval(*right)?;
            match operator {
                // Arithmetic and comparison operators that always require two numbers.
                BinaryOperator::Minus { .. }
                | BinaryOperator::Slash { .. }
                | BinaryOperator::Star { .. }
                | BinaryOperator::Greater { .. }
                | BinaryOperator::GreaterEqual { .. }
                | BinaryOperator::Less { .. }
                | BinaryOperator::LessEqual { .. } => {
                    match (left, right) {
                        // If both operands are numbers, return the result.
                        (V::Number(l), V::Number(r)) => match operator {
                            BinaryOperator::Minus { .. } => V::Number(l - r),
                            BinaryOperator::Slash { .. } => V::Number(l / r),
                            BinaryOperator::Star { .. } => V::Number(l * r),
                            BinaryOperator::GreaterEqual { .. } => V::Boolean(l >= r),
                            BinaryOperator::Greater { .. } => V::Boolean(l > r),
                            BinaryOperator::LessEqual { .. } => V::Boolean(l <= r),
                            BinaryOperator::Less { .. } => V::Boolean(l < r),
                            _ => unreachable!("we already matched to one of these operators"),
                        },
                        (l, r) => {
                            // If at least one operator isn't a number, that's invalid.
                            return Err(RuntimeError::BinaryOpTypeError {
                                operator: operator.clone(),
                                left: l.tp(),
                                right: r.tp(),
                                line: operator.line_number(),
                            });
                        }
                    }
                }
                // Plus is special, since it works on numbers or strings.
                BinaryOperator::Plus { token } => match (left, right) {
                    (V::Number(l), V::Number(r)) => V::Number(l + r),
                    (V::String(l), V::String(r)) => V::String(l + &r),
                    (l, r) => {
                        return Err(RuntimeError::BinaryOpTypeError {
                            operator: BinaryOperator::Plus {
                                token: token.clone(),
                            },
                            left: l.tp(),
                            right: r.tp(),
                            line: token.line,
                        })
                    }
                },
                BinaryOperator::BangEqual { .. } => V::Boolean(left != right),
                BinaryOperator::EqualEqual { .. } => V::Boolean(left == right),
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
    use crate::token::{Token, TokenType};
    use crate::value::LoxType;

    /// Evaluate the given input string, panicking if scanning or parsing fails.
    fn eval_str(input: &str) -> Result<V, RuntimeError> {
        let tokens = scan(input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        interpret(ast)
    }

    /// Assert that a set of inputs produce the corresponding outputs when evaluated.
    fn assert_inputs_resolve(input_and_expected: Vec<(&str, V)>) {
        for (input, expected) in input_and_expected {
            let actual = eval_str(input).unwrap();
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
    fn test_unary_minus_type_errors() {
        let input_and_expected: Vec<(&str, RuntimeError)> = vec![
            (
                "-nil",
                RuntimeError::UnaryOpTypeError {
                    operator: UnaryOperator::Minus {
                        token: Token {
                            tp: TokenType::Minus,
                            lexeme: "-".to_string(),
                            line: 1,
                        },
                    },
                    operand: LoxType::Nil,
                    line: 1,
                },
            ),
            (
                "-true",
                RuntimeError::UnaryOpTypeError {
                    operator: UnaryOperator::Minus {
                        token: Token {
                            tp: TokenType::Minus,
                            lexeme: "-".to_string(),
                            line: 1,
                        },
                    },
                    operand: LoxType::Boolean,
                    line: 1,
                },
            ),
            (
                "-\"abc\"",
                RuntimeError::UnaryOpTypeError {
                    operator: UnaryOperator::Minus {
                        token: Token {
                            tp: TokenType::Minus,
                            lexeme: "-".to_string(),
                            line: 1,
                        },
                    },
                    operand: LoxType::String,
                    line: 1,
                },
            ),
        ];
        for (input, expected) in input_and_expected {
            let actual = eval_str(input).unwrap_err();
            assert_eq!(actual, expected);
        }
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

    #[test]
    fn binary_string_addition() {
        let input_and_expected: Vec<(&str, V)> = vec![
            ("\"abc\" + \"def\"", V::String("abcdef".to_string())),
            ("\"abc\" + \"4\"", V::String("abc4".to_string())),
            ("\"4\" + \"abc\"", V::String("4abc".to_string())),
        ];
        assert_inputs_resolve(input_and_expected);
    }

    #[test]
    fn binary_comparison() {
        let input_and_expected: Vec<(&str, V)> = vec![
            ("4 > 3", V::Boolean(true)),
            ("4 > 4", V::Boolean(false)),
            ("4 >= 3", V::Boolean(true)),
            ("4 >= 4", V::Boolean(true)),
            ("4 < 3", V::Boolean(false)),
            ("4 < 4", V::Boolean(false)),
            ("4 <= 3", V::Boolean(false)),
            ("4 <= 4", V::Boolean(true)),
        ];
        assert_inputs_resolve(input_and_expected);
    }

    #[test]
    fn binary_numeric_type_errors() {
        // Commentary: I went a little crazy on this test, but you can't say it's not thorough.
        // Each numeric binary operator (as a TokenType) and its corresponding error message.
        let binary_ops = vec![
            BinaryOperator::Plus {
                token: Token::new(TokenType::Plus, "+".to_string(), 1),
            },
            BinaryOperator::Minus {
                token: Token::new(TokenType::Minus, "-".to_string(), 1),
            },
            BinaryOperator::Star {
                token: Token::new(TokenType::Star, "*".to_string(), 1),
            },
            BinaryOperator::Slash {
                token: Token::new(TokenType::Slash, "/".to_string(), 1),
            },
            BinaryOperator::Greater {
                token: Token::new(TokenType::Greater, ">".to_string(), 1),
            },
            BinaryOperator::GreaterEqual {
                token: Token::new(TokenType::GreaterEqual, ">=".to_string(), 1),
            },
            BinaryOperator::Less {
                token: Token::new(TokenType::Less, "<".to_string(), 1),
            },
            BinaryOperator::LessEqual {
                token: Token::new(TokenType::LessEqual, "<=".to_string(), 1),
            },
        ];
        // A bunch of cases where the binary operator should fail due to type errors.
        struct TestCase {
            left: &'static str,
            right: &'static str,
            exclude_ops: Vec<&'static str>,
        }
        let cases = vec![
            TestCase {
                left: "4",
                right: "true",
                exclude_ops: vec![],
            },
            TestCase {
                left: "4",
                right: "nil",
                exclude_ops: vec![],
            },
            TestCase {
                left: "4",
                right: "\"abc\"",
                exclude_ops: vec![],
            },
            TestCase {
                left: "true",
                right: "4",
                exclude_ops: vec![],
            },
            TestCase {
                left: "nil",
                right: "4",
                exclude_ops: vec![],
            },
            TestCase {
                left: "\"abc\"",
                right: "4",
                exclude_ops: vec![],
            },
            TestCase {
                left: "nil",
                right: "nil",
                exclude_ops: vec![],
            },
            // Special case: strings can be concatenated with `+`.
            TestCase {
                left: "\"abc\"",
                right: "\"abc\"",
                exclude_ops: vec!["+"],
            },
        ];
        for op in binary_ops {
            for case in &cases {
                // Skip this operator/test-case combo if it's in the exclude list.
                if case.exclude_ops.contains(&op.to_string().as_str()) {
                    continue;
                }
                // Construct the expr string on the fly (e.g. "4 + true") and evaluate it.
                let expr_str = format!("{} {} {}", case.left, op, case.right);
                let err = eval_str(&expr_str).unwrap_err();
                // Construct the expected error and check that it matches the actual error.
                let left = eval_str(case.left).unwrap().tp();
                let right = eval_str(case.right).unwrap().tp();
                let expected_err = RuntimeError::BinaryOpTypeError {
                    operator: op.clone(),
                    left,
                    right,
                    line: 1,
                };
                assert_eq!(err, expected_err, "Expected this error for `{}`", expr_str);
            }
        }
    }

    #[test]
    fn equality() {
        let input_and_expected: Vec<(&str, V)> = vec![
            // Equality using the same type
            ("4 == 4", V::Boolean(true)),
            ("4 != 4", V::Boolean(false)),
            ("4 == 5", V::Boolean(false)),
            ("4 != 5", V::Boolean(true)),
            ("true == true", V::Boolean(true)),
            ("true != true", V::Boolean(false)),
            ("true == false", V::Boolean(false)),
            ("true != false", V::Boolean(true)),
            ("nil == nil", V::Boolean(true)),
            ("nil != nil", V::Boolean(false)),
            ("\"abc\" == \"abc\"", V::Boolean(true)),
            ("\"abc\" != \"abc\"", V::Boolean(false)),
            ("\"abc\" == \"def\"", V::Boolean(false)),
            ("\"abc\" != \"def\"", V::Boolean(true)),
            // Equality across different types (always false)
            ("4 == true", V::Boolean(false)),
            ("4 != true", V::Boolean(true)),
            ("4 == nil", V::Boolean(false)),
            ("4 != nil", V::Boolean(true)),
            ("4 == \"abc\"", V::Boolean(false)),
            ("4 != \"abc\"", V::Boolean(true)),
            ("true == nil", V::Boolean(false)),
            ("true != nil", V::Boolean(true)),
            ("true == \"abc\"", V::Boolean(false)),
            ("true != \"abc\"", V::Boolean(true)),
            ("nil == \"abc\"", V::Boolean(false)),
            ("nil != \"abc\"", V::Boolean(true)),
        ];
        assert_inputs_resolve(input_and_expected);
    }
}
