use crate::ast::{Ast, BinaryOperatorType, Expr, LogicalOperatorType, Stmt, UnaryOperatorType};
use crate::interpret::environment::Environment;
use crate::interpret::RuntimeError;
use crate::value::LoxValue as V;
use crate::value::{Callable, NativeFunction, UserDefinedFunction};
use log::{debug, trace};
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

pub struct Interpreter<W: Write> {
    // Where to direct the output of "print"
    writer: Rc<RefCell<W>>,
    // The global variable environment.
    globals: Rc<RefCell<Environment>>,
    // The current variable environment.
    environment: Rc<RefCell<Environment>>,
}

impl<W: Write> Interpreter<W> {
    pub fn new(writer: W) -> Self {
        let mut root_env = Environment::new(None);

        // Add our native functions (just this one) to the global environment.
        fn clock(_env: Rc<RefCell<Environment>>, _args: Vec<V>) -> Result<V, RuntimeError> {
            // This fetches the seconds since the epoch.
            let now = V::Number(
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs_f64(),
            );
            Ok(now)
        }
        let clock_callable = Callable::Native(NativeFunction::new(0, clock));
        root_env.define("clock", V::Callable(clock_callable));

        // Wrap the root environment in a RefCell and Rc before storing it, since we
        // are going to have a lot of references to it and sometimes need to mutate.
        let globals = Rc::new(RefCell::new(root_env));

        Self {
            writer: Rc::new(RefCell::new(writer)),
            globals: globals.clone(),
            environment: globals.clone(),
        }
    }

    pub fn interpret(&mut self, ast: Ast) -> Result<(), RuntimeError> {
        for stmt in ast {
            self.eval_stmt(stmt)?;
        }
        Ok(())
    }

    pub fn get_environment(&self) -> Rc<RefCell<Environment>> {
        self.environment.clone()
    }

    pub fn set_environment(&mut self, env: Rc<RefCell<Environment>>) {
        self.environment = env;
    }

    pub fn eval_stmt(&mut self, stmt: Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Print(expr) => {
                let value = self.eval_expr(expr)?;
                writeln!(self.writer.borrow_mut(), "{}", value)
                    .map_err(|io_err| RuntimeError::IOError(io_err))?;
            }
            Stmt::Return(expr) => {
                let return_val = match expr {
                    None => V::Nil,
                    Some(expr) => self.eval_expr(expr)?,
                };
                debug!("return statement encountered with value {:?}", return_val);
                return Err(RuntimeError::ReturnCall(return_val));
            }
            Stmt::Expression(expr) => {
                self.eval_expr(expr)?;
            }
            Stmt::Function { name, params, body } => {
                let inner_stmts = match *body {
                    Stmt::Block(stmts) => stmts,
                    _ => panic!("Function body must be a block"),
                };
                let function = Callable::UserDefined(UserDefinedFunction::new(
                    params,
                    inner_stmts,
                    self.environment.clone(),
                ));
                self.environment
                    .borrow_mut()
                    .define(&name, V::Callable(function));
            }
            Stmt::Var { name, initializer } => {
                let value = match initializer {
                    Some(expr) => self.eval_expr(expr)?,
                    None => V::Nil,
                };
                self.environment.borrow_mut().define(&name, value);
            }
            Stmt::While { condition, body } => {
                while self.eval_expr(condition.clone())?.is_truthy() {
                    self.eval_stmt(*body.clone())?;
                }
            }
            Stmt::Block(stmts) => {
                // Create a new subinterpreter with a new environment that points to the
                // current one, and execute code there.
                let new_env = Environment::new(Some(self.environment.clone()));
                let mut subinterpreter = Interpreter {
                    writer: self.writer.clone(),
                    globals: self.globals.clone(),
                    environment: Rc::new(RefCell::new(new_env)),
                };
                for stmt in stmts {
                    subinterpreter.eval_stmt(stmt)?;
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let should_exec = self.eval_expr(condition)?.is_truthy();
                match should_exec {
                    true => self.eval_stmt(*then_branch)?,
                    false => {
                        if let Some(else_branch) = else_branch {
                            self.eval_stmt(*else_branch)?;
                        };
                    }
                }
            }
        }
        Ok(())
    }

    /// Evaluate the given expression and return the result.
    fn eval_expr(&mut self, expr: Expr) -> Result<V, RuntimeError> {
        let evaluated = match expr {
            Expr::Literal { value } => V::new_from_literal(value),
            Expr::Grouping { expression } => self.eval_expr(*expression)?,
            Expr::Unary { operator, right } => {
                let right_val = self.eval_expr(*right)?;
                match operator.tp {
                    UnaryOperatorType::Minus => match right_val {
                        V::Number(n) => V::Number(-n),
                        v => {
                            return Err(RuntimeError::UnaryOpTypeError {
                                operator,
                                operand: v.tp(),
                                line: operator.line,
                            })
                        }
                    },
                    UnaryOperatorType::Bang { .. } => V::Boolean(!right_val.is_truthy()),
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.eval_expr(*left)?;
                let right = self.eval_expr(*right)?;
                let result = match operator.tp {
                    // Arithmetic and comparison operators that always require two numbers.
                    BinaryOperatorType::Minus
                    | BinaryOperatorType::Slash
                    | BinaryOperatorType::Star
                    | BinaryOperatorType::Greater
                    | BinaryOperatorType::GreaterEqual
                    | BinaryOperatorType::Less
                    | BinaryOperatorType::LessEqual => {
                        match (left, right) {
                            // If both operands are numbers, return the result.
                            (V::Number(l), V::Number(r)) => {
                                trace!("Computing {:?} {} {:?}", l, operator.tp, r);
                                match operator.tp {
                                    BinaryOperatorType::Minus => V::Number(l - r),
                                    BinaryOperatorType::Slash => V::Number(l / r),
                                    BinaryOperatorType::Star => V::Number(l * r),
                                    BinaryOperatorType::GreaterEqual => V::Boolean(l >= r),
                                    BinaryOperatorType::Greater => V::Boolean(l > r),
                                    BinaryOperatorType::LessEqual => V::Boolean(l <= r),
                                    BinaryOperatorType::Less => V::Boolean(l < r),
                                    _ => {
                                        unreachable!("we already matched to one of these operators")
                                    }
                                }
                            }
                            (l, r) => {
                                // If at least one operator isn't a number, that's invalid.
                                return Err(RuntimeError::BinaryOpTypeError {
                                    operator,
                                    left: l.tp(),
                                    right: r.tp(),
                                    line: operator.line,
                                });
                            }
                        }
                    }
                    // Plus is special, since it works on numbers or strings.
                    BinaryOperatorType::Plus => match (left, right) {
                        (V::Number(l), V::Number(r)) => V::Number(l + r),
                        (V::String(l), V::String(r)) => V::String(format!("{}{}", l, r)),
                        (l, r) => {
                            return Err(RuntimeError::BinaryOpTypeError {
                                operator,
                                left: l.tp(),
                                right: r.tp(),
                                line: operator.line,
                            });
                        }
                    },
                    BinaryOperatorType::BangEqual => V::Boolean(left != right),
                    BinaryOperatorType::EqualEqual => V::Boolean(left == right),
                };
                result
            }
            Expr::Call { callee, args, line } => {
                let callee = self.eval_expr(*callee)?;
                let arg_values = args
                    .into_iter()
                    .map(|arg| self.eval_expr(arg))
                    .collect::<Vec<_>>()
                    .into_iter()
                    .collect::<Result<Vec<V>, RuntimeError>>()?;
                // Unwrap the callable or throw an error if it's not callable.
                let callable = match callee {
                    V::Callable(callable) => {
                        trace!("Function called with args: {:?}", arg_values[0]);
                        callable
                    }
                    v => {
                        return Err(RuntimeError::CallableTypeError {
                            uncallable_type: v.tp(),
                            line,
                        })
                    }
                };
                // Call it and return the result.
                if arg_values.len() != callable.arity() {
                    return Err(RuntimeError::ArityError {
                        expected: callable.arity(),
                        received: arg_values.len(),
                        line,
                    });
                }
                let subinterpreter = Interpreter {
                    writer: self.writer.clone(),
                    globals: self.globals.clone(),
                    environment: self.globals.clone(),
                };
                // Call the function but trap Return calls (which propagate like errors) instead of propagating them upward.
                match callable.call(subinterpreter, arg_values) {
                    Ok(v) => v,
                    Err(RuntimeError::ReturnCall(v)) => {
                        debug!("catching return call: {:?}", v);
                        v
                    }
                    Err(err) => return Err(err),
                }
            }
            Expr::Variable { name } => match self.environment.borrow().get(&name) {
                Some(v) => v,
                None => return Err(RuntimeError::UndefinedVariable(name)),
            },
            Expr::Assignment { name, value } => {
                let evaluated = self.eval_expr(*value)?;
                self.environment
                    .borrow_mut()
                    .assign(&name, evaluated.clone())?;
                evaluated
            }
            Expr::Logical {
                left,
                right,
                operator,
            } => {
                let left_value = self.eval_expr(*left)?;
                match operator.tp {
                    LogicalOperatorType::And => match left_value.is_truthy() {
                        false => left_value,
                        true => self.eval_expr(*right)?,
                    },
                    LogicalOperatorType::Or => match left_value.is_truthy() {
                        true => left_value,
                        false => self.eval_expr(*right)?,
                    },
                }
            }
        };
        Ok(evaluated)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOperator, UnaryOperator};
    use crate::parse::parse;
    use crate::scan::scan;
    use crate::value::LoxType;

    /// Evaluate a given input string that represents an expression, panicking if scanning or parsing fails.
    fn eval_str(input: &str) -> Result<V, RuntimeError> {
        // Add a semicolon to make the input a valid statement (otherwise the parser will be upset).
        let input = format!("{};", input);
        let tokens = scan(input).unwrap();
        let stmts = parse(tokens).unwrap();
        let mock_writer: Vec<u8> = Vec::new();
        let mut interpreter = Interpreter::new(mock_writer);
        // We expect the AST to contain a single expression.
        let expr = match stmts[0].clone() {
            Stmt::Expression(expr) => expr,
            _ => panic!("Expected an expression statement"),
        };
        interpreter.eval_expr(expr)
    }

    /// Interpret one or more statements and collect the printed output into a string.
    fn exec_ast(input: &str) -> Result<String, RuntimeError> {
        let tokens = scan(input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let mock_writer: Vec<u8> = Vec::new();
        let mut interpreter = Interpreter::new(mock_writer);
        interpreter.interpret(ast)?;
        let written = String::from_utf8(interpreter.writer.borrow().clone()).unwrap();
        Ok(written)
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

    // Tests of expression evaluation.

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
                    operator: UnaryOperator {
                        tp: UnaryOperatorType::Minus,
                        line: 1,
                    },
                    operand: LoxType::Nil,
                    line: 1,
                },
            ),
            (
                "-true",
                RuntimeError::UnaryOpTypeError {
                    operator: UnaryOperator {
                        tp: UnaryOperatorType::Minus,
                        line: 1,
                    },
                    operand: LoxType::Boolean,
                    line: 1,
                },
            ),
            (
                "-\"abc\"",
                RuntimeError::UnaryOpTypeError {
                    operator: UnaryOperator {
                        tp: UnaryOperatorType::Minus,
                        line: 1,
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
            BinaryOperator {
                tp: BinaryOperatorType::Plus,
                line: 1,
            },
            BinaryOperator {
                tp: BinaryOperatorType::Minus,
                line: 1,
            },
            BinaryOperator {
                tp: BinaryOperatorType::Star,
                line: 1,
            },
            BinaryOperator {
                tp: BinaryOperatorType::Slash,
                line: 1,
            },
            BinaryOperator {
                tp: BinaryOperatorType::Greater,
                line: 1,
            },
            BinaryOperator {
                tp: BinaryOperatorType::GreaterEqual,
                line: 1,
            },
            BinaryOperator {
                tp: BinaryOperatorType::Less,
                line: 1,
            },
            BinaryOperator {
                tp: BinaryOperatorType::LessEqual,
                line: 1,
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

    // Tests of statement evaluation.

    #[test]
    fn print_stmt() {
        let output = exec_ast("print 123;").unwrap();
        assert_eq!(output, "123\n");
    }

    #[test]
    fn multiline_print_stmt() {
        let output = exec_ast("print 123 + 456;\ntrue != true;\nprint 3 > 4;").unwrap();
        assert_eq!(output, "579\nfalse\n");
    }

    #[test]
    fn simple_variables() {
        let output = exec_ast("var x = 123; var y = 4; print 5; print x + y;").unwrap();
        assert_eq!(output, "5\n127\n");
    }

    #[test]
    fn variables_with_reassignment() {
        let output = exec_ast("var x = 4; x = 12 * 12; print x;").unwrap();
        assert_eq!(output, "144\n");
        let output = exec_ast("var x = 4; x = (x = 9) * 11; print x;").unwrap();
        assert_eq!(output, "99\n");
    }

    #[test]
    fn blocks() {
        // Outer X is 3
        // Enter block
        // Print outer X (3)
        // Reassign outer X to 9
        // Print outer X again (9)
        // Declare a new, inner X set to 5
        // Reassign inner X to 7
        // Print inner X (7)
        // Print outer X (9)
        let output = exec_ast(
            "var x = 3;\n{ print x; x = 9; print x; var x = 5; x = 7; print x; }\nprint x;",
        )
        .unwrap();
        assert_eq!(output, "3\n9\n7\n9\n");
    }

    #[test]
    fn if_stmt() {
        let output = exec_ast("if (true) { print 1; } else { print 2; }").unwrap();
        assert_eq!(output, "1\n");
    }

    #[test]
    fn if_else_stmt() {
        let output = exec_ast("if ( 3 > 4 ) { print 1; } else { print 2; }").unwrap();
        assert_eq!(output, "2\n");
    }

    #[test]
    fn test_logical() {
        // And
        let ouput = exec_ast("print true and true; print true and false; print false and false; print false and true;").unwrap();
        assert_eq!(ouput, "true\nfalse\nfalse\nfalse\n");
        // Or
        let ouput = exec_ast(
            "print true or true; print true or false; print false or false; print false or true;",
        )
        .unwrap();
        assert_eq!(ouput, "true\ntrue\nfalse\ntrue\n");
        // Associativity
        let ouput =
            exec_ast("print true and true or false and false; print true or false and false;")
                .unwrap();
        assert_eq!(ouput, "true\ntrue\n");
    }

    #[test]
    fn test_logical_short_circuit_and() {
        // And w/ short-circuit.
        let ouput = exec_ast("var x = 4; print false and (x = true); print x;").unwrap();
        assert_eq!(ouput, "false\n4\n");
        // And w/out short-circuit.
        let ouput = exec_ast("var x = 4; print true and (x = true); print x;").unwrap();
        assert_eq!(ouput, "true\ntrue\n");
    }

    #[test]
    fn test_logical_short_circuit_or() {
        // Or w/ short-circuit.
        let ouput = exec_ast("var x = 4; print true or (x = false); print x;").unwrap();
        assert_eq!(ouput, "true\n4\n");
        // Or w/out short-circuit.
        let ouput = exec_ast("var x = 4; print false or (x = false); print x;").unwrap();
        assert_eq!(ouput, "false\nfalse\n");
    }

    #[test]
    fn logical_returns_actual_value_by_truthiness() {
        let ouput = exec_ast("print 1 and nil; print nil and 2;").unwrap();
        assert_eq!(ouput, "nil\nnil\n");

        let output = exec_ast("print 1 or nil; print nil or 2;").unwrap();
        assert_eq!(output, "1\n2\n");
    }

    #[test]
    fn test_while() {
        let output = exec_ast("var x = 0; while (x < 3) { print x; x = x + 1; }").unwrap();
        assert_eq!(output, "0\n1\n2\n");
    }

    #[test]
    fn test_for() {
        let output = exec_ast("for (var x = 0; x < 3; x = x + 1) { print x; }").unwrap();
        assert_eq!(output, "0\n1\n2\n");
        let output = exec_ast("var y = 1; for (; y != 9;) { print y; y = y * 3; }").unwrap();
        assert_eq!(output, "1\n3\n");
    }

    #[test]
    fn native_function_call() {
        let output = exec_ast("print clock();").unwrap();
        let output_as_f64: f64 = output.trim().parse().unwrap();
        assert!(output_as_f64 > 0.0);
    }

    #[test]
    fn udf_call() {
        let output = exec_ast("fun do(a, b) { print(3); } var x = do(3, 4); print x;").unwrap();
        assert_eq!(output, "3\nnil\n");
    }

    #[test]
    fn arity_error() {
        let error = exec_ast("fun do(a, b) { } do(3);").unwrap_err();
        assert_eq!(
            error,
            RuntimeError::ArityError {
                expected: 2,
                received: 1,
                line: 1
            }
        );

        let error = exec_ast("fun do(a, b) { } do(3, 4, 5);").unwrap_err();
        assert_eq!(
            error,
            RuntimeError::ArityError {
                expected: 2,
                received: 3,
                line: 1
            }
        );
    }

    #[test]
    fn udf_can_read_outer_scope() {
        let output = exec_ast("var x = 3; fun do(a, b) { print(x); } do(3, 4);").unwrap();
        assert_eq!(output, "3\n");
    }

    #[test]
    fn udf_can_modify_outer_scope() {
        let output = exec_ast("var x = 3; fun do() { x = 4; } do(); print x;").unwrap();
        assert_eq!(output, "4\n");
    }

    #[test]
    fn udf_can_enclose_variables() {
        let output = exec_ast(
            "var myfunc; { var x = 3; fun inner() { print x; } myfunc = inner;} myfunc();",
        )
        .unwrap();
        assert_eq!(output, "3\n");
    }

    #[test]
    fn udf_cant_read_inner_scope() {
        let error = exec_ast("fun do() { print(x); } {var x = 4; do();}").unwrap_err();
        assert_eq!(error, RuntimeError::UndefinedVariable("x".to_string()));
    }

    #[test]
    fn udf_can_recurse() {
        let output =
            exec_ast("fun decrement(n) { print n; if (n > 1) { decrement(n-1); } } decrement(5);")
                .unwrap();
        assert_eq!(output, "5\n4\n3\n2\n1\n");
    }

    #[test]
    fn udf_with_return() {
        let output = exec_ast("fun do() { return 5; } print do() + 5;").unwrap();
        assert_eq!(output, "10\n");
    }

    #[test]
    fn udf_with_deeply_nested_return() {
        let output = exec_ast("fun do(x) { x = x + 5; while (x < 50) { print x; x = x + x; if (x > 10) { return x; }}} print do(7);").unwrap();
        assert_eq!(output, "12\n24\n");
    }

    #[test]
    fn fib() {
        let output = exec_ast("fun fib(n) { if (n <= 1) { return n; } return fib(n - 1) + fib(n - 2); } print fib(2); print fib(8);").unwrap();
        assert_eq!(output, "1\n21\n");
    }
}
