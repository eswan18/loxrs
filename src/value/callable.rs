use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::Stmt;
use crate::interpret::{Environment, Interpreter, RuntimeError};
use crate::value::LoxValue;
use std::io::Write;

#[derive(Debug, PartialEq, Clone)]
/// A user-defined function that can be called from Lox and is written in Lox.
pub struct UserDefinedFunction {
    param_names: Vec<String>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
}

impl UserDefinedFunction {
    pub fn new(
        param_names: Vec<String>,
        body: Vec<Stmt>,
        closure: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            param_names,
            body,
            closure,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
/// A native (a.k.a. "foreign") function that can be called from Lox, but is written in Rust.
pub struct NativeFunction {
    arity: usize,
    function: fn(Rc<RefCell<Environment>>, Vec<LoxValue>) -> Result<LoxValue, RuntimeError>,
}

impl NativeFunction {
    pub fn new(
        arity: usize,
        function: fn(Rc<RefCell<Environment>>, Vec<LoxValue>) -> Result<LoxValue, RuntimeError>,
    ) -> Self {
        Self { arity, function }
    }
}

#[derive(Debug, Clone, PartialEq)]
/// Any callable object in Lox.
pub enum Callable {
    UserDefined(UserDefinedFunction),
    Native(NativeFunction),
}

impl Callable {
    pub fn arity(&self) -> usize {
        match self {
            Callable::UserDefined(f) => f.param_names.len(),
            Callable::Native(f) => f.arity,
        }
    }

    pub fn call<W>(
        &self,
        mut subinterpreter: Interpreter<W>,
        args: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError>
    where
        W: Write,
    {
        match self {
            Callable::UserDefined(UserDefinedFunction {
                param_names,
                body,
                closure,
            }) => {
                // Note that arity checks happen in the interpreter so we don't worry about them here.
                // Replace the interpreter's environment with this function's closure.
                let subinterpreter_env = Environment::new(Some(closure.clone()));
                subinterpreter.set_environment(Rc::new(RefCell::new(subinterpreter_env)));
                // Start by defining variables for each argument.
                for (name, value) in param_names.iter().zip(args.iter()) {
                    subinterpreter
                        .get_environment()
                        .borrow_mut()
                        .define(name, value.clone());
                }
                // Then execute the body.
                subinterpreter.eval_stmt(Stmt::Block(body.clone()))?;
                Ok(LoxValue::Nil)
            }
            Callable::Native(f) => (f.function)(subinterpreter.get_environment(), args),
        }
    }
}
