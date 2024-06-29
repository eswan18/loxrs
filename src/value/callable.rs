use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::Stmt;
use crate::interpret::{Environment, RuntimeError};
use crate::value::LoxValue;

#[derive(Debug, PartialEq, Clone)]
/// A user-defined function that can be called from Lox and is written in Lox.
pub struct UserDefinedFunction {
    param_names: Vec<String>,
    body: Vec<Stmt>,
}

impl UserDefinedFunction {
    pub fn new(param_names: Vec<String>, body: Vec<Stmt>) -> Self {
        Self { param_names, body }
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

    pub fn call(
        &self,
        env: Rc<RefCell<Environment>>,
        args: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        match self {
            Callable::UserDefined(UserDefinedFunction { param_names, .. }) => {
                // Arity checks happen in the interpreter so we don't worry about them here.
                for (name, value) in param_names.iter().zip(args.iter()) {
                    env.borrow_mut().define(name, value.clone());
                }
                Ok(LoxValue::Nil)
            }
            Callable::Native(f) => (f.function)(env, args),
        }
    }
}
