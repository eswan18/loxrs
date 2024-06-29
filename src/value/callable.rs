use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::Stmt;
use crate::interpret::Environment;
use crate::value::LoxValue;

#[derive(Debug, PartialEq, Clone)]
/// A user-defined function that can be called from Lox and is written in Lox.
pub struct UserDefinedFunction {
    arity: usize,
    body: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
/// A native (a.k.a. "foreign") function that can be called from Lox, but is written in Rust.
pub struct NativeFunction {
    arity: usize,
    function: fn(Rc<RefCell<Environment>>, Vec<LoxValue>) -> LoxValue,
}

impl NativeFunction {
    pub fn new(
        arity: usize,
        function: fn(Rc<RefCell<Environment>>, Vec<LoxValue>) -> LoxValue,
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
            Callable::UserDefined(f) => f.arity,
            Callable::Native(f) => f.arity,
        }
    }

    pub fn call(&self, env: Rc<RefCell<Environment>>, args: Vec<LoxValue>) -> LoxValue {
        match self {
            Callable::UserDefined(f) => {
                todo!();
            }
            Callable::Native(f) => (f.function)(env, args),
        }
    }
}
