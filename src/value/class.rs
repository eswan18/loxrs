use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

use crate::interpret::{Interpreter, RuntimeError};
use crate::value::Callable;
use crate::value::LoxValue;

use super::UserDefinedFunction;

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, UserDefinedFunction>,
}

impl Class {
    pub fn call<W>(
        &self,
        subinterpreter: Interpreter<W>,
        args: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError>
    where
        W: Write,
    {
        let instance = LoxValue::Instance(Instance {
            class: self.clone(),
            fields: HashMap::new(),
        });
        let instance = Rc::new(RefCell::new(instance));
        if let Some(init) = self.find_method("init") {
            let mut callable = init.clone();
            callable = callable.bind(instance.clone());
            let lox_callable = Callable::UserDefined(callable);
            match lox_callable.call(subinterpreter, args) {
                Ok(_) => {}
                // Returns from the initializer are fine, but they just end the function early.
                Err(RuntimeError::ReturnCall(_)) => {}
                Err(e) => return Err(e),
            }
        }
        Rc::try_unwrap(instance)
            .map(|x| Ok(x.into_inner()))
            .unwrap()
    }

    pub fn find_method(&self, name: &str) -> Option<&UserDefinedFunction> {
        self.methods.get(name)
    }

    pub fn arity(&self) -> usize {
        if let Some(init) = self.find_method("init") {
            let lox_callable = Callable::UserDefined(init.clone());
            lox_callable.arity()
        } else {
            0
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    pub class: Class,
    pub fields: HashMap<String, Rc<RefCell<LoxValue>>>,
}

impl Instance {
    /// Get a field from an instance
    /// The instance is passed in as a reference-counted reference to a RefCell so that we can hand it off to method calls and other functions, which is messy (impossible?) with a regular self reference.
    pub fn get(instance: Rc<RefCell<LoxValue>>, name: &str) -> Option<Rc<RefCell<LoxValue>>> {
        let (fields, class) = match instance.borrow().clone() {
            LoxValue::Instance(Instance { fields, class }) => (fields, class),
            _ => return None,
        };
        if let Some(x) = fields.get(name) {
            return Some(x.clone());
        }
        if let Some(method) = class.find_method(name) {
            let mut callable = method.clone();
            callable = callable.bind(instance.clone());
            let lox_callable = LoxValue::Callable(Callable::UserDefined(callable));
            return Some(Rc::new(RefCell::new(lox_callable)));
        }
        None
    }

    pub fn set(&mut self, name: String, value: LoxValue) {
        self.fields.insert(name, Rc::new(RefCell::new(value)));
    }
}
