use crate::value::LoxValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::interpret::RuntimeError;

#[derive(Debug)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new(enclosing: Option<Environment>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: match enclosing {
                Some(e) => Some(Rc::new(RefCell::new(e))),
                None => None,
            },
        }
    }

    /// Define a new variable or overwrite an existing one.
    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.values.insert(name.to_string(), value);
    }

    /// Get the value of a variable if it's set.
    /// Returns a copy of the value, not a reference to the original.
    // Note to future self: Copying here might become an issue, because `x = MyObj(); y = ex; y.prop = 5;` seems like it should modify x, not a copy of it.
    pub fn get(&self, name: &str) -> Option<LoxValue> {
        match self.values.get(name) {
            Some(value) => Some(value.clone()),
            None => {
                if let Some(enclosing) = &self.enclosing {
                    if let Some(value) = enclosing.borrow().get(name) {
                        return Some(value.clone());
                    };
                };
                None
            }
        }
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else {
            if let Some(enclosing) = &self.enclosing {
                enclosing.borrow_mut().assign(name, value)
            } else {
                Err(RuntimeError::UndefinedVariable(name.to_string()))
            }
        }
    }

    /// Set the enclosing environment.
    pub fn set_enclosing(&mut self, enclosing: Environment) {
        self.enclosing = Some(Rc::new(RefCell::new(enclosing)));
    }

    /// Set the enclosing environment to None and give ownership back to the calling code.
    pub fn unset_enclosing(&mut self) -> Option<Environment> {
        match self.enclosing.take() {
            Some(enclosing) => Some(Rc::try_unwrap(enclosing).unwrap().into_inner()),
            None => None,
        }
    }
}
