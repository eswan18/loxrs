use crate::value::LoxValue;
use std::collections::HashMap;

use crate::interpret::RuntimeError;

pub struct Environment {
    values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    /// Define a new variable or overwrite an existing one.
    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.values.insert(name.to_string(), value);
    }

    /// Get the value of a variable if it's set.
    pub fn get(&self, name: &str) -> Option<&LoxValue> {
        self.values.get(name)
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else {
            Err(RuntimeError::UndefinedVariable(name.to_string()))
        }
    }
}
