use crate::value::LoxValue;
use std::collections::HashMap;

pub struct Environment {
    values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Option<&LoxValue> {
        self.values.get(name)
    }
}
