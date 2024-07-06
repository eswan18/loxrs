use std::collections::HashMap;

use crate::value::LoxValue;

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: String,
    // pub methods: Vec<Stmt>,
}

impl Class {
    pub fn new_instance(&self) -> LoxValue {
        LoxValue::Instance(Instance {
            class: self.clone(),
            fields: HashMap::new(),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    pub class: Class,
    fields: HashMap<String, LoxValue>,
}

impl Instance {
    pub fn get(&self, name: &str) -> Option<&LoxValue> {
        self.fields.get(name)
    }

    pub fn set(&mut self, name: String, value: LoxValue) {
        self.fields.insert(name, value);
    }
}