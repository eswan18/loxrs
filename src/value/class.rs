use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
    pub fields: HashMap<String, Rc<RefCell<LoxValue>>>,
}

impl Instance {
    pub fn get(&self, name: &str) -> Option<Rc<RefCell<LoxValue>>> {
        self.fields.get(name).cloned()
    }

    pub fn set(&mut self, name: String, value: LoxValue) {
        self.fields.insert(name, Rc::new(RefCell::new(value)));
    }
}
