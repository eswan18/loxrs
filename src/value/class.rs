use crate::value::LoxValue;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub name: String,
    // pub methods: Vec<Stmt>,
}

impl Class {
    pub fn new_instance(&self) -> LoxValue {
        LoxValue::Instance {
            class: self.clone(),
        }
    }
}
