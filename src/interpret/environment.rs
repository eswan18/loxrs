use log::trace;

use crate::value::LoxValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use crate::interpret::RuntimeError;

#[derive(Debug, PartialEq)]
pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>,
    pub values: HashMap<String, Rc<RefCell<LoxValue>>>,
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_at_depth(f, 0)?;
        Ok(())
    }
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing,
        }
    }

    /// Write the environment and all environments it encloses, preceeded by the depth of each environment.
    fn write_at_depth(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        let comma_separated = self
            .values
            .iter()
            .map(|(key, value)| format!("{} -> {}", key, value.borrow()))
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{} -> {{ {} }}", depth, comma_separated)?;
        if let Some(env) = self.enclosing.clone() {
            write!(f, "\n")?;
            env.borrow().write_at_depth(f, depth + 1)?;
        }
        Ok(())
    }

    /// Define a new variable or overwrite an existing one.
    pub fn define(&mut self, name: &str, value: LoxValue) {
        trace!("Defining {} as {} (type {})", name, value, value.tp());
        if self.values.contains_key(name) {
            trace!("Overwriting {} with {}", name, value);
        }
        let value = Rc::new(RefCell::new(value));
        self.values.insert(name.to_string(), value);
    }

    /// Get the value of a variable if it's set.
    pub fn get(&self, name: &str) -> Option<Rc<RefCell<LoxValue>>> {
        self.values.get(name).map(|v| v.clone())
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            trace!("Assigning {} to {} (depth {})", value, name, self.depth());
            let value = Rc::new(RefCell::new(value));
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

    pub fn depth(&self) -> usize {
        match self.enclosing.as_ref() {
            Some(enclosing) => 1 + enclosing.borrow().depth(),
            None => 0,
        }
    }
}
