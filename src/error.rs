use std::fmt;

#[derive(Debug)]
pub struct LoxError {
    pub exit_code: i32,
    pub text: String,
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}
