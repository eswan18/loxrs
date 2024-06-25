use std::fmt;

use crate::scan;

#[derive(Debug)]
pub struct LoxError {
    pub scan_errors: Vec<scan::ScanError>,
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for error in &self.scan_errors {
            write!(f, "{}", error)?;
        }
        Ok(())
    }
}
