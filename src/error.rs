use std::fmt;

use crate::scan;

#[derive(Debug)]
pub enum Error {
    ScanError(scan::ScanError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ScanError(scan_error) => write!(f, "{}", scan_error),
        }
    }
}

impl std::error::Error for Error {}

impl From<scan::ScanError> for Error {
    fn from(error: scan::ScanError) -> Self {
        Error::ScanError(error)
    }
}
