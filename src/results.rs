use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct ParsingError {
    desc: String, // FIXME: Too basic
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.description().fmt(f)
    }
}

impl Error for ParsingError {
    fn description(&self) -> &str {
        &self.desc[..]
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

impl ParsingError {
    pub fn new(desc: String) -> Self {
        ParsingError { desc }
    }
}

pub type ParsingResult<T> = Result<T, ParsingError>;
