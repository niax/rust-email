use std::fmt;
use std::error::Error;

#[derive(Debug)]
pub struct ParsingError {
    desc: String,
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.description().fmt(f)
    }
}

impl Error for ParsingError {
    fn description(&self) -> &str {
        self.desc.as_slice()
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl ParsingError {
    pub fn new(desc: String) -> Self {
        ParsingError {
            desc: desc
        }
    }
}

pub type ParsingResult<T> = Result<T, ParsingError>;
