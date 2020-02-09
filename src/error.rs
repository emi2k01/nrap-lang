use std::fmt;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexicalError {
}

#[derive(Error, Debug)]
pub enum ParseError {
    Generic,
}

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("main procedure not present")]
    NoMainFound,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseError as PE;
        match &self {
            PE::Generic => write!(f, "There's an error in your program"),
        }
    }
}
