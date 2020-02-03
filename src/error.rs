use std::fmt;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexicalError {
}

#[derive(Error, Debug)]
pub enum ParserError {
    Generic,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParserError as PE;
        match &self {
            PE::Generic => write!(f, "There's an error in your program"),
        }
    }
}
