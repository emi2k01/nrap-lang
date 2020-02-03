#![allow(warnings)]

#[macro_use]
mod macros;

mod ast;
mod lexer;
mod token;
mod error;
mod parser;

use lexer::Lexer;
