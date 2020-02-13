#![allow(clippy::module_inception)]
#![allow(unused)]

#[macro_use]
mod macros;

mod ast;
mod lexer;
mod token;
mod error;
mod parser;
mod interpreter;

use lexer::Lexer;
