#![allow(warnings)]

#[macro_use]
mod macros;

mod ast;
mod lexer;
mod token;
mod error;
mod parser;
mod compiler;
mod bytecode;

use lexer::Lexer;
