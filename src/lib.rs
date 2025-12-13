#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::result_large_err)]
#![allow(clippy::too_many_lines)]

mod ast;
mod check;
mod cli;
mod compiler;
mod error;
mod expr;
mod item;
mod module;
mod pattern;
mod stage;
mod token;
mod types;

pub use ast::parser;
pub use cli::{CliError, report_errors, run};
pub use compiler::{compile_module, compile_std, compile_str};
pub use token::{Token, lexer};
