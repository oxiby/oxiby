#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::result_large_err)]
#![allow(clippy::too_many_lines)]

use chumsky::span::SimpleSpan;

mod ast;
mod check;
mod cli;
mod compiler;
mod error;
mod expr;
mod import;
mod item;
mod module;
mod pattern;
mod token;
mod types;

pub use ast::{make_input, parser};
pub use cli::{CliError, report_errors, run};
pub use compiler::{compile_module, compile_std, compile_str};
pub use token::{Token, lexer};

pub(crate) type Spanned<T> = (T, SimpleSpan);
