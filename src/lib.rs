#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::too_many_lines)]

use chumsky::span::SimpleSpan;

mod ast;
mod cli;
mod compiler;
mod expr;
mod import;
mod item;
mod pattern;
mod token;
mod types;

pub use ast::{make_input, parser};
pub use cli::run;
pub use compiler::{compile_module, compile_std, compile_str};
pub use token::{Token, lexer};

pub(crate) type Spanned<T> = (T, SimpleSpan);
