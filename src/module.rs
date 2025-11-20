use std::path::{Path, PathBuf};

use chumsky::Parser;
use chumsky::error::Rich;

use crate::import::OxibyModulePath;

pub enum Error {
    Program(String, Vec<Rich<'static, String>>),
    Other(String),
}

pub fn module_tokens(file_path: &Path) -> Result<String, Error> {
    let source = read_file(file_path)?;

    let (tokens, lex_errors) = crate::lexer().parse(&source).into_output_errors();

    if let Some(tokens) = tokens {
        Ok(format!("{tokens:#?}"))
    } else {
        Err(Error::Program(
            source.clone(),
            lex_errors
                .into_iter()
                .map(|error| error.map_token(|token| token.to_string()).into_owned())
                .collect(),
        ))
    }
}

pub fn module_ast(file_path: &Path) -> Result<String, Error> {
    let source = read_file(file_path)?;

    let (tokens, lex_errors) = crate::lexer().parse(&source).into_output_errors();

    let parse_errors = if let Some(tokens) = &tokens {
        let (items, parse_errors) = crate::parser(crate::make_input)
            .parse(crate::make_input((0..source.len()).into(), tokens))
            .into_output_errors();

        if let Some(items) = items {
            return Ok(format!("{items:#?}"));
        }

        parse_errors
    } else {
        Vec::new()
    };

    Err(Error::Program(
        source.clone(),
        lex_errors
            .into_iter()
            .map(|error| error.map_token(|token| token.to_string()).into_owned())
            .chain(
                parse_errors
                    .into_iter()
                    .map(|error| error.map_token(|token| token.to_string()).into_owned()),
            )
            .collect(),
    ))
}

pub fn module_program(
    file_path: &Path,
    mut compiled_modules: Vec<(PathBuf, String)>,
) -> Result<Vec<(PathBuf, String)>, Error> {
    let oxiby_module_path: OxibyModulePath = file_path.try_into().map_err(Error::Other)?;
    let source = read_file(file_path)?;

    let (tokens, lex_errors) = crate::lexer().parse(&source).into_output_errors();

    let parse_errors = if let Some(tokens) = &tokens {
        let (items, parse_errors) = crate::parser(crate::make_input)
            .parse(crate::make_input((0..source.len()).into(), tokens))
            .into_output_errors();

        if let Some(items) = items {
            compiled_modules.push((
                file_path.to_path_buf(),
                crate::compile_module(oxiby_module_path, &items, false),
            ));

            return Ok(compiled_modules);
        }

        parse_errors
    } else {
        Vec::new()
    };

    Err(Error::Program(
        source.clone(),
        lex_errors
            .into_iter()
            .map(|error| error.map_token(|token| token.to_string()).into_owned())
            .chain(
                parse_errors
                    .into_iter()
                    .map(|error| error.map_token(|token| token.to_string()).into_owned()),
            )
            .collect(),
    ))
}

fn read_file(file_path: &Path) -> Result<String, Error> {
    std::fs::read_to_string(file_path)
        .map_err(|_| Error::Other(format!("Failed to read path: {}", file_path.display())))
}
