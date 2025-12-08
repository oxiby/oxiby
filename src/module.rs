use std::collections::HashMap;
use std::path::{Path, PathBuf};

use chumsky::Parser;

use crate::check::Checker;
use crate::error::{Error, ErrorWithSource};
use crate::import::OxibyModulePath;
use crate::item::Item;

pub fn module_tokens(source: &str) -> Result<String, Vec<Error>> {
    let (tokens, lex_errors) = crate::lexer().parse(source).into_output_errors();

    if let Some(tokens) = tokens {
        Ok(format!("{tokens:#?}"))
    } else {
        Err(lex_errors.into_iter().map(From::from).collect())
    }
}

pub fn module_ast(source: &str) -> Result<String, Vec<Error>> {
    let (tokens, lex_errors) = crate::lexer().parse(source).into_output_errors();

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

    Err(lex_errors
        .into_iter()
        .map(From::from)
        .chain(parse_errors.into_iter().map(From::from))
        .collect())
}

pub fn module_program(
    input_file: &Path,
    input_file_parent: Option<&Path>,
    source: &str,
    mut compiled_modules: HashMap<PathBuf, String>,
    is_entry: bool,
) -> Result<HashMap<PathBuf, String>, Vec<ErrorWithSource>> {
    let oxiby_module_path: OxibyModulePath = input_file_parent
        .map_or(input_file, |parent| {
            input_file
                .strip_prefix(parent)
                .expect("parent was extracted from the entry file so it should match")
        })
        .try_into()
        .map_err(|error| vec![Error::build(&error).finish_with_source(input_file, source)])?;

    let (tokens, lex_errors) = crate::lexer().parse(source).into_output_errors();

    let parse_errors = if let Some(tokens) = &tokens {
        let (items, parse_errors) = crate::parser(crate::make_input)
            .parse(crate::make_input((0..source.len()).into(), tokens))
            .into_output_errors();

        if let Some(items) = items {
            for item in &items {
                if let Item::Use(item_use) = item
                    && !item_use.is_self_module()
                    && !item_use.is_std_module()
                {
                    let input_file = item_use.file_path(input_file_parent);
                    let source = std::fs::read_to_string(&input_file).map_err(|error| {
                        vec![
                            Error::build("Module read failure")
                                .with_detail(
                                    &format!(
                                        "Could not read source file for module `{}`.",
                                        item_use.module_path()
                                    ),
                                    item_use.span,
                                )
                                .with_note(&format!("File system error: {error}"))
                                .finish_with_source(&input_file, source),
                        ]
                    })?;

                    compiled_modules = module_program(
                        &input_file,
                        input_file_parent,
                        &source,
                        compiled_modules,
                        false,
                    )?;
                }
            }

            if !compiled_modules.contains_key(input_file) {
                compiled_modules.insert(
                    input_file.to_path_buf(),
                    crate::compile_module(oxiby_module_path, &items, false, is_entry),
                );
            }

            return Ok(compiled_modules);
        }

        parse_errors
    } else {
        Vec::new()
    };

    Err(lex_errors
        .into_iter()
        .map(|lex_error| ErrorWithSource::from_error(input_file, source, lex_error.into()))
        .chain(
            parse_errors.into_iter().map(|parse_error| {
                ErrorWithSource::from_error(input_file, source, parse_error.into())
            }),
        )
        .collect())
}

pub fn module_check(source: &str, debug: bool) -> Result<(), Vec<Error>> {
    let (tokens, lex_errors) = crate::lexer().parse(source).into_output_errors();

    let parse_errors = if let Some(tokens) = &tokens {
        let (items, parse_errors) = crate::parser(crate::make_input)
            .parse(crate::make_input((0..source.len()).into(), tokens))
            .into_output_errors();

        if let Some(items) = items {
            let mut checker = Checker::new();

            let result = checker.check(items).map_err(|error| vec![error]);

            if debug {
                dbg!(&checker);
            }

            return result;
        }

        parse_errors
    } else {
        Vec::new()
    };

    if lex_errors.is_empty() && parse_errors.is_empty() {
        return Ok(());
    }

    Err(lex_errors
        .into_iter()
        .map(From::from)
        .chain(parse_errors.into_iter().map(From::from))
        .collect())
}
