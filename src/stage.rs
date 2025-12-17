use std::collections::HashMap;
use std::path::Path;

use chumsky::prelude::{Input, Parser, Spanned};

use crate::check::Checker;
use crate::error::{Error, ErrorWithSource};
use crate::item::Item;
use crate::module::{Module, ModulePath};
use crate::token::Token;

pub fn lex(source: &str, path: &Path) -> Result<Vec<Spanned<Token>>, Vec<ErrorWithSource>> {
    crate::lexer()
        .parse(source)
        .into_result()
        .map_err(|errors| {
            errors
                .into_iter()
                .map(|error| ErrorWithSource::from_error(path, source, error.into()))
                .collect()
        })
}

pub fn parse(source: &str, path: &Path) -> Result<Vec<Item>, Vec<ErrorWithSource>> {
    let tokens = lex(source, path)?;

    crate::parser()
        .parse(tokens.split_spanned((0..source.len()).into()))
        .into_result()
        .map_err(|errors| {
            errors
                .into_iter()
                .map(|error| ErrorWithSource::from_error(path, source, error.into()))
                .collect()
        })
}

pub fn parse_all(
    source: &str,
    path: &Path,
    parent_path: Option<&Path>,
    modules: &mut HashMap<ModulePath, Vec<Item>>,
    is_entry: bool,
) -> Result<(), Vec<ErrorWithSource>> {
    let mut module_path: ModulePath = parent_path
        .map_or(path, |parent| {
            path.strip_prefix(parent)
                .expect("parent was extracted from the entry file so it should match")
        })
        .try_into()
        .map_err(|error| vec![Error::build(&error).finish_with_source(path, source)])?;

    module_path.set_is_entry_module(is_entry);

    let items = parse(source, path)?;

    for item in &items {
        if let Item::Use(item_use) = item
            && !item_use.is_self_module()
            && !item_use.is_std_module()
        {
            let path = item_use.file_path(parent_path);
            let source = std::fs::read_to_string(&path).map_err(|error| {
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
                        .finish_with_source(&path, source),
                ]
            })?;

            parse_all(&source, &path, parent_path, modules, false)?;
        }
    }

    modules.entry(module_path).or_insert(items);

    Ok(())
}

pub fn check(
    source: &str,
    path: &Path,
    parent_path: Option<&Path>,
    mut modules: HashMap<ModulePath, Vec<Item>>,
) -> Result<HashMap<ModulePath, Module>, Vec<ErrorWithSource>> {
    parse_all(source, path, parent_path, &mut modules, true)?;

    let mut modules: HashMap<String, Module> = modules
        .into_iter()
        .map(|(module_path, items)| {
            let module = Module::new(module_path, items);
            (module.to_string(), module)
        })
        .collect();

    let entry_module_path_string = modules
        .iter()
        .find(|(_name, module)| module.is_entry_module())
        .expect("The first file parsed should be marked as the entry module.")
        .0
        .clone();

    modules.extend(crate::compiler::std_modules());

    let mut checker = Checker::new(modules, entry_module_path_string);

    checker
        .check()
        .map_err(|error| vec![ErrorWithSource::from_error(path, source, error)])?;

    let mut modules = checker.into_modules();

    modules.retain(|key, _value| !key.is_std());

    Ok(modules)
}
