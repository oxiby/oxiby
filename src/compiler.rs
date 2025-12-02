#![allow(unused_imports, dead_code, clippy::unused_self)]

use std::collections::HashMap;
use std::path::Path;

use chumsky::Parser;
use chumsky::input::{BorrowInput, Input};
use chumsky::span::SimpleSpan;

use crate::Spanned;
use crate::ast::make_input;
use crate::import::{OxibyModulePath, RubyModuleConstants};
use crate::item::{ImportKind, Item};
use crate::token::Token;

static STD_RB: [[&str; 2]; 5] = [
    ["all.rb", include_str!("../lib/all.rb")],
    ["map.rb", include_str!("../lib/map.rb")],
    ["range.rb", include_str!("../lib/range.rb")],
    ["string.rb", include_str!("../lib/string.rb")],
    ["tuple.rb", include_str!("../lib/tuple.rb")],
];

static STD_OB: [[&str; 2]; 6] = [
    ["fs", include_str!("../lib/fs.ob")],
    ["io", include_str!("../lib/io.ob")],
    ["list", include_str!("../lib/list.ob")],
    ["ops", include_str!("../lib/ops.ob")],
    ["option", include_str!("../lib/option.ob")],
    ["result", include_str!("../lib/result.ob")],
];

pub trait WriteRuby {
    fn write_ruby(&self, scope: &mut Scope);
}

impl<T> WriteRuby for Option<T>
where
    T: WriteRuby,
{
    fn write_ruby(&self, scope: &mut Scope) {
        if let Some(expr) = self {
            expr.write_ruby(scope);
        }
    }
}

impl<T> WriteRuby for Box<T>
where
    T: WriteRuby,
{
    fn write_ruby(&self, scope: &mut Scope) {
        self.as_ref().write_ruby(scope);
    }
}

#[derive(Debug)]
pub struct Scope {
    conditional_nesting: usize,
    items: Vec<ScopeItem>,
    oxiby_module_path: OxibyModulePath,
    imports: HashMap<String, (String, ImportKind)>,
}

impl Scope {
    pub fn new(oxiby_module_path: OxibyModulePath) -> Self {
        Self {
            conditional_nesting: 0,
            items: Vec::new(),
            oxiby_module_path,
            imports: HashMap::new(),
        }
    }

    pub fn nested(&self) -> Self {
        Self {
            conditional_nesting: 0,
            items: Vec::new(),
            oxiby_module_path: self.oxiby_module_path.clone(),
            imports: self.imports.clone(),
        }
    }

    pub fn line<S>(&mut self, s: S)
    where
        S: Into<String>,
    {
        self.items.push(ScopeItem::Line(s.into()));
    }

    pub fn fragment<S>(&mut self, s: S)
    where
        S: Into<String>,
    {
        self.items.push(ScopeItem::LineFragment(s.into()));
    }

    pub fn newline(&mut self) {
        self.items.push(ScopeItem::Newline);
    }

    pub fn block<F>(&mut self, f: F)
    where
        F: Fn(&mut Scope),
    {
        let mut scope = self.nested();
        f(&mut scope);
        self.items.push(ScopeItem::Scope(scope));
    }

    pub fn block_with_end<S, F>(&mut self, begin: S, f: F)
    where
        S: Into<String>,
        F: Fn(&mut Scope),
    {
        let mut scope = self.nested();
        self.line(begin.into());
        f(&mut scope);
        self.items.push(ScopeItem::Scope(scope));
        self.line("end".to_owned());
    }

    pub fn fragment_block_with_end<S, F>(&mut self, begin: S, f: F)
    where
        S: Into<String>,
        F: Fn(&mut Scope),
    {
        let mut scope = self.nested();
        self.fragment(begin.into());
        f(&mut scope);
        self.items.push(ScopeItem::Scope(scope));
        self.fragment("end".to_owned());
    }

    pub fn condition<C, T, E, Fa, Fl>(&mut self, cond: C, then_branch: T, else_branch: E)
    where
        C: Fn(&mut Scope),
        T: Fn(&mut Scope),
        E: Fn(&mut CondTracker<Fa, Fl>),
        Fa: Fn(&mut Scope),
        Fl: Fn(&mut Scope),
    {
        if self.conditional_nesting > 0 {
            self.fragment("elsif ");
        } else {
            self.fragment("if ");
        }
        self.conditional_nesting += 1;

        cond(self);
        self.newline();

        let mut scope = self.nested();
        then_branch(&mut scope);
        self.items.push(ScopeItem::Scope(scope));

        let mut cond_tracker = CondTracker {
            kind: CondTrackerKind::None,
        };
        else_branch(&mut cond_tracker);

        match cond_tracker.kind {
            CondTrackerKind::Last(cb) => {
                self.line("else");
                cb(self);
            }
            CondTrackerKind::Another(cb) => {
                cb(self);
            }
            CondTrackerKind::None => (),
        }

        self.conditional_nesting -= 1;

        if self.conditional_nesting == 0 {
            self.fragment("end");
        }
    }

    pub fn add_import<S>(&mut self, import: S, (path, kind): (S, ImportKind))
    where
        S: Into<String>,
    {
        self.imports.insert(import.into(), (path.into(), kind));
    }

    pub fn resolve_ident(&self, alias: &str) -> Option<(String, ImportKind)> {
        self.imports.get(alias).map(|(path, kind)| {
            (
                // Imports will still constantize `self` even though it's a special module.
                // It's safe to strip off this prefix because we don't allow modules named "self",
                // so if it shows up here, we know it's the special `self` module.
                path.strip_prefix("::Self::")
                    .unwrap_or_else(|| path)
                    .to_string(),
                *kind,
            )
        })
    }

    pub fn ruby_module_constants(&self) -> RubyModuleConstants {
        self.oxiby_module_path.clone().into()
    }

    fn into_output(self, indent: usize) -> String {
        let mut out = String::new();
        let mut scope_items = self.items.into_iter().peekable();

        while let Some(scope_item) = scope_items.next() {
            match scope_item {
                ScopeItem::Line(line) => {
                    out.push_str(&("  ".repeat(indent).clone() + &line));
                    out.push('\n');
                }
                ScopeItem::LineFragment(fragment) => {
                    out.push_str(&("  ".repeat(indent).clone() + &fragment));

                    while let Some(ScopeItem::LineFragment(..)) = scope_items.peek() {
                        if let Some(ScopeItem::LineFragment(fragment)) = scope_items.next() {
                            out.push_str(&fragment);
                        }
                    }
                }
                ScopeItem::Newline => out.push('\n'),
                ScopeItem::Scope(scope) => {
                    out.push_str(&scope.into_output(indent + 1));
                }
            }
        }

        out
    }
}

#[derive(Debug)]
enum ScopeItem {
    Line(String),
    LineFragment(String),
    Newline,
    Scope(Scope),
}

pub struct CondTracker<Fa, Fl>
where
    Fa: Fn(&mut Scope),
    Fl: Fn(&mut Scope),
{
    kind: CondTrackerKind<Fa, Fl>,
}

impl<Fa, Fl> CondTracker<Fa, Fl>
where
    Fa: Fn(&mut Scope),
    Fl: Fn(&mut Scope),
{
    pub fn another(&mut self, cb: Fa) {
        self.kind = CondTrackerKind::Another(cb);
    }

    pub fn last(&mut self, cb: Fl) {
        self.kind = CondTrackerKind::Last(cb);
    }
}

enum CondTrackerKind<Fa, Fl>
where
    Fa: Fn(&mut Scope),
    Fl: Fn(&mut Scope),
{
    Another(Fa),
    Last(Fl),
    None,
}

pub fn compile_items(items: &[Item<'_>], scope: &mut Scope) {
    for (index, item) in items.iter().enumerate() {
        item.write_ruby(scope);

        match item {
            Item::Fn(..) | Item::Struct(..) | Item::Enum(..) => {
                if index < items.len() - 1 {
                    scope.newline();
                }
            }
            Item::Impl(item_impl) => {
                if index < items.len() - 1 && !item_impl.is_empty() {
                    scope.newline();
                }
            }
            _ => (),
        }
    }
}

fn module_body<F>(scope: &mut Scope, constants: &[String], f: F)
where
    F: Fn(&mut Scope) + Clone,
{
    if constants.is_empty() {
        f(scope);
    } else {
        scope.block_with_end(format!("module {}", constants[0]), move |scope| {
            module_body(scope, &constants[1..], f.clone());
        });
    }
}

#[must_use]
pub fn compile_module(
    oxiby_module: OxibyModulePath,
    items: &[Item<'_>],
    is_std: bool,
    is_entry: bool,
) -> String {
    let mut scope = Scope::new(oxiby_module);

    scope.add_import("print_line", ("::Std::Io.print_line", ImportKind::Function));
    scope.add_import("print", ("::Std::Io.print", ImportKind::Function));
    scope.add_import("List", ("::Std::List::List", ImportKind::Type));
    scope.add_import("Option", ("::Std::Option::Option", ImportKind::Type));
    scope.add_import("Some", ("::Std::Option::Option::Some", ImportKind::Variant));
    scope.add_import("None", ("::Std::Option::Option::None", ImportKind::Variant));
    scope.add_import("Range", ("::Std::Range::Range", ImportKind::Type));
    scope.add_import("Result", ("::Std::Result::Result", ImportKind::Type));
    scope.add_import("Ok", ("::Std::Result::Result::Ok", ImportKind::Variant));
    scope.add_import("Err", ("::Std::Result::Result::Err", ImportKind::Variant));

    scope.line("# frozen_string_literal: true");
    scope.newline();

    if !is_std && is_entry {
        scope.line("require_relative \"std/all\"");
        scope.newline();
    }

    let constants = scope.ruby_module_constants();
    let constants_slice = constants.as_slice();

    module_body(&mut scope, constants_slice, |scope| {
        compile_items(items, scope);
    });

    scope.into_output(0)
}

pub fn compile_std(build_dir: &Path) -> Result<(), Vec<String>> {
    let oxiby_dir = build_dir.join("std");

    if oxiby_dir.exists() {
        return Ok(());
    }

    std::fs::create_dir(&oxiby_dir).map_err(|_| {
        vec!["Could not create directory to write Oxiby standard library.".to_string()]
    })?;

    for [name, source] in STD_RB {
        std::fs::write(oxiby_dir.join(name), source).map_err(|error| vec![error.to_string()])?;
    }

    for [name, source] in STD_OB {
        let output = compile_str(&["std", name], source, true)?;
        let path = oxiby_dir.join(format!("{name}.rb"));
        std::fs::write(path, output).map_err(|err| vec![err.to_string()])?;
    }

    Ok(())
}

pub fn compile_str(
    module_path: &[&str],
    source: &str,
    is_std: bool,
) -> Result<String, Vec<String>> {
    let oxiby_module_path: OxibyModulePath = module_path.into();

    let tokens = crate::token::lexer()
        .parse(source)
        .into_result()
        .map_err(|errors| {
            errors
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
        })?;

    let items = crate::ast::parser(make_input)
        .parse(make_input((0..source.len()).into(), &tokens))
        .into_result()
        .map_err(|errors| {
            errors
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
        })?;

    Ok(compile_module(oxiby_module_path, &items, is_std, true))
}
