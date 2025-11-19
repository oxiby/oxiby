use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::ExprIdent;
use crate::import::{OxibyModulePath, RubyModuleConstants};
use crate::token::Token;
use crate::types::TypeIdent;

#[derive(Debug, Clone, PartialEq)]
pub struct ItemUse<'a> {
    path: Vec<ExprIdent<'a>>,
    is_self: bool,
    is_std: bool,
    idents: Vec<ImportedIdent<'a>>,
    span: SimpleSpan,
}

impl<'a> ItemUse<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Use).ignore_then(
            ExprIdent::parser()
                .separated_by(just(Token::Dot))
                .at_least(1)
                .collect::<Vec<_>>()
                .then(
                    choice((
                        ExprIdent::parser()
                            .then(just(Token::Arrow).ignore_then(ExprIdent::parser()).or_not())
                            .map(|(ident, rename)| ImportedIdent::ExprIdent { ident, rename }),
                        TypeIdent::parser()
                            .then(just(Token::Dot).ignore_then(TypeIdent::parser()).or_not())
                            .then(just(Token::Arrow).ignore_then(TypeIdent::parser()).or_not())
                            .map(|((ident, variant), rename)| ImportedIdent::TypeIdent {
                                ident,
                                variant,
                                rename,
                            }),
                    ))
                    .separated_by(just(Token::Comma))
                    .at_least(1)
                    .collect::<Vec<_>>(),
                )
                .try_map(|(path, idents), span| {
                    let first_path_segment = path[0].as_str();
                    let is_self = first_path_segment == "self";
                    let is_std = first_path_segment == "std";

                    // TODO: Like the `try_map` in `ItemFn`, this triggers under the right condition
                    // but the custom error message doesn't show up.
                    if is_self && path.len() > 1 {
                        return Err(Rich::custom(span, "Module paths cannot begin with `self`"));
                    }

                    Ok(ItemUse {
                        path,
                        is_self,
                        is_std,
                        idents,
                        span,
                    })
                })
                .labelled("use statement")
                .as_context(),
        )
    }

    pub fn add_imports(&self, scope: &mut Scope) {
        for ident in &self.idents {
            let alias = match ident {
                ImportedIdent::ExprIdent { ident, rename } => match rename {
                    Some(ident) => ident.as_str(),
                    None => ident.as_str(),
                },
                ImportedIdent::TypeIdent {
                    ident,
                    variant,
                    rename,
                } => match rename {
                    Some(ident) => ident.as_str(),
                    None => match variant {
                        Some(ident) => ident.as_str(),
                        None => ident.as_str(),
                    },
                },
            };

            let (real, kind) = match ident {
                ImportedIdent::ExprIdent { ident, .. } => (ident.to_string(), ImportKind::Function),
                ImportedIdent::TypeIdent { ident, variant, .. } => match variant {
                    Some(variant) => (format!("{ident}::{variant}"), ImportKind::Variant),
                    None => (ident.to_string(), ImportKind::Type),
                },
            };

            let separator = match real.chars().nth(0) {
                Some(chr) if chr.is_uppercase() => "::",
                Some(_) => ".",
                _ => unreachable!("idents always have at least one character"),
            };

            let oxiby_module_path: OxibyModulePath = self.path.clone().into();
            let ruby_module_constants: RubyModuleConstants = oxiby_module_path.into();

            scope.add_import(
                alias.to_owned(),
                (format!("::{ruby_module_constants}{separator}{real}"), kind),
            );
        }
    }
}

impl WriteRuby for ItemUse<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        if self.is_self || self.is_std {
            return;
        }

        let require = self
            .path
            .iter()
            .map(ExprIdent::as_str)
            .collect::<Vec<_>>()
            .join("/");

        scope.line(format!("require_relative \"{require}\""));
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportedIdent<'a> {
    ExprIdent {
        ident: ExprIdent<'a>,
        rename: Option<ExprIdent<'a>>,
    },
    TypeIdent {
        ident: TypeIdent<'a>,
        variant: Option<TypeIdent<'a>>,
        rename: Option<TypeIdent<'a>>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ImportKind {
    Function,
    Type,
    Variant,
}
