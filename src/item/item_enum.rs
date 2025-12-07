use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::Spanned;
use crate::ast::{Record, Visibility};
use crate::compiler::{Scope, WriteRuby};
use crate::expr::ExprIdent;
use crate::item::ItemFn;
use crate::token::Token;
use crate::types::{Constraint, Type, TypeIdent};

#[derive(Debug, Clone, PartialEq)]
pub struct ItemEnum<'a> {
    pub(crate) visibility: Visibility,
    pub(crate) ty: Type<'a>,
    pub(crate) constraints: Option<Vec<Constraint<'a>>>,
    pub(crate) variants: Vec<Variant<'a>>,
    pub(crate) fns: Vec<ItemFn<'a>>,
}

impl<'a> ItemEnum<'a> {
    pub fn parser<I, M>(
        make_input: M,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
        M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
    {
        Visibility::parser()
            .then_ignore(just(Token::Enum))
            .then(Type::parser())
            .then(Constraint::where_parser().or_not())
            .then(
                Variant::parser()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .then(
                        ItemFn::parser(make_input, true)
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map(|(((visibility, ty), constraints), (mut variants, fns))| {
                for function in &fns {
                    if function.has_self_param() {
                        for variant in &mut variants {
                            variant.add_fn(function.clone());
                        }
                    }
                }

                ItemEnum {
                    visibility,
                    ty,
                    constraints,
                    variants,
                    fns,
                }
            })
            .labelled("enum")
            .as_context()
    }
}

impl WriteRuby for ItemEnum<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.block_with_end(format!("class {}", self.ty), |scope| {
            for (index, variant) in self.variants.iter().enumerate() {
                variant.write_ruby(scope);

                if index != self.variants.len() - 1 {
                    scope.newline();
                }
            }

            for function in &self.fns {
                if !function.has_self_param() {
                    scope.newline();
                    function.write_ruby(scope);
                }
            }
        });
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Variant<'a> {
    Unit(TypeIdent<'a>, Vec<ItemFn<'a>>),
    Tuple(TypeIdent<'a>, Vec<Type<'a>>, Vec<ItemFn<'a>>),
    Record(TypeIdent<'a>, Vec<Record<'a>>, Vec<ItemFn<'a>>),
}

impl<'a> Variant<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        choice((
            TypeIdent::parser()
                .then(
                    Type::parser()
                        .separated_by(just(Token::Comma))
                        .at_least(1)
                        .collect()
                        .delimited_by(just(Token::LParen), just(Token::RParen)),
                )
                .map(|(name, tys)| Variant::Tuple(name, tys, Vec::new())),
            TypeIdent::parser()
                .then(
                    ExprIdent::parser()
                        .then_ignore(just(Token::Colon))
                        .then(Type::parser())
                        .map(|(name, ty)| Record { name, ty })
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .at_least(1)
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LBrace), just(Token::RBrace)),
                )
                .map(|(name, fields)| Variant::Record(name, fields, Vec::new())),
            TypeIdent::parser().map(|ty_ident| Variant::Unit(ty_ident, Vec::new())),
        ))
    }

    pub fn add_fn(&mut self, function: ItemFn<'a>) {
        match self {
            Self::Unit(_, fns) | Self::Tuple(_, _, fns) | Self::Record(_, _, fns) => {
                fns.push(function);
            }
        }
    }
}

impl WriteRuby for Variant<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        match self {
            Self::Unit(ty, fns) => {
                scope.block_with_end(format!("class {ty}"), |scope| {
                    scope.block_with_end("def to_s", |scope| {
                        scope.line(format!("\"{ty}\""));
                    });

                    scope.newline();

                    scope.block_with_end("def ==(other)", |scope| {
                        scope.line("self.class == other.class");
                    });

                    for function in fns {
                        scope.newline();
                        function.write_ruby(scope);
                    }
                });
            }
            Self::Tuple(ty, tys, fns) => {
                scope.block_with_end(format!("class {ty}"), |scope| {
                    if !tys.is_empty() {
                        scope.fragment("attr_accessor ");

                        for index in 0..tys.len() {
                            scope.fragment(format!(":__{index}"));

                            if index < tys.len() - 1 {
                                scope.fragment(", ");
                            }
                        }

                        scope.newline();
                        scope.newline();
                    }

                    let def = format!(
                        "def initialize({})",
                        (0..tys.len())
                            .clone()
                            .map(|index| format!("__{index}"))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );

                    scope.block_with_end(def, |scope| {
                        for index in 0..tys.len() {
                            scope.line(format!("self.__{index} = __{index}"));
                        }
                    });

                    scope.newline();

                    scope.block_with_end("def to_s", |scope| {
                        scope.line(format!("\"{ty}\""));
                    });

                    scope.newline();

                    scope.block_with_end("def ==(other)", |scope| {
                        for index in 0..tys.len() {
                            scope.fragment(format!("__{index} == other.__{index}"));

                            if index < tys.len() - 1 {
                                scope.fragment(" && ");
                            }
                        }

                        scope.newline();
                    });

                    scope.newline();

                    scope.block_with_end("def deconstruct", |scope| {
                        scope.line(format!(
                            "[{}]",
                            (0..tys.len())
                                .clone()
                                .map(|index| format!("__{index}"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ));
                    });

                    for function in fns {
                        scope.newline();
                        function.write_ruby(scope);
                    }
                });
            }
            Self::Record(ty, records, fns) => {
                scope.block_with_end(format!("class {ty}"), |scope| {
                    if !records.is_empty() {
                        scope.fragment("attr_accessor ");

                        for (index, record) in records.iter().enumerate() {
                            scope.fragment(format!(":{}", record.name));

                            if index < records.len() - 1 {
                                scope.fragment(", ");
                            }
                        }

                        scope.newline();
                    }

                    let def = format!(
                        "def initialize({})",
                        records
                            .iter()
                            .map(|record| format!("{}:", record.name))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );

                    scope.block_with_end(def, |scope| {
                        for record in records {
                            scope.line(format!("self.{} = {}", record.name, record.name));
                        }
                    });

                    scope.newline();

                    scope.block_with_end("def to_s", |scope| {
                        scope.line(format!("\"{ty}\""));
                    });

                    scope.newline();

                    scope.block_with_end("def ==(other)", |scope| {
                        for (index, record) in records.iter().enumerate() {
                            scope.fragment(format!("{} == other.{}", record.name, record.name));

                            if index < records.len() - 1 {
                                scope.fragment(" && ");
                            }
                        }

                        scope.newline();
                    });
                    scope.newline();

                    scope.block_with_end("def deconstruct_keys(_keys)", |scope| {
                        scope.line(format!(
                            "{{ {} }}",
                            records
                                .iter()
                                .map(|record| format!("{}:", record.name))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ));
                    });

                    for function in fns {
                        scope.newline();
                        function.write_ruby(scope);
                    }
                });
            }
        }
    }
}
