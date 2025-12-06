use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::Spanned;
use crate::ast::{RecordField, TupleField, Visibility};
use crate::compiler::{Scope, WriteRuby};
use crate::item::ItemFn;
use crate::token::Token;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct ItemStruct<'a> {
    pub(crate) visibility: Visibility,
    pub(crate) ty: Type<'a>,
    pub(crate) fields: StructFields<'a>,
    pub(crate) fns: Option<Vec<ItemFn<'a>>>,
}

impl<'a> ItemStruct<'a> {
    pub fn parser<I, M>(
        make_input: M,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
        M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
    {
        Visibility::parser()
            .then_ignore(just(Token::Struct))
            .then(Type::parser())
            .then(choice((
                TupleField::parser()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .map(StructFields::Tuple)
                    .delimited_by(just(Token::LParen), just(Token::RParen))
                    .then(
                        ItemFn::parser(make_input.clone(), true)
                            .repeated()
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::LBrace), just(Token::RBrace))
                            .or_not(),
                    )
                    .map(|(fields, fns)| (fields, fns)),
                RecordField::parser()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .map(StructFields::Record)
                    .then(
                        ItemFn::parser(make_input.clone(), true)
                            .repeated()
                            .collect::<Vec<_>>()
                            .or_not(),
                    )
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .map(|(fields, fns)| (fields, fns)),
                ItemFn::parser(make_input, true)
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .or_not()
                    .map(|fns| (StructFields::Unit, fns)),
            )))
            .map(|((visibility, ty), (fields, fns))| Self {
                visibility,
                ty,
                fields,
                fns,
            })
            .labelled("struct")
            .as_context()
    }

    pub fn tuple_fields(&self) -> Option<&[TupleField<'_>]> {
        if let StructFields::Tuple(fields) = &self.fields {
            Some(fields)
        } else {
            None
        }
    }

    pub fn record_fields(&self) -> Option<&[RecordField<'_>]> {
        if let StructFields::Record(fields) = &self.fields {
            Some(fields)
        } else {
            None
        }
    }
}

impl WriteRuby for ItemStruct<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        match &self.fields {
            StructFields::Unit => {
                scope.block_with_end(format!("class {}", self.ty), |scope| {
                    scope.block_with_end("def to_s", |scope| {
                        scope.line(format!("\"{}\"", self.ty));
                    });

                    scope.newline();

                    scope.block_with_end("def ==(other)", |scope| {
                        scope.line("self.class == other.class");
                    });

                    if let Some(fns) = &self.fns {
                        for function in fns {
                            scope.newline();
                            function.write_ruby(scope);
                        }
                    }
                });
            }
            StructFields::Tuple(tys) => {
                scope.block_with_end(format!("class {}", self.ty), |scope| {
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
                        scope.fragment(format!("\"{}", self.ty));

                        if !tys.is_empty() {
                            scope.fragment("(");

                            for index in 0..tys.len() {
                                scope.fragment(format!("#{{__{index}}}"));

                                if index < tys.len() - 1 {
                                    scope.fragment(", ");
                                }
                            }

                            scope.fragment(")");
                        }

                        scope.fragment("\"");
                        scope.newline();
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

                    if let Some(fns) = &self.fns {
                        for function in fns {
                            scope.newline();
                            function.write_ruby(scope);
                        }
                    }
                });
            }
            StructFields::Record(records) => {
                scope.block_with_end(format!("class {}", self.ty), |scope| {
                    if !records.is_empty() {
                        scope.fragment("attr_accessor ");

                        for (index, record) in records.iter().enumerate() {
                            scope.fragment(format!(":{}", record.name));

                            if index < records.len() - 1 {
                                scope.fragment(", ");
                            }
                        }

                        scope.newline();
                        scope.newline();

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
                    }

                    scope.block_with_end("def to_s", |scope| {
                        scope.fragment(format!("\"{}", self.ty));

                        if !records.is_empty() {
                            scope.fragment(" { ");

                            for (index, record) in records.iter().enumerate() {
                                scope.fragment(format!("{}: #{{{}}}", record.name, record.name));

                                if index < records.len() - 1 {
                                    scope.fragment(", ");
                                }
                            }

                            scope.fragment(" }");
                        }

                        scope.fragment("\"");
                        scope.newline();
                    });

                    scope.newline();

                    scope.block_with_end("def ==(other)", |scope| {
                        if records.is_empty() {
                            scope.fragment("self.class == other.class");
                        } else {
                            for (index, record) in records.iter().enumerate() {
                                scope.fragment(format!("{} == other.{}", record.name, record.name));

                                if index < records.len() - 1 {
                                    scope.fragment(" && ");
                                }
                            }
                        }

                        scope.newline();
                    });

                    if !records.is_empty() {
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
                    }

                    if let Some(fns) = &self.fns {
                        for function in fns {
                            scope.newline();
                            function.write_ruby(scope);
                        }
                    }
                });
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructFields<'a> {
    Unit,
    Tuple(Vec<TupleField<'a>>),
    Record(Vec<RecordField<'a>>),
}
