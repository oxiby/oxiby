use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::Spanned;
use crate::ast::Visibility;
use crate::expr::Expr;
use crate::item::item_fn::Signature;
use crate::token::Token;
use crate::types::{AssociatedType, Constraint, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct ItemTrait<'a> {
    visibility: Visibility,
    name: Type<'a>,
    constraints: Option<Vec<Constraint<'a>>>,
    associated_types: Option<Vec<AssociatedType<'a>>>,
    functions: Vec<TraitFn<'a>>,
}

impl<'a> ItemTrait<'a> {
    pub fn parser<I, M>(
        make_input: M,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
        M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
    {
        Visibility::parser()
            .then_ignore(just(Token::Trait))
            .then(Type::parser())
            .then(Constraint::where_parser().or_not())
            .then_ignore(just(Token::LBrace))
            .then(
                just(Token::Type)
                    .ignore_then(AssociatedType::parser())
                    .repeated()
                    .collect::<Vec<_>>()
                    .or_not(),
            )
            .then(
                TraitFn::parser(make_input, true)
                    .repeated()
                    .collect::<Vec<_>>()
                    .or_not(),
            )
            .then_ignore(just(Token::RBrace))
            .map(
                |((((visibility, name), constraints), associated_types), functions)| Self {
                    visibility,
                    name,
                    constraints,
                    associated_types,
                    functions: functions.unwrap_or_else(Vec::new),
                },
            )
            .labelled("trait")
            .as_context()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitFn<'a> {
    pub(crate) signature: Signature<'a>,
    pub(crate) body: Option<Vec<Expr<'a>>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> TraitFn<'a> {
    pub fn parser<I, M>(
        make_input: M,
        associated_fn: bool,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
        M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
    {
        Signature::parser(associated_fn)
            .then(
                Expr::parser(make_input)
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .or_not(),
            )
            .map_with(|(signature, body), extra| Self {
                signature,
                body,
                span: extra.span(),
            })
            .labelled("trait function")
            .as_context()
    }
}
