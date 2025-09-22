use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use super::ItemFn;
use crate::Spanned;
use crate::ast::Visibility;
use crate::token::Token;
use crate::types::{AssociatedType, Constraint, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct ItemTrait<'a> {
    visibility: Visibility,
    name: Type<'a>,
    constraints: Option<Vec<Constraint<'a>>>,
    associated_types: Option<Vec<AssociatedType<'a>>>,
    functions: Vec<ItemFn<'a>>,
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
                ItemFn::parser(make_input, true)
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
