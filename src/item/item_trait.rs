use chumsky::input::MappedInput;
use chumsky::prelude::*;

use crate::ast::Visibility;
use crate::expr::Expr;
use crate::item::item_fn::Signature;
use crate::token::Token;
use crate::types::{AssociatedType, Constraint, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct ItemTrait {
    pub(crate) visibility: Visibility,
    pub(crate) name: Type,
    pub(crate) constraints: Option<Vec<Constraint>>,
    pub(crate) associated_types: Option<Vec<AssociatedType>>,
    pub(crate) functions: Vec<TraitFn>,
}

impl ItemTrait {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
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
                TraitFn::parser(true)
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
pub struct TraitFn {
    pub(crate) signature: Signature,
    pub(crate) body: Option<Vec<Expr>>,
    pub(crate) span: SimpleSpan,
}

impl TraitFn {
    pub fn parser<'a>(
        associated_fn: bool,
    ) -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        Signature::parser(associated_fn)
            .then(
                Expr::parser()
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
