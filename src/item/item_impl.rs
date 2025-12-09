use chumsky::input::MappedInput;
use chumsky::prelude::*;

use super::ItemFn;
use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;
use crate::types::{AssociatedType, Constraint, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct ItemImpl {
    name: Type,
    target: Type,
    constraints: Option<Vec<Constraint>>,
    associated_types: Option<Vec<AssociatedType>>,
    functions: Vec<ItemFn>,
}

impl ItemImpl {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        just(Token::Impl)
            .ignore_then(Type::parser())
            .then_ignore(just(Token::For))
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
            .then(ItemFn::parser(true).repeated().collect::<Vec<_>>().or_not())
            .then_ignore(just(Token::RBrace))
            .map(
                |((((name, target), constraints), associated_types), functions)| Self {
                    name,
                    target,
                    constraints,
                    associated_types,
                    functions: functions.unwrap_or_else(Vec::new),
                },
            )
            .labelled("impl")
            .as_context()
    }

    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }
}

impl WriteRuby for ItemImpl {
    fn write_ruby(&self, scope: &mut Scope) {
        if self.is_empty() {
            return;
        }

        scope.block_with_end(format!("class {}", self.target), |scope| {
            for (index, func) in self.functions.iter().enumerate() {
                func.write_ruby(scope);

                if index != self.functions.len() - 1 {
                    scope.newline();
                }
            }
        });
    }
}
