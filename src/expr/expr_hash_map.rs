use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprHashMap<'a> {
    pairs: Vec<(Expr<'a>, Expr<'a>)>,
    span: SimpleSpan,
}

impl<'a> ExprHashMap<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        expr.clone()
            .then_ignore(just(Token::Colon))
            .then(expr)
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map_with(|pairs, extra| Self {
                pairs,
                span: extra.span(),
            })
            .labelled("hash map")
            .as_context()
    }
}

impl WriteRuby for ExprHashMap<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("{ ");
        for (index, (key, value)) in self.pairs.iter().enumerate() {
            key.write_ruby(scope);
            scope.fragment(" => ");
            value.write_ruby(scope);

            if index < self.pairs.len() - 1 {
                scope.fragment(", ");
            }
        }
        scope.fragment(" }");
    }
}
