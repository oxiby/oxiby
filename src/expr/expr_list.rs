use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprList<'a> {
    exprs: Vec<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprList<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        expr.clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map_with(|exprs, extra| Self {
                exprs,
                span: extra.span(),
            })
            .labelled("list")
            .as_context()
    }
}

impl WriteRuby for ExprList<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("[");
        for (index, expr) in self.exprs.iter().enumerate() {
            expr.write_ruby(scope);

            if index < self.exprs.len() - 1 {
                scope.fragment(", ");
            }
        }
        scope.fragment("]");
    }
}
