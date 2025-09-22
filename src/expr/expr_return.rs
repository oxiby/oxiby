use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprReturn<'a> {
    expr: Option<Box<Expr<'a>>>,
    span: SimpleSpan,
}

impl<'a> ExprReturn<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Return)
            .ignore_then(expr.or_not())
            .map_with(|expr, extra| Self {
                expr: expr.map(Box::new),
                span: extra.span(),
            })
            .labelled("return")
    }
}

impl WriteRuby for ExprReturn<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        if let Some(expr) = &self.expr {
            scope.fragment("return ");
            expr.write_ruby(scope);
        } else {
            scope.fragment("return");
        }
    }
}
