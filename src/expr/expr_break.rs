use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBreak<'a> {
    expr: Option<Box<Expr<'a>>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprBreak<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Break)
            .ignore_then(expr.or_not())
            .map_with(|expr, extra| Self {
                expr: expr.map(Box::new),
                span: extra.span(),
            })
            .labelled("break")
            .as_context()
    }
}

impl WriteRuby for ExprBreak<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        if let Some(expr) = &self.expr {
            scope.fragment("break ");
            expr.write_ruby(scope);
        } else {
            scope.fragment("break");
        }
    }
}
