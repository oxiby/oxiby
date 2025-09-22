use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::{Expr, ExprBlock};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprLoop<'a> {
    block: ExprBlock<'a>,
    span: SimpleSpan,
}

impl<'a> ExprLoop<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Loop)
            .ignore_then(ExprBlock::parser(expr))
            .map_with(|block, extra| Self {
                block,
                span: extra.span(),
            })
            .labelled("loop")
            .as_context()
    }
}

impl WriteRuby for ExprLoop<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment_block_with_end("loop do", |scope| {
            scope.newline();
            self.block.unscoped().write_ruby(scope);
        });
    }
}
