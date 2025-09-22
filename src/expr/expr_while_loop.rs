use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::{Expr, ExprBlock};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprWhileLoop<'a> {
    condition: Box<Expr<'a>>,
    block: ExprBlock<'a>,
    span: SimpleSpan,
}

impl<'a> ExprWhileLoop<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::While)
            .ignore_then(expr.clone())
            .then(ExprBlock::parser(expr))
            .map_with(|(condition, block), extra| Self {
                condition: Box::new(condition),
                block,
                span: extra.span(),
            })
            .labelled("while loop")
            .as_context()
    }
}

impl WriteRuby for ExprWhileLoop<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("while ");
        self.condition.write_ruby(scope);
        scope.fragment_block_with_end("", |scope| {
            scope.newline();
            self.block.unscoped().write_ruby(scope);
        });
    }
}
