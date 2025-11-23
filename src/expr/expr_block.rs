use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBlock<'a> {
    exprs: Vec<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprBlock<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        expr.repeated()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|exprs, extra| Self {
                exprs,
                span: extra.span(),
            })
            .labelled("block")
            .as_context()
    }

    pub fn iter(&'a self) -> std::slice::Iter<'a, Expr<'a>> {
        self.exprs.iter()
    }

    pub fn unscoped(&self) -> UnscopedExprBlock<'_> {
        UnscopedExprBlock(self)
    }
}

impl WriteRuby for ExprBlock<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.block(|scope| {
            for expr in &self.exprs {
                expr.write_ruby(scope);

                scope.newline();
            }
        });
    }
}

pub struct UnscopedExprBlock<'a>(&'a ExprBlock<'a>);

impl WriteRuby for UnscopedExprBlock<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        for expr in &self.0.exprs {
            expr.write_ruby(scope);

            scope.newline();
        }
    }
}
