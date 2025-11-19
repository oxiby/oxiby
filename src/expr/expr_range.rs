use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprRange<'a> {
    start: Option<Box<Expr<'a>>>,
    end: Option<Box<Expr<'a>>>,
    inclusive: bool,
    span: SimpleSpan,
}

impl<'a> ExprRange<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone + 'a,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        expr.clone()
            .or_not()
            .then_ignore(just(Token::Dot).then(just(Token::Dot)))
            .then(just(Token::Lt).ignored().or_not())
            .then(expr.or_not())
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with(|((start, maybe_exclusive), end), extra| Self {
                start: start.map(Box::new),
                end: end.map(Box::new),
                inclusive: maybe_exclusive.is_none(),
                span: extra.span(),
            })
            .labelled("range")
            .boxed()
    }
}

impl WriteRuby for ExprRange<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("(");

        let unbounded = self.start.is_none() && self.end.is_none();

        if unbounded {
            scope.fragment("nil");
        } else if let Some(start) = &self.start {
            start.write_ruby(scope);
        }

        if self.inclusive {
            scope.fragment("..");
        } else {
            scope.fragment("...");
        }

        if unbounded {
            scope.fragment("nil");
        } else if let Some(end) = &self.end {
            end.write_ruby(scope);
        }

        scope.fragment(")");
    }
}
