use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprParenthesized<'a> {
    expr: Box<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprParenthesized<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        expr.delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with(|expr, extra| Self {
                expr: Box::new(expr),
                span: extra.span(),
            })
            .labelled("parenthesized expression")
            .as_context()
    }
}

impl WriteRuby for ExprParenthesized<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("(");
        self.expr.write_ruby(scope);
        scope.fragment(")");
    }
}

impl Infer for ExprParenthesized<'_> {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        self.expr.infer(checker, context)
    }
}
