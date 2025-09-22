use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::Spanned;
use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::pattern::MatchArm;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprMatch<'a> {
    expr: Box<Expr<'a>>,
    arms: Vec<MatchArm<'a>>,
    span: SimpleSpan,
}

impl<'a> ExprMatch<'a> {
    pub fn parser<I, M>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone + 'a,
        make_input: M,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
        M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
    {
        just(Token::Match)
            .ignore_then(expr.clone())
            .then(
                MatchArm::parser(expr, make_input, true)
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map_with(|(expr, arms), extra| Self {
                expr: Box::new(expr),
                arms,
                span: extra.span(),
            })
            .labelled("match")
            .as_context()
            .boxed()
    }
}

impl WriteRuby for ExprMatch<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("case ");
        self.expr.write_ruby(scope);
        scope.newline();

        for arm in &self.arms {
            arm.write_ruby(scope);
            scope.newline();
        }

        scope.fragment("end");
    }
}
