use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprTuple<'a> {
    exprs: Vec<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprTuple<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone + 'a,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        choice((
            // ()
            just(Token::LParen)
                .ignore_then(just(Token::RParen))
                .ignored()
                .map_with(|(), extra| Self {
                    exprs: Vec::with_capacity(0),
                    span: extra.span(),
                }),
            // (x,)
            just(Token::LParen)
                .ignore_then(expr.clone())
                .then_ignore(just(Token::Comma))
                .then_ignore(just(Token::RParen))
                .map_with(|expr, extra| Self {
                    exprs: vec![expr],
                    span: extra.span(),
                }),
            // (x, x, ..)
            expr.separated_by(just(Token::Comma))
                .at_least(2)
                .collect()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .map_with(|exprs, extra| Self {
                    exprs,
                    span: extra.span(),
                }),
        ))
        .labelled("tuple")
        .as_context()
        .boxed()
    }
}

impl WriteRuby for ExprTuple<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("::Std::Tuple::Tuple.new(fields: [");
        for (index, expr) in self.exprs.iter().enumerate() {
            expr.write_ruby(scope);

            if index < self.exprs.len() - 1 {
                scope.fragment(", ");
            }
        }
        scope.fragment("])");
    }
}

impl Infer for ExprTuple<'_> {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        if self.exprs.is_empty() {
            return Ok(check::Type::unit());
        }

        let mut types = Vec::new();

        for expr in &self.exprs {
            types.push(expr.infer(checker, context)?);
        }

        Ok(check::Type::Tuple(types))
    }
}
