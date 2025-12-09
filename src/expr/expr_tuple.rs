use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprTuple {
    exprs: Vec<Expr>,
    pub(crate) span: SimpleSpan,
}

impl ExprTuple {
    pub fn parser<'a>(
        expr: impl Parser<
            'a,
            MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
            Expr,
            extra::Err<Rich<'a, Token, SimpleSpan>>,
        > + Clone
        + 'a,
    ) -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
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

impl WriteRuby for ExprTuple {
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

impl Infer for ExprTuple {
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
