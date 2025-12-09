use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprRange {
    start: Option<Box<Expr>>,
    end: Option<Box<Expr>>,
    inclusive: bool,
    pub(crate) span: SimpleSpan,
}

impl ExprRange {
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
        expr.clone()
            .or_not()
            .then_ignore(just(Token::Dot).then(just(Token::Dot)))
            .then(choice((
                just(Token::Assign).to(true),
                just(Token::Lt).to(false),
            )))
            .then(expr.or_not())
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with(|((start, inclusive), end), extra| Self {
                start: start.map(Box::new),
                end: end.map(Box::new),
                inclusive,
                span: extra.span(),
            })
            .labelled("range")
            .boxed()
    }
}

impl WriteRuby for ExprRange {
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
