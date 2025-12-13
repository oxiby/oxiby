use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprParenthesized {
    expr: Box<Expr>,
    pub(crate) span: SimpleSpan,
}

impl ExprParenthesized {
    pub fn parser<'a>(
        expr: impl Parser<
            'a,
            MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
            Expr,
            extra::Err<Rich<'a, Token, SimpleSpan>>,
        > + Clone,
    ) -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        expr.delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with(|expr, extra| Self {
                expr: Box::new(expr),
                span: extra.span(),
            })
            .labelled("parenthesized expression")
            .as_context()
    }
}

impl WriteRuby for ExprParenthesized {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("(");
        self.expr.write_ruby(scope);
        scope.fragment(")");
    }
}

impl Infer for ExprParenthesized {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        self.expr.infer(checker)
    }
}
