use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprReturn {
    expr: Option<Box<Expr>>,
    pub(crate) span: SimpleSpan,
}

impl ExprReturn {
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
        just(Token::Return)
            .ignore_then(expr.or_not())
            .map_with(|expr, extra| Self {
                expr: expr.map(Box::new),
                span: extra.span(),
            })
            .labelled("return")
    }
}

impl WriteRuby for ExprReturn {
    fn write_ruby(&self, scope: &mut Scope) {
        if let Some(expr) = &self.expr {
            scope.fragment("return ");
            expr.write_ruby(scope);
        } else {
            scope.fragment("return");
        }
    }
}

impl Infer for ExprReturn {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let ty = match &self.expr {
            Some(expr) => (*expr).infer(checker)?,
            None => check::Type::unit(),
        };

        Ok(ty)
    }
}
