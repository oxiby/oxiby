use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBreak {
    expr: Option<Box<Expr>>,
    pub(crate) span: SimpleSpan,
}

impl ExprBreak {
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
        just(Token::Break)
            .ignore_then(expr.or_not())
            .map_with(|expr, extra| Self {
                expr: expr.map(Box::new),
                span: extra.span(),
            })
            .labelled("break")
            .as_context()
    }
}

impl WriteRuby for ExprBreak {
    fn write_ruby(&self, scope: &mut Scope) {
        if let Some(expr) = &self.expr {
            scope.fragment("break ");
            expr.write_ruby(scope);
        } else {
            scope.fragment("break");
        }
    }
}

impl Infer for ExprBreak {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        let ty = match &self.expr {
            Some(expr) => (*expr).infer(checker, context)?,
            None => check::Type::unit(),
        };

        Ok(ty)
    }
}
