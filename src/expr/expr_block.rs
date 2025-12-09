use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBlock {
    exprs: Vec<Expr>,
    pub(crate) span: SimpleSpan,
}

impl ExprBlock {
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

    pub fn iter(&self) -> std::slice::Iter<'_, Expr> {
        self.exprs.iter()
    }

    pub fn unscoped(&self) -> UnscopedExprBlock {
        UnscopedExprBlock(self.clone())
    }
}

impl WriteRuby for ExprBlock {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.block(|scope| {
            for expr in &self.exprs {
                expr.write_ruby(scope);

                scope.newline();
            }
        });
    }
}

pub struct UnscopedExprBlock(ExprBlock);

impl WriteRuby for UnscopedExprBlock {
    fn write_ruby(&self, scope: &mut Scope) {
        for expr in &self.0.exprs {
            expr.write_ruby(scope);

            scope.newline();
        }
    }
}

impl Infer for ExprBlock {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        let mut inferred = check::Type::unit();

        for expr in &self.exprs {
            inferred = expr.infer(checker, context)?;
        }

        Ok(inferred)
    }
}
