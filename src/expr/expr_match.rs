use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::pattern::{MatchArm, match_bindings};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprMatch {
    expr: Box<Expr>,
    arms: Vec<MatchArm>,
    pub(crate) span: SimpleSpan,
}

impl ExprMatch {
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
        just(Token::Match)
            .ignore_then(expr.clone())
            .then(
                MatchArm::parser(expr, true)
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

impl WriteRuby for ExprMatch {
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

impl Infer for ExprMatch {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let expr_ty = self.expr.infer(checker)?;

        if self.arms.is_empty() {
            return Err(Error::build("Invalid match")
                .with_detail("Match expressions must have at least one arm.", self.span)
                .finish());
        }

        let mut body_ty_and_span = None;

        for arm in &self.arms {
            let bindings = match_bindings(
                checker,
                &expr_ty,
                &arm.pattern,
                self.span,
                self.expr.span(),
                arm.pattern.span(),
            )?;

            checker.push_scope();

            for (name, ty) in bindings {
                checker.push_term_var(name, ty);
            }

            let next_body_ty = arm.body.infer(checker)?;

            if let Some((body_ty, span)) = body_ty_and_span
                && body_ty != next_body_ty
            {
                return Err(Error::type_mismatch()
                    .with_detail("All match arms must be of the same type.", self.span)
                    .with_context(
                        &format!("The first match arm is of type `{body_ty}`..."),
                        span,
                    )
                    .with_context(
                        &format!("...but this match arm is of type `{next_body_ty}`"),
                        arm.span,
                    )
                    .finish());
            }

            body_ty_and_span = Some((next_body_ty, arm.span));
        }

        Ok(body_ty_and_span
            .expect("should have checked there was at least one match arm")
            .0)
    }
}
