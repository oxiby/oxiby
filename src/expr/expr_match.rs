use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::pattern::MatchArm;
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
        // TODO: Ensure patterns are appropriate for the scrutinee.
        self.expr.infer(checker)?;

        let (inferred, span) = match self.arms.first() {
            Some(arm) => (arm.body.infer(checker)?, arm.span),
            None => {
                return Err(Error::build("Invalid match")
                    .with_detail("Match expressions must have at least one arm.", self.span)
                    .finish());
            }
        };

        for arm in self.arms.iter().skip(1) {
            let next_inferred = arm.body.infer(checker)?;

            if inferred != next_inferred {
                return Err(Error::type_mismatch()
                    .with_detail("All match arms must be of the same type.", self.span)
                    .with_context(
                        &format!("The first match arm is of type `{inferred}`..."),
                        span,
                    )
                    .with_context(
                        &format!("...but this match arm is of type `{next_inferred}`"),
                        arm.span,
                    )
                    .finish());
            }
        }

        Ok(inferred)
    }
}
