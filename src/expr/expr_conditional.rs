use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use super::ExprBlock;
use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprConditional<'a> {
    pub(crate) condition: Box<Expr<'a>>,
    pub(crate) then_branch: ExprBlock<'a>,
    pub(crate) else_branch: Option<ConditionalElseBranch<'a>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprConditional<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone + 'a,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        recursive(|cond| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(ExprBlock::parser(expr.clone()))
                .then(
                    just(Token::Else)
                        .ignore_then(choice((
                            ExprBlock::parser(expr.clone()).map(ConditionalElseBranch::Block),
                            cond.map(|c| ConditionalElseBranch::Conditional(Box::new(c))),
                        )))
                        .or_not(),
                )
                .map_with(|((condition, then_branch), else_branch), extra| Self {
                    condition: Box::new(condition),
                    then_branch,
                    else_branch,
                    span: extra.span(),
                })
        })
        .labelled("conditional")
        .as_context()
    }
}

impl WriteRuby for ExprConditional<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.condition(
            |scope| self.condition.write_ruby(scope),
            |scope| {
                for expr in self.then_branch.iter() {
                    expr.write_ruby(scope);
                    scope.newline();
                }
            },
            |cond| match &self.else_branch {
                Some(ConditionalElseBranch::Block(expr_block)) => {
                    cond.last(|scope| expr_block.write_ruby(scope));
                }
                Some(ConditionalElseBranch::Conditional(expr_conditional)) => {
                    cond.another(|scope| expr_conditional.write_ruby(scope));
                }
                None => (),
            },
        );
    }
}

impl Infer for ExprConditional<'_> {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        let condition_type = self.condition.infer(checker, context)?;

        if condition_type != check::Type::boolean() {
            return Err(Error::type_mismatch()
                .with_detail(
                    &format!("Condition was expected to be `Boolean` but was `{condition_type}`",),
                    self.condition.span(),
                )
                .finish());
        }

        let then_type = self.then_branch.infer(checker, context)?;

        let maybe_else = match &self.else_branch {
            Some(ConditionalElseBranch::Block(expr_block)) => {
                Some((expr_block.infer(checker, context)?, expr_block.span))
            }
            Some(ConditionalElseBranch::Conditional(expr_conditional)) => Some((
                expr_conditional.infer(checker, context)?,
                expr_conditional.span,
            )),
            None => None,
        };

        if let Some((else_type, else_span)) = maybe_else {
            if then_type != else_type {
                return Err(Error::type_mismatch()
                    .with_detail(
                        "Each branch of a conditional expression must be the same type.",
                        self.span,
                    )
                    .with_context(
                        &format!("The `if` branch has type `{then_type}`..."),
                        self.then_branch.span,
                    )
                    .with_context(
                        &format!("...but the `else` branch has type `{else_type}`."),
                        else_span,
                    )
                    .finish());
            }
        }

        Ok(then_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConditionalElseBranch<'a> {
    Block(ExprBlock<'a>),
    Conditional(Box<ExprConditional<'a>>),
}
