use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use super::ExprBlock;
use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprConditional<'a> {
    condition: Box<Expr<'a>>,
    then_branch: ExprBlock<'a>,
    else_branch: Option<ConditionalElseBranch<'a>>,
    span: SimpleSpan,
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

#[derive(Debug, Clone, PartialEq)]
pub enum ConditionalElseBranch<'a> {
    Block(ExprBlock<'a>),
    Conditional(Box<ExprConditional<'a>>),
}
