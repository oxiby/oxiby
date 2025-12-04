use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::Spanned;
use crate::check::{Check, Checker, Context, Infer};
use crate::compiler::WriteRuby;
use crate::error::Error;
use crate::expr::Expr;
use crate::pattern::Pattern;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprLet<'a> {
    pub(crate) pattern: Pattern<'a>,
    pub(crate) body: Box<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprLet<'a> {
    pub fn parser<I, M>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone + 'a,
        make_input: M,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
        M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
    {
        just(Token::Let)
            .ignore_then(Pattern::parser(expr.clone(), make_input, false))
            .then_ignore(just(Token::Assign))
            .then(expr)
            .map_with(|(pattern, body), extra| Self {
                pattern,
                body: Box::new(body),
                span: extra.span(),
            })
            .labelled("let")
            .as_context()
    }
}

impl WriteRuby for ExprLet<'_> {
    fn write_ruby(&self, scope: &mut crate::compiler::Scope) {
        match &self.pattern {
            Pattern::Type(pattern_type) => {
                pattern_type.write_ruby(scope);
                scope.fragment(" = ");
                self.body.write_ruby(scope);
            }
            Pattern::Ident(pattern_ident) => {
                pattern_ident.write_ruby(scope);
                scope.fragment(" = ");
                self.body.write_ruby(scope);
            }
            Pattern::Tuple(pattern_tuple) => {
                pattern_tuple.write_ruby(scope);
                scope.fragment(" = ");
                self.body.write_ruby(scope);
            }
            Pattern::Ctor(pattern_ctor) => {
                self.body.write_ruby(scope);
                scope.fragment(" => ");
                pattern_ctor.write_ruby(scope);
            }
            _ => todo!("pattern not yet implemented for let bindings"),
        }
    }
}

impl Check for ExprLet<'_> {
    fn check(&self, _checker: &Checker, context: &mut Context) -> Result<(), Error> {
        match &self.pattern {
            Pattern::Ident(pattern_ident) => {
                context.push(pattern_ident.ident.to_string(), self.body.infer(context)?);
            }
            Pattern::Literal(_) | Pattern::Wildcard => (),
            pattern => todo!("Type checking is not yet implemented for pattern {pattern:?}"),
        }

        Ok(())
    }
}
