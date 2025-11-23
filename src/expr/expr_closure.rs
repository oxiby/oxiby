use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::{Expr, ExprIdent};
use crate::token::Token;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprClosure<'a> {
    params: Vec<FnArg<'a>>,
    return_ty: Option<Type<'a>>,
    body: Vec<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprClosure<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        let param_list = FnArg::parser()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .labelled("closure parameter list");

        just(Token::Fn)
            .ignore_then(param_list)
            .then(just(Token::Arrow).ignore_then(Type::parser()).or_not())
            .then(choice((
                expr.clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
                expr.map(|exp| vec![exp]),
            )))
            .map_with(|((params, return_ty), body), extra| Self {
                params,
                return_ty,
                body,
                span: extra.span(),
            })
            .labelled("list")
            .as_context()
    }
}

impl WriteRuby for ExprClosure<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("-> ");

        if !self.params.is_empty() {
            scope.fragment("(");

            for (index, param) in self.params.iter().enumerate() {
                param.binding.write_ruby(scope);

                if index < self.params.len() - 1 {
                    scope.fragment(", ");
                }
            }

            scope.fragment(")");
        }

        scope.fragment(" { ");

        for (index, expr) in self.body.iter().enumerate() {
            expr.write_ruby(scope);

            if index < self.body.len() - 1 {
                scope.fragment("; ");
            }
        }

        scope.fragment(" }");
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnArg<'a> {
    binding: ExprIdent<'a>,
    ty: Option<Type<'a>>,
}

impl<'a> FnArg<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        ExprIdent::parser()
            .then(just(Token::Colon).ignore_then(Type::parser()).or_not())
            .map(|(binding, ty)| FnArg { binding, ty })
            .labelled("closure argument")
            .as_context()
    }
}
