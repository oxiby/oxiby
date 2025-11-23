use std::fmt::Display;

use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;
use crate::types::TypeIdent;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct ExprIdent<'a> {
    pub(crate) ident: &'a str,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprIdent<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select! {
            Token::TermIdent(ident) = extra => Self { ident, span: extra.span() },
            Token::SelfTerm = extra => Self { ident: "self", span: extra.span() },
        }
        .labelled("identifier")
    }

    pub fn as_str(&self) -> &'a str {
        self.ident
    }
}

impl Display for ExprIdent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl WriteRuby for ExprIdent<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        match scope.resolve_ident(self.ident) {
            Some((path, _kind)) => scope.fragment(path),
            None => scope.fragment(self.ident),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprTypeIdent<'a> {
    ident: TypeIdent<'a>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprTypeIdent<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        TypeIdent::parser().map_with(|ident, extra| Self {
            ident,
            span: extra.span(),
        })
    }
}

impl WriteRuby for ExprTypeIdent<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        self.ident.write_ruby(scope);
    }
}
