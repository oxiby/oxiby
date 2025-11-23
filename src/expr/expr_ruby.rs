use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprRuby<'a> {
    ruby: &'a str,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprRuby<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select_ref! {
            Token::Ruby(ruby) = extra => Self { ruby, span: extra.span() }
        }
        .labelled("ruby block")
        .boxed()
    }
}

impl WriteRuby for ExprRuby<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment(self.ruby);
    }
}
