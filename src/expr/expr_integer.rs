use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprInteger {
    value: u64,
    span: SimpleSpan,
}

impl ExprInteger {
    pub fn parser<'a, I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select! {
            Token::Integer(i) = extra => Self { value: i, span: extra.span() },
        }
        .labelled("integer")
    }
}

impl WriteRuby for ExprInteger {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment(self.value.to_string());
    }
}
