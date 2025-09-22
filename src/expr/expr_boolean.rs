use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBoolean {
    value: bool,
    span: SimpleSpan,
}

impl ExprBoolean {
    pub fn parser<'a, I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select! {
            Token::Boolean(b) = extra => ExprBoolean { value: b, span: extra.span() },
        }
        .labelled("boolean")
    }
}

impl WriteRuby for ExprBoolean {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment(self.value.to_string());
    }
}
