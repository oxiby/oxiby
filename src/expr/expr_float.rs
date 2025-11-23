use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprFloat {
    pub(crate) value: f64,
    pub(crate) span: SimpleSpan,
}

impl ExprFloat {
    pub fn parser<'a, I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select! {
            Token::Float(f) = extra => Self { value: f, span: extra.span() },
        }
        .labelled("float")
    }
}

impl WriteRuby for ExprFloat {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment(self.value.to_string());
    }
}
