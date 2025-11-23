use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprContinue {
    pub(crate) span: SimpleSpan,
}

impl ExprContinue {
    pub fn parser<'a, I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just::<_, I, chumsky::extra::Err<Rich<_>>>(Token::Continue)
            .ignored()
            .map_with(|(), extra| Self { span: extra.span() })
            .labelled("continue")
    }
}

impl WriteRuby for ExprContinue {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("next");
    }
}
