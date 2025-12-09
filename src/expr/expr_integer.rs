use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprInteger {
    pub(crate) value: u64,
    pub(crate) span: SimpleSpan,
}

impl ExprInteger {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
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
