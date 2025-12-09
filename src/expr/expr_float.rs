use chumsky::input::MappedInput;
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
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
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
