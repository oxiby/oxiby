use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBoolean {
    pub(crate) value: bool,
    pub(crate) span: SimpleSpan,
}

impl ExprBoolean {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
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
