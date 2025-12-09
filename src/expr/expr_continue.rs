use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprContinue {
    pub(crate) span: SimpleSpan,
}

impl ExprContinue {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        just::<
            _,
            MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
            chumsky::extra::Err<Rich<_>>,
        >(Token::Continue)
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
