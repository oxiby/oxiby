use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprRuby {
    ruby: String,
    pub(crate) span: SimpleSpan,
}

impl ExprRuby {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        select! {
            Token::Ruby(ruby) = extra => Self { ruby, span: extra.span() }
        }
        .labelled("ruby block")
        .boxed()
    }
}

impl WriteRuby for ExprRuby {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment(&self.ruby);
    }
}
