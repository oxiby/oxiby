use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::{Expr, ExprBlock};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprLoop {
    block: ExprBlock,
    pub(crate) span: SimpleSpan,
}

impl ExprLoop {
    pub fn parser<'a>(
        expr: impl Parser<
            'a,
            MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
            Expr,
            extra::Err<Rich<'a, Token, SimpleSpan>>,
        > + Clone,
    ) -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        just(Token::Loop)
            .ignore_then(ExprBlock::parser(expr))
            .map_with(|block, extra| Self {
                block,
                span: extra.span(),
            })
            .labelled("loop")
            .as_context()
    }
}

impl WriteRuby for ExprLoop {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment_block_with_end("loop do", |scope| {
            scope.newline();
            self.block.unscoped().write_ruby(scope);
        });
    }
}

impl Infer for ExprLoop {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        self.block.infer(checker, context)
    }
}
