use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::{Expr, ExprBlock};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprWhileLoop {
    condition: Box<Expr>,
    block: ExprBlock,
    pub(crate) span: SimpleSpan,
}

impl ExprWhileLoop {
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
        just(Token::While)
            .ignore_then(expr.clone())
            .then(ExprBlock::parser(expr))
            .map_with(|(condition, block), extra| Self {
                condition: Box::new(condition),
                block,
                span: extra.span(),
            })
            .labelled("while loop")
            .as_context()
    }
}

impl WriteRuby for ExprWhileLoop {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("while ");
        self.condition.write_ruby(scope);
        scope.fragment_block_with_end("", |scope| {
            scope.newline();
            self.block.unscoped().write_ruby(scope);
        });
    }
}

impl Infer for ExprWhileLoop {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let condition_type = self.condition.infer(checker)?;

        if condition_type != check::Type::boolean() {
            return Err(Error::type_mismatch()
                .with_detail(
                    &format!("Condition was expected to be `Boolean` but was `{condition_type}`",),
                    self.condition.span(),
                )
                .finish());
        }

        self.block.infer(checker)?;

        Ok(check::Type::unit())
    }
}
