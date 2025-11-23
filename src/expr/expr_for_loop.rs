use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use super::ExprBlock;
use crate::Spanned;
use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::pattern::Pattern;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprForLoop<'a> {
    pattern: Pattern<'a>,
    items: Box<Expr<'a>>,
    block: ExprBlock<'a>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprForLoop<'a> {
    pub fn parser<I, M>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone + 'a,
        make_input: M,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
        M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
    {
        just(Token::For)
            .ignore_then(Pattern::parser(expr.clone(), make_input, false))
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .then(ExprBlock::parser(expr))
            .map_with(|((pattern, items), block), extra| Self {
                pattern,
                items: Box::new(items),
                block,
                span: extra.span(),
            })
            .labelled("for loop")
            .as_context()
    }
}

impl WriteRuby for ExprForLoop<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("(");
        self.items.write_ruby(scope);
        scope.fragment(").each do |");
        match &self.pattern {
            Pattern::Type(pattern_type) => pattern_type.write_ruby(scope),
            Pattern::Ident(pattern_ident) => pattern_ident.write_ruby(scope),
            Pattern::Tuple(pattern_tuple) => pattern_tuple.write_ruby(scope),
            _ => todo!("pattern not yet implemented for for loops"),
        }
        scope.fragment("|");
        scope.block(|scope| {
            scope.newline();
            self.block.unscoped().write_ruby(scope);
        });
        scope.fragment("end");
    }
}
