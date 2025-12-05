use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use super::ExprBlock;
use crate::Spanned;
use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
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

impl Infer for ExprForLoop<'_> {
    fn infer(&self, checker: &Checker, context: &mut Context) -> Result<check::Type, Error> {
        let element_type = match self.items.infer(checker, context)? {
            check::Type::Generic(_, generic_types) => generic_types[0].clone(),
            items_type => {
                return Err(Error::build("Type not iterable")
                    .detail(
                        &format!("Type `{items_type}` is not iterable."),
                        self.items.span(),
                    )
                    .with_context(
                        "Try surrounding the expression with `[` and `]`.",
                        self.items.span(),
                    )
                    .finish());
            }
        };

        context.push_scope();

        match &self.pattern {
            Pattern::Ident(pattern_ident) => {
                context.push_term_var(pattern_ident.ident.as_str(), element_type);
            }
            pattern => {
                todo!("Type checking for for loops is not yet implemented for pattern {pattern:?}")
            }
        }

        let ty = self.block.infer(checker, context)?;

        context.pop_scope();

        Ok(ty)
    }
}
