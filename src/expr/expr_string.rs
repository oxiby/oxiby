use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::Spanned;
use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprString<'a> {
    parts: Vec<ExprStringPart<'a>>,
    span: SimpleSpan,
}

impl<'a> ExprString<'a> {
    pub fn parser<I, M>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
        make_input: M,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
        M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
    {
        let expr_string_part_literal =
            select! { Token::StringPartLiteral(s) => ExprStringPart::Literal(s) };

        let make_input_ = make_input.clone();

        let string_interpolation_tokens = select_ref! {
            Token::StringPartExpr(tokens) = extra => make_input_(extra.span(), tokens.as_slice())
        };

        let expr_string_part_expr = expr
            .map(|expr| ExprStringPart::Expr(Box::new(expr)))
            .nested_in(string_interpolation_tokens);

        let expr_string_part = choice((expr_string_part_literal, expr_string_part_expr));

        expr_string_part
            .repeated()
            .collect::<Vec<_>>()
            .nested_in(select_ref! {
                Token::String(tokens) = extra => make_input(extra.span(), tokens.as_slice()),
            })
            .map_with(|parts, extra| Self {
                parts,
                span: extra.span(),
            })
            .labelled("string")
    }
}

impl WriteRuby for ExprString<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment('"');

        for part in &self.parts {
            part.write_ruby(scope);
        }

        scope.fragment('"');
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum ExprStringPart<'a> {
    Literal(&'a str),
    Expr(Box<Expr<'a>>),
}

impl WriteRuby for ExprStringPart<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        match self {
            Self::Literal(s) => scope.fragment(*s),
            Self::Expr(expr) => {
                scope.fragment("#{");
                expr.write_ruby(scope);
                scope.fragment("}");
            }
        }
    }
}
