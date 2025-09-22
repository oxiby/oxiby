use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::{Expr, ExprIdent};
use crate::token::Token;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStruct<'a> {
    ty: Type<'a>,
    fields: Vec<(ExprIdent<'a>, Expr<'a>)>,
    span: SimpleSpan,
}

impl<'a> ExprStruct<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Type::parser()
            .then(
                ExprIdent::parser()
                    .then_ignore(just(Token::Colon))
                    .then(expr)
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map_with(|(ty, fields), extra| Self {
                ty,
                fields,
                span: extra.span(),
            })
            .labelled("struct construction")
            .as_context()
    }
}

impl WriteRuby for ExprStruct<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        let string = self.ty.to_string();

        let path = match scope.resolve_ident(&string) {
            Some((path, _kind)) => path,
            None => string,
        };

        scope.fragment(format!("{path}.new"));

        if self.fields.is_empty() {
            return;
        }

        scope.fragment("(");

        for (index, (name, value)) in self.fields.iter().enumerate() {
            scope.fragment(format!("{name}: "));
            value.write_ruby(scope);

            if index < self.fields.len() - 1 {
                scope.fragment(", ");
            }
        }

        scope.fragment(")");
    }
}
