use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::{Expr, ExprIdent};
use crate::token::Token;
use crate::types::TypeIdent;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprEnum<'a> {
    ty: TypeIdent<'a>,
    variant: TypeIdent<'a>,
    constructor: EnumConstructor<'a>,
    span: SimpleSpan,
}

impl<'a> ExprEnum<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        TypeIdent::parser()
            .then_ignore(just(Token::Dot))
            .then(TypeIdent::parser())
            .then(
                choice((
                    expr.clone()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .map(EnumConstructor::Tuple),
                    ExprIdent::parser()
                        .then_ignore(just(Token::Colon))
                        .then(expr)
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LBrace), just(Token::RBrace))
                        .map(EnumConstructor::Struct),
                ))
                .or_not(),
            )
            .map_with(|((ty, variant), constructor), extra| Self {
                ty,
                variant,
                constructor: constructor.unwrap_or(EnumConstructor::Unit),
                span: extra.span(),
            })
            .labelled("enum construction")
            .as_context()
    }
}

impl WriteRuby for ExprEnum<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        self.ty.write_ruby(scope);
        scope.fragment(format!("::{}.allocate", self.variant));

        if self.constructor == EnumConstructor::Unit {
            return;
        }

        scope.fragment(".tap { |__oxiby_new| __oxiby_new.send(:initialize, ");

        match &self.constructor {
            EnumConstructor::Unit => unreachable!("checked for this case above"),
            EnumConstructor::Tuple(exprs) => {
                for (index, expr) in exprs.iter().enumerate() {
                    expr.write_ruby(scope);

                    if index < exprs.len() - 1 {
                        scope.fragment(", ");
                    }
                }
            }
            EnumConstructor::Struct(fields) => {
                for (index, (name, value)) in fields.iter().enumerate() {
                    scope.fragment(format!("{name}: "));
                    value.write_ruby(scope);

                    if index < fields.len() - 1 {
                        scope.fragment(", ");
                    }
                }
            }
        }

        scope.fragment(") }");
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumConstructor<'a> {
    Unit,
    Tuple(Vec<Expr<'a>>),
    Struct(Vec<(ExprIdent<'a>, Expr<'a>)>),
}
