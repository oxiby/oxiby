use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprString {
    pub(crate) parts: Vec<ExprStringPart>,
    pub(crate) span: SimpleSpan,
}

impl ExprString {
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
        let expr_string_part_literal =
            select! { Token::StringPartLiteral(s) => ExprStringPart::Literal(s) };

        let string_interpolation_tokens = select_ref! {
            Token::StringPartExpr(tokens) = extra => tokens.split_spanned(extra.span())
        };

        let expr_string_part_expr = expr
            .map(|expr| ExprStringPart::Expr(Box::new(expr)))
            .nested_in(string_interpolation_tokens);

        let expr_string_part = choice((expr_string_part_literal, expr_string_part_expr));

        expr_string_part
            .repeated()
            .collect::<Vec<_>>()
            .nested_in(select_ref! {
                Token::String(tokens) = extra => tokens.split_spanned(extra.span()),
            })
            .map_with(|parts, extra| Self {
                parts,
                span: extra.span(),
            })
            .labelled("string")
    }
}

impl WriteRuby for ExprString {
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
pub enum ExprStringPart {
    Literal(String),
    Expr(Box<Expr>),
}

impl WriteRuby for ExprStringPart {
    fn write_ruby(&self, scope: &mut Scope) {
        match self {
            Self::Literal(s) => scope.fragment(s),
            Self::Expr(expr) => {
                scope.fragment("#{");
                expr.write_ruby(scope);
                scope.fragment("}");
            }
        }
    }
}
