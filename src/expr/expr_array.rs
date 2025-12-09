use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::ast::Operator;
use crate::expr::{Expr, ExprBinary, ExprList, ExprMap};
use crate::token::Token;

pub fn expr_array_parser<'a>(
    expr: impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Expr,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone,
) -> impl Parser<
    'a,
    MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
    Expr,
    extra::Err<Rich<'a, Token, SimpleSpan>>,
> + Clone {
    expr.clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map_with(|exprs, extra| {
            if exprs.is_empty() {
                return Expr::List(ExprList {
                    exprs,
                    span: extra.span(),
                });
            }

            if exprs.iter().all(is_assign_expr) {
                let pairs = exprs
                    .into_iter()
                    .map(|expr| {
                        if let Expr::Binary(expr_binary) = expr {
                            let ExprBinary { lhs, rhs, .. } = expr_binary;

                            (*lhs, *rhs)
                        } else {
                            unreachable!("is_assign_expr should ensure all exprs are ExprBinary");
                        }
                    })
                    .collect::<Vec<_>>();

                Expr::Map(ExprMap {
                    pairs,
                    span: extra.span(),
                })
            } else {
                Expr::List(ExprList {
                    exprs,
                    span: extra.span(),
                })
            }
        })
        .labelled("list or map")
        .as_context()
}

fn is_assign_expr(expr: &Expr) -> bool {
    if let Expr::Binary(expr_binary) = expr
        && let ExprBinary {
            op: Operator::Assign,
            ..
        } = expr_binary
    {
        true
    } else {
        false
    }
}
