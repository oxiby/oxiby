use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Function, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::{Expr, ExprIdent};
use crate::token::Token;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprClosure {
    params: Vec<FnArg>,
    return_ty: Option<Type>,
    body: Vec<Expr>,
    pub(crate) span: SimpleSpan,
}

impl ExprClosure {
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
        let param_list = FnArg::parser()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .labelled("closure parameter list");

        just(Token::Fn)
            .ignore_then(param_list)
            .then(just(Token::Arrow).ignore_then(Type::parser()).or_not())
            .then(choice((
                expr.clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
                expr.map(|exp| vec![exp]),
            )))
            .map_with(|((params, return_ty), body), extra| Self {
                params,
                return_ty,
                body,
                span: extra.span(),
            })
            .labelled("list")
            .as_context()
    }
}

impl WriteRuby for ExprClosure {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("-> ");

        if !self.params.is_empty() {
            scope.fragment("(");

            for (index, param) in self.params.iter().enumerate() {
                param.binding.write_ruby(scope);

                if index < self.params.len() - 1 {
                    scope.fragment(", ");
                }
            }

            scope.fragment(")");
        }

        scope.fragment(" { ");

        for (index, expr) in self.body.iter().enumerate() {
            expr.write_ruby(scope);

            if index < self.body.len() - 1 {
                scope.fragment("; ");
            }
        }

        scope.fragment(" }");
    }
}

impl Infer for ExprClosure {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let mut positional_params = Vec::with_capacity(self.params.len());

        checker.push_scope();

        for param in &self.params {
            let FnArg {
                binding,
                ty: maybe_ty,
            } = param;

            let ty = maybe_ty
                .as_ref()
                .map_or_else(|| checker.create_type_var(), |ty| ty.clone().into());

            positional_params.push(ty.clone());
            checker.push_term_var(binding.as_str(), ty);
        }

        let mut return_type = check::Type::unit();

        for expr in &self.body {
            return_type = expr.infer(checker)?;
        }

        checker.pop_scope();

        let ret = check::Type::Fn(Function {
            name: None,
            is_static: true,
            positional_params,
            keyword_params: Vec::new(),
            return_type: Box::new(return_type),
        });

        Ok(ret)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnArg {
    binding: ExprIdent,
    ty: Option<Type>,
}

impl FnArg {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        ExprIdent::parser()
            .then(just(Token::Colon).ignore_then(Type::parser()).or_not())
            .map(|(binding, ty)| FnArg { binding, ty })
            .labelled("closure argument")
            .as_context()
    }
}
