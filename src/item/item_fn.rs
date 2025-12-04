use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::Spanned;
use crate::ast::Visibility;
use crate::compiler::{Scope, WriteRuby};
use crate::expr::{Expr, ExprIdent};
use crate::token::Token;
use crate::types::{Constraint, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct ItemFn<'a> {
    pub(crate) signature: Signature<'a>,
    pub(crate) body: Vec<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ItemFn<'a> {
    pub fn parser<I, M>(
        make_input: M,
        associated_fn: bool,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
        M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
    {
        Signature::parser(associated_fn)
            .then(
                Expr::parser(make_input)
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map_with(|(signature, body), extra| Self {
                signature,
                body,
                span: extra.span(),
            })
            .labelled("function")
            .as_context()
    }

    #[inline]
    pub fn has_params(&self) -> bool {
        self.has_self_param() || self.has_positional_params() || self.has_keyword_params()
    }

    #[inline]
    pub fn has_self_param(&self) -> bool {
        self.signature.self_param
    }

    #[inline]
    pub fn has_explicit_params(&self) -> bool {
        !self.signature.positional_params.is_empty() || !self.signature.keyword_params.is_empty()
    }

    #[inline]
    pub fn has_positional_params(&self) -> bool {
        !self.signature.positional_params.is_empty()
    }

    #[inline]
    pub fn has_keyword_params(&self) -> bool {
        !self.signature.keyword_params.is_empty()
    }
}

impl WriteRuby for ItemFn<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        let mut def = if self.signature.self_param {
            format!("def {}", self.signature.name)
        } else {
            format!("def self.{}", self.signature.name)
        };

        if self.has_explicit_params() {
            def.push('(');

            def.push_str(
                &self
                    .signature
                    .positional_params
                    .iter()
                    .map(|param| param.ident.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            );

            if self.has_positional_params() && self.has_keyword_params() {
                def.push_str(", ");
            }

            def.push_str(
                &self
                    .signature
                    .keyword_params
                    .iter()
                    .map(|param| format!("{}:", param.ident))
                    .collect::<Vec<_>>()
                    .join(", "),
            );

            def.push(')');
        }

        scope.block_with_end(def, |scope| {
            for expr in &self.body {
                expr.write_ruby(scope);
                scope.newline();
            }
        });

        if self.signature.name.as_str() == "main" {
            scope.newline();
            scope.line("main");
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Signature<'a> {
    pub(crate) visibility: Visibility,
    pub(crate) name: ExprIdent<'a>,
    pub(crate) self_param: bool,
    pub(crate) associated_fn: bool,
    pub(crate) positional_params: Vec<FnParam<'a>>,
    pub(crate) keyword_params: Vec<FnParam<'a>>,
    pub(crate) return_ty: Option<Type<'a>>,
    pub(crate) constraints: Option<Vec<Constraint<'a>>>,
    pub(crate) span: SimpleSpan,
}

impl<'a> Signature<'a> {
    pub fn parser<I>(
        associated_fn: bool,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        let param_list = choice((
            just(Token::SelfTerm)
                .map(|_| (true, Vec::with_capacity(0), Vec::with_capacity(0)))
                .delimited_by(just(Token::LParen), just(Token::RParen)),
            just(Token::SelfTerm)
                .ignore_then(just(Token::Comma))
                .or_not()
                .then(
                    FnParam::parser()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then(
                    just(Token::Colon)
                        .ignore_then(FnParam::parser())
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .map(|((maybe_self, pos_params), kw_params)| {
                    (maybe_self.is_some(), pos_params, kw_params)
                }),
        ))
        .labelled("function parameter list")
        .boxed();

        Visibility::parser()
            .then_ignore(just(Token::Fn))
            .then(ExprIdent::parser())
            .then(param_list)
            .then(just(Token::Arrow).ignore_then(Type::parser()).or_not())
            .then(Constraint::where_parser().or_not())
            .try_map(
                move |((((visibility, name), params), return_ty), constraints), span| {
                    let (self_param, positional_params, keyword_params) = params;

                    // TODO: These checks seem to cause a parsing error in the right conditions,
                    // but the custom error messages don't appear in the output. Instead, the error
                    // will say: found 'fn' expected named field, function, or '}' in struct
                    if self_param && name.as_str() == "initialize" {
                        return Err(Rich::custom(
                            span,
                            "`initialize` is a reserved name for instance methods",
                        ));
                    } else if !self_param && name.as_str() == "allocate" {
                        return Err(Rich::custom(
                            span,
                            "`allocate` is a reserved name for static methods",
                        ));
                    }

                    Ok(Self {
                        visibility,
                        name,
                        self_param,
                        associated_fn,
                        positional_params,
                        keyword_params,
                        return_ty,
                        constraints,
                        span,
                    })
                },
            )
            .labelled("function signature")
            .as_context()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam<'a> {
    pub(crate) ident: ExprIdent<'a>,
    pub(crate) ty: Type<'a>,
    pub(crate) span: SimpleSpan,
}

impl<'a> FnParam<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        ExprIdent::parser()
            .then_ignore(just(Token::Colon))
            .then(Type::parser())
            .map_with(|(ident, ty), extra| Self {
                ident,
                ty,
                span: extra.span(),
            })
            .labelled("positional function parameter")
            .as_context()
    }
}
