use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::{Expr, ExprIdent};
use crate::token::Token;
use crate::types::TypeIdent;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall<'a> {
    name: CallIdent<'a>,
    self_arg: bool,
    positional_args: Vec<Expr<'a>>,
    keyword_args: Vec<(ExprIdent<'a>, Expr<'a>)>,
    is_field: bool,
    span: SimpleSpan,
}

#[derive(Debug, Clone, PartialEq)]
enum FnArg<'a> {
    Pos(Expr<'a>),
    Kw(ExprIdent<'a>, Expr<'a>),
}

impl<'a> ExprCall<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone + 'a,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        let positional_arg = expr.clone().map(FnArg::Pos);

        let keyword_arg = ExprIdent::parser()
            .then_ignore(just(Token::Colon))
            .then(expr)
            .map(|(ident, expr)| FnArg::Kw(ident, expr));

        choice((
            ExprIdent::parser().map(CallIdent::Expr),
            TypeIdent::parser().map(CallIdent::Type),
        ))
        .then(
            just(Token::SelfTerm)
                .then_ignore(just(Token::Comma))
                .or_not()
                .then(
                    choice((keyword_arg, positional_arg))
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .try_map(|(name, (maybe_self_arg, fn_args)), span| {
            let mut positional_args = Vec::new();
            let mut keyword_args = Vec::new();
            let mut keyword_arg_seen = false;

            for fn_arg in fn_args {
                match fn_arg {
                    FnArg::Pos(expr) => {
                        if keyword_arg_seen {
                            return Err(Rich::custom(
                                span,
                                "Positional function arguments cannot follow keyword arguments",
                            ));
                        }

                        positional_args.push(expr);
                    }
                    FnArg::Kw(ident, expr) => {
                        keyword_arg_seen = true;

                        keyword_args.push((ident, expr));
                    }
                }
            }

            Ok(Self {
                name,
                self_arg: maybe_self_arg.is_some(),
                positional_args,
                keyword_args,
                is_field: false,
                span,
            })
        })
        .labelled("call")
        .as_context()
        .boxed()
    }

    pub fn set_field(&mut self) {
        self.is_field = true;
    }

    #[inline]
    pub fn has_args(&self) -> bool {
        self.has_self_arg() || self.has_positional_args() || self.has_keyword_args()
    }

    #[inline]
    pub fn has_self_arg(&self) -> bool {
        self.self_arg
    }

    #[inline]
    pub fn has_positional_args(&self) -> bool {
        !self.positional_args.is_empty()
    }

    #[inline]
    pub fn has_keyword_args(&self) -> bool {
        !self.keyword_args.is_empty()
    }
}

impl WriteRuby for ExprCall<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        // If this call is a field, render its name without going through import resolution so it
        // doesn't get confused with a free function of the same name.
        if self.is_field {
            scope.fragment(self.name.as_str());
        } else {
            self.name.write_ruby(scope);
        }

        if !self.has_args() {
            return;
        }

        scope.fragment("(");

        for (index, arg) in self.positional_args.iter().enumerate() {
            arg.write_ruby(scope);

            if index < self.positional_args.len() - 1 {
                scope.fragment(", ");
            }
        }

        if self.has_positional_args() && self.has_keyword_args() {
            scope.fragment(", ");
        }

        for (index, arg) in self.keyword_args.iter().enumerate() {
            scope.fragment(arg.0.as_str());
            scope.fragment(": ");
            arg.1.write_ruby(scope);

            if index < self.keyword_args.len() - 1 {
                scope.fragment(", ");
            }
        }

        scope.fragment(")");
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallIdent<'a> {
    Expr(ExprIdent<'a>),
    Type(TypeIdent<'a>),
}

impl CallIdent<'_> {
    fn as_str(&self) -> &str {
        match self {
            Self::Expr(ident) => ident.as_str(),
            Self::Type(ident) => ident.as_str(),
        }
    }
}

impl WriteRuby for CallIdent<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        match self {
            Self::Expr(ident) => ident.write_ruby(scope),
            // A type being called as a function is calling a tuple struct constructor.
            Self::Type(ident) => {
                // TODO: Normally `TypeIdent` should handle its own writing, but it appends ".new" to
                // variants after import resolution which we don't want here. Find a cleaner way to
                // handle this.
                let string = ident.to_string();
                match scope.resolve_ident(&string) {
                    Some((path, _kind)) => scope.fragment(path),
                    None => scope.fragment(string),
                }

                scope.fragment(".new");
            }
        }
    }
}
