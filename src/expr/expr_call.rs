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
    positional_args: Vec<FnPosArg<'a>>,
    keyword_args: Vec<FnKwArg<'a>>,
    is_field: bool,
    span: SimpleSpan,
}

#[derive(Debug, Clone, PartialEq)]
struct FnPosArg<'a>(Expr<'a>);

#[derive(Debug, Clone, PartialEq)]
struct FnKwArg<'a>(ExprIdent<'a>, Expr<'a>);

#[derive(Debug, Clone, PartialEq)]
enum FnArgs<'a> {
    Pos(Vec<FnPosArg<'a>>),
    Kw(Vec<FnKwArg<'a>>),
    Both(Vec<FnPosArg<'a>>, Vec<FnKwArg<'a>>),
}

impl<'a> ExprCall<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone + 'a,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        let positional_args = expr
            .clone()
            .map(FnPosArg)
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .boxed();

        let keyword_args = ExprIdent::parser()
            .then_ignore(just(Token::Colon))
            .then(expr)
            .map(|(ident, expr)| FnKwArg(ident, expr))
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .boxed();

        choice((
            ExprIdent::parser().map(CallIdent::Expr),
            TypeIdent::parser().map(CallIdent::Type),
        ))
        .then_ignore(just(Token::LParen))
        .then(just(Token::SelfTerm).or_not())
        .then(choice((
            positional_args.clone().map(FnArgs::Pos),
            keyword_args.clone().map(FnArgs::Kw),
            positional_args
                .then_ignore(just(Token::Comma))
                .then(keyword_args)
                .map(|(pos, kw)| FnArgs::Both(pos, kw)),
        )))
        .then_ignore(just(Token::RParen))
        .map_with(|((name, maybe_self_arg), fn_args), extra| {
            let (positional_args, keyword_args): (Vec<FnPosArg>, Vec<FnKwArg>) = match fn_args {
                FnArgs::Pos(positional_args) => (positional_args, Vec::with_capacity(0)),
                FnArgs::Kw(keyword_args) => (Vec::with_capacity(0), keyword_args),
                FnArgs::Both(positional_args, keyword_args) => (positional_args, keyword_args),
            };

            Self {
                name,
                self_arg: maybe_self_arg.is_some(),
                positional_args,
                keyword_args,
                is_field: false,
                span: extra.span(),
            }
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
            arg.0.write_ruby(scope);

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
