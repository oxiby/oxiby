use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use itertools::{EitherOrBoth, Itertools};

use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::{Expr, ExprIdent};
use crate::token::Token;
use crate::types::TypeIdent;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall<'a> {
    pub(crate) name: CallIdent<'a>,
    pub(crate) self_arg: bool,
    pub(crate) positional_args: Vec<Expr<'a>>,
    pub(crate) keyword_args: Vec<(ExprIdent<'a>, Expr<'a>)>,
    pub(crate) is_field: bool,
    pub(crate) span: SimpleSpan,
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
            .then_ignore(just(Token::Assign))
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

#[derive(Clone, Copy, Debug)]
pub enum Noun {
    Function,
    Struct,
    Variant,
}

impl Noun {
    fn uppercase(self) -> String {
        match self {
            Self::Function => "Function".to_string(),
            Self::Struct => "Struct".to_string(),
            Self::Variant => "Variant".to_string(),
        }
    }

    fn lowercase(self) -> String {
        match self {
            Self::Function => "function".to_string(),
            Self::Struct => "struct".to_string(),
            Self::Variant => "variant".to_string(),
        }
    }
}

pub fn infer_function<'a>(
    checker: &mut Checker,
    context: &mut Context,
    function: &check::Function,
    call_args: impl Iterator<Item = &'a Expr<'a>>,
    span: SimpleSpan,
    noun: Noun,
) -> Result<check::Type, Error> {
    for pair in function.positional_params.iter().zip_longest(call_args) {
        match pair {
            EitherOrBoth::Both(ty, expr) => {
                let expr_ty = expr.infer(checker, context)?;

                if *ty != expr_ty {
                    return Err(Error::type_mismatch()
                        .with_detail(
                            &format!("Argument was expected to be `{ty}` but was `{expr_ty}`."),
                            expr.span(),
                        )
                        .finish());
                }
            }
            EitherOrBoth::Left(ty) => {
                return Err(Error::build("Missing argument")
                    .with_detail(
                        &format!(
                            "{} expects an additional argument of type `{ty}` but it was not \
                             given.",
                            noun.uppercase(),
                        ),
                        span,
                    )
                    .with_help(&format!(
                        "Try including an additional argument of type `{ty}`."
                    ))
                    .finish());
            }
            EitherOrBoth::Right(expr) => {
                let expr_ty = expr.infer(checker, context)?;

                return Err(Error::build("Extra argument")
                    .with_detail(
                        &(match &function.name {
                            Some(name) => format!(
                                "Argument of type `{expr_ty}` is not expected by {} `{}`.",
                                noun.lowercase(),
                                name,
                            ),
                            None => format!(
                                "Argument of type `{expr_ty}` is not expected by {}.",
                                noun.lowercase(),
                            ),
                        }),
                        expr.span(),
                    )
                    .with_help("Try omitting this argument.")
                    .finish());
            }
        }
    }

    Ok(*function.return_type.clone())
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

impl Infer for ExprCall<'_> {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        let name = self.name.as_str();

        if let Some(ty) = context.get(name) {
            match ty {
                check::Type::Fn(function) => return infer_function(
                    checker,
                    context,
                    &function,
                    self.positional_args.iter(),
                    self.span,
                    Noun::Function,
                ),
                ty => return Err(Error::type_mismatch()
                    .with_detail(
                        &format!(
                            "Value `{name}` is of type `{ty}` but is being called as a function."
                        ),
                        self.span,
                    )
                    .finish()),
            }
        }

        let (ty, members) = match checker.type_constructors.get(name) {
            Some((ty, members)) => (ty.clone(), members.clone()),
            None => panic!("TODO: Couldn't infer ExprCall."),
        };

        if let Some(tuple_constructor_ty) = members.value_constructors.get(name) {
            let check::Type::Fn(function) = tuple_constructor_ty else {
                panic!("TODO: Tuple constructor wasn't a function.");
            };

            infer_function(
                checker,
                context,
                function,
                self.positional_args.iter(),
                self.span,
                Noun::Struct,
            )
        } else {
            Err(Error::build("Invalid struct literal")
                .with_detail(
                    &format!(
                        "Struct `{ty}` is not a tuple struct and cannot be constructed with \
                         the syntax `{ty}(...)`."
                    ),
                    self.span,
                )
                .with_help(
                    &(if matches!(ty, check::Type::RecordStruct(_, _)) {
                        format!("Try using record struct syntax: `{ty} {{ ... }}`")
                    } else {
                        format!(
                            "Try using unit struct syntax by omitting the parenthesized \
                             arguments: `{ty}`"
                        )
                    }),
                )
                .finish())
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallIdent<'a> {
    Expr(ExprIdent<'a>),
    Type(TypeIdent<'a>),
}

impl CallIdent<'_> {
    pub fn as_str(&self) -> &str {
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
