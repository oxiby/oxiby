use std::fmt::Display;

use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::ExprIdent;
use crate::item::ImportKind;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Constraint<'a> {
    pub tyvar: TyVar<'a>,
    pub requirements: Option<Vec<Type<'a>>>,
    pub default: Option<Type<'a>>,
}

impl<'a> Constraint<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        choice((
            just(Token::SelfType).to(TyVar::SelfType),
            ExprIdent::parser().map(TyVar::ExprIdent),
        ))
        .then(
            just(Token::Colon)
                .ignore_then(Type::parser().separated_by(just(Token::Add)).collect())
                .or_not(),
        )
        .then(just(Token::Assign).ignore_then(Type::parser()).or_not())
        .map(|((tyvar, requirements), default)| Constraint {
            tyvar,
            requirements,
            default,
        })
        .labelled("constraint")
        .as_context()
    }

    pub fn list_parser<I>()
    -> impl Parser<'a, I, Vec<Self>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Self::parser()
            .separated_by(just(Token::Comma))
            .at_least(1)
            .collect::<Vec<_>>()
            .labelled("constraint list")
            .as_context()
    }

    pub fn where_parser<I>()
    -> impl Parser<'a, I, Vec<Self>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Where).ignore_then(Self::list_parser())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyVar<'a> {
    SelfType,
    ExprIdent(ExprIdent<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssociatedType<'a> {
    pub ty: Type<'a>,
    pub requirements: Option<Vec<Type<'a>>>,
    pub default: Option<Type<'a>>,
}

impl<'a> AssociatedType<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Type::parser()
            .then(
                just(Token::Colon)
                    .ignore_then(Type::parser().separated_by(just(Token::Add)).collect())
                    .or_not(),
            )
            .then(just(Token::Assign).ignore_then(Type::parser()).or_not())
            .map(|((ty, requirements), default)| Self {
                ty,
                requirements,
                default,
            })
            .labelled("associated type")
            .as_context()
            .boxed()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeIdent<'a> {
    ident: &'a str,
    span: SimpleSpan,
}

impl<'a> TypeIdent<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        select! {
            Token::TypeIdent(ident) => ident,
            Token::SelfType => "Self",
        }
        .map_with(|ident, extra| Self {
            ident,
            span: extra.span(),
        })
        .labelled("type identifier")
    }

    pub fn as_str(&self) -> &'a str {
        self.ident
    }
}

impl Display for TypeIdent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl WriteRuby for TypeIdent<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        let string = self.to_string();
        match scope.resolve_ident(&string) {
            Some((path, kind)) => match kind {
                ImportKind::Variant => scope.fragment(format!("{path}.new")),
                _ => scope.fragment(path),
            },
            None => scope.fragment(string),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type<'a> {
    Concrete(ConcreteType<'a>),
    Variable(ExprIdent<'a>),
    Tuple(Vec<Type<'a>>),
    Fn(Option<Vec<Type<'a>>>, Option<Box<Type<'a>>>),
}

impl<'a> Type<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        let mut type_parser = Recursive::declare();
        let mut concrete_type_parser = Recursive::declare();

        concrete_type_parser.define(
            TypeIdent::parser()
                .then_ignore(just(Token::Dot))
                .or_not()
                .then(
                    TypeIdent::parser().then(
                        type_parser
                            .clone()
                            .separated_by(just(Token::Comma))
                            .at_least(1)
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::Lt), just(Token::Gt))
                            .or_not(),
                    ),
                )
                .map(|(qual, (ident, params))| ConcreteType {
                    qual,
                    ident,
                    params,
                })
                .labelled("concrete type")
                .boxed(),
        );

        type_parser.define(
            choice((
                ExprIdent::parser().map(Self::Variable),
                type_parser
                    .clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen))
                    .map(Self::Tuple),
                TypeIdent::parser()
                    .then_ignore(just(Token::LParen))
                    .then(
                        type_parser
                            .clone()
                            .separated_by(just(Token::Comma))
                            .collect::<Vec<_>>()
                            .or_not(),
                    )
                    .then_ignore(just(Token::RParen))
                    .then(
                        just(Token::Arrow)
                            .ignore_then(type_parser.clone())
                            .map(Box::new)
                            .or_not(),
                    )
                    .try_map(|((ident, params), ret), span| {
                        if ident.as_str() != "Fn" {
                            return Err(Rich::custom(
                                span,
                                format!("expected 'Fn', found '{ident}'"),
                            ));
                        }

                        Ok(Self::Fn(params, ret))
                    })
                    .boxed(),
                concrete_type_parser.map(Self::Concrete),
            ))
            .boxed(),
        );

        type_parser.labelled("type").boxed()
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Self::Concrete(concrete_type) = &self {
            return write!(f, "{}", concrete_type.ident);
        }

        unreachable!("should not be writing anything but a concrete type as a Ruby class name")
    }
}

impl WriteRuby for Type<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        if let Self::Concrete(concrete_type) = &self {
            return concrete_type.write_ruby(scope);
        }

        unreachable!("should not be writing anything but a concrete type as a Ruby class name")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConcreteType<'a> {
    pub(crate) qual: Option<TypeIdent<'a>>,
    pub(crate) ident: TypeIdent<'a>,
    pub(crate) params: Option<Vec<Type<'a>>>,
}

impl Display for ConcreteType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl WriteRuby for ConcreteType<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        self.ident.write_ruby(scope);
    }
}
