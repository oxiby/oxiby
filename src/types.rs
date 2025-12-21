use std::fmt::Display;

use chumsky::input::MappedInput;
use chumsky::prelude::*;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::ExprIdent;
use crate::item::ImportKind;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Constraint {
    pub tyvar: TyVar,
    pub requirements: Option<Vec<Type>>,
    pub default: Option<Type>,
}

impl Constraint {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        choice((
            just(Token::SelfType).to(TyVar::SelfType),
            ExprIdent::parser().map(TyVar::ExprIdent),
        ))
        .then(
            just(Token::Is)
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

    #[allow(clippy::type_complexity)]
    pub fn list_parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Vec<Self>,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        Self::parser()
            .separated_by(just(Token::Comma))
            .at_least(1)
            .collect::<Vec<_>>()
            .labelled("constraint list")
            .as_context()
    }

    #[allow(clippy::type_complexity)]
    pub fn where_parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Vec<Self>,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        just(Token::Where).ignore_then(Self::list_parser())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyVar {
    SelfType,
    ExprIdent(ExprIdent),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssociatedType {
    pub name: ExprIdent,
    pub requirements: Option<Vec<Type>>,
    pub default: Option<Type>,
}

impl AssociatedType {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        ExprIdent::parser()
            .then(
                just(Token::Is)
                    .ignore_then(Type::parser().separated_by(just(Token::Add)).collect())
                    .or_not(),
            )
            .then(just(Token::Assign).ignore_then(Type::parser()).or_not())
            .map(|((name, requirements), default)| Self {
                name,
                requirements,
                default,
            })
            .labelled("associated type")
            .as_context()
            .boxed()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeIdent {
    ident: String,
    pub(crate) span: SimpleSpan,
}

impl TypeIdent {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        select! {
            Token::TypeIdent(ident) => ident,
            Token::SelfType => "Self".to_string(),
        }
        .map_with(|ident, extra| Self {
            ident,
            span: extra.span(),
        })
        .labelled("type identifier")
    }

    pub fn as_str(&self) -> &str {
        &self.ident
    }
}

impl Display for TypeIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl WriteRuby for TypeIdent {
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
pub enum Type {
    Concrete(ConcreteType),
    Variable(ExprIdent),
    Tuple(Vec<Type>),
    Fn(Option<Vec<Type>>, Option<Box<Type>>),
}

impl Type {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
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
                .map_with(|(qual, (ident, params)), extra| ConcreteType {
                    qual,
                    ident,
                    params,
                    span: extra.span(),
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
                    .then(
                        type_parser
                            .clone()
                            .separated_by(just(Token::Comma))
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::LParen), just(Token::RParen))
                            .or_not(),
                    )
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

    pub fn span(&self) -> Option<SimpleSpan> {
        match self {
            Self::Concrete(concrete_type) => Some(concrete_type.span),
            Self::Variable(expr_ident) => Some(expr_ident.span),
            Self::Tuple(types) => types
                .iter()
                .filter_map(Type::span)
                .reduce(|merged, span| merged.union(span)),
            Self::Fn(maybe_param_types, maybe_return_ty) => {
                maybe_param_types.as_ref().and_then(|param_types| {
                    param_types
                        .iter()
                        .filter_map(Type::span)
                        .reduce(|merged, span| merged.union(span))
                        .and_then(|params_span| {
                            maybe_return_ty.as_ref().and_then(|return_ty| {
                                return_ty
                                    .span()
                                    .map(|return_span| params_span.union(return_span))
                            })
                        })
                })
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Self::Concrete(concrete_type) = &self {
            return write!(f, "{}", concrete_type.ident);
        }

        unreachable!("should not be writing anything but a concrete type as a Ruby class name")
    }
}

impl WriteRuby for Type {
    fn write_ruby(&self, scope: &mut Scope) {
        if let Self::Concrete(concrete_type) = &self {
            return concrete_type.write_ruby(scope);
        }

        unreachable!("should not be writing anything but a concrete type as a Ruby class name")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConcreteType {
    pub(crate) qual: Option<TypeIdent>,
    pub(crate) ident: TypeIdent,
    pub(crate) params: Option<Vec<Type>>,
    span: SimpleSpan,
}

impl Display for ConcreteType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl WriteRuby for ConcreteType {
    fn write_ruby(&self, scope: &mut Scope) {
        self.ident.write_ruby(scope);
    }
}
