use std::fmt::Display;

use chumsky::input::MappedInput;
use chumsky::prelude::*;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::{Expr, ExprBoolean, ExprFloat, ExprIdent, ExprInteger, ExprString};
use crate::token::Token;
use crate::types::{Type, TypeIdent};

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Literal(PatternLiteral), // "example"
    Type(PatternType),       // x: Example
    Ident(PatternIdent),     // x
    Tuple(PatternTuple),     // (pattern, pattern)
    Ctor(PatternCtor),       // Example, Example(x, y), Example { x, y -> y2, z -> z2 = "foo" }
    Wildcard,                // _
}

impl Pattern {
    pub fn parser<'a>(
        expr: impl Parser<
            'a,
            MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
            Expr,
            extra::Err<Rich<'a, Token, SimpleSpan>>,
        > + Clone
        + 'a,
        is_bracketed: bool,
    ) -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        let mut pattern_parser = Recursive::declare();
        let mut pattern_ctor_parser = Recursive::declare();

        pattern_parser.define(choice((
            just(Token::Underscore).to(Self::Wildcard),
            PatternLiteral::parser(expr.clone()).map(Self::Literal),
            PatternType::parser().map(Self::Type),
            PatternIdent::parser().map(Self::Ident),
            pattern_ctor_parser.clone().map(Self::Ctor),
            pattern_parser
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .at_least(2)
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .map(move |patterns| {
                    Self::Tuple(PatternTuple {
                        patterns,
                        is_bracketed,
                    })
                }),
        )));

        pattern_ctor_parser.define(
            TypeIdent::parser()
                .then_ignore(just(Token::Dot))
                .or_not()
                .then(TypeIdent::parser())
                .then(
                    choice((
                        just(Token::Underscore)
                            .delimited_by(just(Token::LParen), just(Token::RParen))
                            .to(CtorFields::Wildcard),
                        ExprIdent::parser()
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::LParen), just(Token::RParen))
                            .map(CtorFields::Tuple),
                        ExprIdent::parser()
                            .then(just(Token::Arrow).ignore_then(ExprIdent::parser()).or_not())
                            .then(
                                just(Token::Assign)
                                    .ignore_then(pattern_parser.clone())
                                    .or_not(),
                            )
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::LBrace), just(Token::RBrace))
                            .map(|pieces| {
                                pieces
                                    .into_iter()
                                    .map(|((name, rename), pattern)| CtorStructField {
                                        name,
                                        rename,
                                        pattern,
                                    })
                                    .collect::<Vec<_>>()
                            })
                            .map(CtorFields::Struct),
                    ))
                    .or_not(),
                )
                .map(|((parent_ty_ident, ty_ident), fields)| PatternCtor {
                    parent_ty_ident,
                    ty_ident,
                    fields: fields.unwrap_or(CtorFields::Unit),
                })
                .labelled("constructor pattern")
                .as_context()
                .boxed(),
        );

        pattern_parser.labelled("pattern").boxed()
    }
}

impl WriteRuby for Pattern {
    fn write_ruby(&self, scope: &mut Scope) {
        match self {
            Self::Literal(pattern_literal) => pattern_literal.write_ruby(scope),
            Self::Type(pattern_type) => pattern_type.write_ruby(scope),
            Self::Ident(pattern_ident) => pattern_ident.write_ruby(scope),
            Self::Tuple(pattern_tuple) => pattern_tuple.write_ruby(scope),
            Self::Ctor(pattern_ctor) => pattern_ctor.write_ruby(scope),
            Self::Wildcard => (),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PatternLiteral {
    Boolean(ExprBoolean),
    Integer(ExprInteger),
    Float(ExprFloat),
    String(ExprString),
}

impl PatternLiteral {
    pub fn parser<'a>(
        expr: impl Parser<
            'a,
            MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
            Expr,
            extra::Err<Rich<'a, Token, SimpleSpan>>,
        > + Clone
        + 'a,
    ) -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        choice((
            ExprBoolean::parser().map(Self::Boolean),
            ExprInteger::parser().map(Self::Integer),
            ExprFloat::parser().map(Self::Float),
            ExprString::parser(expr).map(Self::String),
        ))
        .labelled("literal pattern")
        .as_context()
        .boxed()
    }
}

impl WriteRuby for PatternLiteral {
    fn write_ruby(&self, scope: &mut Scope) {
        match self {
            Self::Boolean(boolean) => boolean.write_ruby(scope),
            Self::Integer(integer) => integer.write_ruby(scope),
            Self::Float(float) => float.write_ruby(scope),
            Self::String(string) => string.write_ruby(scope),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PatternIdent {
    pub(crate) ident: ExprIdent,
}

impl PatternIdent {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        ExprIdent::parser()
            .map(|ident| Self { ident })
            .labelled("ident pattern")
            .as_context()
            .boxed()
    }
}

impl Display for PatternIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl WriteRuby for PatternIdent {
    fn write_ruby(&self, scope: &mut Scope) {
        self.ident.write_ruby(scope);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PatternType {
    pub(crate) ident: ExprIdent,
    pub(crate) ty: Type,
}

impl PatternType {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        ExprIdent::parser()
            .then_ignore(just(Token::Colon))
            .then(Type::parser())
            .map(|(ident, ty)| Self { ident, ty })
            .labelled("type ascription pattern")
            .as_context()
            .boxed()
    }
}

impl Display for PatternType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl WriteRuby for PatternType {
    fn write_ruby(&self, scope: &mut Scope) {
        self.ident.write_ruby(scope);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PatternTuple {
    pub(crate) patterns: Vec<Pattern>,
    is_bracketed: bool,
}

impl WriteRuby for PatternTuple {
    fn write_ruby(&self, scope: &mut Scope) {
        if self.is_bracketed {
            scope.fragment("[");
        }

        for (index, pattern) in self.patterns.iter().enumerate() {
            pattern.write_ruby(scope);

            if index < self.patterns.len() - 1 {
                scope.fragment(", ");
            }
        }

        if self.is_bracketed {
            scope.fragment("]");
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PatternCtor {
    pub(crate) parent_ty_ident: Option<TypeIdent>,
    pub(crate) ty_ident: TypeIdent,
    pub(crate) fields: CtorFields,
}

impl WriteRuby for PatternCtor {
    fn write_ruby(&self, scope: &mut Scope) {
        // TODO: There are other places parent type idents appear. Make this more general.
        if let Some(parent_ty_ident) = &self.parent_ty_ident {
            parent_ty_ident.write_ruby(scope);
            scope.fragment("::");
        }

        // TODO: Normally `TypeIdent` should handle its own writing, but it appends ".new" to
        // variants after import resolution which we don't want here. Find a cleaner way to handle
        // this.
        let ty_ident_string = self.ty_ident.to_string();

        // If the type is A.B we shouldn't try to resolve B on its own.
        if self.parent_ty_ident.is_none() {
            match scope.resolve_ident(&ty_ident_string) {
                Some((path, _kind)) => scope.fragment(path),
                None => scope.fragment(ty_ident_string),
            }
        } else {
            scope.fragment(ty_ident_string);
        }

        match &self.fields {
            CtorFields::Unit | CtorFields::Wildcard => (),
            CtorFields::Tuple(fields) => scope.fragment(format!(
                "({})",
                fields
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
            )),
            CtorFields::Struct(fields) => {
                scope.fragment("(");

                for (index, field) in fields.iter().enumerate() {
                    let CtorStructField {
                        name,
                        rename,
                        pattern,
                    } = field;

                    match (rename, pattern) {
                        (Some(rename), Some(pattern)) => {
                            scope.fragment(format!("{name}: "));
                            pattern.write_ruby(scope);
                            scope.fragment(format!(" => {rename}"));
                        }
                        (Some(rename), None) => scope.fragment(format!("{name}: {rename}")),
                        (None, Some(pattern)) => {
                            scope.fragment(format!("{name}: "));
                            pattern.write_ruby(scope);
                        }
                        _ => scope.fragment(format!("{name}:")),
                    }

                    if index < fields.len() - 1 {
                        scope.fragment(", ");
                    }
                }

                scope.fragment(")");
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CtorFields {
    Unit,
    Tuple(Vec<ExprIdent>),
    Struct(Vec<CtorStructField>),
    Wildcard,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CtorStructField {
    pub(crate) name: ExprIdent,
    pub(crate) rename: Option<ExprIdent>,
    pub(crate) pattern: Option<Pattern>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub(crate) pattern: Pattern,
    pub(crate) body: Expr,
    pub(crate) span: SimpleSpan,
}

impl MatchArm {
    pub fn parser<'a>(
        expr: impl Parser<
            'a,
            MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
            Expr,
            extra::Err<Rich<'a, Token, SimpleSpan>>,
        > + Clone
        + 'a,
        is_bracketed: bool,
    ) -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        Pattern::parser(expr.clone(), is_bracketed)
            .then_ignore(just(Token::Arrow))
            .then(expr)
            .map_with(|(pattern, body), extra| Self {
                pattern,
                body,
                span: extra.span(),
            })
            .labelled("match arm")
            .as_context()
            .boxed()
    }
}

impl WriteRuby for MatchArm {
    fn write_ruby(&self, scope: &mut Scope) {
        if let Pattern::Wildcard = self.pattern {
            scope.fragment("else");
        } else {
            scope.fragment("in ");
        }

        self.pattern.write_ruby(scope);

        scope.newline();

        scope.block(|scope| {
            self.body.write_ruby(scope);
        });
    }
}
