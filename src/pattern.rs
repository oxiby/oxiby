use std::collections::HashSet;
use std::fmt::Display;

use chumsky::input::MappedInput;
use chumsky::prelude::*;

use crate::check::{self, Checker};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
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
                .map_with(move |patterns, extra| {
                    Self::Tuple(PatternTuple {
                        patterns,
                        is_bracketed,
                        span: extra.span(),
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
                .map_with(|((parent_ty_ident, ty_ident), fields), extra| PatternCtor {
                    parent_ty_ident,
                    ty_ident,
                    fields: fields.unwrap_or(CtorFields::Unit),
                    span: extra.span(),
                })
                .labelled("constructor pattern")
                .as_context()
                .boxed(),
        );

        pattern_parser.labelled("pattern").boxed()
    }

    pub fn span(&self) -> SimpleSpan {
        match self {
            Self::Literal(pattern_literal) => pattern_literal.span(),
            Self::Type(pattern_type) => pattern_type.span,
            Self::Ident(pattern_ident) => pattern_ident.span,
            Self::Tuple(pattern_tuple) => pattern_tuple.span,
            Self::Ctor(pattern_ctor) => pattern_ctor.span,
            Self::Wildcard => self.span(),
        }
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

    pub fn span(&self) -> SimpleSpan {
        match self {
            Self::Boolean(expr_boolean) => expr_boolean.span,
            Self::Integer(expr_integer) => expr_integer.span,
            Self::Float(expr_float) => expr_float.span,
            Self::String(expr_string) => expr_string.span,
        }
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
    span: SimpleSpan,
}

impl PatternIdent {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        ExprIdent::parser()
            .map_with(|ident, extra| Self {
                ident,
                span: extra.span(),
            })
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
    span: SimpleSpan,
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
            .map_with(|(ident, ty), extra| Self {
                ident,
                ty,
                span: extra.span(),
            })
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
    span: SimpleSpan,
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
    span: SimpleSpan,
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

pub fn match_bindings(
    checker: &mut Checker,
    expr_ty: &check::Type,
    pattern: &Pattern,
    span: SimpleSpan,
    expr_span: SimpleSpan,
    pattern_span: SimpleSpan,
) -> Result<Vec<(String, check::Type)>, Error> {
    let mut bindings = Vec::new();

    match &pattern {
        Pattern::Ident(pattern_ident) => {
            bindings.push((pattern_ident.ident.to_string(), expr_ty.clone()));
        }
        // TODO: Ensure the scrutinee matches literal patterns.
        Pattern::Literal(_) | Pattern::Wildcard => {}
        Pattern::Tuple(pattern_tuple) => {
            let check::Type::Tuple(tys) = expr_ty else {
                return Err(Error::type_mismatch()
                    .with_detail(
                        &format!(
                            "A value of type `{expr_ty}` cannot be destructured into a tuple.",
                        ),
                        span,
                    )
                    .with_context(
                        &format!("The expression being matched is of type `{expr_ty}`..."),
                        expr_span,
                    )
                    .with_context(
                        "...but this match arm tries to destructure it as a tuple.",
                        pattern_span,
                    )
                    .finish());
            };

            if pattern_tuple.patterns.len() != tys.len() {
                return Err(Error::type_mismatch()
                    .with_detail(
                        "Expected tuple of size {}, found one of size {}.",
                        pattern_span,
                    )
                    .finish());
            }

            for (pattern, ty) in pattern_tuple.patterns.iter().zip(tys.iter()) {
                match pattern {
                    Pattern::Ident(pattern_ident) => {
                        bindings.push((pattern_ident.ident.to_string(), ty.clone()));
                    }
                    _ => todo!("TODO: Unhandled pattern within tuple pattern: {pattern:?}"),
                }
            }
        }
        Pattern::Ctor(pattern_ctor) => {
            let name = pattern_ctor
                .parent_ty_ident
                .as_ref()
                .unwrap_or(&pattern_ctor.ty_ident)
                .as_str();

            let ctor_ty = match checker.get_type_constructor(name) {
                Some((ctor_ty, members)) => {
                    if pattern_ctor.parent_ty_ident.is_some() {
                        let value_ctor_name = pattern_ctor.ty_ident.as_str();

                        match members.get_value_constructor(value_ctor_name) {
                            Some(ctor_ty) => ctor_ty.clone(),
                            None => {
                                return Err(Error::build("Unknown variant")
                                    .with_detail(
                                        &format!(
                                            "Type `{name}` has no variant `{value_ctor_name}`."
                                        ),
                                        span,
                                    )
                                    .with_help(&format!(
                                        "Available variants: {}.",
                                        members.value_constructor_names().join(", ")
                                    ))
                                    .finish());
                            }
                        }
                    } else {
                        ctor_ty.clone()
                    }
                }
                None => match checker.get_value_constructor(name) {
                    Some(ctor_ty) => ctor_ty.clone(),
                    None => {
                        return Err(Error::build("Unknown type")
                            .with_detail(&format!("Type `{name}` is not in scope."), span)
                            .with_help("You might need to import this type from another module.")
                            .finish());
                    }
                },
            };

            match (&pattern_ctor.fields, ctor_ty) {
                (CtorFields::Unit, _ty) => {}
                (CtorFields::Tuple(idents), ty) => {
                    let check::Type::Fn(function) = &ty else {
                        todo!(
                            "TODO: Is it possible for a CtorFields;:Tuple to contain something \
                             other than a function?"
                        );
                    };

                    if idents.len() != function.positional_params.len() {
                        return Err(Error::build("Wrong number of fields")
                            .with_detail(
                                &format!(
                                    "Tuple type `{ty}` requires {} {}, but the pattern contained \
                                     {}",
                                    if function.positional_params.len() == 1 {
                                        "field"
                                    } else {
                                        "fields"
                                    },
                                    function.positional_params.len(),
                                    idents.len(),
                                ),
                                pattern_span,
                            )
                            .finish());
                    }

                    for (ident, ty) in idents.iter().zip(function.positional_params.iter()) {
                        bindings.push((ident.ident.clone(), ty.clone()));
                    }
                }
                (
                    CtorFields::Struct(pattern_fields),
                    check::Type::RecordStruct(ty_ctor, ty_fields),
                ) => {
                    let mut seen_fields = HashSet::new();

                    for pattern_field in pattern_fields {
                        let Some((_, field_ty)) = ty_fields
                            .iter()
                            .find(|(name, _)| name == pattern_field.name.as_str())
                        else {
                            return Err(Error::build("Unknown field")
                                .with_detail(
                                    &format!(
                                        "Type `{}` has no field `{}`.",
                                        ty_ctor.base_name(),
                                        pattern_field.name.as_str(),
                                    ),
                                    pattern_field.name.span,
                                )
                                .finish());
                        };

                        seen_fields.insert(pattern_field.name.to_string());

                        // TODO: Check for a subpattern.
                        // TODO: This variable needs to be in a new scope.
                        bindings.push((
                            pattern_field
                                .rename
                                .as_ref()
                                .unwrap_or(&pattern_field.name)
                                .to_string(),
                            field_ty.clone(),
                        ));
                    }

                    let unmatched_fields = ty_fields
                        .iter()
                        .filter_map(|(name, _ty)| {
                            if seen_fields.contains(name) {
                                None
                            } else {
                                Some(name)
                            }
                        })
                        .collect::<Vec<_>>();

                    if !unmatched_fields.is_empty() {
                        return Err(Error::build("Missing fields")
                            .with_detail(
                                &format!(
                                    "Type `{}` has additional fields that must be destructured in \
                                     the pattern: {}.",
                                    ty_ctor.base_name(),
                                    unmatched_fields
                                        .iter()
                                        .map(|name| format!("`{name}`"))
                                        .collect::<Vec<_>>()
                                        .join(", "),
                                ),
                                span,
                            )
                            .finish());
                    }
                }
                (CtorFields::Wildcard, _ty) => {
                    // TODO: Check ctor arity?
                }
                unhandled_ty => todo!("TODO: Unhandled ctor pattern: {unhandled_ty:?}"),
            }
        }
        Pattern::Type(pattern_type) => {
            let name: check::Type = pattern_type.ty.clone().into();

            let ascribed_ty = match checker.get_type_constructor(&name.base_name()) {
                Some((ascribed_ty, _members)) => ascribed_ty.clone(),
                None => {
                    return Err(Error::build("Unknown type")
                        .with_detail(&format!("Type `{name}` is not in scope."), span)
                        .finish());
                }
            };

            if ascribed_ty != *expr_ty {
                return Err(Error::type_mismatch()
                    .with_detail(
                        &format!(
                            "Expression was expected to be `{ascribed_ty}` but was `{expr_ty}`."
                        ),
                        span,
                    )
                    .finish());
            }

            bindings.push((pattern_type.ident.to_string(), expr_ty.clone()));
        }
    }

    Ok(bindings)
}
