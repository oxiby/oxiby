use std::collections::HashSet;

use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::pattern::{CtorFields, Pattern};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprLet {
    pub(crate) pattern: Pattern,
    pub(crate) body: Box<Expr>,
    pub(crate) span: SimpleSpan,
}

impl ExprLet {
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
        just(Token::Let)
            .ignore_then(Pattern::parser(expr.clone(), false))
            .then_ignore(just(Token::Assign))
            .then(expr)
            .map_with(|(pattern, body), extra| Self {
                pattern,
                body: Box::new(body),
                span: extra.span(),
            })
            .labelled("let")
            .as_context()
    }

    fn write_body(&self, scope: &mut Scope) {
        if let Expr::TypeIdent(expr_type_ident) = &*self.body {
            scope.fragment(format!("{}.new", expr_type_ident.as_str()));
        } else {
            self.body.write_ruby(scope);
        }
    }
}

impl WriteRuby for ExprLet {
    fn write_ruby(&self, scope: &mut Scope) {
        match &self.pattern {
            Pattern::Type(pattern_type) => {
                pattern_type.write_ruby(scope);
                scope.fragment(" = ");
                self.write_body(scope);
            }
            Pattern::Ident(pattern_ident) => {
                pattern_ident.write_ruby(scope);
                scope.fragment(" = ");
                self.write_body(scope);
            }
            Pattern::Tuple(pattern_tuple) => {
                pattern_tuple.write_ruby(scope);
                scope.fragment(" = ");
                self.write_body(scope);
            }
            Pattern::Ctor(pattern_ctor) => {
                self.write_body(scope);
                scope.fragment(" => ");
                pattern_ctor.write_ruby(scope);
            }
            _ => todo!("pattern not yet implemented for let bindings"),
        }
    }
}

impl Infer for ExprLet {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let mut inferred = check::Type::unit();

        match &self.pattern {
            Pattern::Ident(pattern_ident) => {
                inferred = self.body.infer(checker)?;
                checker.push_term_var(pattern_ident.ident.to_string(), inferred.clone());
            }
            Pattern::Literal(_) | Pattern::Wildcard => (),
            Pattern::Tuple(pattern_tuple) => {
                inferred = self.body.infer(checker)?;

                let check::Type::Tuple(tys) = &inferred else {
                    return Err(Error::type_mismatch()
                        .with_detail(
                            &format!(
                                "Expected tuple due to tuple pattern destructuring, but found \
                                 `{inferred}`."
                            ),
                            self.body.span(),
                        )
                        .finish());
                };

                if pattern_tuple.patterns.len() != tys.len() {
                    return Err(Error::type_mismatch()
                        .with_detail(
                            "Expected tuple of size {}, found one of size {}.",
                            self.span,
                        )
                        .finish());
                }

                for (pattern, ty) in pattern_tuple.patterns.iter().zip(tys.iter()) {
                    match pattern {
                        Pattern::Ident(pattern_ident) => {
                            checker.push_term_var(pattern_ident.ident.to_string(), ty.clone());
                        }
                        _ => todo!("TODO: Unhandled pattern within tuple pattern: {pattern:?}"),
                    }
                }
            }
            Pattern::Ctor(pattern_ctor) => {
                let name = pattern_ctor.ty_ident.as_str();

                let (ctor_ty, _members) = match checker.get_type_constructor(name) {
                    Some((ctor_ty, members)) => (ctor_ty.clone(), members.clone()),
                    None => {
                        return Err(Error::build("Unknown type")
                            .with_detail(&format!("Type `{name}` is not in scope."), self.span)
                            .with_help("You might need to import this type from another module.")
                            .finish());
                    }
                };

                match (&pattern_ctor.fields, ctor_ty) {
                    (CtorFields::Unit, unhandled_ty) => {
                        todo!("TODO: Unit struct pattern against type {unhandled_ty}")
                    }
                    (CtorFields::Tuple(_idents), unhandled_ty) => {
                        todo!("TODO: Tuple struct pattern against type {unhandled_ty}")
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
                            checker.push_term_var(
                                pattern_field
                                    .rename
                                    .as_ref()
                                    .unwrap_or(&pattern_field.name)
                                    .to_string(),
                                field_ty.clone(),
                            );
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
                                        "Type `{}` has additional fields that must be \
                                         destructured in the pattern: {}.",
                                        ty_ctor.base_name(),
                                        unmatched_fields
                                            .iter()
                                            .map(|name| format!("`{name}`"))
                                            .collect::<Vec<_>>()
                                            .join(", "),
                                    ),
                                    self.span,
                                )
                                .finish());
                        }
                    }
                    (CtorFields::Wildcard, unhandled_ty) => {
                        todo!("TODO: Wildcard struct pattern against type {unhandled_ty}")
                    }
                    unhandled_ty => todo!("TODO: Unhandled ctor pattern: {unhandled_ty:?}"),
                }
            }
            Pattern::Type(pattern_type) => todo!(
                "TODO: Type checking for let bindings is not yet implemented for pattern \
                 {pattern_type:?}"
            ),
        }

        Ok(inferred)
    }
}
