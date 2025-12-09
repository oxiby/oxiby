use std::collections::{HashMap, HashSet};

use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::{Expr, ExprIdent};
use crate::token::Token;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStruct<'a> {
    ty: Type<'a>,
    fields: Vec<(ExprIdent<'a>, Expr<'a>)>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprStruct<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Type::parser()
            .then(
                ExprIdent::parser()
                    .then_ignore(just(Token::Assign))
                    .then(expr)
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map_with(|(ty, fields), extra| Self {
                ty,
                fields,
                span: extra.span(),
            })
            .labelled("struct construction")
            .as_context()
    }
}

impl WriteRuby for ExprStruct<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        let string = self.ty.to_string();

        if string == "Self" {
            scope.fragment("allocate");
        } else {
            let path = match scope.resolve_ident(&string) {
                Some((path, _kind)) => path,
                None => string,
            };

            scope.fragment(format!("{path}.allocate"));
        }

        if self.fields.is_empty() {
            return;
        }

        scope.fragment(".tap { |__oxiby_new| __oxiby_new.send(:initialize, ");

        for (index, (name, value)) in self.fields.iter().enumerate() {
            scope.fragment(format!("{name}: "));
            value.write_ruby(scope);

            if index < self.fields.len() - 1 {
                scope.fragment(", ");
            }
        }

        scope.fragment(") }");
    }
}

impl Infer for ExprStruct<'_> {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        let name = self.ty.to_string();

        let (ty, members) = match checker.type_constructors.get(&name) {
            Some((ty, members)) => (ty.clone(), members.clone()),
            None => {
                return Err(Error::build("Unknown type")
                    .with_detail(&format!("Type `{name}` is not in scope."), self.span)
                    .with_help("You might need to import this type from another module.")
                    .finish());
            }
        };

        let check::Type::RecordStruct(ty, fields) = ty else {
            return Err(Error::build("Invalid struct literal")
                .with_detail(
                    &format!(
                        "Struct `{ty}` is not a record struct and cannot be constructed with the \
                         syntax `{ty} {{ ... }}`."
                    ),
                    self.span,
                )
                .with_help(
                    &(if members.value_constructors.contains_key(&name) {
                        format!("Try using tuple struct syntax: `{ty}(...)`")
                    } else {
                        format!(
                            "Try using unit struct syntax by omitting the braced fields: `{ty}`"
                        )
                    }),
                )
                .finish());
        };

        check_records(
            checker,
            context,
            &name,
            fields.iter(),
            self.fields.iter(),
            self.span,
            false,
        )?;

        Ok(*ty.clone())
    }
}

pub fn check_records<'a>(
    checker: &mut Checker,
    context: &mut Context,
    name: &str,
    field_types: impl Iterator<Item = &'a (String, check::Type)>,
    field_values: impl Iterator<Item = &'a (ExprIdent<'a>, Expr<'a>)>,
    span: SimpleSpan,
    is_variant: bool,
) -> Result<(), Error> {
    let mut expr_fields = HashMap::new();
    let mut expr_field_names = HashSet::with_capacity(field_values.size_hint().0);

    for expr_field in field_values {
        let field_name = expr_field.0.as_str();

        expr_fields.insert(
            field_name.to_string(),
            (
                expr_field.1.clone(),
                expr_field.0.span.union(expr_field.1.span()),
            ),
        );

        expr_field_names.insert(field_name);
    }

    for field in field_types {
        let field_name = &field.0;

        let Some(expr_field) = expr_fields.get(field_name) else {
            return Err(Error::build("Missing field")
                .with_detail(
                    &format!(
                        "Field `{field_name}` of {} `{name}` must be given.",
                        if is_variant { "variant" } else { "struct" }
                    ),
                    span,
                )
                .finish());
        };

        let expr_type = expr_field.0.infer(checker, context)?;

        if expr_type != field.1 {
            return Err(Error::type_mismatch()
                .with_detail(
                    &format!(
                        "Field `{field_name}` of {} `{name}` is type `{}`, but a value of type \
                         `{expr_type}` was given.",
                        if is_variant { "variant" } else { "type" },
                        field.1
                    ),
                    expr_field.0.span(),
                )
                .finish());
        }

        expr_field_names.remove(field_name.as_str());
    }

    if let Some(field_name) = expr_field_names.into_iter().next() {
        return Err(Error::build("Extra field")
            .with_detail(
                &format!(
                    "{} `{name}` has no field named `{field_name}`.",
                    if is_variant { "Variant" } else { "Struct" }
                ),
                expr_fields
                    .get(field_name)
                    .expect("should be present because the key was generated from the collection")
                    .1,
            )
            .with_help(&format!("Try omitting the field `{field_name}`."))
            .finish());
    }

    Ok(())
}
