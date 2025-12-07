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
    fn infer(&self, checker: &Checker, context: &mut Context) -> Result<check::Type, Error> {
        let name = self.ty.to_string();

        let Some((ty, _members)) = checker.type_constructors.get(&name) else {
            return Err(Error::build("Unknown type")
                .with_detail(&format!("Type `{name}` is not in scope."), self.span)
                .with_help("You might need to import this type from another module.")
                .finish());
        };

        let check::Type::RecordStruct(ty, fields) = ty else {
            return Err(Error::build("Invalid struct literal")
                .with_detail(
                    &format!(
                        "Type `{ty}` is not a record struct and cannot be constructed with the \
                         syntax `{ty} {{ ... }}`."
                    ),
                    self.span,
                )
                .finish());
        };

        let mut expr_fields = HashMap::new();
        let mut expr_field_names = HashSet::with_capacity(self.fields.len());

        for expr_field in &self.fields {
            let name = expr_field.0.as_str();
            expr_fields.insert(
                name.to_string(),
                (
                    expr_field.1.clone(),
                    expr_field.0.span.union(expr_field.1.span()),
                ),
            );
            expr_field_names.insert(name);
        }

        for field in fields {
            let name = &field.0;

            let Some(expr_field) = expr_fields.get(name) else {
                return Err(Error::build("Missing field")
                    .with_detail(
                        &format!("Field `{name}` of struct `{ty}` must be given."),
                        self.span,
                    )
                    .finish());
            };

            let expr_type = expr_field.0.infer(checker, context)?;

            if expr_type != field.1 {
                return Err(Error::type_mismatch()
                    .with_detail(
                        &format!(
                            "Field `{name}` of type `{ty}` is type `{}`, but a value of type \
                             `{expr_type}` was given.",
                            field.1
                        ),
                        expr_field.0.span(),
                    )
                    .finish());
            }

            expr_field_names.remove(name.as_str());
        }

        if let Some(name) = expr_field_names.into_iter().next() {
            return Err(Error::build("Unknown field")
                .with_detail(
                    &format!("Struct {ty} has no field named {name}."),
                    expr_fields
                        .get(name)
                        .expect(
                            "should be present because the key was generated from the collection",
                        )
                        .1,
                )
                .finish());
        }

        Ok(*ty.clone())
    }
}
