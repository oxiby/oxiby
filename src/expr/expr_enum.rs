use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::{Expr, ExprIdent, Noun, check_records, infer_function};
use crate::token::Token;
use crate::types::TypeIdent;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprEnum {
    ty: TypeIdent,
    variant: TypeIdent,
    constructor: EnumConstructor,
    pub(crate) span: SimpleSpan,
}

impl ExprEnum {
    pub fn parser<'a>(
        expr: impl Parser<
            'a,
            MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
            Expr,
            extra::Err<Rich<'a, Token, SimpleSpan>>,
        > + Clone,
    ) -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        TypeIdent::parser()
            .then_ignore(just(Token::Dot))
            .then(TypeIdent::parser())
            .then(
                choice((
                    expr.clone()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .map(EnumConstructor::Tuple),
                    ExprIdent::parser()
                        .then_ignore(just(Token::Assign))
                        .then(expr)
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LBrace), just(Token::RBrace))
                        .map(EnumConstructor::Struct),
                ))
                .or_not(),
            )
            .map_with(|((ty, variant), constructor), extra| Self {
                ty,
                variant,
                constructor: constructor.unwrap_or(EnumConstructor::Unit),
                span: extra.span(),
            })
            .labelled("enum construction")
            .as_context()
    }
}

impl WriteRuby for ExprEnum {
    fn write_ruby(&self, scope: &mut Scope) {
        self.ty.write_ruby(scope);
        scope.fragment(format!("::{}.allocate", self.variant));

        if self.constructor == EnumConstructor::Unit {
            return;
        }

        scope.fragment(".tap { |__oxiby_new| __oxiby_new.send(:initialize, ");

        match &self.constructor {
            EnumConstructor::Unit => unreachable!("checked for this case above"),
            EnumConstructor::Tuple(exprs) => {
                for (index, expr) in exprs.iter().enumerate() {
                    expr.write_ruby(scope);

                    if index < exprs.len() - 1 {
                        scope.fragment(", ");
                    }
                }
            }
            EnumConstructor::Struct(fields) => {
                for (index, (name, value)) in fields.iter().enumerate() {
                    scope.fragment(format!("{name}: "));
                    value.write_ruby(scope);

                    if index < fields.len() - 1 {
                        scope.fragment(", ");
                    }
                }
            }
        }

        scope.fragment(") }");
    }
}

impl Infer for ExprEnum {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let name = self.ty.as_str();

        let (ty, members) = match checker.get_type_constructor(name) {
            Some((ty, members)) => (ty.clone(), members.clone()),
            None => {
                return Err(Error::build("Unknown type")
                    .with_detail(&format!("Type `{name}` is not in scope."), self.span)
                    .with_help("You might need to import this type from another module.")
                    .finish());
            }
        };

        let variant_name = self.variant.as_str();

        let Some(variant_ty) = members.get_value_constructor(variant_name) else {
            return Err(Error::build("Unknown variant")
                .with_detail(
                    &format!("Enum `{name}` has no `{variant_name}` variant."),
                    self.span,
                )
                .with_help(&format!(
                    "`{name}` has the following variants: {}.",
                    members.value_constructor_names().join(", ")
                ))
                .finish());
        };

        match &self.constructor {
            EnumConstructor::Unit => {
                if !matches!(variant_ty, check::Type::Constructor(_)) {
                    return Err(Error::build("Invalid enum variant literal")
                        .with_detail(
                            &format!(
                                "Variant `{variant_name}` is not a unit variant and cannot be \
                                 constructed with the syntax `{variant_name}`."
                            ),
                            self.span,
                        )
                        .with_help(
                            &(if let Some(check::Type::Fn(_)) =
                                members.get_value_constructor(variant_name)
                            {
                                format!("Try using tuple variant syntax: `{variant_name}(...)`")
                            } else {
                                format!(
                                    "Try using record variant syntax: `{variant_name} {{ ... }}`"
                                )
                            }),
                        )
                        .finish());
                }
            }
            EnumConstructor::Tuple(fields) => {
                let check::Type::Fn(function) = variant_ty else {
                    return Err(Error::build("Invalid enum variant literal")
                        .with_detail(
                            &format!(
                                "Variant `{variant_name}` is not a tuple variant and cannot be \
                                 constructed with the syntax `{variant_name}(...)`."
                            ),
                            self.span,
                        )
                        .with_help(
                            &(if matches!(variant_ty, check::Type::RecordStruct { .. }) {
                                format!(
                                    "Try using record variant syntax: `{variant_name} {{ ... }}`"
                                )
                            } else {
                                format!(
                                    "Try using unit variant syntax by omitting the parenthesized \
                                     arguments: `{variant_name}`"
                                )
                            }),
                        )
                        .finish());
                };

                infer_function(checker, function, fields.iter(), self.span, Noun::Variant)?;
            }
            EnumConstructor::Struct(records) => {
                let check::Type::RecordStruct { fields, .. } = variant_ty else {
                    return Err(Error::build("Invalid enum variant literal")
                        .with_detail(
                            &format!(
                                "Variant `{variant_name}` is not a record variant and cannot be \
                                 constructed with the syntax `{variant_name} {{ ... }}`."
                            ),
                            self.span,
                        )
                        .with_help(
                            &(if let Some(check::Type::Fn(_)) =
                                members.get_value_constructor(variant_name)
                            {
                                format!("Try using tuple variant syntax: `{variant_name}(...)`")
                            } else {
                                format!(
                                    "Try using unit variant syntax by omitting the braced fields: \
                                     `{variant_name}`"
                                )
                            }),
                        )
                        .finish());
                };

                check_records(
                    checker,
                    variant_name,
                    fields.iter(),
                    records.iter(),
                    self.span,
                    true,
                )?;
            }
        }

        // TODO: Return real type.
        Ok(ty.clone())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumConstructor {
    Unit,
    Tuple(Vec<Expr>),
    Struct(Vec<(ExprIdent, Expr)>),
}
