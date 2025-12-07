use chumsky::input::BorrowInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::{Expr, ExprIdent, check_records, infer_function};
use crate::token::Token;
use crate::types::TypeIdent;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprEnum<'a> {
    ty: TypeIdent<'a>,
    variant: TypeIdent<'a>,
    constructor: EnumConstructor<'a>,
    pub(crate) span: SimpleSpan,
}

impl<'a> ExprEnum<'a> {
    pub fn parser<I>(
        expr: impl Parser<'a, I, Expr<'a>, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
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

impl WriteRuby for ExprEnum<'_> {
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

impl Infer for ExprEnum<'_> {
    fn infer(&self, checker: &Checker, context: &mut Context) -> Result<check::Type, Error> {
        let name = self.ty.as_str();

        let Some((ty, members)) = checker.type_constructors.get(name) else {
            return Err(Error::build("Unknown type")
                .with_detail(&format!("Type `{name}` is not in scope."), self.span)
                .with_help("You might need to import this type from another module.")
                .finish());
        };

        let variant_name = self.variant.as_str();

        let Some(variant_ty) = members.value_constructors.get(variant_name) else {
            return Err(Error::build("Unknown variant")
                .with_detail(
                    &format!("Enum `{name}` has no `{variant_name}` variant."),
                    self.span,
                )
                .with_help(&format!(
                    "`{name}` has the following variants: {}.",
                    members
                        .value_constructors
                        .keys()
                        .map(|key| format!("`{key}`"))
                        .collect::<Vec<_>>()
                        .join(", ")
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
                        .finish());
                };

                infer_function(checker, context, function, fields.iter(), self.span, true)?;
            }
            EnumConstructor::Struct(records) => {
                let check::Type::RecordStruct(_variant_name_ty, fields) = variant_ty else {
                    return Err(Error::build("Invalid enum variant literal")
                        .with_detail(
                            &format!(
                                "Variant `{variant_name}` is not a record variant and cannot be \
                                 constructed with the syntax `{variant_name} {{ ... }}`."
                            ),
                            self.span,
                        )
                        .finish());
                };

                check_records(
                    checker,
                    context,
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
pub enum EnumConstructor<'a> {
    Unit,
    Tuple(Vec<Expr<'a>>),
    Struct(Vec<(ExprIdent<'a>, Expr<'a>)>),
}
