use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprMap {
    pub(crate) pairs: Vec<(Expr, Expr)>,
    pub(crate) span: SimpleSpan,
}

impl WriteRuby for ExprMap {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("{ ");
        for (index, (key, value)) in self.pairs.iter().enumerate() {
            key.write_ruby(scope);
            scope.fragment(" => ");
            value.write_ruby(scope);

            if index < self.pairs.len() - 1 {
                scope.fragment(", ");
            }
        }
        scope.fragment(" }");
    }
}

impl Infer for ExprMap {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let (inferred_key, key_span, inferred_value, value_span) = match self.pairs.first() {
            Some((key, value)) => (
                key.infer(checker)?,
                key.span(),
                value.infer(checker)?,
                value.span(),
            ),
            None => {
                return Ok(check::Type::Generic {
                    name: Box::new(check::Type::constructor("Map")),
                    params: vec![check::Type::variable("k"), check::Type::variable("v")],
                });
            }
        };

        for (key, value) in self.pairs.iter().skip(1) {
            let (next_inferred_key, next_inferred_value) =
                (key.infer(checker)?, value.infer(checker)?);

            if inferred_key != next_inferred_key {
                return Err(Error::type_mismatch()
                    .with_detail("All map keys must be of the same type", self.span)
                    .with_context(
                        &format!("The first pair's key is of type `{inferred_key}`..."),
                        key_span,
                    )
                    .with_context(
                        &format!("...but this pair's key is of type `{next_inferred_key}`."),
                        key.span(),
                    )
                    .finish());
            }

            if inferred_value != next_inferred_value {
                return Err(Error::type_mismatch()
                    .with_detail("All map values must be of the same type", self.span)
                    .with_context(
                        &format!("The first pair's value is of type `{inferred_value}`..."),
                        value_span,
                    )
                    .with_context(
                        &format!("...but this pair's value is of type `{next_inferred_value}`."),
                        value.span(),
                    )
                    .finish());
            }
        }

        Ok(check::Type::Generic {
            name: Box::new(check::Type::constructor("Map")),
            params: vec![inferred_key, inferred_value],
        })
    }
}
