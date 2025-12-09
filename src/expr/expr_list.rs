use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprList {
    pub(crate) exprs: Vec<Expr>,
    pub(crate) span: SimpleSpan,
}

impl WriteRuby for ExprList {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("[");
        for (index, expr) in self.exprs.iter().enumerate() {
            expr.write_ruby(scope);

            if index < self.exprs.len() - 1 {
                scope.fragment(", ");
            }
        }
        scope.fragment("]");
    }
}

impl Infer for ExprList {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        let (inferred, span) = match self.exprs.first() {
            Some(expr) => (expr.infer(checker, context)?, expr.span()),
            None => {
                return Ok(check::Type::Generic(
                    Box::new(check::Type::constructor("List")),
                    vec![check::Type::unit()],
                ));
            }
        };

        for expr in self.exprs.iter().skip(1) {
            let next_inferred = expr.infer(checker, context)?;

            if inferred != next_inferred {
                return Err(Error::type_mismatch()
                    .with_detail("All list elements must be of the same type", self.span)
                    .with_context(
                        &format!("The first element of this list is of type `{inferred}`..."),
                        span,
                    )
                    .with_context(
                        &format!("...but this element of the list is of type `{next_inferred}`."),
                        expr.span(),
                    )
                    .finish());
            }
        }

        Ok(check::Type::Generic(
            Box::new(check::Type::constructor("List")),
            vec![inferred],
        ))
    }
}
