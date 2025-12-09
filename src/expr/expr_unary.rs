use chumsky::span::SimpleSpan;

use crate::ast::Operator;
use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary<'a> {
    pub(crate) op: Operator,
    pub(crate) expr: Box<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl WriteRuby for ExprUnary<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment(self.op.to_string());
        self.expr.write_ruby(scope);
    }
}

impl Infer for ExprUnary<'_> {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        let ty = (*self.expr).infer(checker, context)?;

        match self.op {
            Operator::Sub => match ty {
                check::Type::Primitive(primitive)
                    if primitive == check::PrimitiveType::Integer
                        || primitive == check::PrimitiveType::Float => {}
                _ => {
                    return Err(Error::build("Invalid unary expression")
                        .with_detail(
                            &format!(
                                "The `-` unary operator can only be applied to expressions of \
                                 type `Integer` or `Float`, but was applied to expression of type \
                                 `{ty}`."
                            ),
                            self.span,
                        )
                        .finish());
                }
            },
            Operator::Not => {
                if matches!(ty, check::Type::Primitive(primitive) if primitive == check::PrimitiveType::Boolean)
                {
                } else {
                    return Err(Error::build("Invalid unary expression")
                        .with_detail(
                            &format!(
                                "The `!` unary operator can only be applied to expressions of \
                                 type `Boolean`, but was applied to expression of type `{ty}`."
                            ),
                            self.span,
                        )
                        .finish());
                }
            }
            operator => {
                return Err(Error::build("Invalid unary expression")
                    .with_detail(
                        &format!("`{operator}` cannot be used in prefix position."),
                        self.span,
                    )
                    .finish());
            }
        }

        Ok(ty)
    }
}
