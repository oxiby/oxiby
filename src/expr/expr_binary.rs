use chumsky::span::SimpleSpan;

use crate::ast::Operator;
use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    pub(crate) op: Operator,
    pub(crate) lhs: Box<Expr>,
    pub(crate) rhs: Box<Expr>,
    pub(crate) span: SimpleSpan,
}

impl WriteRuby for ExprBinary {
    fn write_ruby(&self, scope: &mut Scope) {
        self.lhs.write_ruby(scope);
        scope.fragment(format!(" {} ", self.op));
        self.rhs.write_ruby(scope);
    }
}

impl Infer for ExprBinary {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let lhs_type = self.lhs.infer(checker)?;
        let rhs_type = self.rhs.infer(checker)?;

        Ok(match &self.op {
            Operator::Assign => rhs_type,
            Operator::AddAssign
            | Operator::DivAssign
            | Operator::MultAssign
            | Operator::SubAssign => {
                if lhs_type != rhs_type {
                    return Err(Error::type_mismatch()
                        .with_detail(
                            &format!(
                                "Expected right-hand side of `{}` operation to be `{lhs_type}`, \
                                 but was `{rhs_type}`.",
                                self.op,
                            ),
                            self.rhs.span(),
                        )
                        .finish());
                }

                lhs_type
            }
            Operator::And | Operator::Or => {
                if !lhs_type.is_boolean() {
                    return Err(Error::type_mismatch()
                        .with_detail(
                            &format!(
                                "Expected left-hand side of `{}` operation to be `Boolean`, but \
                                 was `{lhs_type}`.",
                                self.op,
                            ),
                            self.lhs.span(),
                        )
                        .finish());
                } else if !rhs_type.is_boolean() {
                    return Err(Error::type_mismatch()
                        .with_detail(
                            &format!(
                                "Expected right-hand side of `{}` operation to be `Boolean`, but \
                                 was `{rhs_type}`.",
                                self.op,
                            ),
                            self.rhs.span(),
                        )
                        .finish());
                }

                lhs_type
            }
            | Operator::Eq
            | Operator::Ne
            | Operator::LtEq
            | Operator::GtEq
            | Operator::Lt
            | Operator::Gt => check::Type::boolean(),
            Operator::Mul | Operator::Div | Operator::Mod | Operator::Add | Operator::Sub => {
                if lhs_type.is_variable() || rhs_type.is_variable() {
                    rhs_type
                } else {
                    if !(lhs_type.is_integer() || lhs_type.is_float()) {
                        return Err(Error::type_mismatch()
                            .with_detail(
                                &format!(
                                    "Expected left-hand side of `{}` operation to be `Integer` or \
                                     `Float`, but was `{lhs_type}`.",
                                    self.op
                                ),
                                self.lhs.span(),
                            )
                            .finish());
                    } else if !(rhs_type.is_integer() || rhs_type.is_float()) {
                        return Err(Error::type_mismatch()
                            .with_detail(
                                &format!(
                                    "Expected right-hand side of `+` operation to be `Integer` or \
                                     `Float`, but was `{rhs_type}`."
                                ),
                                self.rhs.span(),
                            )
                            .finish());
                    }

                    rhs_type
                }
            }
            Operator::Not => unreachable!("Cannot have a binary `Not` expression."),
        })
    }
}
