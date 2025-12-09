use chumsky::span::SimpleSpan;

use crate::ast::Operator;
use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary<'a> {
    pub(crate) op: Operator,
    pub(crate) lhs: Box<Expr<'a>>,
    pub(crate) rhs: Box<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl WriteRuby for ExprBinary<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        self.lhs.write_ruby(scope);
        scope.fragment(format!(" {} ", self.op));
        self.rhs.write_ruby(scope);
    }
}

impl Infer for ExprBinary<'_> {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        let lhs_type = self.lhs.infer(checker, context)?;
        let rhs_type = self.rhs.infer(checker, context)?;

        // For now, assume that lhs implements the operator and that rhs is the appropriate type.
        Ok(match &self.op {
            Operator::Assign => rhs_type,
            Operator::AddAssign
            | Operator::DivAssign
            | Operator::MultAssign
            | Operator::SubAssign => lhs_type,
            Operator::And
            | Operator::Or
            | Operator::Eq
            | Operator::Ne
            | Operator::LtEq
            | Operator::GtEq
            | Operator::Lt
            | Operator::Gt => check::Type::boolean(),
            Operator::Mul | Operator::Div | Operator::Mod | Operator::Add | Operator::Sub => {
                check::Type::integer()
            }
            Operator::Not => unreachable!("Cannot have a binary `Not` expression."),
        })
    }
}
