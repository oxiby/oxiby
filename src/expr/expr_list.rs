use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprList<'a> {
    pub(crate) exprs: Vec<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl WriteRuby for ExprList<'_> {
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
