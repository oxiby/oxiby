use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprMap<'a> {
    pub(crate) pairs: Vec<(Expr<'a>, Expr<'a>)>,
    pub(crate) span: SimpleSpan,
}

impl WriteRuby for ExprMap<'_> {
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
