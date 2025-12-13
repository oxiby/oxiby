use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIndex {
    pub(crate) expr: Box<Expr>,
    pub(crate) index: Box<Expr>,
    pub(crate) span: SimpleSpan,
}

impl WriteRuby for ExprIndex {
    fn write_ruby(&self, scope: &mut Scope) {
        self.expr.write_ruby(scope);
        scope.fragment("[");
        self.index.write_ruby(scope);
        scope.fragment("]");
    }
}

impl Infer for ExprIndex {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let expr_ty = self.expr.infer(checker)?;
        let index_ty = (*self.index).infer(checker)?;

        if let check::Type::Generic(generic_ty, ty_params) = expr_ty.clone() {
            match *generic_ty {
                check::Type::Constructor(name) if name == "List" => match index_ty {
                    check::Type::Primitive(check::PrimitiveType::Integer) => {
                        return Ok(ty_params[0].clone());
                    }
                    _ => {
                        return Err(Error::build("Invalid index")
                            .with_detail(
                                &format!("`{index_ty}` cannot be used to index a list."),
                                self.index.span(),
                            )
                            .with_context(
                                &format!("This expression is of type `{expr_ty}`."),
                                self.expr.span(),
                            )
                            .with_help("Try indexing the list with a value of type `Integer`.")
                            .finish());
                    }
                },
                check::Type::Constructor(ref name) if name == "Map" => {
                    if index_ty == ty_params[0] {
                        return Ok(ty_params[1].clone());
                    }

                    return Err(Error::build("Invalid index")
                        .with_detail(
                            &format!(
                                "`{index_ty}` cannot used to index a map with `{}` keys.",
                                ty_params[0]
                            ),
                            self.index.span(),
                        )
                        .with_context(
                            &format!("This expression is of type `{expr_ty}`."),
                            self.expr.span(),
                        )
                        .with_help(&format!(
                            "Try indexing this map with a value of type `{}`.",
                            ty_params[0]
                        ))
                        .finish());
                }
                _ => (),
            }
        }

        Err(Error::build("Invalid index")
            .with_detail(
                &format!("Cannot index into a value of type `{expr_ty}`."),
                self.expr.span(),
            )
            .with_context("The index expression is found here.", self.index.span())
            .with_note("Only lists and maps can be indexed.")
            .finish())
    }
}
