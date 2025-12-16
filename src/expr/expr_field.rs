use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::{Expr, Noun, infer_function};

#[derive(Debug, Clone, PartialEq)]
pub struct ExprField {
    pub(crate) lhs: Box<Expr>,
    pub(crate) rhs: Box<Expr>,
    pub(crate) span: SimpleSpan,
}

impl WriteRuby for ExprField {
    fn write_ruby(&self, scope: &mut Scope) {
        match *self.lhs {
            Expr::ExprIdent(ref expr_ident) if expr_ident.as_str() == "self" => (),
            Expr::TypeIdent(ref expr_type_ident) => {
                let ident_str = expr_type_ident.as_str();

                let resolved_ident_string = match scope.resolve_ident(ident_str) {
                    Some((path, _kind)) => path,
                    None => ident_str.to_string(),
                };

                scope.fragment(resolved_ident_string);
                scope.fragment(".");
            }
            _ => {
                self.lhs.write_ruby(scope);
                scope.fragment(".");
            }
        }

        if let Expr::Integer(_) = *self.rhs {
            scope.fragment("__");
        }

        self.rhs.write_ruby(scope);
    }
}

impl Infer for ExprField {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let (lhs_ty, is_static) = match &*self.lhs {
            Expr::TypeIdent(expr_type_ident) => {
                let name = expr_type_ident.as_str();

                match checker.get_type_constructor(name) {
                    Some((lhs_ty, _members)) => (lhs_ty.clone(), true),
                    None => {
                        return Err(Error::build("Unknown type")
                            .with_detail(
                                &format!("Type `{name}` is not in scope."),
                                expr_type_ident.span,
                            )
                            .with_help("You might need to import this type from another module.")
                            .finish());
                    }
                }
            }
            lhs => (lhs.infer(checker)?, false),
        };

        let members = match checker.get_type_constructor(&lhs_ty.base_name()) {
            Some((_, members)) => members.clone(),
            None => todo!("TODO: No type constructor found for ExprField where LHS is {lhs_ty}"),
        };

        let rhs_ty = match &*self.rhs {
            Expr::Integer(expr_integer) => {
                let Some(check::Type::Fn(function)) =
                    members.get_value_constructor(&lhs_ty.base_name())
                else {
                    todo!(
                        "TODO: Field where RHS is an integer and LHS isn't a function/tuple struct"
                    );
                };

                let param_index = usize::try_from(expr_integer.value).map_err(|_| {
                    Error::build("Unknown field")
                        .with_detail(
                            &format!("Type `{}` has no field `{}`.", lhs_ty, expr_integer.value),
                            self.rhs.span(),
                        )
                        .finish()
                })?;

                match function.positional_params.get(param_index) {
                    Some(param_ty) => param_ty.clone(),
                    None => {
                        return Err(Error::build("Unknown field")
                            .with_detail(
                                &format!(
                                    "Type `{}` has no field `{}`.",
                                    lhs_ty, expr_integer.value
                                ),
                                self.rhs.span(),
                            )
                            .finish());
                    }
                }
            }
            Expr::Call(expr_call) => {
                let name = expr_call.name.as_str();

                let Some(rhs_ty) = members.get_function(name) else {
                    return Err(Error::build("Unknown method")
                        .with_detail(
                            &format!("Type `{lhs_ty}` has no method `{name}`."),
                            expr_call.span,
                        )
                        .finish());
                };

                let check::Type::Fn(function) = rhs_ty else {
                    todo!(
                        "TODO: In what situation would the type of a function stored in `members` \
                         not be check::Type::Fn?"
                    );
                };

                if is_static != function.is_static {
                    if function.is_static {
                        return Err(Error::build("Invalid method call")
                            .with_detail(
                                &format!(
                                    "`{name}` is being called as an instance method, but it is a \
                                     static method."
                                ),
                                self.rhs.span(),
                            )
                            .with_help(&format!("Try using the syntax `{lhs_ty}.{name}(...)`."))
                            .finish());
                    }

                    return Err(Error::build("Invalid method call")
                        .with_detail(
                            &format!(
                                "`{name}` is being called as a static method, but it is an \
                                 instance method."
                            ),
                            self.rhs.span(),
                        )
                        .with_help(&format!(
                            "Try calling the method on a value of type `{lhs_ty}`."
                        ))
                        .finish());
                }

                infer_function(
                    checker,
                    function,
                    expr_call.positional_args.iter(),
                    expr_call.span,
                    Noun::Function,
                )?
            }
            _ => todo!("TODO: Unhandled ExprField RHS"),
        };

        Ok(rhs_ty)
    }
}
