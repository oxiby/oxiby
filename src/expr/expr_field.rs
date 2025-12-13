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
        let ty: check::Type = if let Expr::TypeIdent(ref expr_type_ident) = *self.lhs {
            let (lhs_ty, members) = match checker.get_type_constructor(expr_type_ident.as_str()) {
                Some((lhs_ty, members)) => (lhs_ty.clone(), members.clone()),
                None => {
                    return Err(Error::build("Unknown type")
                        .with_detail(
                            &format!("Type `{}` is not in scope.", expr_type_ident.as_str()),
                            expr_type_ident.span,
                        )
                        .with_help("You might need to import this type from another module.")
                        .finish());
                }
            };

            if let Expr::Call(ref expr_call) = *self.rhs {
                let name = expr_call.name.as_str();

                let Some(rhs_ty) = members.get_function(name) else {
                    return Err(Error::build("Unknown method")
                        .with_detail(
                            &format!("Type `{lhs_ty}` does not have a method `{name}`.",),
                            expr_call.span,
                        )
                        .finish());
                };

                let check::Type::Fn(function) = rhs_ty else {
                    return Err(Error::type_mismatch()
                        .with_detail(
                            &format!(
                                "Value `{}` is of type `{lhs_ty}` but is being called as a \
                                 function.",
                                rhs_ty.full_name()
                            ),
                            self.span,
                        )
                        .finish());
                };

                if !function.is_static {
                    return Err(Error::build("Invalid static method call")
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
            } else {
                todo!(
                    "TODO: Inference for fields where `lhs` is a type identifier and `rhs` isn't \
                     a call."
                );
            }
        } else if let Expr::ExprIdent(ref expr_ident) = *self.lhs {
            let lhs_ty = checker.find_contextual(expr_ident.as_str(), expr_ident.span)?;

            let members = match checker.get_type_constructor(&lhs_ty.full_name()) {
                Some((_ty, members)) => members.clone(),
                None => panic!(
                    "Should always exist because we were able to find the type in the context."
                ),
            };

            if let Expr::Call(ref expr_call) = *self.rhs {
                let name = expr_call.name.as_str();

                let Some(rhs_ty) = members.get_function(name) else {
                    return Err(Error::build("Unknown method")
                        .with_detail(
                            &format!("Type `{lhs_ty}` does not have a method `{name}`.",),
                            expr_call.span,
                        )
                        .finish());
                };

                let check::Type::Fn(function) = rhs_ty else {
                    return Err(Error::type_mismatch()
                        .with_detail(
                            &format!(
                                "Value `{}` is of type `{rhs_ty}` but is being called as a \
                                 function.",
                                rhs_ty.full_name()
                            ),
                            self.span,
                        )
                        .finish());
                };

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
                infer_function(
                    checker,
                    function,
                    expr_call.positional_args.iter(),
                    expr_call.span,
                    Noun::Function,
                )?
            } else if let Expr::Integer(expr_integer) = &*self.rhs {
                let Some(check::Type::Fn(function)) =
                    members.get_value_constructor(&lhs_ty.base_name())
                else {
                    return Err(Error::build("Unknown field")
                        .with_detail(
                            &format!("Type `{}` has no field `{}`.", lhs_ty, expr_integer.value),
                            self.rhs.span(),
                        )
                        .finish());
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
                    Some(expr_ty) => expr_ty.clone(),
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
            } else {
                todo!(
                    "TODO: Inference for fields where `lhs` is an expression identifier and `rhs` \
                     isn't a call. rhs: {:?}",
                    self.rhs
                );
            }
        } else {
            todo!("TODO: Inference for fields where `lhs` isn't an identifier. Expr: {self:?}");
        };

        Ok(ty)
    }
}
