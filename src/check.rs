#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt::Display;

use chumsky::span::{SimpleSpan, Span};
use itertools::{EitherOrBoth, Itertools};

use crate::ast::Operator;
use crate::error::Error;
use crate::expr::{Expr, ExprBinary, ExprCall, ExprConditional, ExprLet};
use crate::item::{Item, ItemFn};
use crate::pattern::Pattern;

#[derive(Debug, Clone)]
pub struct Context(Vec<Entry>);

impl Context {
    fn get(&self, name: &str) -> Option<Type> {
        self.0
            .iter()
            .rev()
            .find(|var| var.0 == name)
            .map(|entry| entry.1.clone())
    }

    fn find(&self, name: &str, span: SimpleSpan) -> Result<Type, Error> {
        self.get(name).ok_or_else(|| {
            Error::build("Unknown binding")
                .detail(
                    &format!("Cannot find binding `{name}` in this scope."),
                    span,
                )
                .finish()
        })
    }

    fn push<S>(&mut self, name: S, ty: Type)
    where
        S: Into<String>,
    {
        self.0.push(Entry(name.into(), ty));
    }
}

#[derive(Debug, Clone)]
pub struct Entry(String, Type);

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct TypeVar(usize);

#[derive(Debug, Clone, Hash, PartialEq)]
enum Type {
    Constructor(String),
    Tuple(Vec<Type>),
    Fn(Func),
}

impl Type {
    fn constructor<S>(s: S) -> Self
    where
        S: Into<String>,
    {
        Self::Constructor(s.into())
    }

    fn unit() -> Self {
        Self::Tuple(Vec::with_capacity(0))
    }

    fn is_unit(&self) -> bool {
        matches!(self, Self::Tuple(types) if types.is_empty())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Constructor(name) => name,
            Self::Tuple(types) => &format!(
                "({})",
                types
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Fn(func) => &format!("<function \"{}\">", func.name),
        };

        write!(f, "{s}")
    }
}

impl From<crate::types::Type<'_>> for Type {
    fn from(value: crate::types::Type) -> Self {
        match value {
            crate::types::Type::Concrete(concrete_type) => {
                Self::constructor(concrete_type.ident.to_string())
            }
            crate::types::Type::Tuple(types) => {
                let types: Vec<_> = types.into_iter().map(Into::into).collect();

                if types.is_empty() {
                    Self::unit()
                } else {
                    Self::Tuple(types)
                }
            }
            _ => todo!("Only concrete types can be convereted to crate::check::Type right now"),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
struct Func {
    name: String,
    positional_params: Vec<Type>,
    keyword_params: Vec<(String, Type)>,
    return_type: Box<Type>,
}

impl Func {
    pub fn new(
        name: String,
        positional_params: Vec<Type>,
        keyword_params: Vec<(String, Type)>,
        return_type: Type,
    ) -> Self {
        Self {
            name,
            positional_params,
            keyword_params,
            return_type: Box::new(return_type),
        }
    }
}

#[derive(Debug)]
pub struct Checker {
    counter: usize,
    type_constructors: HashMap<String, Type>,
    value_constructors: HashMap<String, String>,
    functions: HashMap<String, Func>,
}

impl Checker {
    pub fn new() -> Self {
        let mut this = Self {
            counter: 0,
            type_constructors: HashMap::new(),
            value_constructors: HashMap::new(),
            functions: HashMap::new(),
        };

        let boolean = Type::constructor("Boolean");
        let float = Type::constructor("Float");
        let integer = Type::constructor("Integer");
        let string = Type::constructor("String");

        this.type_constructors.insert("Boolean".to_owned(), boolean);
        this.type_constructors.insert("Float".to_owned(), float);
        this.type_constructors.insert("Integer".to_owned(), integer);
        this.type_constructors
            .insert("String".to_owned(), string.clone());

        this.functions.insert(
            "print_line".to_owned(),
            Func::new("print_line".to_owned(), vec![string], vec![], Type::unit()),
        );

        this
    }

    pub fn create_type_var(&mut self) -> TypeVar {
        let type_var = TypeVar(self.counter);

        self.counter += 1;

        type_var
    }

    pub fn infer(&mut self, items: Vec<Item>) -> Result<(), Error> {
        // First pass: Collect function signatures.
        for item in &items {
            if let Item::Fn(item_fn) = item {
                let pos: Vec<_> = item_fn
                    .signature
                    .positional_params
                    .iter()
                    .cloned()
                    .map(|param| param.ty.into())
                    .collect();

                let kw: Vec<_> = item_fn
                    .signature
                    .keyword_params
                    .iter()
                    .cloned()
                    .map(|param| (param.ident.to_string(), param.ty.into()))
                    .collect();

                let ret = item_fn
                    .signature
                    .return_ty
                    .clone()
                    .map_or_else(Type::unit, Into::into);

                let name = item_fn.signature.name.to_string();

                if name == "main" && !(pos.is_empty() && kw.is_empty() && ret.is_unit()) {
                    let mut error = Error::build("Invalid signature for `main`")
                        .detail(
                            "The `main` function cannot have input or output.",
                            item_fn.signature.span,
                        )
                        .into_contextual();

                    if !pos.is_empty() {
                        error = error.with_context(
                            if pos.len() == 1 {
                                "This positional parameter is not allowed."
                            } else {
                                "These positional parameters are not allowed."
                            },
                            item_fn
                                .signature
                                .positional_params
                                .iter()
                                .map(|fn_param| fn_param.span)
                                .reduce(|merged, span| merged.union(span))
                                .expect("should be called on non-empty positional params"),
                        );
                    }

                    if !kw.is_empty() {
                        error = error.with_context(
                            if kw.len() == 1 {
                                "This keyword parameter is not allowed."
                            } else {
                                "These keyword parameters are not allowed."
                            },
                            item_fn
                                .signature
                                .keyword_params
                                .iter()
                                .map(|fn_param| fn_param.span)
                                .reduce(|merged, span| merged.union(span))
                                .expect("should be called on non-empty keyword params"),
                        );
                    }

                    if !ret.is_unit() {
                        error = error.with_context(
                            "Must be `()` or explicit return type must be omitted.",
                            item_fn
                                .signature
                                .return_ty
                                .as_ref()
                                .expect("should be called on a non-unit type")
                                .span()
                                .expect("should be called on a non-unit type"),
                        );
                    }

                    return Err(error.finish());
                }

                self.functions
                    .insert(name.clone(), Func::new(name, pos, kw, ret));
            }
        }

        // Second pass: Check function bodies.
        for item in items {
            if let Item::Fn(item_fn) = item {
                let mut context = Context(
                    self.functions
                        .iter()
                        .map(|(name, func)| Entry(name.clone(), Type::Fn(func.clone())))
                        .collect(),
                );

                for param in item_fn
                    .signature
                    .positional_params
                    .iter()
                    .chain(item_fn.signature.keyword_params.iter())
                {
                    context.push(param.ident.to_string(), param.ty.clone().into());
                }

                self.check_item_fn(&item_fn, &mut context)?;
            }
        }

        Ok(())
    }

    fn check_item_fn(&self, item_fn: &ItemFn<'_>, context: &mut Context) -> Result<(), Error> {
        let mut inferred = Type::unit();

        for expr in &item_fn.body {
            match &expr {
                // Literals
                Expr::Boolean(_) => {
                    inferred = self
                        .type_constructors
                        .get("Boolean")
                        .expect("`Boolean` should be known")
                        .clone();
                }
                Expr::Integer(_) => {
                    inferred = self
                        .type_constructors
                        .get("Integer")
                        .expect("`Integer` should be known")
                        .clone();
                }
                Expr::Float(_) => {
                    inferred = self
                        .type_constructors
                        .get("Float")
                        .expect("`Float` should be known")
                        .clone();
                }
                Expr::String(_) => {
                    inferred = self
                        .type_constructors
                        .get("String")
                        .expect("`String` should be known")
                        .clone();
                }
                Expr::Range(_) => {
                    inferred = self
                        .type_constructors
                        .get("Range")
                        .expect("`Range` should be known")
                        .clone();
                }

                // Identifiers
                Expr::ExprIdent(expr_ident) => {
                    let name = expr_ident.as_str();

                    inferred = context.find(name, expr.span())?;
                }

                // Calls
                Expr::Call(expr_call) => self.check_expr_call(expr_call, context, expr.span())?,

                // Control flow
                Expr::Conditional(expr_conditional) => {
                    self.check_expr_conditional(expr_conditional, context, expr.span())?;
                }

                // Patterns
                Expr::Let(expr_let) => self.check_expr_let(expr_let, context, expr.span())?,

                _ => todo!("Type checking not yet implemented for expression {expr:?}"),
            }
        }

        let declared = item_fn
            .signature
            .return_ty
            .clone()
            .map_or_else(Type::unit, Into::into);

        if inferred != declared {
            return Err(Error::type_mismatch()
                .detail(
                    &format!(
                        "Return type is declared as `{declared}` but the inferred type is \
                         `{inferred}`."
                    ),
                    item_fn
                        .signature
                        .return_ty
                        .as_ref()
                        .map_or(item_fn.signature.span, |ty| {
                            ty.span().unwrap_or(item_fn.signature.span)
                        }),
                )
                .finish());
        }

        Ok(())
    }

    fn check_expr_call(
        &self,
        expr_call: &ExprCall<'_>,
        context: &Context,
        span: SimpleSpan,
    ) -> Result<(), Error> {
        let name = expr_call.name.as_str();

        match context.find(name, span)? {
            Type::Fn(func) => {
                for pair in func
                    .positional_params
                    .iter()
                    .zip_longest(expr_call.positional_args.iter())
                {
                    match pair {
                        EitherOrBoth::Both(ty, expr) => {
                            let expr_ty = self.infer_expr_type(expr, context)?;

                            if *ty != expr_ty {
                                return Err(Error::type_mismatch()
                                    .detail(
                                        &format!(
                                            "Argument was expected to be `{ty}` but was \
                                             `{expr_ty}`."
                                        ),
                                        expr.span(),
                                    )
                                    .finish());
                            }
                        }
                        EitherOrBoth::Left(ty) => {
                            return Err(Error::build("Missing argument")
                                .detail(
                                    &format!(
                                        "Function expects argument of type `{ty}` but it was not \
                                         given."
                                    ),
                                    span,
                                )
                                .finish());
                        }
                        EitherOrBoth::Right(expr) => {
                            let expr_ty = self.infer_expr_type(expr, context)?;

                            return Err(Error::build("Extra argument")
                                .detail(
                                    &format!(
                                        "Argument of type `{expr_ty}` is not expected by function \
                                         `{name}`."
                                    ),
                                    expr.span(),
                                )
                                .finish());
                        }
                    }
                }
            }
            ty => {
                return Err(Error::type_mismatch()
                    .detail(
                        &format!(
                            "Value `{name}` is of type `{ty}` but is being called as a function."
                        ),
                        span,
                    )
                    .finish());
            }
        }

        Ok(())
    }

    fn check_expr_let(
        &self,
        expr_let: &ExprLet<'_>,
        context: &mut Context,
        _span: SimpleSpan,
    ) -> Result<(), Error> {
        match &expr_let.pattern {
            Pattern::Ident(pattern_ident) => {
                context.push(
                    pattern_ident.ident.to_string(),
                    self.infer_expr_type(&expr_let.body, context)?,
                );
            }
            Pattern::Literal(_) | Pattern::Wildcard => (),
            pattern => todo!("Type checking is not yet implemented for pattern {pattern:?}"),
        }

        Ok(())
    }

    fn check_expr_conditional(
        &self,
        expr_conditional: &ExprConditional<'_>,
        context: &mut Context,
        _span: SimpleSpan,
    ) -> Result<(), Error> {
        let condition_type = self.infer_expr_type(&expr_conditional.condition, context)?;

        if condition_type
            != *self
                .type_constructors
                .get("Boolean")
                .expect("`Boolean` should be known")
        {
            return Err(Error::type_mismatch()
                .detail(
                    &format!("Condition was expected to be `Boolean` but was `{condition_type}`",),
                    expr_conditional.condition.span(),
                )
                .finish());
        }

        // TODO: Check condition body.

        Ok(())
    }

    fn infer_expr_type(&self, expr: &Expr<'_>, context: &Context) -> Result<Type, Error> {
        let ty = match expr {
            // Literals
            Expr::Boolean(..) => Type::constructor("Boolean"),
            Expr::Float(..) => Type::constructor("Float"),
            Expr::Integer(..) => Type::constructor("Integer"),
            Expr::String(..) => Type::constructor("String"),
            Expr::ExprIdent(ident) => context.find(ident.as_str(), expr.span())?,

            // Calls
            Expr::Call(expr_call) => match context.find(expr_call.name.as_str(), expr.span())? {
                Type::Fn(func) => *func.return_type,
                ty => {
                    return Err(Error::type_mismatch()
                        .detail(
                            &format!(
                                "Value `{}` is of type `{ty}` but is being called as a function.",
                                expr_call.name.as_str()
                            ),
                            expr.span(),
                        )
                        .finish());
                }
            },

            // Misc.
            Expr::Binary(expr_binary) => self.infer_expr_binary_type(expr_binary, context)?,

            _ => todo!("Type inference not yet implemented for expression {expr:?}"),
        };

        Ok(ty)
    }

    fn infer_expr_binary_type(
        &self,
        expr_binary: &ExprBinary<'_>,
        context: &Context,
    ) -> Result<Type, Error> {
        let lhs_type = self.infer_expr_type(&expr_binary.lhs, context)?;
        let rhs_type = self.infer_expr_type(&expr_binary.rhs, context)?;

        // For now, assume that lhs implements the operator and that rhs is the appropriate type.
        Ok(match expr_binary.op {
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
            | Operator::Gt => Type::constructor("Boolean"),
            Operator::Mul | Operator::Div | Operator::Mod | Operator::Add | Operator::Sub => {
                Type::constructor("Integer")
            }
            Operator::Not => unreachable!("Cannot have a binary `Not` expression."),
        })
    }
}
