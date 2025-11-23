#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt::Display;

use chumsky::error::Rich;
use chumsky::span::SimpleSpan;

use crate::expr::Expr;
use crate::item::{Item, ItemFn};

pub type Error = Rich<'static, String>;

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
        self.get(name)
            .ok_or_else(|| Rich::custom(span, format!("Cannot find value `{name}` in this scope")))
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
            Self::Fn(_) => "<function>",
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
                    .positional_params
                    .iter()
                    .cloned()
                    .map(|param| param.ty.into())
                    .collect();

                let kw: Vec<_> = item_fn
                    .keyword_params
                    .iter()
                    .cloned()
                    .map(|param| (param.ident.to_string(), param.ty.into()))
                    .collect();

                let ret = item_fn
                    .return_ty
                    .clone()
                    .map_or_else(Type::unit, Into::into);

                let name = item_fn.name.to_string();

                if name == "main" && !(pos.is_empty() && kw.is_empty() && ret.is_unit()) {
                    return Err(Rich::custom(
                        item_fn.span,
                        "The `main` function cannot have parameters and must return ()".to_owned(),
                    ));
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
                    .positional_params
                    .iter()
                    .chain(item_fn.keyword_params.iter())
                {
                    context.push(param.ident.to_string(), param.ty.clone().into());
                }

                Self::check_fn(item_fn, &context)?;
            }
        }

        Ok(())
    }

    fn check_fn(item_fn: ItemFn<'_>, context: &Context) -> Result<(), Error> {
        if let Some(exprs) = item_fn.body {
            for expr in exprs {
                match &expr {
                    Expr::Call(expr_call) => {
                        let name = expr_call.name.as_str();

                        match context.find(name, expr.span())? {
                            Type::Fn(func) => {
                                for (ty, expr) in func
                                    .positional_params
                                    .iter()
                                    .zip(expr_call.positional_args.iter())
                                {
                                    let expr_ty = Self::infer_expr_type(expr, context)?;

                                    if *ty != expr_ty {
                                        return Err(Rich::custom(
                                            expr.span(),
                                            format!(
                                                "Argument was expected to be `{ty}` but was \
                                                 `{expr_ty}`",
                                            ),
                                        ));
                                    }
                                }
                            }
                            _ => {
                                return Err(Rich::custom(
                                    expr.span(),
                                    format!("Value `{name}` is not callable"),
                                ));
                            }
                        }
                    }
                    Expr::ExprIdent(expr_ident) => {
                        let name = expr_ident.as_str();

                        context.find(name, expr.span())?;
                    }
                    _ => todo!("Not all expressions can be type checked yet"),
                }
            }
        }

        Ok(())
    }

    fn infer_expr_type(expr: &Expr<'_>, context: &Context) -> Result<Type, Error> {
        let ty = match expr {
            Expr::Boolean(..) => Type::constructor("Boolean"),
            Expr::Float(..) => Type::constructor("Float"),
            Expr::Integer(..) => Type::constructor("Integer"),
            Expr::String(..) => Type::constructor("String"),
            Expr::ExprIdent(ident) => context.find(ident.as_str(), expr.span())?,
            _ => todo!("Not all expressions can be inferred yet"),
        };

        Ok(ty)
    }
}
