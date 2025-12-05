#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt::Display;

use chumsky::span::{SimpleSpan, Span};

use crate::error::Error;
use crate::item::Item;

pub trait Infer {
    fn infer(&self, checker: &Checker, context: &mut Context) -> Result<Type, Error>;
}

#[derive(Debug, Clone)]
pub struct Context(Vec<Entry>);

impl Context {
    pub fn get(&self, name: &str) -> Option<Type> {
        for entry in self.0.iter().rev() {
            if let Entry::TermVar(var, ty) = entry
                && var == name
            {
                return Some(ty.clone());
            }
        }

        None
    }

    pub fn find(&self, name: &str, span: SimpleSpan) -> Result<Type, Error> {
        self.get(name).ok_or_else(|| {
            Error::build("Unknown binding")
                .with_detail(
                    &format!("Cannot find binding `{name}` in this scope."),
                    span,
                )
                .finish()
        })
    }

    pub fn push_term_var<S>(&mut self, name: S, ty: Type)
    where
        S: Into<String>,
    {
        self.0.push(Entry::TermVar(name.into(), ty));
    }

    pub fn push_scope(&mut self) {
        self.0.push(Entry::Scope);
    }

    pub fn pop_scope(&mut self) {
        loop {
            if let Some(entry) = self.0.pop()
                && matches!(entry, Entry::Scope)
            {
                break;
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Entry {
    TermVar(String, Type),
    Scope,
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct TypeVar(usize);

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Type {
    Core(CoreType),
    Constructor(String),
    Generic(Box<Type>, Vec<Type>),
    Variable(String),
    Tuple(Vec<Type>),
    Fn(Func),
}

impl Type {
    pub fn boolean() -> Self {
        Self::Core(CoreType::Boolean)
    }

    pub fn float() -> Self {
        Self::Core(CoreType::Float)
    }

    pub fn integer() -> Self {
        Self::Core(CoreType::Integer)
    }

    pub fn string() -> Self {
        Self::Core(CoreType::String)
    }

    pub fn range() -> Self {
        Self::Core(CoreType::Range)
    }

    pub fn constructor<S>(s: S) -> Self
    where
        S: Into<String>,
    {
        Self::Constructor(s.into())
    }

    pub fn constructor_name(&self) -> Option<&str> {
        if let Self::Constructor(name) = self {
            Some(name)
        } else {
            None
        }
    }

    pub fn variable<S>(s: S) -> Self
    where
        S: Into<String>,
    {
        Self::Variable(s.into())
    }
    pub fn unit() -> Self {
        Self::Tuple(Vec::with_capacity(0))
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Tuple(types) if types.is_empty())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Core(core_type) => &format!("{core_type}"),
            Self::Constructor(name) => name,
            Self::Generic(ty, ty_vars) => &format!(
                "{}<{}>",
                ty,
                ty_vars
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Self::Variable(variable) => variable,
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
            crate::types::Type::Concrete(concrete_type) => match concrete_type.ident.as_str() {
                "Boolean" => Self::boolean(),
                "Float" => Self::float(),
                "Integer" => Self::integer(),
                "String" => Self::string(),
                "Range" => Self::range(),
                name => Self::constructor(name.to_string()),
            },
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

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum CoreType {
    Boolean,
    Float,
    Integer,
    String,
    Range,
}

impl Display for CoreType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Boolean => "Boolean",
            Self::Float => "Float",
            Self::Integer => "Integer",
            Self::String => "String",
            Self::Range => "Range",
        };

        write!(f, "{s}")
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Func {
    pub(crate) name: String,
    pub(crate) positional_params: Vec<Type>,
    pub(crate) keyword_params: Vec<(String, Type)>,
    pub(crate) return_type: Box<Type>,
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

pub struct Env {}

#[derive(Debug)]
pub struct Checker {
    counter: usize,
    pub(crate) type_constructors: HashMap<String, (Type, Vec<Type>)>,
    pub(crate) value_constructors: HashMap<String, Type>,
    pub(crate) variables: HashMap<String, Type>,
}

impl Checker {
    pub fn new() -> Self {
        let mut this = Self {
            counter: 0,
            type_constructors: HashMap::new(),
            value_constructors: HashMap::new(),
            variables: HashMap::new(),
        };

        this.type_constructors
            .insert("Boolean".to_owned(), (Type::boolean(), vec![]));
        this.type_constructors
            .insert("Float".to_owned(), (Type::float(), vec![]));
        this.type_constructors
            .insert("Integer".to_owned(), (Type::integer(), vec![]));
        this.type_constructors
            .insert("String".to_owned(), (Type::string(), vec![]));
        this.type_constructors
            .insert("Range".to_owned(), (Type::range(), vec![]));

        this.type_constructors.insert(
            "List".to_owned(),
            (Type::constructor("list"), vec![Type::variable("t")]),
        );

        this.variables.insert(
            "print_line".to_owned(),
            Type::Fn(Func::new(
                "print_line".to_owned(),
                vec![Type::string()],
                vec![],
                Type::unit(),
            )),
        );

        this
    }

    pub fn create_type_var(&mut self) -> TypeVar {
        let type_var = TypeVar(self.counter);

        self.counter += 1;

        type_var
    }

    pub fn check(&mut self, items: Vec<Item>) -> Result<(), Error> {
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
                    let mut error = Error::build("Invalid signature for `main`").with_detail(
                        "The `main` function cannot have input or output.",
                        item_fn.signature.span,
                    );

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

                self.variables
                    .insert(name.clone(), Type::Fn(Func::new(name, pos, kw, ret)));
            }
        }

        // Second pass: Check function bodies.
        for item in items {
            if let Item::Fn(item_fn) = item {
                let mut context = Context(
                    self.variables
                        .iter()
                        .map(|(name, ty)| Entry::TermVar(name.clone(), ty.clone()))
                        .collect(),
                );

                for param in item_fn
                    .signature
                    .positional_params
                    .iter()
                    .chain(item_fn.signature.keyword_params.iter())
                {
                    context.push_term_var(param.ident.to_string(), param.ty.clone().into());
                }

                item_fn.infer(self, &mut context)?;
            }
        }

        Ok(())
    }
}
