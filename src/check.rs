#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt::Display;

use chumsky::span::{SimpleSpan, Span};

use crate::error::Error;
use crate::item::{Item, ItemFn, Variant};

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
    Primitive(PrimitiveType),
    Constructor(String),
    Generic(Box<Type>, Vec<Type>),
    Variable(String),
    Tuple(Vec<Type>),
    RecordStruct(Box<Type>, Vec<(String, Type)>),
    Fn(Function),
}

impl Type {
    pub fn boolean() -> Self {
        Self::Primitive(PrimitiveType::Boolean)
    }

    pub fn float() -> Self {
        Self::Primitive(PrimitiveType::Float)
    }

    pub fn integer() -> Self {
        Self::Primitive(PrimitiveType::Integer)
    }

    pub fn string() -> Self {
        Self::Primitive(PrimitiveType::String)
    }

    pub fn range() -> Self {
        Self::Primitive(PrimitiveType::Range)
    }

    pub fn list() -> Self {
        Self::Generic(
            Box::new(Self::constructor("List")),
            vec![Self::variable("t")],
        )
    }

    pub fn constructor<S>(s: S) -> Self
    where
        S: Into<String>,
    {
        Self::Constructor(s.into())
    }

    pub fn name(&self) -> String {
        match self {
            Self::Primitive(primitive_type) => primitive_type.to_string(),
            Self::Constructor(name) => name.to_string(),
            Self::Generic(ty, _) | Self::RecordStruct(ty, _) => ty.to_string(),
            Self::Variable(variable) => variable.to_string(),
            Self::Tuple(_) => "<tuple name placeholder>".to_string(),
            Self::Fn(_) => "<function name placeholder>".to_string(),
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
            Self::Primitive(primitive_type) => &format!("{primitive_type}"),
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
            Self::RecordStruct(ty, _) => &format!("{ty}",),
            Self::Fn(func) => match &func.name {
                Some(name) => &format!("<function \"{name}\">"),
                None => "<function>",
            },
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
            crate::types::Type::Variable(expr_ident) => Self::Variable(expr_ident.to_string()),
            crate::types::Type::Tuple(types) => {
                let types: Vec<_> = types.into_iter().map(Into::into).collect();

                if types.is_empty() {
                    Self::unit()
                } else {
                    Self::Tuple(types)
                }
            }
            crate::types::Type::Fn(maybe_params, maybe_return_ty) => {
                let function = Function::new(
                    None,
                    true,
                    maybe_params.map_or_else(Vec::new, |params| {
                        params.into_iter().map(From::from).collect()
                    }),
                    Vec::new(),
                    maybe_return_ty.map_or_else(Self::unit, |return_ty| (*return_ty).into()),
                );
                Self::Fn(function)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum PrimitiveType {
    Boolean,
    Float,
    Integer,
    String,
    Range,
}

impl Display for PrimitiveType {
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
pub struct Function {
    pub(crate) name: Option<String>,
    pub(crate) is_static: bool,
    pub(crate) positional_params: Vec<Type>,
    pub(crate) keyword_params: Vec<(String, Type)>,
    pub(crate) return_type: Box<Type>,
}

impl Function {
    pub fn new<N>(
        name: N,
        is_static: bool,
        positional_params: Vec<Type>,
        keyword_params: Vec<(String, Type)>,
        return_type: Type,
    ) -> Self
    where
        N: Into<Option<String>>,
    {
        Self {
            name: name.into(),
            is_static,
            positional_params,
            keyword_params,
            return_type: Box::new(return_type),
        }
    }
    pub fn r#static(
        name: String,
        positional_params: Vec<Type>,
        keyword_params: Vec<(String, Type)>,
        return_type: Type,
    ) -> Self {
        Self {
            name: Some(name),
            is_static: true,
            positional_params,
            keyword_params,
            return_type: Box::new(return_type),
        }
    }
    pub fn instance(
        name: String,
        positional_params: Vec<Type>,
        keyword_params: Vec<(String, Type)>,
        return_type: Type,
    ) -> Self {
        Self {
            name: Some(name),
            is_static: false,
            positional_params,
            keyword_params,
            return_type: Box::new(return_type),
        }
    }
}

#[derive(Debug)]
pub struct TypeMembers {
    pub(crate) value_constructors: HashMap<String, Type>,
    pub(crate) functions: HashMap<String, Type>,
}

impl TypeMembers {
    pub fn new() -> Self {
        Self {
            value_constructors: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Checker {
    counter: usize,
    pub(crate) type_constructors: HashMap<String, (Type, TypeMembers)>,
    pub(crate) variables: HashMap<String, Type>,
}

impl Checker {
    pub fn new() -> Self {
        let mut this = Self {
            counter: 0,
            type_constructors: HashMap::new(),
            variables: HashMap::new(),
        };

        this.type_constructors
            .insert("Boolean".to_owned(), (Type::boolean(), TypeMembers::new()));
        this.type_constructors
            .insert("Float".to_owned(), (Type::float(), TypeMembers::new()));
        this.type_constructors
            .insert("Integer".to_owned(), (Type::integer(), TypeMembers::new()));
        this.type_constructors
            .insert("String".to_owned(), (Type::string(), TypeMembers::new()));
        this.type_constructors
            .insert("Range".to_owned(), (Type::range(), TypeMembers::new()));

        this.type_constructors
            .insert("List".to_owned(), (Type::list(), TypeMembers::new()));

        this.variables.insert(
            "print_line".to_owned(),
            Type::Fn(Function::r#static(
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
        // First pass: Collect declarations.
        for item in &items {
            if let Item::Fn(item_fn) = item {
                let (name, function) = collect_fn(item_fn)?;

                self.variables.insert(name, function);
            } else if let Item::Struct(item_struct) = item {
                let mut ty: Type = item_struct.ty.clone().into();

                let name = match ty {
                    Type::Primitive(primitive_type) => {
                        return Err(Error::build("Duplicate type definition")
                            .with_detail(
                                &format!(
                                    "This type name conflicts with the primitive type \
                                     `{primitive_type}`."
                                ),
                                item_struct.ty.span().unwrap_or((0..0).into()),
                            )
                            .finish());
                    }
                    Type::Constructor(ref name) => name.clone(),
                    _ => todo!("Type {ty} is not yet supported by the type checker"),
                };

                let mut members = TypeMembers::new();

                if let Some(fields) = item_struct.tuple_fields() {
                    ty = Type::constructor(&name);

                    let member_fields =
                        fields.iter().map(|field| field.ty.clone().into()).collect();

                    members.value_constructors.insert(
                        name.clone(),
                        Type::Fn(Function::r#static(
                            name.clone(),
                            member_fields,
                            Vec::new(),
                            ty.clone(),
                        )),
                    );
                } else if let Some(fields) = item_struct.record_fields() {
                    ty = Type::RecordStruct(
                        Box::new(ty),
                        fields
                            .iter()
                            .map(|field| (field.name.to_string(), field.ty.clone().into()))
                            .collect(),
                    );
                }

                if let Some(functions) = &item_struct.fns {
                    for function in functions {
                        let (name, func) = collect_fn(function)?;

                        members.functions.insert(name, func);
                    }
                }

                self.type_constructors.insert(name.clone(), (ty, members));
            } else if let Item::Enum(item_enum) = item {
                let ty: Type = item_enum.ty.clone().into();

                let name = match ty {
                    Type::Primitive(primitive_type) => {
                        return Err(Error::build("Duplicate type definition")
                            .with_detail(
                                &format!(
                                    "This type name conflicts with the primitive type \
                                     `{primitive_type}`."
                                ),
                                item_enum.ty.span().unwrap_or((0..0).into()),
                            )
                            .finish());
                    }
                    Type::Constructor(ref name) => name.clone(),
                    _ => todo!("Type {ty} is not yet supported by the type checker"),
                };

                let mut members = TypeMembers::new();

                for variant in &item_enum.variants {
                    match variant {
                        Variant::Unit(ty_ident, _) => {
                            let name = ty_ident.to_string();

                            members
                                .value_constructors
                                .insert(name.clone(), Type::constructor(name));
                        }
                        Variant::Tuple(ty_ident, fields, _) => {
                            let name = ty_ident.to_string();

                            let fields = fields.iter().map(|field| field.clone().into()).collect();

                            let variant_ty = Type::Fn(Function::r#static(
                                name.clone(),
                                fields,
                                Vec::new(),
                                ty.clone(),
                            ));

                            members.value_constructors.insert(name.clone(), variant_ty);
                        }
                        Variant::Record(ty_ident, records, _) => {
                            let name = ty_ident.to_string();

                            let fields = records
                                .iter()
                                .map(|record| (record.name.to_string(), record.ty.clone().into()))
                                .collect();

                            let variant_ty =
                                Type::RecordStruct(Box::new(Type::constructor(&name)), fields);

                            members.value_constructors.insert(name.clone(), variant_ty);
                        }
                    }
                }

                self.type_constructors.insert(name, (ty, members));
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

fn collect_fn(item_fn: &ItemFn<'_>) -> Result<(String, Type), Error> {
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

    let is_static = !item_fn.signature.self_param;

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

    Ok((
        name.clone(),
        Type::Fn(Function::new(name, is_static, pos, kw, ret)),
    ))
}
