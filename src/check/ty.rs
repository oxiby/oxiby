use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};

use chumsky::span::SimpleSpan;

use crate::item::Item;
use crate::module::{Module, ModulePath};

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Constructor(String),
    Generic {
        name: Box<Type>,
        params: Vec<Type>,
    },
    Variable(String),
    Tuple(Vec<Type>),
    RecordStruct {
        name: Box<Type>,
        fields: Vec<(String, Type)>,
    },
    Fn(Function),
    Import {
        module_name: String,
        import_name: String,
        variant: Option<String>,
        rename: Option<String>,
    },
}

impl Type {
    pub fn boolean() -> Self {
        Self::Primitive(PrimitiveType::Boolean)
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::Boolean))
    }

    pub fn float() -> Self {
        Self::Primitive(PrimitiveType::Float)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::Float))
    }

    pub fn integer() -> Self {
        Self::Primitive(PrimitiveType::Integer)
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::Integer))
    }

    pub fn string() -> Self {
        Self::Primitive(PrimitiveType::String)
    }

    #[expect(dead_code)]
    pub fn is_string(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::String))
    }

    pub fn range() -> Self {
        Self::Primitive(PrimitiveType::Range)
    }

    #[expect(dead_code)]
    pub fn is_range(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::Range))
    }

    pub fn list() -> Self {
        Self::Generic {
            name: Box::new(Self::constructor("List")),
            params: vec![Self::variable("t")],
        }
    }

    #[expect(dead_code)]
    pub fn is_list(&self) -> bool {
        if let Self::Generic {
            name: constructor, ..
        } = self
            && let Self::Constructor(ref name) = **constructor
            && name == "List"
        {
            return true;
        }

        false
    }

    pub fn constructor<S>(s: S) -> Self
    where
        S: Into<String>,
    {
        Self::Constructor(s.into())
    }

    pub fn base_name(&self) -> String {
        match self {
            Self::Primitive(primitive_type) => primitive_type.to_string(),
            Self::Constructor(name) => name.clone(),
            Self::Generic { name, .. } | Self::RecordStruct { name, .. } => name.base_name(),
            Self::Variable(variable) => variable.clone(),
            Self::Tuple(_) => "<tuple name placeholder>".to_string(),
            Self::Fn(_) => "<function name placeholder>".to_string(),
            Self::Import {
                module_name,
                import_name,
                variant,
                rename,
            } => format!(
                "{module_name} {import_name}{}{}",
                if let Some(variant) = variant {
                    format!(".{variant}")
                } else {
                    String::new()
                },
                if let Some(rename) = rename {
                    format!(" -> {rename}")
                } else {
                    String::new()
                },
            ),
        }
    }

    pub fn full_name(&self) -> String {
        match self {
            Self::Primitive(primitive_type) => primitive_type.to_string(),
            Self::Constructor(name) => name.clone(),
            ty @ Self::Generic { .. } => ty.to_string(),
            Self::RecordStruct { name, .. } => name.to_string(),
            Self::Variable(variable) => variable.clone(),
            Self::Tuple(_) => "<tuple name placeholder>".to_string(),
            Self::Fn(_) => "<function name placeholder>".to_string(),
            Self::Import {
                module_name,
                import_name,
                variant,
                rename,
            } => format!(
                "{module_name} {import_name}{}{}",
                if let Some(variant) = variant {
                    format!(".{variant}")
                } else {
                    String::new()
                },
                if let Some(rename) = rename {
                    format!(" -> {rename}")
                } else {
                    String::new()
                },
            ),
        }
    }

    pub fn import<M, N>(module_name: M, import_name: N) -> Self
    where
        M: Into<String>,
        N: Into<String>,
    {
        Self::Import {
            module_name: module_name.into(),
            import_name: import_name.into(),
            variant: None,
            rename: None,
        }
    }

    pub fn import_renamed<M, N, R>(module_name: M, import_name: N, rename: R) -> Self
    where
        M: Into<String>,
        N: Into<String>,
        R: Into<String>,
    {
        Self::Import {
            module_name: module_name.into(),
            import_name: import_name.into(),
            variant: None,
            rename: Some(rename.into()),
        }
    }

    pub fn import_variant<M, N, V>(module_name: M, import_name: N, variant: V) -> Self
    where
        M: Into<String>,
        N: Into<String>,
        V: Into<String>,
    {
        Self::Import {
            module_name: module_name.into(),
            import_name: import_name.into(),
            variant: Some(variant.into()),
            rename: None,
        }
    }

    pub fn import_variant_renamed<M, N, V, R>(
        module_name: M,
        import_name: N,
        variant: V,
        rename: R,
    ) -> Self
    where
        M: Into<String>,
        N: Into<String>,
        V: Into<String>,
        R: Into<String>,
    {
        Self::Import {
            module_name: module_name.into(),
            import_name: import_name.into(),
            variant: Some(variant.into()),
            rename: Some(rename.into()),
        }
    }

    pub fn variable<S>(s: S) -> Self
    where
        S: Into<String>,
    {
        Self::Variable(s.into())
    }

    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable(_))
    }

    pub fn unit() -> Self {
        Self::Tuple(Vec::with_capacity(0))
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Tuple(types) if types.is_empty())
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }

        match (self, other) {
            // Reflexive cases
            (Self::Primitive(a), Self::Primitive(b)) if a == b => true,
            (Self::Constructor(a), Self::Constructor(b)) if a == b => true,
            (Self::Variable(a), Self::Variable(b)) if a == b => true,

            // Generics
            (
                Self::Generic {
                    name: t,
                    params: t_params,
                },
                Self::Generic {
                    name: u,
                    params: u_params,
                },
            ) if t == u => {
                for (t_param, u_param) in t_params.iter().zip(u_params.iter()) {
                    if !t_param.is_subtype_of(u_param) {
                        return false;
                    }
                }

                true
            }

            // Any type is a subtype of an unconstrained type variable
            (_, Self::Variable(_)) => true,

            // Anything without an explicit rule above is not a subtype
            _ => false,
        }
    }

    pub fn substitute(&self, variable: &str, replacement: &Self) -> Self {
        let mut substituted = self.clone();

        match substituted {
            Self::Generic { ref mut params, .. } => {
                for param in params {
                    if matches!(param, Type::Variable(name) if name == variable) {
                        *param = replacement.clone();
                    }
                }
            }
            Self::RecordStruct {
                ref mut name,
                ref mut fields,
            } => {
                **name = name.substitute(variable, replacement);

                for (_name, field_ty) in fields {
                    *field_ty = field_ty.substitute(variable, replacement);
                }
            }
            Self::Primitive(_primitive_type) => {}
            Self::Variable(ref old_variable) => {
                if old_variable == variable {
                    substituted = replacement.clone();
                }
            }
            _ => {
                todo!("TODO: Type variable substitution for {self}");
            }
        }

        substituted
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Primitive(primitive_type) => &format!("{primitive_type}"),
            Self::Constructor(name) => name,
            Self::Generic { name, params } => &format!(
                "{}<{}>",
                name,
                params
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Self::Variable(_) => "type variable",
            Self::Tuple(types) => &format!(
                "({})",
                types
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::RecordStruct { name, .. } => &format!("{name}",),
            Self::Fn(func) => match &func.name {
                Some(name) => &format!("<function \"{name}\">"),
                None => "<function>",
            },
            Self::Import {
                module_name,
                import_name,
                variant,
                rename,
            } => &format!(
                "<import module=\"{module_name}\" import=\"{import_name}\"{}{}>",
                if let Some(variant) = variant {
                    format!(" variant=\"{variant}\"")
                } else {
                    String::new()
                },
                if let Some(rename) = rename {
                    format!(" rename=\"{rename}\"")
                } else {
                    String::new()
                },
            ),
        };

        write!(f, "{s}")
    }
}

impl From<crate::types::Type> for Type {
    fn from(value: crate::types::Type) -> Self {
        match value {
            crate::types::Type::Concrete(concrete_type) => match concrete_type.ident.as_str() {
                "Boolean" => Self::boolean(),
                "Float" => Self::float(),
                "Integer" => Self::integer(),
                "String" => Self::string(),
                "Range" => Self::range(),
                name => {
                    let constructor = Self::constructor(name.to_string());

                    if let Some(params) = concrete_type.params {
                        Self::Generic {
                            name: Box::new(constructor),
                            params: params.iter().cloned().map(Into::into).collect(),
                        }
                    } else {
                        constructor
                    }
                }
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
    name: Option<String>,
    is_static: bool,
    positional_params: Vec<Type>,
    keyword_params: Vec<(String, Type)>,
    return_type: Box<Type>,
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

    #[expect(dead_code)]
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

    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn has_name(&self) -> bool {
        self.name.is_some()
    }

    pub fn is_static(&self) -> bool {
        self.is_static
    }

    pub fn positional_params_count(&self) -> usize {
        self.positional_params.len()
    }

    pub fn get_positional_param(&self, index: usize) -> Option<&Type> {
        self.positional_params.get(index)
    }

    pub fn positional_params(&self) -> impl Iterator<Item = &Type> {
        self.positional_params.iter()
    }

    pub fn positional_params_mut(&mut self) -> impl Iterator<Item = &mut Type> {
        self.positional_params.iter_mut()
    }

    pub fn return_type(&self) -> &Type {
        &self.return_type
    }

    fn substitute(&self, variable: &str, replacement: &Type) -> Self {
        let mut substituted = self.clone();

        for param in &mut substituted.positional_params {
            if matches!(param, Type::Variable(name) if name == variable) {
                *param = replacement.clone();
            }
        }

        for (_, param) in &mut substituted.keyword_params {
            if matches!(param, Type::Variable(name) if name == variable) {
                *param = replacement.clone();
            }
        }

        *substituted.return_type = substituted.return_type.substitute(variable, replacement);

        substituted
    }
}

#[derive(Clone, Debug)]
pub struct TypeMembers {
    value_constructors: HashMap<String, Type>,
    functions: HashMap<String, Type>,
}

impl TypeMembers {
    pub fn new() -> Self {
        Self {
            value_constructors: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn has_value_constructors(&self) -> bool {
        self.value_constructors.is_empty()
    }

    pub fn has_value_constructor(&self, name: &str) -> bool {
        self.value_constructors.contains_key(name)
    }

    pub fn get_value_constructor(&self, name: &str) -> Option<&Type> {
        self.value_constructors.get(name)
    }

    pub fn value_constructor_names(&self) -> Vec<String> {
        self.value_constructors
            .keys()
            .map(|key| format!("`{key}`"))
            .collect::<Vec<_>>()
    }

    pub fn add_value_constructor<N>(&mut self, name: &N, ty: Type)
    where
        N: ToString,
    {
        self.value_constructors.insert(name.to_string(), ty);
    }

    pub fn get_function(&self, name: &str) -> Option<&Type> {
        self.functions.get(name)
    }

    pub fn add_function<N>(&mut self, name: &N, ty: Type)
    where
        N: ToString,
    {
        self.functions.insert(name.to_string(), ty);
    }

    pub fn substitute(&self, variable: &str, replacement: &Type) -> Self {
        let mut substituted = self.clone();

        for ctor_ty in substituted.value_constructors.values_mut() {
            match ctor_ty {
                Type::Fn(function) => {
                    *ctor_ty = Type::Fn(function.substitute(variable, replacement));
                }
                _ => todo!(
                    "TypeMembers::substitute can only replace value constructors that are \
                     functions."
                ),
            }
        }

        for fn_ty in substituted.functions.values_mut() {
            match fn_ty {
                Type::Fn(function) => {
                    *fn_ty = Type::Fn(function.substitute(variable, replacement));
                }
                _ => todo!(
                    "TypeMembers::substitute can only replace value constructors that are \
                     functions."
                ),
            }
        }

        substituted
    }
}

#[derive(Clone)]
pub struct ModuleTypes {
    module: Module,
    is_entry_module: bool,
    type_constructors: HashMap<String, (Type, TypeMembers)>,
    value_constructors: HashMap<String, Type>,
    functions: HashMap<String, Type>,
    closures: HashSet<SimpleSpan>,
}

impl ModuleTypes {
    pub fn new(module: Module) -> Self {
        let is_entry_module = module.is_entry_module();

        Self {
            module,
            is_entry_module,
            type_constructors: HashMap::new(),
            value_constructors: HashMap::new(),
            functions: HashMap::new(),
            closures: HashSet::new(),
        }
    }

    pub fn module(&self) -> &Module {
        &self.module
    }

    pub fn module_path(&self) -> &ModulePath {
        self.module.module_path()
    }

    pub fn is_entry_module(&self) -> bool {
        self.is_entry_module
    }

    pub fn is_std(&self) -> bool {
        self.module.is_std()
    }

    pub fn get_type_constructor(&self, name: &str) -> Option<(&Type, &TypeMembers)> {
        self.type_constructors.get(name).map(|(ty, mem)| (ty, mem))
    }

    pub fn add_type_constructor<N>(&mut self, name: &N, contents: (Type, TypeMembers))
    where
        N: ToString + ?Sized,
    {
        self.type_constructors.insert(name.to_string(), contents);
    }

    pub fn get_value_constructor(&self, name: &str) -> Option<&Type> {
        self.value_constructors.get(name)
    }

    pub fn add_value_constructor<N>(&mut self, name: &N, ty: Type)
    where
        N: ToString + ?Sized,
    {
        self.value_constructors.insert(name.to_string(), ty);
    }

    pub fn functions(&self) -> impl Iterator<Item = (&String, &Type)> {
        self.functions.iter()
    }

    pub fn get_function(&self, name: &str) -> Option<&Type> {
        self.functions.get(name)
    }

    pub fn add_function<N>(&mut self, name: &N, ty: Type)
    where
        N: ToString + ?Sized,
    {
        self.functions.insert(name.to_string(), ty);
    }

    pub fn mark_closure(&mut self, span: SimpleSpan) {
        self.closures.insert(span);
    }

    pub fn items(&self) -> &[Item] {
        self.module.items()
    }

    pub fn into_module(mut self) -> Module {
        self.module.extend_closures(self.closures);

        self.module
    }
}

// Manual impl of Debug so all the items in the module aren't printed.
impl Debug for ModuleTypes {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_struct("ModuleTypes")
            .field("module", &self.module.to_string())
            .field("is_entry_module", &self.is_entry_module)
            .field("type_constructors", &self.type_constructors)
            .field("value_constructors", &self.value_constructors)
            .field("functions", &self.functions)
            .field("closures", &self.closures)
            .finish()
    }
}
