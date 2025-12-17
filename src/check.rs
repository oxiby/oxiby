#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use std::fmt::Display;

use chumsky::span::{SimpleSpan, Span};

use crate::error::Error;
use crate::item::{ImportedIdent, Item, ItemFn, Variant};
use crate::module::{Module, ModulePath};

pub trait Infer {
    fn infer(&self, checker: &mut Checker) -> Result<Type, Error>;
}

#[derive(Debug, Clone)]
pub enum Entry {
    TermVar(String, Type),
    Scope,
}

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Constructor(String),
    Generic(Box<Type>, Vec<Type>),
    Variable(String),
    Tuple(Vec<Type>),
    RecordStruct(Box<Type>, Vec<(String, Type)>),
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

    pub fn is_string(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::String))
    }

    pub fn range() -> Self {
        Self::Primitive(PrimitiveType::Range)
    }

    pub fn is_range(&self) -> bool {
        matches!(self, Self::Primitive(PrimitiveType::Range))
    }

    pub fn list() -> Self {
        Self::Generic(
            Box::new(Self::constructor("List")),
            vec![Self::variable("t")],
        )
    }

    pub fn is_list(&self) -> bool {
        if let Self::Generic(constructor, _) = self
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
            Self::Generic(ty, _) | Self::RecordStruct(ty, _) => ty.to_string(),
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
            ty @ Self::Generic(..) => ty.to_string(),
            Self::RecordStruct(ty, _) => ty.to_string(),
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
            (Self::Generic(t, t_params), Self::Generic(u, u_params)) if t == u => {
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

    fn substitute(self, variable: &str, replacement: &Self) -> Self {
        let mut substituted = self.clone();

        match substituted {
            Self::Generic(ref mut _ty, ref mut params) => {
                for param in params {
                    if matches!(param, Type::Variable(name) if name == variable) {
                        *param = replacement.clone();
                    }
                }
            }
            Self::Primitive(_primitive_type) => {}
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
            Self::Generic(ty, ty_vars) => &format!(
                "{}<{}>",
                ty,
                ty_vars
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
            Self::RecordStruct(ty, _) => &format!("{ty}",),
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
                        Self::Generic(
                            Box::new(constructor),
                            params.iter().cloned().map(Into::into).collect(),
                        )
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

    pub fn get_function(&self, name: &str) -> Option<&Type> {
        self.functions.get(name)
    }

    fn substitute(&self, variable: &str, replacement: &Type) -> Self {
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

#[derive(Clone, Debug)]
pub struct ModuleTypes {
    module: Module,
    is_entry_module: bool,
    pub type_constructors: HashMap<String, (Type, TypeMembers)>,
    pub value_constructors: HashMap<String, Type>,
    pub functions: HashMap<String, Type>,
    pub closures: HashSet<SimpleSpan>,
}

impl ModuleTypes {
    pub fn new(module: Module) -> Self {
        let is_entry_module = module.is_entry_module();
        let mut type_constructors = HashMap::new();
        let mut value_constructors = HashMap::new();
        let mut functions = HashMap::new();

        type_constructors.insert("Boolean".to_owned(), (Type::boolean(), TypeMembers::new()));
        type_constructors.insert("Float".to_owned(), (Type::float(), TypeMembers::new()));
        type_constructors.insert("Integer".to_owned(), (Type::integer(), TypeMembers::new()));
        type_constructors.insert("String".to_owned(), (Type::string(), TypeMembers::new()));
        type_constructors.insert("Range".to_owned(), (Type::range(), TypeMembers::new()));

        let list_ty = Type::list();
        type_constructors.insert(list_ty.base_name(), (list_ty, TypeMembers::new()));

        if !module.is_std() {
            functions.insert(
                "print_line".to_string(),
                Type::import("std.io", "print_line"),
            );
            functions.insert("print".to_string(), Type::import("std.io", "print"));
            type_constructors.insert(
                "List".to_string(),
                (Type::import("std.list", "List"), TypeMembers::new()),
            );
            type_constructors.insert(
                "Option".to_string(),
                (Type::import("std.option", "Option"), TypeMembers::new()),
            );
            value_constructors.insert(
                "Some".to_string(),
                Type::import_variant("std.option", "Option", "Some"),
            );
            value_constructors.insert(
                "None".to_string(),
                Type::import_variant("std.option", "Option", "None"),
            );
            type_constructors.insert(
                "Result".to_string(),
                (Type::import("std.result", "Result"), TypeMembers::new()),
            );
            value_constructors.insert(
                "Ok".to_string(),
                Type::import_variant("std.result", "Result", "Ok"),
            );
            value_constructors.insert(
                "Err".to_string(),
                Type::import_variant("std.result", "Result", "Err"),
            );
        }

        Self {
            module,
            is_entry_module,
            type_constructors,
            value_constructors,
            functions,
            closures: HashSet::new(),
        }
    }

    pub fn get_type_constructor(&self, name: &str) -> Option<(&Type, &TypeMembers)> {
        self.type_constructors.get(name).map(|(ty, mem)| (ty, mem))
    }

    pub fn add_type_constructor<N>(&mut self, name: &N, contents: (Type, TypeMembers))
    where
        N: ToString,
    {
        self.type_constructors.insert(name.to_string(), contents);
    }

    pub fn get_value_constructor(&self, name: &str) -> Option<&Type> {
        self.value_constructors.get(name)
    }

    pub fn add_value_constructor<N>(&mut self, name: &N, ty: Type)
    where
        N: ToString,
    {
        self.value_constructors.insert(name.to_string(), ty);
    }

    pub fn functions(&self) -> impl Iterator<Item = (&String, &Type)> {
        self.functions.iter()
    }

    pub fn add_function<N>(&mut self, name: &N, ty: Type)
    where
        N: ToString,
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

#[derive(Debug)]
pub struct Checker {
    counter: usize,
    context: Vec<Entry>,
    current_module: String,
    modules: HashMap<String, ModuleTypes>,
}

impl Checker {
    pub fn new(modules: HashMap<String, Module>, entry_module_path_string: String) -> Self {
        Self {
            counter: 0,
            context: Vec::new(),
            current_module: entry_module_path_string,
            modules: modules
                .into_iter()
                .map(|(module_path, module)| (module_path, ModuleTypes::new(module)))
                .collect(),
        }
    }

    pub fn get_contextual(&self, name: &str) -> Option<Type> {
        for entry in self.context.iter().rev() {
            if let Entry::TermVar(var, ty) = entry
                && var == name
            {
                match ty {
                    Type::Import {
                        module_name,
                        import_name,
                        variant: _,
                        rename: _,
                    } => {
                        let module = self.modules.get(module_name).unwrap();
                        return Some(module.functions.get(import_name).unwrap().clone());
                    }
                    ty => return Some(ty.clone()),
                }
            }
        }

        None
    }

    pub fn find_contextual(&self, name: &str, span: SimpleSpan) -> Result<Type, Error> {
        self.get_contextual(name).ok_or_else(|| {
            Error::build("Unknown binding")
                .with_detail(
                    &format!("Cannot find binding `{name}` in this scope."),
                    span,
                )
                .finish()
        })
    }

    pub fn replace_contextual(&mut self, name: &str, replacement: Type) {
        for entry in self.context.iter_mut().rev() {
            if let Entry::TermVar(var, ty) = entry
                && var == name
            {
                *ty = replacement;

                return;
            }
        }

        panic!("Attempted to replace nonexistent contexual type `{name}`.");
    }

    pub fn push_term_var<S>(&mut self, name: S, ty: Type)
    where
        S: Into<String>,
    {
        self.context.push(Entry::TermVar(name.into(), ty));
    }

    pub fn push_scope(&mut self) {
        self.context.push(Entry::Scope);
    }

    pub fn pop_scope(&mut self) {
        loop {
            if let Some(entry) = self.context.pop()
                && matches!(entry, Entry::Scope)
            {
                break;
            }
        }
    }

    pub fn mark_closure(&mut self, span: SimpleSpan) {
        self.current_module_mut().mark_closure(span);
    }

    pub fn debug(&self) {
        eprintln!(
            "{:#?}",
            self.modules
                .clone()
                .into_iter()
                .filter(|(name, _module_types)| !name.starts_with("std"))
                .collect::<HashMap<String, ModuleTypes>>()
        );
    }

    pub fn into_modules(self) -> HashMap<ModulePath, Module> {
        self.modules
            .into_iter()
            .map(|(module_path_string, module_type)| {
                // Since the ModulePath was converted to a string for use as a hash map key during
                // type checking, it loses the information about whether or not it was the entry
                // module, but the module itself has retained this information, so when we recreate
                // the ModulePath from its string representation, we copy this information back.
                let mut module_path: ModulePath = module_path_string.as_str().into();
                module_path.set_is_entry_module(module_type.is_entry_module);

                (module_path, module_type.into_module())
            })
            .collect()
    }

    pub fn get_type_constructor(&self, name: &str) -> Option<(&Type, &TypeMembers)> {
        self.current_module()
            .get_type_constructor(name)
            .and_then(|(ty, mem)| {
                if let Type::Import {
                    module_name,
                    import_name,
                    variant: _,
                    rename: _,
                } = ty
                {
                    self.modules[module_name].get_type_constructor(import_name)
                } else {
                    Some((ty, mem))
                }
            })
    }

    pub fn get_value_constructor(&self, name: &str) -> Option<&Type> {
        self.current_module()
            .get_value_constructor(name)
            .and_then(|ty| {
                if let Type::Import {
                    module_name,
                    import_name,
                    variant,
                    rename: _,
                } = ty
                {
                    if let Some(variant) = variant {
                        if let Some((_ty, members)) =
                            self.modules[module_name].get_type_constructor(import_name)
                        {
                            members.get_value_constructor(variant)
                        } else {
                            None
                        }
                    } else {
                        self.modules[module_name].get_value_constructor(import_name)
                    }
                } else {
                    Some(ty)
                }
            })
    }

    pub fn create_type_var(&mut self) -> Type {
        let type_var = Type::variable(self.counter.to_string());

        self.counter += 1;

        type_var
    }

    pub fn substitute(&mut self, target: &Type, variable: &str, replacement: &Type) -> Type {
        let Some((_ty, members)) = self
            .current_module()
            .type_constructors
            .get(&target.base_name())
        else {
            return target.clone();
        };

        let new_ty = target.clone().substitute(variable, replacement);
        let new_members = members.substitute(variable, replacement);

        self.current_module_mut()
            .add_type_constructor(&new_ty.full_name(), (new_ty.clone(), new_members));

        new_ty
    }

    pub fn current_module(&self) -> &ModuleTypes {
        &self.modules[&self.current_module]
    }

    fn current_module_mut(&mut self) -> &mut ModuleTypes {
        self.modules
            .get_mut(&self.current_module)
            .expect("self.current_module should always be a valid key")
    }

    pub fn check(&mut self) -> Result<(), Error> {
        let mut seen_modules = HashSet::new();

        self.collect_declarations(
            self.current_module().items().to_vec().as_slice(),
            &mut seen_modules,
        )?;

        self.check_one(self.current_module().items().to_vec())
    }

    pub fn collect_declarations(
        &mut self,
        items: &[Item],
        seen_modules: &mut HashSet<ModulePath>,
    ) -> Result<(), Error> {
        let mut imported_modules: HashSet<ModulePath> = HashSet::new();
        imported_modules.insert("std.io".into());
        imported_modules.insert("std.list".into());
        imported_modules.insert("std.option".into());
        imported_modules.insert("std.result".into());

        for item in items {
            if let Item::Use(item_use) = item {
                let module_path: ModulePath = item_use.path.clone().into();
                let module_path_string = if item_use.is_self {
                    self.current_module.clone()
                } else {
                    imported_modules.insert(module_path.clone());

                    module_path.to_string()
                };

                for import in &item_use.idents {
                    match import {
                        ImportedIdent::ExprIdent { ident, rename } => match rename {
                            Some(rename) => {
                                self.current_module_mut().add_function(
                                    rename,
                                    Type::import_renamed(
                                        &module_path_string,
                                        ident.to_string(),
                                        rename.to_string(),
                                    ),
                                );
                            }
                            None => {
                                self.current_module_mut().add_function(
                                    ident,
                                    Type::import(&module_path_string, ident.to_string()),
                                );
                            }
                        },
                        ImportedIdent::TypeIdent {
                            ident,
                            variant,
                            rename,
                        } => {
                            if let Some(variant) = variant {
                                match rename {
                                    Some(rename) => {
                                        self.current_module_mut().add_value_constructor(
                                            rename,
                                            Type::import_variant_renamed(
                                                &module_path_string,
                                                ident.to_string(),
                                                variant.to_string(),
                                                rename.to_string(),
                                            ),
                                        );
                                    }
                                    None => {
                                        self.current_module_mut().add_value_constructor(
                                            &variant,
                                            Type::import_variant(
                                                &module_path_string,
                                                ident.to_string(),
                                                variant.to_string(),
                                            ),
                                        );
                                    }
                                }
                            } else {
                                let name = rename.as_ref().unwrap_or(ident).to_string();

                                self.current_module_mut().add_type_constructor(
                                    &name,
                                    (Type::import(&module_path_string, &name), TypeMembers::new()),
                                );
                            }
                        }
                    }
                }
            } else if let Item::Fn(item_fn) = item {
                let (name, function) = collect_fn(item_fn)?;

                self.current_module_mut().add_function(&name, function);
            } else if let Item::Struct(item_struct) = item {
                let mut ty: Type = item_struct.ty.clone().into();

                if let Type::Primitive(primitive_type) = ty {
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

                let mut members = TypeMembers::new();

                if let Some(fields) = item_struct.tuple_fields() {
                    let member_fields =
                        fields.iter().map(|field| field.ty.clone().into()).collect();

                    members.value_constructors.insert(
                        ty.base_name(),
                        Type::Fn(Function::r#static(
                            ty.base_name(),
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

                self.current_module_mut()
                    .add_type_constructor(&ty.base_name(), (ty, members));
            } else if let Item::Enum(item_enum) = item {
                let ty: Type = item_enum.ty.clone().into();

                if let Type::Primitive(primitive_type) = ty {
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

                self.current_module_mut()
                    .add_type_constructor(&ty.base_name(), (ty, members));
            }
        }

        for module_path in imported_modules {
            // Don't try to collect the same module twice.
            if seen_modules.contains(&module_path) {
                continue;
            }

            // Get the name of the module to switch to.
            let next_module = module_path.to_string();

            // Remember which module we're in now before switching to the next one.
            let previous_module = std::mem::replace(&mut self.current_module, next_module);

            // Mark the one we're switching to as seen for the `contains` check above.
            seen_modules.insert(module_path.clone());

            // Collect the next module.
            let items = self.current_module_mut().items().to_vec();
            self.collect_declarations(&items, seen_modules)?;

            // Switch back to the original module.
            self.current_module = previous_module;
        }

        Ok(())
    }

    pub fn check_one(&mut self, items: Vec<Item>) -> Result<(), Error> {
        for item in items {
            if let Item::Fn(item_fn) = item {
                self.context = self
                    .current_module()
                    .functions()
                    .map(|(name, ty)| Entry::TermVar(name.clone(), ty.clone()))
                    .collect();

                for param in item_fn
                    .signature
                    .positional_params
                    .iter()
                    .chain(item_fn.signature.keyword_params.iter())
                {
                    self.push_term_var(param.ident.to_string(), param.ty.clone().into());
                }

                item_fn.infer(self)?;
            }
        }

        Ok(())
    }
}

fn collect_fn(item_fn: &ItemFn) -> Result<(String, Type), Error> {
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
