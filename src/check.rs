use std::collections::{HashMap, HashSet};

use chumsky::span::{SimpleSpan, Span};

use crate::error::Error;
use crate::item::{ImportedIdent, Item, ItemFn, Variant};
use crate::module::{Module, ModulePath};

mod ty;

pub use self::ty::{Function, ModuleTypes, PrimitiveType, Type, TypeMembers};

pub trait Infer {
    fn infer(&self, checker: &mut Checker) -> Result<Type, Error>;
}

#[derive(Debug, Clone)]
pub enum Entry {
    TermVar(String, Type),
    Scope,
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

    pub fn module_source(&self, name: &'_ str) -> &str {
        self.modules.get(name).unwrap().module().source()
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
                        return Some(module.get_function(import_name).unwrap().clone());
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

    pub fn debug(&self, debug_std: bool) {
        let modules = if debug_std {
            self.modules
                .clone()
                .into_iter()
                .collect::<HashMap<String, ModuleTypes>>()
        } else {
            self.modules
                .clone()
                .into_iter()
                .filter(|(name, _module_types)| !name.starts_with("std"))
                .collect::<HashMap<String, ModuleTypes>>()
        };

        eprintln!("{modules:#?}");
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
                module_path.set_is_entry_module(module_type.is_entry_module());

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

    #[allow(dead_code)]
    pub fn substitute(&mut self, target: &Type, variable: &str, replacement: &Type) -> Type {
        let Some((_ty, members)) = self
            .current_module()
            .get_type_constructor(&target.base_name())
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

    pub fn is_current_module_std(&self) -> bool {
        self.current_module().is_std()
    }

    pub fn check(&mut self) -> Result<(), (ModulePath, Error)> {
        let mut seen_modules = HashSet::new();

        self.collect_declarations(
            self.current_module().items().to_vec().as_slice(),
            &mut seen_modules,
        )
        .map_err(|error| (self.current_module().module_path().clone(), error))?;

        for module in seen_modules {
            self.current_module = module.to_string();
            self.check_one(self.current_module().items().to_vec())
                .map_err(|error| (self.current_module().module_path().clone(), error))?;
        }

        Ok(())
    }

    pub fn collect_declarations(
        &mut self,
        items: &[Item],
        seen_modules: &mut HashSet<ModulePath>,
    ) -> Result<(), Error> {
        let mut imported_modules: HashSet<ModulePath> = HashSet::new();
        imported_modules.insert(self.current_module().module_path().clone());

        let is_std = self.current_module().is_std();
        let current = self.current_module_mut();

        current.add_type_constructor("Boolean", (Type::boolean(), TypeMembers::new()));
        current.add_type_constructor("Float", (Type::float(), TypeMembers::new()));
        current.add_type_constructor("Integer", (Type::integer(), TypeMembers::new()));
        current.add_type_constructor("String", (Type::string(), TypeMembers::new()));
        current.add_type_constructor("Range", (Type::range(), TypeMembers::new()));
        current.add_type_constructor("List", (Type::list(), TypeMembers::new()));

        if !is_std {
            imported_modules.insert("std.io".into());
            imported_modules.insert("std.list".into());
            imported_modules.insert("std.option".into());
            imported_modules.insert("std.result".into());

            current.add_function("print_line", Type::import("std.io", "print_line"));
            current.add_function("print", Type::import("std.io", "print"));
            current.add_type_constructor(
                "List",
                (Type::import("std.list", "List"), TypeMembers::new()),
            );
            current.add_type_constructor(
                "Option",
                (Type::import("std.option", "Option"), TypeMembers::new()),
            );
            current.add_value_constructor(
                "Some",
                Type::import_variant("std.option", "Option", "Some"),
            );
            current.add_value_constructor(
                "None",
                Type::import_variant("std.option", "Option", "None"),
            );
            current.add_type_constructor(
                "Result",
                (Type::import("std.result", "Result"), TypeMembers::new()),
            );
            current.add_value_constructor("Ok", Type::import_variant("std.result", "Result", "Ok"));
            current
                .add_value_constructor("Err", Type::import_variant("std.result", "Result", "Err"));
        }

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

                    members.add_value_constructor(
                        &ty.base_name(),
                        Type::Fn(Function::r#static(
                            ty.base_name(),
                            member_fields,
                            Vec::new(),
                            ty.clone(),
                        )),
                    );
                } else if let Some(fields) = item_struct.record_fields() {
                    ty = Type::RecordStruct {
                        name: Box::new(ty),
                        fields: fields
                            .iter()
                            .map(|field| (field.name.to_string(), field.ty.clone().into()))
                            .collect(),
                    };
                }

                if let Some(functions) = &item_struct.fns {
                    for function in functions {
                        let (name, func) = collect_fn(function)?;

                        members.add_function(&name, func);
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

                            members.add_value_constructor(&name, Type::constructor(&name));
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

                            members.add_value_constructor(&name, variant_ty);
                        }
                        Variant::Record(ty_ident, records, _) => {
                            let name = ty_ident.to_string();

                            let fields = records
                                .iter()
                                .map(|record| (record.name.to_string(), record.ty.clone().into()))
                                .collect();

                            let variant_ty = Type::RecordStruct {
                                name: Box::new(Type::constructor(&name)),
                                fields,
                            };

                            members.add_value_constructor(&name, variant_ty);
                        }
                    }
                }

                for function in &item_enum.fns {
                    let (name, func) = collect_fn(function)?;

                    members.add_function(&name, func);
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
