use crate::check::Type;
use crate::item::{ItemFn, Signature, TraitFn};

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

    pub fn has_positional_params(&self) -> bool {
        !self.positional_params.is_empty()
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

    pub fn has_keyword_params(&self) -> bool {
        !self.keyword_params.is_empty()
    }

    pub fn keyword_params_count(&self) -> usize {
        self.keyword_params.len()
    }

    pub fn return_type(&self) -> &Type {
        &self.return_type
    }

    pub fn substitute(&self, variable: &str, replacement: &Type) -> Self {
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

impl From<Signature> for Function {
    fn from(value: Signature) -> Self {
        Self::new(
            Some(value.name.to_string()),
            !value.self_param,
            value
                .positional_params
                .iter()
                .cloned()
                .map(|param| param.ty.into())
                .collect(),
            value
                .keyword_params
                .iter()
                .cloned()
                .map(|param| (param.ident.to_string(), param.ty.into()))
                .collect(),
            value.return_ty.clone().map_or_else(Type::unit, Into::into),
        )
    }
}

impl From<ItemFn> for Function {
    fn from(value: ItemFn) -> Self {
        value.signature.into()
    }
}

impl From<TraitFn> for Function {
    fn from(value: TraitFn) -> Self {
        value.signature.into()
    }
}
