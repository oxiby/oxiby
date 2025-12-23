use std::collections::HashMap;
use std::fmt::Display;

use crate::check::{self, Function};
use crate::item::{ItemImpl, ItemTrait};
use crate::types::{self, TyVar};

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct Trait {
    ty: check::Type,
    constraints: Vec<Constraint>,
    associated_types: Vec<AssociatedType>,
    functions: Vec<Function>,
}

impl Trait {
    pub fn base_name(&self) -> String {
        self.ty.base_name()
    }
}

impl From<ItemTrait> for Trait {
    fn from(value: ItemTrait) -> Self {
        Self {
            ty: value.name.into(),
            constraints: value.constraints.map_or_else(Vec::new, |constraints| {
                constraints.into_iter().map(Into::into).collect()
            }),
            associated_types: value
                .associated_types
                .map_or_else(Vec::new, |associated_types| {
                    associated_types.into_iter().map(Into::into).collect()
                }),
            functions: value.functions.into_iter().map(Into::into).collect(),
        }
    }
}

impl Display for Trait {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct Impl {
    ty: check::Type,
    target: check::Type,
    constraints: Vec<Constraint>,
    associated_types: Vec<AssociatedType>,
    functions: Vec<Function>,
}

impl Impl {
    pub fn ty_base_name(&self) -> String {
        self.ty.base_name()
    }

    pub fn target_base_name(&self) -> String {
        self.target.base_name()
    }
}

impl From<ItemImpl> for Impl {
    fn from(value: ItemImpl) -> Self {
        Self {
            ty: value.name.into(),
            target: value.target.into(),
            constraints: value.constraints.map_or_else(Vec::new, |constraints| {
                constraints.into_iter().map(Into::into).collect()
            }),
            associated_types: value
                .associated_types
                .map_or_else(Vec::new, |associated_types| {
                    associated_types.into_iter().map(Into::into).collect()
                }),
            functions: value.functions.into_iter().map(Into::into).collect(),
        }
    }
}

pub type TraitImpls = HashMap<String, Impl>;

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Constraint {
    variable: String,
    requirements: Vec<check::Type>,
    default: Option<check::Type>,
}

impl Constraint {
    pub fn requirements(&self) -> &[check::Type] {
        &self.requirements
    }
}

impl From<types::Constraint> for Constraint {
    fn from(value: types::Constraint) -> Self {
        Self {
            variable: match value.tyvar {
                TyVar::SelfType => "Self".to_string(),
                TyVar::ExprIdent(expr_ident) => expr_ident.to_string(),
            },
            requirements: value.requirements.map_or_else(Vec::new, |requirements| {
                requirements.into_iter().map(Into::into).collect()
            }),
            default: value.default.map(Into::into),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct AssociatedType {
    name: String,
    requirements: Vec<check::Type>,
    default: Option<check::Type>,
}

impl From<types::AssociatedType> for AssociatedType {
    fn from(value: types::AssociatedType) -> Self {
        Self {
            name: value.name.to_string(),
            requirements: value.requirements.map_or_else(Vec::new, |requirements| {
                requirements.into_iter().map(Into::into).collect()
            }),
            default: value.default.map(Into::into),
        }
    }
}
