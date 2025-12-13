mod item_enum;
mod item_fn;
mod item_impl;
mod item_struct;
mod item_trait;
mod item_use;

pub use self::item_enum::{ItemEnum, Variant};
pub use self::item_fn::ItemFn;
pub use self::item_impl::ItemImpl;
pub use self::item_struct::ItemStruct;
pub use self::item_trait::ItemTrait;
pub use self::item_use::{ImportKind, ImportedIdent, ItemUse};
use crate::compiler::{Scope, WriteRuby};

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Use(ItemUse),
    Fn(ItemFn),
    Struct(ItemStruct),
    Enum(ItemEnum),
    Trait(ItemTrait),
    Impl(ItemImpl),
}

impl WriteRuby for Item {
    fn write_ruby(&self, scope: &mut Scope) {
        match self {
            Self::Fn(item_fn) => item_fn.write_ruby(scope),
            Self::Struct(item_struct) => item_struct.write_ruby(scope),
            Self::Enum(item_enum) => item_enum.write_ruby(scope),
            Self::Impl(item_impl) => item_impl.write_ruby(scope),
            Self::Use(item_use) => {
                item_use.add_imports(scope);
                item_use.write_ruby(scope);
            }
            // Trait definitions do not have corresponding Ruby code.
            Self::Trait(..) => (),
        }
    }
}
