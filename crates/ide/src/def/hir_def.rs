use la_arena::Idx;

use crate::{InFile, impl_intern, impl_from};

use super::{ DefDatabase, module::{Function, Adt, Variant}};
use crate::impl_intern_key;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub salsa::InternId);
pub type FunctionLoc = InFile<Idx<Function>>;
impl_intern!(
    FunctionId,
    FunctionLoc,
    intern_function,
    lookup_intern_function
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AdtId(pub salsa::InternId);
pub type AdtLoc = InFile<Idx<Adt>>;
impl_intern!(
    AdtId,
    AdtLoc,
    intern_adt,
    lookup_intern_adt
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariantId(pub salsa::InternId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariantLoc { pub parent: AdtId, pub value: InFile<Idx<Variant>> }
impl_intern!(
    VariantId,
    VariantLoc,
    intern_variant,
    lookup_intern_variant
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleDefId {
    FunctionId(FunctionId),
    AdtId(AdtId),
    VariantId(VariantId),
}

impl_from!(
    FunctionId,
    AdtId,
    VariantId
    for ModuleDefId 
);

#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl salsa::InternKey for $name {
            fn from_intern_id(v: salsa::InternId) -> Self {
                $name(v)
            }
            fn as_intern_id(&self) -> salsa::InternId {
                self.0
            }
        }
    };
}

trait Intern {
    type ID;
    fn intern(self, db: &dyn DefDatabase) -> Self::ID;
}

pub trait Lookup {
    type Data;
    fn lookup(&self, db: &dyn DefDatabase) -> Self::Data;
}

#[macro_export]
macro_rules! impl_intern {
    ($id:ident, $loc:ident, $intern:ident, $lookup:ident) => {
        impl_intern_key!($id);

        impl Intern for $loc {
            type ID = $id;
            fn intern(self, db: &dyn DefDatabase) -> $id {
                db.$intern(self)
            }
        }

        impl Lookup for $id {
            type Data = $loc;
            fn lookup(&self, db: &dyn DefDatabase) -> $loc {
                db.$lookup(*self)
            }
        }
    };
}