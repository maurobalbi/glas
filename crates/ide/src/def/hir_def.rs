use la_arena::Idx;

use crate::{impl_from, impl_intern, FileId, InFile};

use super::{
    hir::{Adt, Field, Function, Import, Module, TypeAlias, Variant},
    module::{AdtData, FieldData, FunctionData, ImportData, TypeAliasData, VariantData},
    DefDatabase,
};
use crate::impl_intern_key;

macro_rules! from_id {
    ($(($id:path, $ty:path)),*) => {$(
        impl From<$id> for $ty {
            fn from(id: $id) -> $ty {
                $ty { id }
            }
        }
        impl From<$ty> for $id {
            fn from(ty: $ty) -> $id {
                ty.id
            }
        }
    )*}
}

from_id!(
    (FunctionId, Function),
    (AdtId, Adt),
    (FileId, Module),
    (TypeAliasId, TypeAlias),
    (ImportId, Import)
);

impl From<VariantId> for Variant {
    fn from(value: VariantId) -> Self {
        Variant {
            parent: value.parent,
            id: value.local_id,
        }
    }
}

impl From<FieldId> for Field {
    fn from(value: FieldId) -> Self {
        Field {
            parent: value.parent,
            id: value.local_id,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub salsa::InternId);
pub type FunctionLoc = InFile<Idx<FunctionData>>;
impl_intern!(
    FunctionId,
    FunctionLoc,
    intern_function,
    lookup_intern_function
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ImportId(pub salsa::InternId);
pub type ImportLoc = InFile<Idx<ImportData>>;
impl_intern!(ImportId, ImportLoc, intern_import, lookup_intern_import);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AdtId(pub salsa::InternId);
pub type AdtLoc = InFile<Idx<AdtData>>;
impl_intern!(AdtId, AdtLoc, intern_adt, lookup_intern_adt);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeAliasId(pub salsa::InternId);
pub type TypeAliasLoc = InFile<Idx<TypeAliasData>>;
impl_intern!(
    TypeAliasId,
    TypeAliasLoc,
    intern_type_alias,
    lookup_intern_type_alias
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariantId {
    pub parent: AdtId,
    pub local_id: LocalVariantId,
}

pub type LocalVariantId = Idx<VariantData>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldId {
    pub parent: VariantId,
    pub local_id: LocalFieldId,
}

pub type LocalFieldId = Idx<FieldData>;

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleDefId {
    FunctionId(FunctionId),
    AdtId(AdtId),
    VariantId(VariantId),
    TypeAliasId(TypeAliasId),
}

impl_from!(
    FunctionId,
    AdtId,
    VariantId,
    TypeAliasId
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
