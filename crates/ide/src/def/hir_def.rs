use la_arena::Idx;

use crate::{InFile, impl_intern, impl_from};

use super::{ DefDatabase, module::Function};
use crate::impl_intern_key;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(salsa::InternId);
pub type FunctionLoc = InFile<Idx<Function>>;
impl_intern!(
    FunctionId,
    FunctionLoc,
    intern_function,
    lookup_intern_function
);

pub enum ModuleDefId {
    FunctionId(FunctionId)
}

impl_from!(
    FunctionId
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