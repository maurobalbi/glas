use la_arena::Idx;

use crate::{impl_from, DefDatabase, PackageId};

use super::{ module::{FunctionId, Name}, nameres::ModuleScope};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Package {
    pub(crate) id: PackageId
}

impl Package {
    pub fn dependencies(self, db: &dyn DefDatabase) -> Vec<PackageDependency> {
        Vec::new()
    }

    pub fn target(self, db: &dyn DefDatabase) -> Target {
        return Target::Erlang
    }
}

#[derive(Debug)]
pub struct PackageDependency {
    pub package: Package,
    pub name: Name,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module {
    pub(crate) id: ModuleId,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId {
    package: PackageId,
    id: Idx<ModuleScope>,
}

/// The defs which can be visible in the module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleDef {
    Function(Function),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) id: FunctionId
}

impl_from!(
    Function
    for ModuleDef
);

pub enum Target {
    Javascript,
    Erlang
}
