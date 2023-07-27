use la_arena::Idx;

use crate::{impl_from, DefDatabase, SourceRootId};

use super::{scope::ExprScopes, FunctionId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Package {
    pub(crate) id: SourceRootId,
}

impl Package {
    pub fn dependencies(self, _db: &dyn DefDatabase) -> Vec<PackageDependency> {
        Vec::new()
    }

    pub fn target(self, db: &dyn DefDatabase) -> crate::base::Target {
        db.source_root_package_info(SourceRootId(0))
            .map_or(crate::base::Target::default(), |s| s.target.clone())
    }
}

#[derive(Debug)]
pub struct PackageDependency {
    pub package: Package,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module {
    pub(crate) id: ModuleId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId {
    package: SourceRootId,
    id: Idx<ExprScopes>,
}

/// The defs which can be visible in the module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleDef {
    Function(Function),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) id: FunctionId,
}

impl_from!(
    Function
    for ModuleDef
);
