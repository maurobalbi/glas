use la_arena::Idx;
use smol_str::SmolStr;

use crate::{impl_from, DefDatabase, SourceRootId};

use super::{scope::ExprScopes, FunctionId, hir_def::{AdtId, LocalVariantId}, module::ConstructorField};

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
pub struct Adt {
    pub(crate) id: AdtId
}

impl Adt {
    pub fn variants(self, db: &dyn DefDatabase) -> Vec<Variant>{
        let loc = db.lookup_intern_adt(self.id);
        let items = &db.module_items(loc.file_id);
        let adt = &items[loc.value];
        adt.variants.clone().map(|v| Variant {parent: self.id, id: v}).collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variant {
    pub(crate) parent: AdtId,
    pub(crate) id: LocalVariantId,
}

impl Variant {
    pub fn name(self, db: &dyn DefDatabase) -> SmolStr {
        let loc = db.lookup_intern_adt(self.parent);
        let items = &db.module_items(loc.file_id);
        let variant = &items[self.id];
        variant.name.clone()
    }

    pub fn fields(self, db: &dyn DefDatabase) -> Vec<ConstructorField> {
        let loc = db.lookup_intern_adt(self.parent);
        let items = &db.module_items(loc.file_id);
        let variant = &items[self.id]; 
        variant.fields.clone()
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) id: FunctionId,
}

impl_from!(
    Function
    for ModuleDef
);
