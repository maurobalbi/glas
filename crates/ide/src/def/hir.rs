use la_arena::Idx;
use smol_str::SmolStr;
use syntax::{
    ast::{self, Pattern},
    AstPtr,
};

use crate::{
    impl_from,
    ty::{self, TyDatabase},
    DefDatabase, InFile, SourceRootId,
};

use super::{
    hir_def::{AdtId, LocalVariantId},
    module::{Field, PatternId, VariantData},
    module_items,
    scope::ExprScopes,
    FunctionId, InternDatabase,
};

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
    Variant(Variant),
    Adt(Adt),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Adt {
    pub(crate) id: AdtId,
}

impl Adt {
    pub fn name(self, db: &dyn DefDatabase) -> SmolStr {
        let adt = db.lookup_intern_adt(self.id);
        let adt_data = &db.module_items(adt.file_id)[adt.value];
        adt_data.name.clone()
    }

    pub fn variants(self, db: &dyn DefDatabase) -> Vec<Variant> {
        let loc = db.lookup_intern_adt(self.id);
        let items = &db.module_items(loc.file_id);
        let adt = &items[loc.value];
        adt.variants
            .clone()
            .map(|v| Variant {
                parent: self.id,
                id: v,
            })
            .collect()
    }

    //type
    // pub fn type_() ->  {

    // }
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

    pub fn fields(&self, db: &dyn DefDatabase) -> Vec<Field> {
        self.variant_data(db).fields
    }

    fn variant_data(&self, db: &dyn DefDatabase) -> VariantData {
        let adt = db.lookup_intern_adt(self.parent);
        let module_items = db.module_items(adt.file_id);
        module_items[self.id].clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) id: FunctionId,
}

impl Function {
    pub fn name(self, db: &dyn DefDatabase) -> SmolStr {
        let func = db.lookup_intern_function(self.id);
        let func_data = &db.module_items(func.file_id)[func.value];
        func_data.name.clone()
    }

    pub fn ty(&self, db: &dyn TyDatabase) -> ty::Ty {
        let infer = db.infer_function(self.id);
        infer.fn_ty.clone()
    }
}

impl_from!(
    Function
    for ModuleDef
);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Local {
    pub(crate) parent: FunctionId,
    pub(crate) pat_id: PatternId,
}

impl Local {
    pub fn source(&self, db: &dyn DefDatabase) -> ast::Pattern {
        let (_body, source_map) = db.body_with_source_map(self.parent);
        let src = source_map
            .node_for_pattern(self.pat_id)
            .expect("This is a compiler bug!");
        let root = db.parse(src.file_id).syntax_node();
        src.value.to_node(&root)
    }

    pub fn ty(self, db: &dyn TyDatabase) -> ty::Ty {
        let def = self.parent;
        let infer = db.infer_function(def);
        let ty = infer.ty_for_pattern(self.pat_id).clone();
        ty
    }
}
