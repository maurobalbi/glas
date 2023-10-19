use std::{collections::HashMap, sync::Arc};

use smol_str::SmolStr;
use syntax::ast::{self, AstNode};

use crate::{
    impl_from,
    ty::{self, Ty, TyDatabase},
    DefDatabase, FileId, InFile, ModuleMap, SourceRootId,
};

use super::{
    hir_def::{AdtId, LocalFieldId, LocalVariantId, TypeAliasId, VariantId},
    module::{AdtData, FieldData, FunctionData, Param, PatternId, TypeAliasData, VariantData},
    FunctionId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Package {
    pub(crate) id: SourceRootId,
}

impl Package {
    pub fn dependencies(self, db: &dyn DefDatabase) -> Vec<Package> {
        let package_id = db.source_root_package(self.id);
        let graph = db.package_graph();
        let package = graph[*package_id].clone();
        package
            .dependencies
            .iter()
            .map(|p| {
                let gleam_toml = graph[p.package].gleam_toml;
                let sid = db.file_source_root(gleam_toml);
                Package { id: sid }
            })
            .collect()
    }

    pub fn module_map(self, db: &dyn DefDatabase) -> Arc<ModuleMap> {
        db.module_map(self.id)
    }

    pub fn visible_modules(self, db: &dyn DefDatabase) -> Arc<ModuleMap> {
        let mut module_map = ModuleMap::default();
        self.dependencies(db).iter().for_each(|m| {
            let map = m.module_map(db);
            for m in map.iter() {
                module_map.insert(m.0, m.1.clone());
            }
        });
        for m in self.module_map(db).iter() {
            module_map.insert(m.0, m.1.clone());
        }
        Arc::new(module_map)
    }

    // pub fn target(self, db: &dyn DefDatabase) -> crate::base::Target {
    //     db.source_root_package_info(SourceRootId(0))
    //         .map_or(crate::base::Target::default(), |s| s.target.clone())
    // }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module {
    pub(crate) id: FileId,
}

impl Module {
    pub fn name(self, db: &dyn DefDatabase) -> SmolStr {
        let root = db.file_source_root(self.id);
        let module_map = db.module_map(root);
        module_map
            .module_name_for_file(self.id)
            .expect("This is a compiler bug!")
    }

    pub fn package(self, db: &dyn DefDatabase) -> Package {
        let root = db.file_source_root(self.id);
        Package { id: root }
    }
}

/// The defs which can be visible in the module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleDef {
    Function(Function),
    Variant(Variant),
    Adt(Adt),
    TypeAlias(TypeAlias),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeAlias {
    pub(crate) id: TypeAliasId,
}

impl TypeAlias {
    pub fn name(self, db: &dyn DefDatabase) -> SmolStr {
        let type_alias = db.lookup_intern_type_alias(self.id);
        let type_alias_data = &db.module_items(type_alias.file_id)[type_alias.value];
        type_alias_data.name.clone()
    }

    pub fn data(&self, db: &dyn DefDatabase) -> TypeAliasData {
        let type_alias = db.lookup_intern_type_alias(self.id);
        let module_items = db.module_items(type_alias.file_id);
        module_items[type_alias.value].clone()
    }

    pub fn module(&self, db: &dyn DefDatabase) -> Module {
        db.lookup_intern_type_alias(self.id).file_id.into()
    }
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

    pub fn common_fields(self, db: &dyn DefDatabase) -> HashMap<SmolStr, Field> {
        let mut fields = HashMap::new();
        for (name, _) in self.data(db).common_fields.iter() {
            if let Some(variant) = self.variants(db).first() {
                if let Some(field) = variant
                    .fields(db)
                    .iter()
                    .find(|f| f.label(db) == Some(name.clone()))
                {
                    fields.insert(name.clone(), *field);
                }
            }
        }
        fields
    }

    pub fn data(&self, db: &dyn DefDatabase) -> AdtData {
        let adt = db.lookup_intern_adt(self.id);
        let module_items = db.module_items(adt.file_id);
        module_items[adt.value].clone()
    }

    pub fn module(&self, db: &dyn DefDatabase) -> Module {
        db.lookup_intern_adt(self.id).file_id.into()
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

    pub fn parent(self) -> Adt {
        Adt { id: self.parent }
    }

    pub fn fields(&self, db: &dyn DefDatabase) -> Vec<Field> {
        self.data(db)
            .fields
            .map(|field_id| Field {
                parent: VariantId {
                    parent: self.parent,
                    local_id: self.id,
                },
                id: field_id,
            })
            .collect()
    }

    pub fn module(&self, db: &dyn DefDatabase) -> Module {
        let loc = db.lookup_intern_adt(self.parent);
        Module { id: loc.file_id }
    }

    fn data(&self, db: &dyn DefDatabase) -> VariantData {
        let adt = db.lookup_intern_adt(self.parent);
        let module_items = db.module_items(adt.file_id);
        module_items[self.id].clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Field {
    pub(crate) parent: VariantId,
    pub(crate) id: LocalFieldId,
}

impl Field {
    pub fn label(self, db: &dyn DefDatabase) -> Option<SmolStr> {
        let data = self.data(db);
        data.label
    }

    pub fn ty(self, db: &dyn DefDatabase) -> Ty {
        self.data(db).type_ref
    }

    fn data(&self, db: &dyn DefDatabase) -> FieldData {
        let adt = db.lookup_intern_adt(self.parent.parent);
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
        self.data(db).name.clone()
    }

    pub fn params(self, db: &dyn DefDatabase) -> Vec<Param> {
        self.data(db).params.clone()
    }

    pub fn ty(&self, db: &dyn TyDatabase) -> ty::Ty {
        let infer = db.infer_function(self.id);
        infer.fn_ty.clone()
    }

    fn data(self, db: &dyn DefDatabase) -> FunctionData {
        let func = db.lookup_intern_function(self.id);
        db.module_items(func.file_id)[func.value].clone()
    }

    pub fn module(&self, db: &dyn DefDatabase) -> Module {
        db.lookup_intern_function(self.id).file_id.into()
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
    pub fn source(&self, db: &dyn DefDatabase) -> InFile<ast::Pattern> {
        let (_body, source_map) = db.body_with_source_map(self.parent);
        let src = source_map
            .node_for_pattern(self.pat_id)
            .expect("This is a compiler bug!");
        let root = db.parse(src.file_id).syntax_node();
        src.with_value(src.value.clone().to_node(&root))
    }

    pub fn ty(self, db: &dyn TyDatabase) -> ty::Ty {
        let def = self.parent;
        let infer = db.infer_function(def);

        infer.ty_for_pattern(self.pat_id).clone()
    }

    pub fn name(self, db: &dyn DefDatabase) -> SmolStr {
        SmolStr::from(self.source(db).value.syntax().to_string())
    }
}

#[derive(Clone, Eq, Debug, PartialEq)]
pub enum BuiltIn {
    Nil,
    Ok,
    Error,
}

impl BuiltIn {
    pub fn values() -> HashMap<SmolStr, BuiltIn> {
        [
            ("Nil".into(), BuiltIn::Nil),
            ("Ok".into(), BuiltIn::Ok),
            ("Error".into(), BuiltIn::Error),
        ]
        .into_iter()
        .collect()
    }
}
