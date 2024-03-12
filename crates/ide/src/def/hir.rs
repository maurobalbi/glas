use std::{collections::HashMap, sync::Arc};

use smol_str::SmolStr;
use syntax::ast::{self, AstNode, HasDocParts};

use crate::{
    impl_from,
    ty::{self, TyDatabase},
    DefDatabase, FileId, InFile, ModuleMap, SourceRootId,
};

use super::{
    hir_def::{AdtId, ImportId, LocalFieldId, LocalVariantId, TypeAliasId, VariantId},
    module::{
        AdtData, FieldData, FunctionData, ImportData, Param, PatternId, TypeAliasData, TypeRef,
        VariantData,
    },
    resolver::resolver_for_toplevel,
    semantics::Definition,
    source::HasSource,
    FunctionId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Package {
    pub(crate) id: SourceRootId,
}

impl Package {
    pub fn is_local(self, db: &dyn DefDatabase) -> bool {
        let package_id = db.source_root_package(self.id);
        let graph = db.package_graph();
        let package = graph[*package_id].clone();
        package.is_local
    }

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

    pub fn import_accessor(self, db: &dyn DefDatabase) -> SmolStr {
        let name = self.name(db);
        SmolStr::from(
            name.split("/")
                .last()
                .expect("This is a compiler error. A module should always have a name"),
        )
    }

    pub fn docs(&self, db: &dyn DefDatabase) -> String {
        self.source(db)
            .expect("This should not happen")
            .value
            .doc_text()
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

    pub fn generic_params(self, db: &dyn DefDatabase) -> Vec<TypeRef> {
        let data = self.data(db);
        data.params
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

    pub fn docs(&self, db: &dyn DefDatabase) -> String {
        self.source(db)
            .expect("This should not happen")
            .value
            .doc_text()
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

    pub fn docs(&self, db: &dyn DefDatabase) -> String {
        self.source(db)
            .expect("This should not happen")
            .value
            .doc_text()
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

    // returns the type of the field as in the definition of the custom type
    pub fn ty(self, db: &dyn DefDatabase) -> TypeRef {
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

    pub fn docs(&self, db: &dyn DefDatabase) -> String {
        self.source(db)
            .expect("This should not happen")
            .value
            .doc_text()
    }
}

impl_from!(
    Function
    for ModuleDef
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Import {
    pub(crate) id: ImportId,
}

impl Import {
    pub fn imported_name(self, db: &dyn DefDatabase) -> SmolStr {
        self.data(db).unqualified_name.clone()
    }

    pub fn is_type_import(self, db: &dyn DefDatabase) -> bool {
        self.data(db).is_type_import
    }

    pub fn imported_alias(self, db: &dyn DefDatabase) -> Option<SmolStr> {
        self.data(db).unqualified_as_name.clone()
    }

    pub fn definition(self, db: &dyn DefDatabase) -> Option<Definition> {
        let module = self.imported_from_module(db)?;
        let resolver = resolver_for_toplevel(db, module);
        if self.is_type_import(db) {
            resolver
                .resolve_type(&self.imported_name(db))
                .map(Into::into)
        } else {
            resolver
                .resolve_name(&self.imported_name(db))
                .map(Into::into)
        }
    }

    pub fn import_from_module_name(self, db: &dyn DefDatabase) -> SmolStr {
        let import = db.lookup_intern_import(self.id);
        let module_idx = self.data(db).module;
        let module = &db.module_items(import.file_id)[module_idx];
        module
            .as_name
            .clone()
            .unwrap_or_else(|| module.accessor.clone())
    }

    pub fn imported_from_module(self, db: &dyn DefDatabase) -> Option<FileId> {
        let import = db.lookup_intern_import(self.id);
        let module_accessor = self.import_from_module_name(db);
        let resolver = resolver_for_toplevel(db, import.file_id);
        resolver.resolve_module(&module_accessor)
    }

    fn data(self, db: &dyn DefDatabase) -> ImportData {
        let import = db.lookup_intern_import(self.id);
        db.module_items(import.file_id)[import.value].clone()
    }

    pub fn module(&self, db: &dyn DefDatabase) -> Module {
        db.lookup_intern_import(self.id).file_id.into()
    }
}

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
    True,
    False,
}

impl BuiltIn {
    pub fn values() -> HashMap<SmolStr, BuiltIn> {
        [
            ("Nil".into(), BuiltIn::Nil),
            ("Ok".into(), BuiltIn::Ok),
            ("Error".into(), BuiltIn::Error),
            ("True".into(), BuiltIn::True),
            ("False".into(), BuiltIn::False),
        ]
        .into_iter()
        .collect()
    }
}
