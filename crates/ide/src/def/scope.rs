use std::{collections::HashMap, ops, sync::Arc};

use indexmap::IndexMap;
use la_arena::{Arena, ArenaMap, Idx};
use petgraph::stable_graph::StableGraph;
use salsa::InternId;
use smol_str::SmolStr;
use syntax::{ast, AstPtr};

use crate::{DefDatabase, FileId, ModuleMap};

use super::{
    body::Body,
    hir::Module,
    hir_def::{
        AdtId, AdtLoc, ConstId, ConstLoc, FunctionLoc, ImportId, ImportLoc, ModuleDefId,
        TypeAliasId, TypeAliasLoc, VariantId,
    },
    module::{Clause, Expr, ExprId, ImportData, Pattern, PatternId, Statement, Visibility},
    resolver::ResolveResult,
    resolver_for_expr, FunctionId, ModuleItemData,
};

pub fn module_scope_with_map_query(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> (Arc<ModuleScope>, Arc<ModuleSourceMap>) {
    let module_data = db.module_items(file_id);
    let module_map = Module { id: file_id }.package(db).visible_modules(db);

    let mut scope = ModuleScope::default();
    let mut module_source_map = ModuleSourceMap::default();

    for (_, imported_module) in module_data.module_imports() {
        let Some(file) = module_map.file_for_module_name(&imported_module.name) else {
            // ToDo: report diagnostics
            continue;
        };

        let local_accessor = imported_module
            .as_name
            .clone()
            .unwrap_or_else(|| imported_module.accessor.clone());

        scope.modules.insert(local_accessor, file);
    }

    for (import_id, import) in module_data.unqualified_imports() {
        let import_loc = db.intern_import(ImportLoc {
            file_id,
            value: import_id,
        });

        module_source_map
            .import_map
            .insert(import.ast_ptr.clone(), import_loc);
        for (is_type_import, val) in scope.resolve_import(db, &module_map, &module_data, import) {
            match val {
                ModuleDefId::AdtId(_) if is_type_import => {
                    scope.types.insert(import.local_name(), val)
                }
                ModuleDefId::TypeAliasId(_) if is_type_import => {
                    scope.types.insert(import.local_name(), val)
                }
                _ if !is_type_import => scope.values.insert(import.local_name(), val),
                _ => None,
            };
        }
    }

    for (func_id, func) in module_data.functions() {
        let name = &func.name;
        let func_loc = db.intern_function(FunctionLoc {
            file_id,
            value: func_id,
        });

        module_source_map
            .function_map
            .insert(func.ast_ptr.clone(), func_loc);
        let def = ModuleDefId::FunctionId(func_loc);
        scope.values.insert(name.clone(), def.clone());
        scope
            .declarations
            .entry(name.clone())
            .or_default()
            .push((def, func.visibility.clone()));
    }

    for (alias_id, alias) in module_data.type_alias() {
        let name = &alias.name;
        let alias_loc = db.intern_type_alias(TypeAliasLoc {
            file_id,
            value: alias_id,
        });
        let def = ModuleDefId::TypeAliasId(alias_loc);

        module_source_map
            .type_alias_map
            .insert(alias.ast_ptr.clone(), alias_loc);
        scope.types.insert(name.clone(), def.clone());
        scope
            .declarations
            .entry(name.clone())
            .or_default()
            .push((def, alias.visibility.clone()));
    }

    for (constant_id, constant) in module_data.constants() {
        let name = &constant.name;
        let const_loc = db.intern_const(ConstLoc {
            file_id,
            value: constant_id,
        });
        let def = ModuleDefId::ModuleConstant(const_loc);

        module_source_map
            .const_map
            .insert(constant.ast_ptr.clone(), const_loc);

        scope.values.insert(name.clone(), def.clone());
        scope
            .declarations
            .entry(name.clone())
            .or_default()
            .push((def, constant.visibility.clone()));
    }

    for (adt_id, adt) in module_data.adts() {
        let name = &adt.name;
        let adt_loc = db.intern_adt(AdtLoc {
            file_id,
            value: adt_id,
        });
        let def = ModuleDefId::AdtId(adt_loc);

        module_source_map
            .adt_map
            .insert(adt.ast_ptr.clone(), adt_loc);
        scope.types.insert(name.clone(), def.clone());
        scope
            .declarations
            .entry(name.clone())
            .or_default()
            .push((def, adt.visibility.clone()));

        for variant_id in adt.variants.clone() {
            let variant = &module_data[variant_id];
            let name = &variant.name;

            let variant_id = VariantId {
                parent: adt_loc,
                local_id: variant_id,
            };
            let def = ModuleDefId::VariantId(variant_id);

            module_source_map
                .variant_map
                .insert(variant.ast_ptr.clone(), variant_id);
            scope.values.insert(name.clone(), def.clone());

            //use visibility of adt
            scope
                .declarations
                .entry(name.clone())
                .or_default()
                .push((def, adt.visibility.clone()));
        }
    }

    (Arc::new(scope), Arc::new(module_source_map))
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleSourceMap {
    function_map: HashMap<AstPtr<ast::Function>, FunctionId>,
    adt_map: HashMap<AstPtr<ast::Adt>, AdtId>,
    variant_map: HashMap<AstPtr<ast::Variant>, VariantId>,
    type_alias_map: HashMap<AstPtr<ast::TypeAlias>, TypeAliasId>,
    import_map: HashMap<AstPtr<ast::UnqualifiedImport>, ImportId>,
    const_map: HashMap<AstPtr<ast::ModuleConstant>, ConstId>,
}

impl ModuleSourceMap {
    pub fn node_to_function(&self, node: &ast::Function) -> Option<FunctionId> {
        let src = AstPtr::new(node);
        self.function_map.get(&src).copied()
    }

    pub fn node_to_constant(&self, node: &ast::ModuleConstant) -> Option<ConstId> {
        let src = AstPtr::new(node);
        self.const_map.get(&src).copied()
    }

    pub fn node_to_import(&self, node: &ast::UnqualifiedImport) -> Option<ImportId> {
        let src = AstPtr::new(node);
        self.import_map.get(&src).copied()
    }

    pub fn node_to_adt(&self, node: &ast::Adt) -> Option<AdtId> {
        let src = AstPtr::new(node);
        self.adt_map.get(&src).copied()
    }

    pub fn node_to_variant(&self, node: &ast::Variant) -> Option<VariantId> {
        let src = AstPtr::new(node);
        self.variant_map.get(&src).copied()
    }

    pub fn node_to_type_alias(&self, node: &ast::TypeAlias) -> Option<TypeAliasId> {
        let src = AstPtr::new(node);
        self.type_alias_map.get(&src).copied()
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleScope {
    /// The values visible in this module including imports
    values: IndexMap<SmolStr, ModuleDefId>,
    types: IndexMap<SmolStr, ModuleDefId>,

    modules: IndexMap<SmolStr, FileId>,

    /// The defs declared in this module which can potentially be imported in another module
    /// Split into value and type declarations
    declarations: IndexMap<SmolStr, Vec<(ModuleDefId, Visibility)>>,
}

impl ModuleScope {
    pub fn resolve_name_locally(&self, name: &SmolStr) -> Option<&ModuleDefId> {
        self.values.get(name)
    }

    pub fn resovlve_type(&self, name: SmolStr) -> Option<&ModuleDefId> {
        self.types.get(&name)
    }

    pub fn resolve_module(&self, name: &SmolStr) -> Option<&FileId> {
        self.modules.get(name)
    }

    pub fn values(
        &self,
    ) -> impl Iterator<Item = (&SmolStr, &ModuleDefId)> + ExactSizeIterator + '_ {
        self.values.iter()
    }

    pub fn types(&self) -> impl Iterator<Item = (&SmolStr, &ModuleDefId)> + ExactSizeIterator + '_ {
        self.types.iter()
    }

    pub fn declarations(
        &self,
    ) -> impl Iterator<Item = &Vec<(ModuleDefId, Visibility)>> + ExactSizeIterator + '_ {
        self.declarations.iter().map(|v| v.1)
    }

    fn resolve_import(
        &self,
        db: &dyn DefDatabase,
        module_map: &ModuleMap,
        module_items: &ModuleItemData,
        import: &ImportData,
    ) -> Vec<(bool, ModuleDefId)> {
        let ImportData {
            unqualified_name: unqualifed_name,
            module,
            is_type_import: is_type,
            ..
        } = import;
        let module = &module_items[*module];

        let Some(file_id) = module_map.file_for_module_name(&module.name) else {
            return Vec::new();
        };
        let scope = db.module_scope(file_id);
        let Some(items) = scope.declarations.get(unqualifed_name) else {
            return Vec::new();
        };
        items
            .iter()
            .filter(|i| i.1 == Visibility::Public)
            .map(|i| (is_type.clone(), i.0.clone()))
            .collect()
    }
}

pub type ScopeId = Idx<ScopeData>;

impl ops::Index<ScopeId> for ExprScopes {
    type Output = ScopeData;
    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExprScopes {
    scopes: Arena<ScopeData>,
    scope_by_expr: ArenaMap<ExprId, ScopeId>,
}

impl ExprScopes {
    pub fn expr_scopes_query(db: &dyn DefDatabase, func: FunctionId) -> Arc<ExprScopes> {
        let body = db.body(func);

        let mut this = ExprScopes {
            scopes: Arena::default(),
            scope_by_expr: ArenaMap::default(),
        };
        let root = this.root_scope();
        for param in &body.params {
            this.add_bindings(body.as_ref(), root, &param.0);
        }
        this.traverse_expr(body.as_ref(), body.body_expr, root);
        Arc::new(this)
    }

    fn root_scope(&mut self) -> ScopeId {
        self.scopes.alloc(ScopeData {
            parent: None,
            entries: vec![],
        })
    }

    pub fn shrink_to_fit(&mut self) {
        self.scopes.shrink_to_fit();
        // The size of `scope_by_expr` should be precise.
    }

    pub fn entries(&self, scope: ScopeId) -> &[ScopeEntry] {
        &self.scopes[scope].entries
    }

    pub fn scope_for_expr(&self, expr_id: ExprId) -> Option<ScopeId> {
        self.scope_by_expr.get(expr_id).copied()
    }

    pub fn scope_chain(&self, scope: Option<ScopeId>) -> impl Iterator<Item = ScopeId> + '_ {
        std::iter::successors(scope, move |&scope| self.scopes[scope].parent)
    }

    pub fn resolve_name_in_scope(&self, scope: ScopeId, name: &SmolStr) -> Option<&ScopeEntry> {
        self.scope_chain(Some(scope))
            .find_map(|scope| self.entries(scope).iter().find(|it| it.name == *name))
    }

    fn traverse_expr(&mut self, body: &Body, expr: ExprId, scope: ScopeId) {
        self.scope_by_expr.insert(expr, scope);

        match &body[expr] {
            Expr::Block { stmts } => {
                self.traverse_expr_stmts(body, stmts, scope);
            }
            Expr::Call { func, args } => {
                for arg in args {
                    self.traverse_expr(body, arg.1, scope);
                }
                self.traverse_expr(body, *func, scope);
            }
            Expr::Spread { expr } => {
                self.traverse_expr(body, *expr, scope);
            }
            Expr::Binary { left, right, op: _ } => {
                self.traverse_expr(body, *left, scope);
                self.traverse_expr(body, *right, scope);
            }
            Expr::Pipe { left, right } => {
                self.traverse_expr(body, *left, scope);
                self.traverse_expr(body, *right, scope);
            }
            Expr::FieldAccess {
                base: container,
                label: _,
                base_string: _,
                label_name: _,
            } => {
                self.traverse_expr(body, *container, scope);
            }
            Expr::TupleIndex {
                base: container,
                index: _,
                base_string: _,
            } => {
                self.traverse_expr(body, *container, scope);
            }
            Expr::Case { subjects, clauses } => {
                for s in subjects.iter() {
                    self.traverse_expr(body, *s, scope);
                }

                clauses.iter().for_each(|Clause { patterns, expr }| {
                    let clause_scope = self.scopes.alloc(ScopeData {
                        parent: Some(scope),
                        entries: Vec::new(),
                    });
                    for pat in patterns.iter() {
                        self.add_bindings(body, clause_scope, pat);
                    }
                    self.traverse_expr(body, *expr, clause_scope);
                });
            }
            Expr::Lambda {
                body: lam_body,
                params,
            } => {
                let body_scope = self.scopes.alloc(ScopeData {
                    parent: Some(scope),
                    entries: Vec::new(),
                });
                for param in params.clone() {
                    self.add_bindings(body, body_scope, &param);
                }
                self.traverse_expr(body, *lam_body, body_scope);
            }
            Expr::Tuple { fields } => {
                for field in fields.iter() {
                    self.traverse_expr(body, *field, scope);
                }
            }
            Expr::List { elements } => {
                for elem in elements.iter() {
                    self.traverse_expr(body, *elem, scope);
                }
            }
            _ => {}
        }
    }

    fn traverse_expr_stmts(&mut self, body: &Body, stmts: &Vec<Statement>, scope: ScopeId) {
        let mut scope = scope;
        for stmt in stmts {
            match stmt {
                Statement::Let {
                    pattern,
                    body: body_expr,
                } => {
                    self.traverse_expr(body, *body_expr, scope);
                    scope = self.scopes.alloc(ScopeData {
                        parent: Some(scope),
                        entries: Vec::new(),
                    });
                    self.add_bindings(body, scope, pattern);
                }
                Statement::Expr { expr } => {
                    self.traverse_expr(body, *expr, scope);
                }
                Statement::Use { patterns, expr } => {
                    self.traverse_expr(body, *expr, scope);
                    scope = self.scopes.alloc(ScopeData {
                        parent: Some(scope),
                        entries: Vec::new(),
                    });
                    for pattern in patterns {
                        self.add_bindings(body, scope, pattern);
                    }
                }
            }
        }
    }

    fn add_bindings(&mut self, body: &Body, scope: ScopeId, pattern_id: &PatternId) {
        let pattern = &body[*pattern_id];
        match &pattern {
            Pattern::Variable { name } => {
                self.scopes[scope].entries.push(ScopeEntry {
                    name: name.clone(),
                    pat: *pattern_id,
                });
            }
            Pattern::Missing => {}
            Pattern::Tuple { fields } => {
                for pattern in fields {
                    self.add_bindings(body, scope, pattern);
                }
            }
            Pattern::Spread { name } => {
                if let Some(name) = name.clone() {
                    self.scopes[scope].entries.push(ScopeEntry {
                        name,
                        pat: *pattern_id,
                    })
                }
            }
            Pattern::AlternativePattern { patterns } => {
                for pattern in patterns {
                    self.add_bindings(body, scope, pattern);
                }
            }
            Pattern::List { elements } => {
                for element in elements {
                    self.add_bindings(body, scope, element);
                }
            }
            Pattern::Hole => {}
            Pattern::VariantRef {
                name: _,
                module: _,
                fields,
            } => {
                for (_, field) in fields {
                    self.add_bindings(body, scope, field);
                }
            }
            Pattern::Literal { kind: _ } => {}
            Pattern::AsPattern { pattern, as_name } => {
                self.add_bindings(body, scope, pattern);
                as_name.map(|as_name| self.add_bindings(body, scope, &as_name));
            }
            Pattern::Concat { pattern } => {
                self.add_bindings(body, scope, pattern);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeEntry {
    name: SmolStr,
    pat: PatternId,
}

impl ScopeEntry {
    pub fn name(&self) -> &SmolStr {
        &self.name
    }

    pub fn pat(&self) -> PatternId {
        self.pat
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<ScopeId>,
    entries: Vec<ScopeEntry>,
}

pub(crate) fn dependency_order_query(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> Vec<Vec<FunctionId>> {
    let scopes = db.module_scope(file_id);
    let mut edges = Vec::new();
    for (func, _) in scopes.declarations().flatten() {
        if let ModuleDefId::FunctionId(owner_id) = func {
            let body = db.body(*owner_id);
            edges.push((owner_id.0.as_u32(), owner_id.0.as_u32()));
            body.exprs().for_each(|(e_id, expr)| {
                if let Expr::Variable(name) = expr {
                    let resolver = resolver_for_expr(db, *owner_id, e_id);
                    let Some(ResolveResult::Function(fn_id)) = resolver.resolve_name(name) else {
                        return;
                    };
                    edges.push((owner_id.0.as_u32(), fn_id.id.0.as_u32()))
                }
            });
        }
    }

    let graph: StableGraph<(), u32> = StableGraph::from_edges(edges);
    petgraph::algo::kosaraju_scc(&graph)
        .into_iter()
        .map(|v| {
            v.into_iter()
                .map(|v| FunctionId(InternId::from(v.index() as u32)))
                .collect()
        })
        .collect()
}
