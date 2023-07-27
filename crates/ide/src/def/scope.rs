use std::{ops, sync::Arc};

use indexmap::IndexMap;
use la_arena::{Arena, ArenaMap, Idx};
use petgraph::stable_graph::StableGraph;
use salsa::InternId;
use smol_str::SmolStr;

use crate::{DefDatabase, FileId, InFile};

use super::{
    body::Body,
    hir_def::{AdtLoc, FunctionLoc, ModuleDefId, VariantLoc},
    module::{Expr, ExprId, Import, Pattern, PatternId, Statement, Visibility},
    resolver::ResolveResult,
    resolver_for_expr, FunctionId,
};

pub fn module_scope_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleScope> {
    let module_data = db.module_items(file_id);

    let mut scope = ModuleScope::default();

    for (_, import) in module_data.imports() {
        if let Some(val) = scope.resolve_import(db, import) {
            match val {
                ModuleDefId::AdtId(_) => scope.types.insert(import.local_name(), val),
                _ => scope.values.insert(import.local_name(), val),
            };
        }
    }

    for (func_id, func) in module_data.functions() {
        let name = &func.name;
        let func_loc = db.intern_function(FunctionLoc {
            file_id,
            value: func_id,
        });
        let def = ModuleDefId::FunctionId(func_loc);
        scope.values.insert(name.clone(), def.clone());
        scope
            .declarations
            .insert(name.clone(), (def, func.visibility.clone()));
    }

    for (adt_id, adt) in module_data.adts() {
        let name = &adt.name;
        let adt_loc = db.intern_adt(AdtLoc {
            file_id,
            value: adt_id,
        });
        let def = ModuleDefId::AdtId(adt_loc);
        scope.types.insert(name.clone(), def.clone());
        scope
            .declarations
            .insert(name.clone(), (def, adt.visibility.clone()));

        for variant_id in adt.constructors.clone() {
            let variant = &module_data[variant_id];
            let name = &variant.name;
            let variant_loc = db.intern_variant(VariantLoc {
                parent: adt_loc,
                value: InFile {
                    value: variant_id,
                    file_id,
                },
            });
            let def = ModuleDefId::VariantId(variant_loc);
            scope.values.insert(name.clone(), def.clone());

            //use visibility of adt
            scope
                .declarations
                .insert(name.clone(), (def, adt.visibility.clone()));
        }
    }

    Arc::new(scope)
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleScope {
    /// The values visible in this module including imports
    values: IndexMap<SmolStr, ModuleDefId>,
    types: IndexMap<SmolStr, ModuleDefId>,

    /// The defs declared in this module which can potentially be imported in another module
    declarations: IndexMap<SmolStr, (ModuleDefId, Visibility)>,
}

impl ModuleScope {
    pub fn resolve_name_locally(&self, name: SmolStr) -> Option<&ModuleDefId> {
        self.values.get(&name)
    }

    pub fn values(&self) -> impl Iterator<Item = &ModuleDefId> + ExactSizeIterator + '_ {
        self.values.iter().map(|v| v.1)
    }

    pub fn types(&self) -> impl Iterator<Item = &ModuleDefId> + ExactSizeIterator + '_ {
        self.types.iter().map(|v| v.1)
    }

    pub fn declarations(
        &self,
    ) -> impl Iterator<Item = &(ModuleDefId, Visibility)> + ExactSizeIterator + '_ {
        self.declarations.iter().map(|v| v.1)
    }

    fn resolve_import(&self, db: &dyn DefDatabase, import: &Import) -> Option<ModuleDefId> {
        let Import {
            unqualified_name: unqualifed_name,
            module,
            ..
        } = import;
        let file_id = db.module_map().file_for_module_name(module.clone())?;
        let scope = db.module_scope(file_id);
        let Some((item, Visibility::Public)) = scope.declarations.get(unqualifed_name) else {
            return None
        };
        Some(item.clone())
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
            this.add_bindings(body.as_ref(), root, param);
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
                    self.traverse_expr(body, *arg, scope);
                }
                self.traverse_expr(body, *func, scope);
            }
            Expr::Binary { left, right, op: _ } => {
                self.traverse_expr(body, *left, scope);
                self.traverse_expr(body, *right, scope);
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
                Statement::Expr { expr, .. } => {
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
        match pattern {
            Pattern::Variable { name } => {
                self.scopes[scope].entries.push(ScopeEntry {
                    name: name.clone(),
                    pat: *pattern_id,
                });
            }
            Pattern::Record { args: _ } => todo!(),
            Pattern::Missing => {}
            Pattern::Tuple { fields } => {
                for pattern in fields {
                    self.add_bindings(body, scope, pattern);
                }
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
    // label: Option<(LabelId, Name)>,
    entries: Vec<ScopeEntry>,
}

// // Refactor to rust resolution
// /// Name resolution of all references.
// #[derive(Default, Debug, Clone, PartialEq, Eq)]
// pub struct LocalNameResolution {
//     // `None` value for unresolved names.
//     // Mauro: Make an enum for all potential resolvable names. E.g. Generics, etc
//     resolve_map: HashMap<ExprId, Option<PatternId>>,
//     // // All names from the common pattern `inherit (builtins) ...`.
//     // // This is used for tracking builtins names even through alising.
//     // inherited_builtins: HashSet<NameId>,
// }

// impl LocalNameResolution {
//     pub(crate) fn name_resolution_query(
//         db: &dyn DefDatabase,
//         function_id: FunctionId,
//     ) -> Arc<Self> {
//         let loc = db.lookup_intern_function(function_id);
//         let body = db.body(function_id);
//         let scopes = db.scopes(function_id);
//         tracing::info!("Scopes: {:#?}", scopes);
//         let resolve_map = body
//             .exprs()
//             .filter_map(|(e, kind)| match kind {
//                 Expr::NameRef(name) => Some((e, scopes.resolve_name(e, name))),
//                 _ => None,
//             })
//             .collect::<HashMap<_, _>>();

//         Arc::new(Self { resolve_map })
//     }

//     pub fn get(&self, expr: ExprId) -> Option<&ResolveResult> {
//         self.resolve_map.get(&expr)?.as_ref()
//     }
// }

pub(crate) fn dependency_order_query(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> Vec<Vec<FunctionId>> {
    let scopes = db.module_scope(file_id);
    let mut edges = Vec::new();
    for (func, _) in scopes.declarations() {
        match func {
            ModuleDefId::FunctionId(owner_id) => {
                let body = db.body(owner_id.clone());
                edges.push((owner_id.clone().0.as_u32(), owner_id.clone().0.as_u32()));
                body.exprs().for_each(|(e_id, expr)|
                    match expr {
                        Expr::NameRef(name) => {
                            let resolver = resolver_for_expr(db, owner_id.clone(), e_id);
                            let Some(ResolveResult::FunctionId(fn_id)) = resolver.resolve_name(name) else {return};
                            edges.push((owner_id.clone().0.as_u32(), fn_id.0.as_u32()))
                        },
                        _ => {},
                });
            }
            _ => {}
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
    // graph::into_dependency_order(graph).into_iter().map(|v| v.into_iter().map(|v| v.index()).collect()).collect()
}
