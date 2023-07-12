use std::{collections::HashMap, iter, ops, sync::Arc};

use la_arena::{Arena, ArenaMap, Idx};
use smol_str::SmolStr;

use crate::{DefDatabase, FileId};

use super::{
    body::Body,
    module::{Expr, ExprId, Import, Pattern, PatternId, Statement, Visibility},
    FunctionId, hir_def::ModuleDefId,
};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleScope {
    values: HashMap<SmolStr, (ModuleDefId, Visibility)>,

    /// The defs declared in this scope. Each def has a single scope where it is
    /// declared.
    declarations: Vec<ModuleDefId>,
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
        let func = db.lookup_intern_function(func);
        let func = db.module_items(func.file_id)[func.value];
        let mut this = ExprScopes {
            scopes: Arena::default(),
            scope_by_expr: ArenaMap::default(),
        };
        let mut root = this.root_scope();
        for param in &body.params {
            this.add_bindings(body.as_ref(), root, param);
        }
        this.traverse_expr(
            body.as_ref(),
            body.body_expr
                .expect("body should always have an body_expr!"),
            root,
        );
        Arc::new(this)
    }

    fn root_scope(&mut self) -> ScopeId {
        self.scopes.alloc(ScopeData {
            parent: None,
            entries: vec![],
        })
    }

    // pub(crate) fn expr_scopes_query(db: &dyn DefDatabase, function_id: FunctionId) -> Arc<Self> {

    //     let mut this = Self {
    //         scopes: Arena::new(),
    //         scope_by_expr: ArenaMap::with_capacity(module.exprs.len()),
    //     };

    //     let import_definitions = module.imports().filter_map(|(_, import)| {
    //         Some((import.local_name(), this.resolve_import(db, import)?))
    //     });

    //     let definitions = module
    //         .functions()
    //         .map(|(_, f)| (module[f.name].text.clone(), (f.name, file_id)));

    //     let import_scope = this.scopes.alloc(ScopeData {
    //         parent: None,
    //         kind: ScopeKind::Imports(import_definitions.collect()),
    //     });

    //     let root_scope = this.scopes.alloc(ScopeData {
    //         parent: Some(import_scope),
    //         kind: ScopeKind::Definitions(definitions.collect()),
    //     });

    //     for (function, _) in module.functions.iter() {
    //         this.traverse_function(function, &module, root_scope);
    //     }

    //     this.shrink_to_fit();
    //     Arc::new(this)
    // }

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
                    tracing::info!("SCOPE: {:#?}", scope);
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

    fn add_bindings(&self, body: &Body, scope: ScopeId, pattern_id: &PatternId) {
        let pattern = &body[*pattern_id];
        match pattern {
            Pattern::Variable { name } => {
                self.scopes[scope].entries.push(ScopeEntry {
                    name: *name,
                    pat: *pattern_id,
                });
            }
            Pattern::Record { args: _ } => todo!(),
            Pattern::Missing => {}
        }
    }

    // fn resolve_import(&self, db: &dyn DefDatabase, import: &Import) -> Option<(NameId, FileId)> {
    //     let Import {
    //         unqualified_name: unqualifed_name,
    //         module,
    //         ..
    //     } = import;
    //     let file_id = db.module_map().file_for_module_name(module.clone())?;
    //     let scopes = db.expr_scopes(file_id);
    //     let Some((_, scope)) = scopes.scopes.iter().nth(1) else {return None};
    //     let name_id = scope.as_definitions()?.get(unqualifed_name)?;
    //     Some((name_id.0, file_id))
    // }
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

//     pub(crate) fn dependency_order_query(db: &dyn DefDatabase, file_id: FileId) -> Vec<Vec<u32>> {
//         let module = db.module(file_id);
//         let scopes = db.scopes(file_id);
//         let edges = module
//             .exprs()
//             .filter_map(|(e_id, expr)| match expr {
//                 Expr::NameRef(name) => {
//                     let ResolveResult((name_id, f_id)) = scopes.resolve_name(e_id, name)?;
//                     if file_id != f_id || module[name_id].kind != NameKind::Function {
//                         return None;
//                     };
//                     Some((
//                         name_id.into_raw().into(),
//                         module.expr_to_owner.get(&e_id).unwrap().into_raw().into(),
//                     ))
//                 }
//                 _ => None,
//             })
//             .collect::<Vec<(u32, u32)>>();
//         let graph: StableGraph<(), u32> = StableGraph::from_edges(edges);
//         petgraph::algo::kosaraju_scc(&graph)
//             .into_iter()
//             .map(|v| v.into_iter().map(|v| v.index() as u32).collect())
//             .collect()
//         // graph::into_dependency_order(graph).into_iter().map(|v| v.into_iter().map(|v| v.index()).collect()).collect()
//     }

//     pub fn get(&self, expr: ExprId) -> Option<&ResolveResult> {
//         self.resolve_map.get(&expr)?.as_ref()
//     }
// }
