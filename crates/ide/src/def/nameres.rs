use std::{collections::{HashMap}, iter, ops, sync::Arc};

use la_arena::{Arena, ArenaMap, Idx};
use petgraph::stable_graph::StableGraph;
use smol_str::SmolStr;

use crate::{DefDatabase, FileId};

use super::{module::{
    Expr, ExprId, FunctionId, Import, ModuleData, Name, NameId, Pattern, PatternId, Statement, NameKind,
}};

/// The resolve result of a name reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolveResult(pub Definition);

pub type Definition = (NameId, FileId);

pub type ScopeId = Idx<ScopeData>;

impl ops::Index<ScopeId> for ModuleScope {
    type Output = ScopeData;
    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleScope {
    file_id: FileId,
    scopes: Arena<ScopeData>,
    scope_by_expr: ArenaMap<ExprId, ScopeId>,
}

impl ModuleScope {
    pub(crate) fn module_scopes_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<Self> {
        let module = db.module(file_id);
        let mut this = Self {
            file_id,
            scopes: Arena::new(),
            scope_by_expr: ArenaMap::with_capacity(module.exprs.len()),
        };

        let import_definitions = module.imports().filter_map(|(_, import)| {
            Some((import.local_name(), this.resolve_import(db, import)?))
        });

        let definitions = module
            .functions()
            .map(|(_, f)| (module[f.name].text.clone(), (f.name, file_id)));

        let import_scope = this.scopes.alloc(ScopeData {
            parent: None,
            kind: ScopeKind::Imports(import_definitions.collect()),
        });

        let root_scope = this.scopes.alloc(ScopeData {
            parent: Some(import_scope),
            kind: ScopeKind::Definitions(definitions.collect()),
        });

        for (function, _) in module.functions.iter() {
            this.traverse_function(function, &module, root_scope);
        }

        this.shrink_to_fit();
        Arc::new(this)
    }

    pub fn shrink_to_fit(&mut self) {
        self.scopes.shrink_to_fit();
        // The size of `scope_by_expr` should be precise.
    }

    pub fn scope_for_expr(&self, expr_id: ExprId) -> Option<ScopeId> {
        self.scope_by_expr.get(expr_id).copied()
    }

    pub fn ancestors(&self, scope_id: ScopeId) -> impl Iterator<Item = &'_ ScopeData> + '_ {
        iter::successors(Some(scope_id), |&i| self[i].parent).map(|i| &self[i])
    }

    /// Resolve a name in the scope of an Expr.
    fn resolve_name(&self, expr_id: ExprId, name: &SmolStr) -> Option<ResolveResult> {
        let scope = self.scope_for_expr(expr_id)?;
        //     // 1. Local defs.
        if let Some(name) = self
            .ancestors(scope)
            .find_map(|data| data.as_definitions()?.get(name))
        {
            return Some(ResolveResult(*name));
        }

        if let Some(name) = self
            .ancestors(scope)
            .find_map(|data| data.as_imports()?.get(name))
        {
            return Some(ResolveResult(*name));
        }
        None
    }

    fn traverse_function(&mut self, function_id: FunctionId, module: &ModuleData, scope: ScopeId) {
        let mut defs = HashMap::default();

        let function = &module[function_id];

        for param in &function.params {
            let name = &module[param.name];
            defs.insert(name.text.clone(), (param.name, self.file_id));
        }

        let scope = if !defs.is_empty() {
            self.scopes.alloc(ScopeData {
                parent: Some(scope),
                kind: ScopeKind::Definitions(defs),
            })
        } else {
            scope
        };

        self.traverse_expr(module, function.body, scope);
    }

    fn traverse_expr(&mut self, module: &ModuleData, expr: ExprId, scope: ScopeId) {
        self.scope_by_expr.insert(expr, scope);

        match &module[expr] {
            Expr::Block { stmts } => {
                self.traverse_expr_stmts(module, stmts, scope);
            }
            Expr::Call { func, args } => {
                for arg in args {
                    self.traverse_expr(module, *arg, scope);
                }
                self.traverse_expr(module, *func, scope);
            }
            Expr::Binary { left, right, op: _ } => {
                self.traverse_expr(module, *left, scope);
                self.traverse_expr(module, *right, scope);
            }
            _ => {}
        }
    }

    fn traverse_expr_stmts(&mut self, module: &ModuleData, stmts: &Vec<Statement>, scope: ScopeId) {
        let mut scope = scope;
        for stmt in stmts {
            match stmt {
                Statement::Let { pattern, body } => {
                    tracing::info!("SCOPE: {:#?}", scope);
                    self.traverse_expr(module, *body, scope);
                    let mut defs = HashMap::new();
                    self.collect_pattern(module, pattern, &mut defs);
                    scope = self.scopes.alloc(ScopeData {
                        parent: Some(scope),
                        kind: ScopeKind::Definitions(defs),
                    });
                }
                Statement::Expr { expr, .. } => {
                    self.traverse_expr(module, *expr, scope);
                }
                Statement::Use { patterns, expr } => {
                    self.traverse_expr(module, *expr, scope);
                    let mut defs = HashMap::new();
                    for pattern in patterns {
                        self.collect_pattern(module, pattern, &mut defs);
                    }
                    scope = self.scopes.alloc(ScopeData {
                        parent: Some(scope),
                        kind: ScopeKind::Definitions(defs),
                    })
                }
            }
        }
    }

    fn collect_pattern(
        &self,
        module: &ModuleData,
        pattern: &PatternId,
        defs: &mut HashMap<SmolStr, Definition>,
    ) {
        let pattern = &module[*pattern];
        match pattern {
            Pattern::Variable { name } => {
                defs.insert(
                    module[*name].text.clone().into(),
                    ((*name).clone(), self.file_id),
                );
            }
            Pattern::Record { args: _ } => todo!(),
        }
    }

    fn resolve_import(&self, db: &dyn DefDatabase, import: &Import) -> Option<(NameId, FileId)> {
        let Import {
            unqualifed_name,
            module,
            ..
        } = import;
        let file_id = db.module_map().file_for_module_name(module.clone())?;
        let scopes = db.scopes(file_id);
        let Some((_, scope)) = scopes.scopes.iter().nth(1) else {return None};
        let name_id = scope.as_definitions()?.get(unqualifed_name)?;
        Some((name_id.0, file_id))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeEntry {
    name: Name,
    // pat: PatId,
}

impl ScopeEntry {
    pub fn name(&self) -> &Name {
        &self.name
    }

    // pub fn pat(&self) -> PatId {
    //     self.pat
    // }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<ScopeId>,
    // label: Option<(LabelId, Name)>,
    kind: ScopeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ScopeKind {
    Definitions(HashMap<SmolStr, Definition>),
    Imports(HashMap<SmolStr, Definition>),
}

impl ScopeData {
    pub fn as_definitions(&self) -> Option<&HashMap<SmolStr, Definition>> {
        match &self.kind {
            ScopeKind::Definitions(defs) => Some(defs),
            _ => None,
        }
    }

    pub fn as_imports(&self) -> Option<&HashMap<SmolStr, Definition>> {
        match &self.kind {
            ScopeKind::Imports(defs) => Some(defs),
            _ => None,
        }
    }
}

/// Name resolution of all references.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct NameResolution {
    // `None` value for unresolved names.
    // Mauro: Make an enum for all potential resolvable names. E.g. Generics, etc
    resolve_map: HashMap<ExprId, Option<ResolveResult>>,
    // // All names from the common pattern `inherit (builtins) ...`.
    // // This is used for tracking builtins names even through alising.
    // inherited_builtins: HashSet<NameId>,
}

impl NameResolution {
    pub(crate) fn name_resolution_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<Self> {
        let module = db.module(file_id);
        let scopes = db.scopes(file_id);
        tracing::info!("Scopes: {:#?}", scopes);
        let resolve_map = module
            .exprs()
            .filter_map(|(e, kind)| match kind {
                Expr::NameRef(name) => Some((e, scopes.resolve_name(e, name))),
                _ => None,
            })
            .collect::<HashMap<_, _>>();

        Arc::new(Self { resolve_map })
    }

    pub(crate) fn dependency_order_query(db: &dyn DefDatabase, file_id: FileId) -> Vec<Vec<usize>> {
        let module = db.module(file_id);
        let scopes = db.scopes(file_id);
        let edges = module.exprs().filter_map(|(e_id, expr)| match expr {
            Expr::NameRef(name) => {
                let ResolveResult((name_id, f_id)) = scopes.resolve_name(e_id, name)?;
                if file_id != f_id || module[name_id].kind != NameKind::Function{
                    return None;
                };
                Some((name_id.into_raw().into(), module.expr_to_owner.get(&e_id).unwrap().into_raw().into()))
            },
            _ => None,
        }).collect::<Vec<(u32, u32)>>();
        let graph:StableGraph<(), u32> = StableGraph::from_edges(edges);
        petgraph::algo::kosaraju_scc(&graph).into_iter().map(|v| v.into_iter().map(|v| v.index()).collect()).collect()
        // graph::into_dependency_order(graph).into_iter().map(|v| v.into_iter().map(|v| v.index()).collect()).collect()
    }

    pub fn get(&self, expr: ExprId) -> Option<&ResolveResult> {
        self.resolve_map.get(&expr)?.as_ref()
    }
}
