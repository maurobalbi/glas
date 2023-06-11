use std::{
    collections::{HashMap},
    iter, ops,
    sync::Arc,
};

use la_arena::{Arena, ArenaMap, Idx};
use smol_str::SmolStr;

use crate::{DefDatabase, FileId};

use super::{
    module::{
        Expr, ExprId, FunctionId, ModuleData, ModuleStatementId, Name, NameId, Statement,
        Visibility,
    },
};

// #[derive(Debug, Default, PartialEq, Eq)]
// pub struct ModuleScope {
//     /// Where does this module come from?
//     types: HashMap<Name, (ModuleStatementId, Visibility)>,
//     values: HashMap<Name, (ModuleStatementId, Visibility)>,
//     unresolved: HashSet<Name>,

//     /// The defs declared in this scope. Each def has a single scope where it is
//     /// declared.
//     declarations: Vec<ModuleStatementId>,
// }

/// The resolve result of a name reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveResult {
    /// Reference to a Name.
    Definition(NameId),
}

pub type ScopeId = Idx<ScopeData>;

impl ops::Index<ScopeId> for ModuleScope {
    type Output = ScopeData;
    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleScope {
    scopes: Arena<ScopeData>,
    scope_by_expr: ArenaMap<ExprId, ScopeId>,
}

impl ModuleScope {
    pub(crate) fn module_scopes_query(db: &dyn DefDatabase, file_id: FileId) -> Arc<Self> {
        let module = db.module(file_id);
        let mut this = Self {
            scopes: Arena::new(),
            scope_by_expr: ArenaMap::with_capacity(module.exprs.len()),
        };

        let definitions = module
            .functions
            .iter()
            .map(|(idx, f)| (module[f.name].text.clone(), f.name));

        let root_scope = this.scopes.alloc(ScopeData {
            parent: None,
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
            return Some(ResolveResult::Definition(*name));
        }
        None
    }

    fn traverse_function(&mut self, function_id: FunctionId, module: &ModuleData, scope: ScopeId) {
        let mut defs = HashMap::default();

        let function = &module[function_id];

        for param in &function.params {
            let name = &module[param.name];
            defs.insert(name.text.clone(), param.name);
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
                self.traverse_expr(module, *func, scope);
            }
            _ => {}
        }
    }

    fn traverse_expr_stmts(&mut self, module: &ModuleData, stmts: &Vec<Statement>, scope: ScopeId) {
        let mut scope = scope;
        for stmt in stmts {
            match stmt {
                Statement::Let { name, body } => {
                    tracing::info!("SCOPE: {:#?}", scope);
                    self.traverse_expr(module, *body, scope);
                    let defs = [(module[*name].text.clone(), *name)].into_iter().collect();
                    scope = self.scopes.alloc(ScopeData {
                        parent: Some(scope),
                        kind: ScopeKind::Definitions(defs),
                    });
                }
                Statement::Expr { expr, .. } => {
                    self.traverse_expr(module, *expr, scope);
                }
            }
        }
    }
    //      self.scope_by_expr.insert(expr, scope);

    //     match &module[expr] {
    //         Expr::Lambda(param, pat, body) => {
    //             let mut defs = HashMap::default();
    //             if let &Some(name_id) = param {
    //                 defs.insert(module[name_id].text.clone(), name_id);
    //             }
    //             if let Some(pat) = pat {
    //                 for name_id in pat.fields.iter().filter_map(|(opt_id, _)| *opt_id) {
    //                     defs.insert(module[name_id].text.clone(), name_id);
    //                 }
    //             }

    //             let scope = if !defs.is_empty() {
    //                 self.scopes.alloc(ScopeData {
    //                     parent: Some(scope),
    //                     kind: ScopeKind::Definitions(defs),
    //                 })
    //             } else {
    //                 scope
    //             };

    //             if let Some(pat) = pat {
    //                 for default_expr in pat.fields.iter().filter_map(|(_, e)| *e) {
    //                     self.traverse_expr(module, default_expr, scope);
    //                 }
    //             }
    //             self.traverse_expr(module, *body, scope);
    //         }
    //         Expr::With(env, body) => {
    //             self.traverse_expr(module, *env, scope);
    //             let scope = self.scopes.alloc(ScopeData {
    //                 parent: Some(scope),
    //                 kind: ScopeKind::WithExpr(expr),
    //             });
    //             self.traverse_expr(module, *body, scope);
    //         }
    //         Expr::Attrset(bindings) | Expr::RecAttrset(bindings) | Expr::LetAttrset(bindings) => {
    //             self.traverse_bindings(module, bindings, scope);
    //         }
    //         Expr::LetIn(bindings, body) => {
    //             let scope = self.traverse_bindings(module, bindings, scope);
    //             self.traverse_expr(module, *body, scope);
    //         }
    //         e => e.walk_child_exprs(|e| self.traverse_expr(module, e, scope)),
    //     }
    // }

    // fn traverse_bindings(
    //     &mut self,
    //     module: &Module,
    //     bindings: &Bindings,
    //     scope: ScopeId,
    // ) -> ScopeId {
    //     let mut defs = HashMap::default();

    //     for &(name, value) in bindings.statics.iter() {
    //         if module[name].kind.is_definition() {
    //             defs.insert(module[name].text.clone(), name);
    //         }

    //         // Inherited attrs are resolved in the outer scope.
    //         if let BindingValue::Inherit(expr) = value {
    //             assert!(matches!(&module[expr], Expr::Reference(_)));
    //             self.traverse_expr(module, expr, scope);
    //         }
    //     }

    //     let scope = if defs.is_empty() {
    //         scope
    //     } else {
    //         self.scopes.alloc(ScopeData {
    //             parent: Some(scope),
    //             kind: ScopeKind::Definitions(defs),
    //         })
    //     };

    //     for &(_, value) in bindings.statics.iter() {
    //         match value {
    //             // Traversed before.
    //             BindingValue::Inherit(_) |
    //             // Traversed later.
    //             BindingValue::InheritFrom(_) => {},
    //             BindingValue::Expr(e) => {
    //                 self.traverse_expr(module, e, scope);
    //             }
    //         }
    //     }
    //     for &e in bindings.inherit_froms.iter() {
    //         self.traverse_expr(module, e, scope);
    //     }
    //     for &(k, v) in bindings.dynamics.iter() {
    //         self.traverse_expr(module, k, scope);
    //         self.traverse_expr(module, v, scope);
    //     }
    //     scope
    // }
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
    Definitions(HashMap<SmolStr, NameId>),
}

impl ScopeData {
    pub fn as_definitions(&self) -> Option<&HashMap<SmolStr, NameId>> {
        match &self.kind {
            ScopeKind::Definitions(defs) => Some(defs),
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
            .filter_map(|(e, kind)| {
                match kind {
                    // Inherited attrs are also translated into Expr::References.
                    Expr::NameRef(name) => Some((e, scopes.resolve_name(e, name))),
                    _ => None,
                }
            })
            .collect::<HashMap<_, _>>();

        Arc::new(Self { resolve_map })
    }

    pub fn get(&self, expr: ExprId) -> Option<&ResolveResult> {
        self.resolve_map.get(&expr)?.as_ref()
    }
}
