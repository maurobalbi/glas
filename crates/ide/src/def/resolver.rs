use std::sync::Arc;

use indexmap::{map::Entry, IndexMap};
use smol_str::SmolStr;

use crate::{DefDatabase, FileId, ty::Ty};

use super::{
    hir_def::{VariantId, ModuleDefId, AdtId},
    module::{ExprId, PatternId},
    scope::{ModuleScope, ScopeId},
    ExprScopes, FunctionId,
};

#[derive(Debug, Clone)]
pub struct Resolver {
    /// The stack of scopes, where the inner-most scope is the last item.
    ///
    /// When using, you generally want to process the scopes in reverse order,
    /// there's `scopes` *method* for that.
    scopes: Vec<Scope>,
    module_scope: Arc<ModuleScope>,
}
#[derive(Debug, Clone)]
struct ExprScope {
    owner: FunctionId,
    expr_scopes: Arc<ExprScopes>,
    scope_id: ScopeId,
}

#[derive(Debug, Clone)]
enum Scope {
    /// Brings the generic parameters of an item into scope
    // GenericParams { def: GenericDefId, params: Interned<GenericParams> },
    /// Local bindings
    ExprScope(ExprScope),
}

#[derive(Default)]
pub struct ScopeNames {
    map: IndexMap<SmolStr, ResolveResult>,
}

impl ScopeNames {
    fn add(&mut self, name: &SmolStr, def: ResolveResult) {
        match self.map.entry(name.clone()) {
            Entry::Occupied(_) => {}
            Entry::Vacant(entry) => {
                entry.insert(def);
            }
        }
    }
}

pub enum ResolveResult {
    LocalBinding(PatternId),
    FunctionId(FunctionId),
    VariantId(VariantId),
}

impl Resolver {
    pub fn names_in_scope(&self) -> IndexMap<SmolStr, ResolveResult> {
        let mut map = ScopeNames::default();

        for scope in self.scopes() {
            match scope {
                Scope::ExprScope(scope) => {
                    for expr_scope in scope.expr_scopes.scope_chain(Some(scope.scope_id)) {
                        let entries = scope.expr_scopes.entries(expr_scope);
                        for entry in entries {
                            map.add(entry.name(), ResolveResult::LocalBinding(entry.pat()))
                        };
                    };
                },
            }
        };

        for (name, moddef) in self.module_scope.values() {
            match moddef {
                super::hir_def::ModuleDefId::FunctionId(it) => {
                    map.add(name, ResolveResult::FunctionId(it.clone()))
                },
                super::hir_def::ModuleDefId::AdtId(_) => {},
                super::hir_def::ModuleDefId::VariantId(it) => {
                    map.add(name, ResolveResult::VariantId(it.clone()))
                },
            }
        }

        map.map
    }

    pub fn resolve_type(&self, name: &SmolStr) -> Option<AdtId> {
        match self.module_scope.resovlve_type(name.clone()) {
            Some(ModuleDefId::AdtId(t)) => Some(t.clone()),
            _ => None
        }
    }

    pub fn resolve_name(&self, name: &SmolStr) -> Option<ResolveResult> {
        for scope in self.scopes() {
            match scope {
                Scope::ExprScope(scope) => {
                    let entry = scope
                        .expr_scopes
                        .resolve_name_in_scope(scope.scope_id, name);

                    if let Some(e) = entry {
                        return Some(ResolveResult::LocalBinding(e.pat()));
                    }
                } // Scope::GenericParams { params, def } => {}
            }
        }

        if let Some(res) = self.module_scope.resolve_name_locally(name.clone()) {
            match *res {
                super::hir_def::ModuleDefId::FunctionId(it) => {
                    return Some(ResolveResult::FunctionId(it))
                }
                super::hir_def::ModuleDefId::AdtId(_) => {}
                super::hir_def::ModuleDefId::VariantId(it) => {
                    return Some(ResolveResult::VariantId(it))
                }
            }
        }

        None
    }

    pub fn body_owner(&self) -> Option<FunctionId> {
        self.scopes().find_map(|scope| match scope {
            Scope::ExprScope(it) => Some(it.owner),
            _ => None,
        })
    }

    fn scopes(&self) -> impl Iterator<Item = &Scope> {
        self.scopes.iter().rev()
    }
}

// needs arbitrary_self_types to be a method... or maybe move to the def?
pub fn resolver_for_expr(db: &dyn DefDatabase, owner: FunctionId, expr_id: ExprId) -> Resolver {
    let scopes = db.expr_scopes(owner);
    resolver_for_scope(db, owner, scopes.scope_for_expr(expr_id))
}

pub fn resolver_for_toplevel(db: &dyn DefDatabase, file: FileId) -> Resolver {
    let item_map = db.module_scope(file);
    Resolver {
        scopes: Vec::with_capacity(0),
        module_scope: item_map,
    }
}

pub fn resolver_for_scope(
    db: &dyn DefDatabase,
    owner: FunctionId,
    scope_id: Option<ScopeId>,
) -> Resolver {
    let func = db.lookup_intern_function(owner);
    let item_map = db.module_scope(func.file_id);
    let scopes = db.expr_scopes(owner);
    let scope_chain = scopes.scope_chain(scope_id).collect::<Vec<_>>();
    let mut r = Resolver {
        scopes: Vec::with_capacity(scope_chain.len()),
        module_scope: item_map,
    };
    r.scopes.reserve(scope_chain.len());

    // r.push_generic_params_scope(db, owner);

    for scope in scope_chain.into_iter().rev() {
        r = r.push_expr_scope(owner, Arc::clone(&scopes), scope);
    }
    r
}

impl Resolver {
    fn push_scope(mut self, scope: Scope) -> Resolver {
        self.scopes.push(scope);
        self
    }

    fn push_expr_scope(
        self,
        owner: FunctionId,
        expr_scopes: Arc<ExprScopes>,
        scope_id: ScopeId,
    ) -> Resolver {
        self.push_scope(Scope::ExprScope(ExprScope {
            owner,
            expr_scopes,
            scope_id,
        }))
    }
}
