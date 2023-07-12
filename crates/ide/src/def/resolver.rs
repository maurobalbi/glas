use std::{sync::Arc, ops::Deref};

use smol_str::SmolStr;

use crate::DefDatabase;

use super::{lower::ModuleItemData, FunctionId, ExprScopes, scope::{ScopeId, ModuleScope}, module::{ExprId, PatternId}};

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

pub enum ResolveResult {
    LocalBinding(PatternId),
    FunctionId(FunctionId)
}

impl Resolver {
   pub fn resolve_name(
        &self,
        db: &dyn DefDatabase,
        name: &SmolStr,
    ) -> Option<ResolveResult> {
        for scope in self.scopes() {
            match scope {
                Scope::ExprScope(scope) => {
                    let entry = scope
                        .expr_scopes
                        .entries(scope.scope_id)
                        .iter()
                        .find(|entry| entry.name() == name);

                    if let Some(e) = entry {
                        return Some(ResolveResult::LocalBinding(e.pat()));
                    }
                }
                // Scope::GenericParams { params, def } => {}

            }
        }

        if let res @ Some(_) = self.module_scope.resolve_name_locally(db, name) {
            return res;
        }

        None
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

pub fn resolver_for_scope(
    db: &dyn DefDatabase,
    owner: FunctionId,
    scope_id: Option<ScopeId>,
) -> Resolver {
    let func = db.lookup_intern_function(owner);
    let item_map = db.module_items(func.file_id);
    let scopes = db.expr_scopes(owner);
    let scope_chain = scopes.scope_chain(scope_id).collect::<Vec<_>>();
    let mut r = Resolver {
        scopes: Vec::with_capacity(scope_chain.len()),
        module_scope: item_map
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

    // fn push_generic_params_scope(self, db: &dyn DefDatabase, def: GenericDefId) -> Resolver {
    //     let params = db.generic_params(def);
    //     self.push_scope(Scope::GenericParams { def, params })
    //     self
    // }

    fn push_expr_scope(
        self,
        owner: FunctionId,
        expr_scopes: Arc<ExprScopes>,
        scope_id: ScopeId,
    ) -> Resolver {
        self.push_scope(Scope::ExprScope(ExprScope { owner, expr_scopes, scope_id }))
    }
}
