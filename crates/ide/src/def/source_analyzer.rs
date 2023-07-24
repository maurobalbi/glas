use std::sync::Arc;

use syntax::{
    ast::{self, AstNode},
    SyntaxNode, TextSize, AstPtr,
};

use crate::{DefDatabase, InFile, FileId, ty::{InferenceResult, TyDatabase}};

use super::{
    body::{BodySourceMap, Body},
    hir_def::{FunctionId, ModuleDefId},
    resolver::{resolver_for_scope, Resolver},
    scope::{ExprScopes, ScopeId},
};

#[derive(Debug)]
pub(crate) struct SourceAnalyzer {
    pub(crate) file_id: FileId,
    pub(crate) resolver: Resolver,
    def: Option<(FunctionId, Arc<Body>, Arc<BodySourceMap>)>,
    infer: Option<Arc<InferenceResult>>,
}

impl SourceAnalyzer {
    pub(crate) fn new_for_function(
        db: &dyn TyDatabase,
        fn_id: FunctionId,
        node @ InFile { file_id, .. }: InFile<&SyntaxNode>,
    ) -> SourceAnalyzer {
        let (body, source_map) = db.body_with_source_map(fn_id);
        let scopes = db.expr_scopes(fn_id);
        let scope = scope_for(&scopes, &source_map, node);
        let resolver = resolver_for_scope(db.upcast(), fn_id, scope);
        SourceAnalyzer {
            resolver,
            def: Some((fn_id, body, source_map)),
            infer: Some(db.infer_function(fn_id)),
            file_id,
        }
    }
}

fn scope_for(
    scopes: &ExprScopes,
    source_map: &BodySourceMap,
    node: InFile<&SyntaxNode>,
) -> Option<ScopeId> {
    node.value
        .ancestors()
        .filter_map(ast::Expr::cast)
        .filter_map(|it| source_map.expr_for_node(InFile::new(node.file_id, &it)))
        .find_map(|it| scopes.scope_for_expr(it))
}

pub fn find_def(db: &dyn DefDatabase, node: InFile<&SyntaxNode>) -> Option<ModuleDefId> {
    let module = db.module_scope(node.file_id);
    for node in node.ancestors() {
        if let Some(def) = ast::ModuleStatement::cast(node.value) {
            match def {
                ast::ModuleStatement::Function(it) => {
                    for (id, _) in module.declarations() {
                        match id {
                            ModuleDefId::FunctionId(func_id) => {
                                let func = db.lookup_intern_function(*func_id);
                                let data = &db.module_items(node.file_id)[func.value];
                                if data.ast_ptr == AstPtr::new(&it) {
                                    return Some(id.clone());
                                }
                            }
                            _ => {}
                        }
                    }
                },
                _ => {}
            }
        }
    }
    None
}
