use std::sync::Arc;

use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode},
    AstPtr, SyntaxNode,
};

use crate::{
    ty::{FieldResolution, InferenceResult, Ty, TyDatabase},
    DefDatabase, FileId, InFile,
};

use super::{
    body::{Body, BodySourceMap},
    hir_def::{AdtId, FunctionId, ModuleDefId},
    module::{ExprId, PatternId},
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

    pub(crate) fn new_for_resolver(
        resolver: Resolver,
        node: InFile<&SyntaxNode>,
    ) -> SourceAnalyzer {
        SourceAnalyzer {
            resolver,
            def: None,
            infer: None,
            file_id: node.file_id,
        }
    }

    fn body_source_map(&self) -> Option<&BodySourceMap> {
        self.def.as_ref().map(|(.., source_map)| &**source_map)
    }
    fn body(&self) -> Option<&Body> {
        self.def.as_ref().map(|(_, body, _)| &**body)
    }

    fn expr_id(&self, expr: &ast::Expr) -> Option<ExprId> {
        let src = InFile::new(self.file_id, expr.clone());
        let sm = self.body_source_map()?;
        sm.expr_for_node(src.as_ref())
    }

    fn pattern_id(&self, pat: &ast::Pattern) -> Option<PatternId> {
        // FIXME: macros, see `expr_id`
        let src = InFile {
            file_id: self.file_id,
            value: pat,
        };
        self.body_source_map()?.pattern_for_node(src)
    }

    pub(crate) fn type_of_expr(&self, db: &dyn DefDatabase, expr: &ast::Expr) -> Option<Ty> {
        let expr_id = self.expr_id(expr)?;
        let infer = self.infer.as_ref()?;
        let ty = infer.ty_for_expr(expr_id).clone();
        Some(ty)
    }

    pub(crate) fn type_of_pattern(
        &self,
        db: &dyn DefDatabase,
        pattern: &ast::Pattern,
    ) -> Option<Ty> {
        let pattern_id = self.pattern_id(pattern)?;
        let infer = self.infer.as_ref()?;
        let ty = infer.ty_for_pattern(pattern_id).clone();
        Some(ty)
    }

    pub(crate) fn resolve_field(&self, call: &ast::FieldAccessExpr) -> Option<FieldResolution> {
        let expr_id = self.expr_id(&call.clone().into())?;
        self.infer
            .as_ref()
            .and_then(|i| i.resolve_field(expr_id))
            .clone()
    }

    pub(crate) fn resolve_module(&self, expr: &ast::Expr) -> Option<FileId> {
        let expr_id = self.expr_id(&expr)?;
        self.infer
            .as_ref()
            .and_then(|i| i.resolve_module(expr_id))
            .clone()
    }

    // pub(crate) fn resolve_field_access_expr(
    //     &self,
    //     db: &dyn DefDatabase,
    //     field_expr: &ast::FieldAccessExpr,
    // ) -> Option<FunctionId> {
    //     let base_ty = self.type_of_expr(db, &field_expr.container()?)?;

    // let (op_trait, op_fn) = self.lang_trait_fn(db, LangItem::Index, &name![index])?;
    // // HACK: subst for all methods coincides with that for their trait because the methods
    // // don't have any generic parameters, so we skip building another subst for the methods.
    // let substs = hir_ty::TyBuilder::subst_for_def(db, op_trait, None)
    //     .push(base_ty.clone())
    //     .push(index_ty.clone())
    //     .build();
    // Some(self.resolve_impl_method_or_trait_def(db, op_fn, substs))
    // }
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
