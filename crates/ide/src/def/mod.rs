pub mod body;
mod fields;
pub mod hir;
pub mod hir_def;
mod lower;
pub mod module;
pub mod resolver;
mod scope;
pub mod semantics;
pub mod source;
pub mod source_analyzer;

use std::sync::Arc;

use crate::base::SourceDatabase;
use crate::FileId;
use salsa;

use syntax::{AstPtr, Parse};

pub use semantics::{classify_node, find_container, Semantics};
pub use syntax::ast::{AstNode, BinaryOpKind as BinaryOp, Expr, UnaryOpKind as UnaryOp};

use self::body::{Body, BodySourceMap};
use self::hir_def::{AdtId, AdtLoc, FunctionId, FunctionLoc, TypeAliasLoc, TypeAliasId};
use self::lower::lower_module;
pub use self::lower::ModuleItemData;
use self::scope::{
    dependency_order_query, module_scope_with_map_query, ExprScopes, ModuleScope, ModuleSourceMap,
};
pub use resolver::resolver_for_expr;

#[salsa::query_group(InternDatabaseStorage)]
pub trait InternDatabase: SourceDatabase {
    #[salsa::interned]
    fn intern_function(&self, loc: FunctionLoc) -> FunctionId;

    #[salsa::interned]
    fn intern_adt(&self, loc: AdtLoc) -> AdtId;
    
    #[salsa::interned]
    fn intern_type_alias(&self, loc: TypeAliasLoc) -> TypeAliasId;
}

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase + InternDatabase {
    fn parse(&self, file_id: FileId) -> Parse;

    fn body_with_source_map(&self, function_id: FunctionId) -> (Arc<Body>, Arc<BodySourceMap>);

    fn body(&self, function_id: FunctionId) -> Arc<Body>;

    fn body_source_map(&self, funcion_id: FunctionId) -> Arc<BodySourceMap>;

    fn module_items(&self, file_id: FileId) -> Arc<ModuleItemData>;

    #[salsa::invoke(ExprScopes::expr_scopes_query)]
    fn expr_scopes(&self, function_id: FunctionId) -> Arc<ExprScopes>;

    #[salsa::invoke(module_scope_with_map_query)]
    fn module_scope_with_map(&self, file_id: FileId) -> (Arc<ModuleScope>, Arc<ModuleSourceMap>);

    fn module_scope(&self, file_id: FileId) -> Arc<ModuleScope>;

    fn module_source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap>;
    // #[salsa::invoke(LocalNameResolution::name_resolution_query)]
    // fn name_resolution(&self, file_id: FileId) -> Arc<LocalNameResolution>;

    #[salsa::invoke(dependency_order_query)]
    fn dependency_order(&self, file_id: FileId) -> Vec<Vec<FunctionId>>;
}

fn parse(db: &dyn DefDatabase, file_id: FileId) -> Parse {
    let content = db.file_content(file_id);
    syntax::parse_module(&content)
}

fn module_items(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleItemData> {
    let parse = db.parse(file_id);
    Arc::new(lower_module(db, parse))
}

fn body_with_source_map(
    db: &dyn DefDatabase,
    function_id: FunctionId,
) -> (Arc<Body>, Arc<BodySourceMap>) {
    let (mut body, mut body_source_map) = body::lower(db, function_id);
    body.shink_to_fit();
    body_source_map.shrink_to_fit();
    (Arc::new(body), Arc::new(body_source_map))
}

fn body_source_map(db: &dyn DefDatabase, function_id: FunctionId) -> Arc<BodySourceMap> {
    db.body_with_source_map(function_id).1
}

fn body(db: &dyn DefDatabase, function_id: FunctionId) -> Arc<Body> {
    db.body_with_source_map(function_id).0
}

fn module_scope(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleScope> {
    db.module_scope_with_map(file_id).0
}

fn module_source_map(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleSourceMap> {
    db.module_scope_with_map(file_id).1
}
