mod lower;
mod nameres;
pub mod module;
mod hir;

use std::{
    sync::Arc,
};

use crate::FileId;
use crate::{base::SourceDatabase};

use syntax::{AstPtr, Parse };

pub use syntax::ast::{AstNode, BinaryOpKind as BinaryOp, Expr, UnaryOpKind as UnaryOp};

pub use self::module::{ModuleData, ModuleSourceMap};
pub use self::nameres::{ResolveResult, NameResolution, ModuleScope};

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    fn parse(&self, file_id: FileId) -> Parse;
   
    fn module_with_source_map(&self, file_id: FileId) -> (Arc<ModuleData>, Arc<ModuleSourceMap>);
    
    fn module(&self, file_id: FileId) -> Arc<ModuleData>;
    
    fn source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap>;
    
    #[salsa::invoke(ModuleScope::module_scopes_query)]
    fn scopes(&self, file_id: FileId) -> Arc<ModuleScope>;

    #[salsa::invoke(NameResolution::name_resolution_query)]
    fn name_resolution(&self, file_id: FileId) -> Arc<NameResolution>;

    #[salsa::invoke(NameResolution::dependency_order_query)]
    fn dependency_order(&self, file_id: FileId) -> Vec<Vec<usize>>;
}

fn parse(db: &dyn DefDatabase, file_id: FileId) -> Parse {
    let content = db.file_content(file_id);
    syntax::parse_module(&content)
}

fn module_with_source_map(db: &dyn DefDatabase, file_id: FileId) -> (Arc<ModuleData>, Arc<ModuleSourceMap>){
    let parse = db.parse(file_id);
    let (mut item_data, mut source_map) = lower::lower(db,  parse);
    item_data.shink_to_fit();
    source_map.shrink_to_fit();
    (Arc::new(item_data), Arc::new(source_map))
}

fn source_map(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleSourceMap> {
    db.module_with_source_map(file_id).1
}

fn module(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleData> {
    db.module_with_source_map(file_id).0
}
