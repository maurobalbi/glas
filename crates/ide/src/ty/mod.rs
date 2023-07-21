mod infer;
mod union_find;
mod display;

#[cfg(test)]
mod tests;

use std::{sync::Arc, collections::HashMap};

pub use infer::InferenceResult;

use crate::{DefDatabase, FileId, def::{module::NameId, hir_def::FunctionId}, ide::Upcast};


#[salsa::query_group(TyDatabaseStorage)]
pub trait TyDatabase: DefDatabase + Upcast<dyn DefDatabase>{
    #[salsa::invoke(infer::infer_function_query)]
    fn infer_function(&self, fn_id: FunctionId) -> Arc<InferenceResult>;
    
    #[salsa::invoke(infer::infer_function_group_query)]
    fn infer_function_group(&self, group: Vec<FunctionId>) -> HashMap<FunctionId, InferenceResult>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unknown,
    Generic { idx: u32 },
    Int,
    Float,
    String,
    Function {
        params: Arc<Vec<Ty>>, 
        return_: Arc<Ty>
    },
    Tuple {
        fields: Arc<Vec<Ty>>
    },
}
