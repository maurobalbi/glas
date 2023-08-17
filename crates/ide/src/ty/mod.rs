pub mod display;
mod infer;
mod union_find;

#[cfg(test)]
mod tests;

use std::{collections::HashMap, sync::Arc};

pub use infer::InferenceResult;
use smol_str::SmolStr;

use crate::{
    def::hir_def::{AdtId, FunctionId},
    ide::Upcast,
    DefDatabase,
};

#[salsa::query_group(TyDatabaseStorage)]
pub trait TyDatabase: DefDatabase + Upcast<dyn DefDatabase> {
    #[salsa::invoke(infer::infer_function_query)]
    fn infer_function(&self, fn_id: FunctionId) -> Arc<InferenceResult>;

    #[salsa::invoke(infer::infer_function_group_query)]
    fn infer_function_group(&self, group: Vec<FunctionId>) -> HashMap<FunctionId, InferenceResult>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unknown,
    Generic {name: SmolStr},
    Bool,
    Int,
    Float,
    String,
    List {
        of: Arc<Ty>
    },
    Function {
        params: Arc<Vec<Ty>>,
        return_: Arc<Ty>,
    },
    Adt {
        name: SmolStr, 
        params: Arc<Vec<Ty>>,
    },
    Tuple {
        fields: Arc<Vec<Ty>>,
    },
}
