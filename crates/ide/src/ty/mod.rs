pub mod display;
mod infer;
mod union_find;

#[cfg(test)]
mod tests;

use std::{collections::HashMap, sync::Arc};

pub use infer::{FieldResolution, InferenceResult};
use smol_str::SmolStr;
use syntax::ast;

use crate::{def::hir_def::{FunctionId, AdtId}, ide::Upcast, DefDatabase};

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
    Hole,
    Generic {
        name: SmolStr,
    },
    Nil,
    Bool,
    Int,
    Float,
    String,
    BitArray,
    Result {
        ok: Arc<Ty>,
        err: Arc<Ty>,
    },
    List {
        of: Arc<Ty>,
    },
    // ToDo: when making types for functions,
    Function {
        params: Arc<Vec<Ty>>,
        return_: Arc<Ty>,
    },
    Adt {
        // ToDo: refactor, since this is not quite accurate enough: other types might be qualified also
        adt_id: AdtId, 
        params: Arc<Vec<Ty>>,
    },
    Tuple {
        fields: Arc<Vec<Ty>>,
    },
}


