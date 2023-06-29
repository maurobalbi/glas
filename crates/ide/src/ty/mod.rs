mod infer;
mod union_find;

#[cfg(test)]
mod tests;

use std::sync::Arc;

pub use infer::InferenceResult;

use crate::{DefDatabase, FileId};


#[salsa::query_group(TyDatabaseStorage)]
pub trait TyDatabase: DefDatabase {
    #[salsa::invoke(infer::infer_query)]
    fn infer(&self, file: FileId) -> Arc<InferenceResult>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unknown,
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
