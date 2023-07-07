mod infer;
mod union_find;

#[cfg(test)]
mod tests;

use std::sync::Arc;

pub use infer::InferenceResult;
use smol_str::SmolStr;

use crate::{DefDatabase, FileId, def::module::NameId};


#[salsa::query_group(TyDatabaseStorage)]
pub trait TyDatabase: DefDatabase {
    #[salsa::invoke(infer::infer_query)]
    fn infer(&self, name: NameId, file: FileId) -> Arc<(Ty, InferenceResult)>;
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
