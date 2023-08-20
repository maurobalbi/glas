pub mod display;
mod infer;
mod union_find;

#[cfg(test)]
mod tests;

use std::{collections::HashMap, sync::Arc};

pub use infer::{FieldResolution, InferenceResult};
use smol_str::SmolStr;
use syntax::ast;

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
    Generic {
        name: SmolStr,
    },
    Bool,
    Int,
    Float,
    String,
    List {
        of: Arc<Ty>,
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

pub fn ty_from_ast_opt(type_ast: Option<ast::TypeExpr>) -> Option<Ty> {
    match type_ast {
        Some(t) => Some(ty_from_ast(t)),
        None => None,
    }
}

pub fn ty_from_ast(ast_expr: ast::TypeExpr) -> Ty {
    tracing::info!("{:?}", ast_expr);
    match ast_expr {
        ast::TypeExpr::FnType(_fn_type) => Ty::Unknown,
        ast::TypeExpr::TupleType(_) => Ty::Unknown,
        ast::TypeExpr::TypeNameRef(t) => {
            if let Some(ty) = t.constructor_name().and_then(|t| t.text()) {
                match ty.as_str() {
                    "Int" => return Ty::Int,
                    "Float" => return Ty::Float,
                    "String" => return Ty::String,
                    a => return Ty::Generic { name: a.into() },
                }
            }
            Ty::Unknown
        }
        ast::TypeExpr::TypeApplication(ty) => {
            let name = ty
                .type_constructor()
                .and_then(|t| t.constructor_name())
                .and_then(|c| c.text());
            let Some(name) = name else {
                    return Ty::Unknown
                };
            let mut arguments = Vec::new();
            if let Some(args) = ty.arg_list() {
                for arg in args.args() {
                    let arg = ty_from_ast_opt(arg.arg());
                    if arg.is_some() {
                        arguments.push(arg.unwrap());
                    }
                }
            }
            Ty::Adt {
                name,
                params: Arc::new(arguments),
            }
        }
    }
}
