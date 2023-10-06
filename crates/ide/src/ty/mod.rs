pub mod display;
mod infer;
mod union_find;

#[cfg(test)]
mod tests;

use std::{collections::HashMap, sync::Arc};

pub use infer::{FieldResolution, InferenceResult};
use smol_str::SmolStr;
use syntax::ast;

use crate::{def::hir_def::FunctionId, ide::Upcast, DefDatabase};

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
        module: Option<SmolStr>,
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
    match ast_expr {
        ast::TypeExpr::FnType(it) => {
            let mut fn_params = Vec::new();
            if let Some(params) = it.param_list() {
                for ty in params.params() {
                    fn_params.push(ty_from_ast(ty));
                }
            };
            let ret = it.return_().map_or_else(|| Ty::Unknown, |r| ty_from_ast(r));
            Ty::Function {
                params: Arc::new(fn_params),
                return_: Arc::new(ret),
            }
        }
        ast::TypeExpr::TupleType(it) => {
            let mut fields = Vec::new();
            for ty in it.field_types() {
                fields.push(ty_from_ast(ty));
            }
            Ty::Tuple {
                fields: Arc::new(fields),
            }
        }
        ast::TypeExpr::TypeNameRef(t) => {
            if let Some((ty, token)) = t
                .constructor_name()
                .and_then(|t| Some((t.text()?, t.token()?)))
            {
                match ty.as_str() {
                    "Int" => return Ty::Int,
                    "Float" => return Ty::Float,
                    "String" => return Ty::String,
                    "Bool" => return Ty::Bool,
                    "Nil" => return Ty::Nil,
                    // ToDo: Diagnostics
                    a if token.kind() == syntax::SyntaxKind::U_IDENT => {
                        return Ty::Adt {
                            module: t.module().and_then(|m| m.text()),
                            name: a.into(),
                            params: Arc::new(Vec::new()),
                        }
                    }
                    a => return Ty::Generic { name: a.into() },
                }
            }
            Ty::Unknown
        }
        ast::TypeExpr::TypeApplication(ty) => {
            let type_constr = ty.type_constructor();
            let name = type_constr
                .clone()
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
            match name.as_str() {
                "List" => {
                    return Ty::List {
                        of: Arc::new(arguments.get(0).unwrap_or(&Ty::Unknown).clone()),
                    }
                }
                "Result" => {
                    let ok = Arc::new(arguments.get(0).unwrap_or(&Ty::Unknown).clone());
                    let err = Arc::new(arguments.get(1).unwrap_or(&Ty::Unknown).clone());
                    return Ty::Result { ok, err };
                }
                _ => {}
            };
            Ty::Adt {
                module: type_constr.and_then(|t| t.module()).and_then(|m| m.text()),
                name,
                params: Arc::new(arguments),
            }
        }
        ast::TypeExpr::Hole(_) => Ty::Hole,
    }
}
