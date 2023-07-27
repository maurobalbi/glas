use std::{
    borrow::BorrowMut, collections::HashMap, default, hash::Hash, mem, ops::Deref, sync::Arc,
};

use la_arena::{Arena, ArenaMap};
use syntax::ast::BinaryOpKind;

use crate::{
    base::Dependency,
    def::{
        body::{self, Body},
        hir_def::{AdtId, FunctionId},
        module::{Expr, ExprId, Function, Literal, PatternId, Statement},
        resolver::ResolveResult,
        resolver_for_expr,
    },
    DefDatabase, FileId,
};

use super::{union_find::UnionFind, TyDatabase};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TyVar(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    Unknown { idx: u32 },
    Generic { idx: u32 },
    Bool,
    Int,
    Float,
    String,
    Adt { adt_id: AdtId, params: Vec<TyVar> },
    Function { params: Vec<TyVar>, return_: TyVar },
    Tuple { fields: Vec<TyVar> },
}

impl Ty {
    fn intern(self, ctx: &mut InferCtx<'_>) -> TyVar {
        TyVar(ctx.table.push(self))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceResult {
    pattern_ty_map: ArenaMap<PatternId, super::Ty>,
    expr_ty_map: ArenaMap<ExprId, super::Ty>,
    // field_resolution: HashMap<ExprId, FieldId>,
    pub fn_ty: super::Ty,
}

impl InferenceResult {
    pub fn ty_for_pat(&self, pattern: PatternId) -> super::Ty {
        self.pattern_ty_map[pattern].clone()
    }

    pub fn ty_for_expr(&self, expr: ExprId) -> super::Ty {
        self.expr_ty_map[expr].clone()
    }
}

pub(crate) fn infer_function_query(db: &dyn TyDatabase, fn_id: FunctionId) -> Arc<InferenceResult> {
    let fn_i = db.lookup_intern_function(fn_id);
    let deps = db.dependency_order(fn_i.file_id);
    let group = deps
        .into_iter()
        .find(|v| v.contains(&fn_id))
        .expect("This is a compiler error!");
    Arc::new(db.infer_function_group(group).get(&fn_id).unwrap().clone())
}

pub(crate) fn infer_function_group_query(
    db: &dyn TyDatabase,
    group: Vec<FunctionId>,
) -> HashMap<FunctionId, InferenceResult> {
    // let fn_in_file = db.lookup_intern_function(fn_id);
    // let deps = db.dependency_order(fn_in_file.file_id);

    // let func_group: Vec<u32> = deps
    //     .into_iter()
    //     .filter(|v| v.contains(&fn_id.0.as_u32()))
    //     .flatten()
    //     .collect();
    let mut idx = 0;
    let mut table = UnionFind::new(0, |_| {
        idx += 1;
        Ty::Unknown { idx }
    });

    let mut fn_to_ctx = HashMap::new();
    let mut fn_to_ty_var = HashMap::new();

    for f in group.into_iter() {
        let body = db.body(f);
        let mut ctx = InferCtx {
            db,
            table: &mut table,
            idx,
            fn_id: f,
            body: body.deref(),
            body_ctx: BodyCtx::default(),
        };
        let ty = ctx.infer_function(&body);
        fn_to_ty_var.insert(f, ty);

        let inferred_ctx = mem::replace(&mut ctx.body_ctx, BodyCtx::default());
        fn_to_ctx.insert(f, inferred_ctx);
    }

    finish_infer(table, fn_to_ctx, fn_to_ty_var)
}

#[derive(Default)]
struct BodyCtx {
    pattern_to_ty: HashMap<PatternId, TyVar>,
    expr_to_ty: HashMap<ExprId, TyVar>,
}
struct InferCtx<'db> {
    db: &'db dyn TyDatabase,
    body_ctx: BodyCtx,
    idx: u32,
    fn_id: FunctionId,
    body: &'db Body,
    /// The arena for both unification and interning.
    /// First `module.names().len() + module.exprs().len()` elements are types of each names and
    /// exprs, to allow recursive definition.
    table: &'db mut UnionFind<Ty>,
}

impl<'db> InferCtx<'db> {
    fn new_ty_var(&mut self) -> TyVar {
        self.idx += 1;
        TyVar(self.table.push(Ty::Unknown { idx: self.idx }))
    }

    fn ty_for_pattern(&mut self, i: PatternId) -> TyVar {
        let ty_var = self.new_ty_var();
        self.body_ctx.pattern_to_ty.insert(i, ty_var);
        ty_var
    }

    fn ty_for_expr(&mut self, i: ExprId) -> TyVar {
        let ty_var = self.new_ty_var();
        self.body_ctx.expr_to_ty.insert(i, ty_var);
        ty_var
    }

    fn make_type(&mut self, ty: super::Ty, env: &mut HashMap<u32, TyVar>) -> TyVar {
        let ty = match ty {
            super::Ty::Unknown => return self.new_ty_var(),
            super::Ty::Generic { idx } => {
                return match env.get(&idx) {
                    Some(var) => *var,
                    None => {
                        let var = self.new_ty_var();
                        env.insert(idx, var);
                        var
                    }
                }
            }
            super::Ty::Bool => Ty::Bool,
            super::Ty::Int => Ty::Int,
            super::Ty::Float => Ty::Float,
            super::Ty::String => Ty::String,
            super::Ty::Function { params, return_ } => {
                let mut pars = Vec::new();
                for param in params.deref().into_iter() {
                    let par_var = self.new_ty_var();
                    let par_ty = self.make_type(param.clone(), env);
                    self.unify_var(par_var, par_ty);
                    pars.push(par_var);
                }
                let ret = self.new_ty_var();
                let ret_ty = self.make_type(return_.deref().clone(), env);
                self.unify_var(ret, ret_ty);
                Ty::Function {
                    params: pars,
                    return_: ret,
                }
            }
            super::Ty::Tuple { fields } => todo!(),
            super::Ty::Adt { adt_id, params } => {
                let mut pars = Vec::new();
                for param in params.deref().into_iter() {
                    let par_var = self.new_ty_var();
                    let par_ty = self.make_type(param.clone(), env);
                    self.unify_var(par_var, par_ty);
                    pars.push(par_var);
                }
                Ty::Adt {
                    params: pars,
                    adt_id,
                }
            }
        };
        TyVar(self.table.push(ty))
    }

    fn infer_function(&mut self, body: &Body) -> TyVar {
        let mut param_tys = Vec::new();
        for param in &body.params {
            let param_ty = self.new_ty_var();
            param_tys.push(param_ty);
            let pat_ty = self.ty_for_pattern(*param);
            self.unify_var(param_ty, pat_ty);
        }
        let body_ty = self.infer_expr(body.body_expr);
        Ty::Function {
            params: param_tys,
            return_: body_ty,
        }
        .intern(self)
    }

    fn infer_stmt(&mut self, stmt: Statement) -> TyVar {
        match stmt {
            Statement::Let { pattern: _, body } => {
                //infer pat
                self.infer_expr(body)
            }
            Statement::Expr { expr } => self.infer_expr(expr),
            Statement::Use {
                patterns: _,
                expr: _,
            } => todo![],
        }
    }

    fn infer_expr(&mut self, e: ExprId) -> TyVar {
        let ty = self.infer_expr_inner(e);
        let placeholder_ty = self.ty_for_expr(e);
        self.unify_var(placeholder_ty, ty);
        ty
    }

    fn infer_expr_inner(&mut self, e: ExprId) -> TyVar {
        match &self.body[e] {
            Expr::Missing => self.new_ty_var(),
            Expr::Literal(lit) => match lit {
                Literal::Int(_) => Ty::Int,
                Literal::Float(_) => Ty::Float,
                Literal::String(_) => Ty::String,
            }
            .intern(self),
            Expr::Block { stmts } => {
                let mut tail = self.new_ty_var();
                for stmt in stmts {
                    tail = self.infer_stmt(stmt.clone());
                    tracing::info!("STATEMNT: {:?} {:?}", stmt, tail);
                }
                tail
            }
            Expr::Binary { left, right, op } => {
                let lhs_ty = self.infer_expr(*left);
                let rhs_ty = self.infer_expr(*right);
                let Some(op) = op else { return self.new_ty_var() };
                match op {
                    BinaryOpKind::IntAdd
                    | BinaryOpKind::IntSub
                    | BinaryOpKind::IntMul
                    | BinaryOpKind::IntDiv
                    | BinaryOpKind::IntMod => {
                        // TODO: Arguments have type: int | float.
                        self.unify_var_ty(lhs_ty, Ty::Int);
                        self.unify_var(lhs_ty, rhs_ty);
                        lhs_ty
                    }
                    BinaryOpKind::FloatAdd
                    | BinaryOpKind::FloatSub
                    | BinaryOpKind::FloatMul
                    | BinaryOpKind::FloatDiv => {
                        self.unify_var_ty(lhs_ty, Ty::Float);
                        self.unify_var(lhs_ty, rhs_ty);
                        lhs_ty
                    }
                    BinaryOpKind::IntGT
                    | BinaryOpKind::IntLT
                    | BinaryOpKind::IntGTE
                    | BinaryOpKind::IntLTE => {
                        self.unify_var_ty(lhs_ty, Ty::Int);
                        self.unify_var(lhs_ty, rhs_ty);
                        let bin_var = self.new_ty_var();
                        self.unify_var_ty(bin_var, Ty::Bool);
                        bin_var
                    }
                    BinaryOpKind::FloatGT
                    | BinaryOpKind::FloatLT
                    | BinaryOpKind::FloatGTE
                    | BinaryOpKind::FloatLTE => {
                        self.unify_var_ty(lhs_ty, Ty::Float);
                        self.unify_var(lhs_ty, rhs_ty);
                        let bin_var = self.new_ty_var();
                        self.unify_var_ty(bin_var, Ty::Bool);
                        bin_var
                    }

                }
            }
            Expr::Call { func, args } => {
                let mut arg_tys = Vec::new();
                for arg in args {
                    let param_ty = self.new_ty_var();
                    arg_tys.push(param_ty);
                    let arg_ty = self.infer_expr(*arg);
                    self.unify_var(arg_ty, param_ty)
                }
                let ret_ty = self.new_ty_var();
                let fun_ty = self.infer_expr(*func);

                self.unify_var_ty(
                    fun_ty,
                    Ty::Function {
                        params: arg_tys,
                        return_: ret_ty,
                    },
                );
                ret_ty
            }
            Expr::NameRef(name) => {
                let db = self.db;
                let resolver = resolver_for_expr(db.upcast(), self.fn_id, e);
                match resolver.resolve_name(name) {
                    Some(ResolveResult::LocalBinding(pat)) => {
                        let pattern = self.body_ctx.pattern_to_ty.get(&pat);
                        match pattern {
                            Some(ty_var) => *ty_var,
                            None => self.new_ty_var(),
                        }
                    }
                    Some(ResolveResult::FunctionId(fn_id)) => {
                        let inferred_ty = self.db.infer_function(fn_id);
                        let mut env = HashMap::new();
                        self.make_type(inferred_ty.fn_ty.clone(), &mut env)
                    }
                    Some(ResolveResult::VariantId(var_id)) => {
                        let variant = db.lookup_intern_variant(var_id);
                        let mut env = HashMap::new();
                        self.make_type(
                            super::Ty::Adt {
                                adt_id: variant.parent,
                                params: Arc::new(Vec::new()),
                            },
                            &mut env,
                        )
                    }
                    // No resolution found, user error!?! report diagnostic!
                    None => self.new_ty_var(),
                }
            }
        }
    }

    fn unify_var_ty(&mut self, var: TyVar, rhs: Ty) {
        let lhs = mem::replace(self.table.get_mut(var.0), Ty::Unknown { idx: 0 });
        let ret = self.unify(lhs, rhs);
        *self.table.get_mut(var.0) = ret;
    }

    fn unify_var(&mut self, lhs: TyVar, rhs: TyVar) {
        let (var, rhs) = self.table.unify(lhs.0, rhs.0);
        let Some(rhs) = rhs else { return };
        self.unify_var_ty(TyVar(var), rhs);
    }

    fn unify(&mut self, lhs: Ty, rhs: Ty) -> Ty {
        match (lhs, rhs) {
            (Ty::Unknown { idx }, Ty::Unknown { idx: _ }) => Ty::Unknown { idx },
            (Ty::Unknown { .. }, other) | (other, Ty::Unknown { .. }) => other,
            (Ty::Generic { idx: _ }, other) | (other, Ty::Generic { idx: _ }) => other,
            (
                Ty::Function {
                    params: params1,
                    return_: ret1,
                },
                Ty::Function {
                    params: params2,
                    return_: ret2,
                },
            ) => {
                for (p1, p2) in params1.clone().into_iter().zip(params2.into_iter()) {
                    self.unify_var(p1, p2)
                }
                self.unify_var(ret1.clone(), ret2);
                Ty::Function {
                    params: params1,
                    return_: ret1,
                }
            }
            (lhs, rhs) => {
                if lhs != rhs {
                    tracing::info!("ERROR: {:?} {:?}", lhs, rhs);
                };
                lhs
            }
        }
    }

    // fn finish(mut self) -> InferenceResult {
    //     let mut i = Collector::new(&mut self.table);

    //     let name_cnt = self.module.names().len();
    //     let expr_cnt = self.module.exprs().len();
    //     let mut name_ty_map = ArenaMap::with_capacity(name_cnt);
    //     let mut expr_ty_map = ArenaMap::with_capacity(expr_cnt);
    //     for (name, _) in self.module.names() {
    //         let ty = TyVar(u32::from(name.into_raw()));
    //         name_ty_map.insert(name, i.collect(ty));
    //     }
    //     for (expr, _) in self.module.exprs() {
    //         let ty = TyVar(name_cnt as u32 + u32::from(expr.into_raw()));
    //         expr_ty_map.insert(expr, i.collect(ty));
    //     }

    //     InferenceResult {
    //         pattern_ty_map: name_ty_map,
    //         expr_ty_map,
    //     }
    // }
}

fn finish_infer(
    mut table: UnionFind<Ty>,
    body_ctx: HashMap<FunctionId, BodyCtx>,
    fn_ty: HashMap<FunctionId, TyVar>,
) -> HashMap<FunctionId, InferenceResult> {
    let mut i = Collector::new(&mut table);

    let mut inference_map = HashMap::new();

    for (fn_id, ctx) in body_ctx.into_iter() {
        let mut pattern_ty_map = ArenaMap::new();
        let mut expr_ty_map = ArenaMap::new();

        for (pattern, ty) in ctx.pattern_to_ty {
            pattern_ty_map.insert(pattern, i.collect(ty));
        }

        for (expr, ty) in ctx.expr_to_ty {
            expr_ty_map.insert(expr, i.collect(ty));
        }

        inference_map.insert(
            fn_id,
            InferenceResult {
                fn_ty: i.collect(*fn_ty.get(&fn_id).expect("Compiler error")),
                pattern_ty_map,
                expr_ty_map,
            },
        );
    }

    inference_map
}
/// Traverse the table and freeze all `Ty`s into immutable ones.
struct Collector<'a> {
    cache: Vec<Option<super::Ty>>,
    table: &'a mut UnionFind<Ty>,
}

impl<'a> Collector<'a> {
    fn new(table: &'a mut UnionFind<Ty>) -> Self {
        Self {
            cache: vec![None; table.len()],
            table,
        }
    }

    fn collect(&mut self, ty: TyVar) -> super::Ty {
        let i = self.table.find(ty.0);
        if let Some(ty) = self.cache[i as usize].clone() {
            return ty;
        }

        // // Prevent cycles.
        self.cache[i as usize] = Some(super::Ty::Unknown);
        let ret = self.collect_uncached(i);
        self.cache[i as usize] = Some(ret.clone());
        ret
    }

    fn collect_uncached(&mut self, i: u32) -> super::Ty {
        let ty = mem::replace(self.table.get_mut(i), Ty::Unknown { idx: 0 });
        match &ty {
            Ty::Unknown { idx } => super::Ty::Generic { idx: idx.clone() },
            Ty::Generic { idx } => super::Ty::Generic { idx: idx.clone() },
            Ty::Bool => super::Ty::Bool,
            Ty::Int => super::Ty::Int,
            Ty::Float => super::Ty::Float,
            Ty::String => super::Ty::String,
            Ty::Function { params, return_ } => {
                let mut super_params = Vec::new();
                for param in params {
                    super_params.push(self.collect(*param));
                }
                let super_return = self.collect(*return_);
                super::Ty::Function {
                    params: Arc::new(super_params),
                    return_: Arc::new(super_return),
                }
            }
            Ty::Tuple { fields: _ } => todo!(),
            Ty::Adt { adt_id, params } => super::Ty::Adt {
                adt_id: *adt_id,
                params: Arc::new(Vec::new()),
            },
        }
    }
}
