use std::{cmp::min, collections::HashMap, mem, ops::Deref, sync::Arc};

use la_arena::ArenaMap;
use smol_str::SmolStr;
use syntax::ast::{BinaryOpKind, LiteralKind};

use crate::def::{
    body::Body,
    hir::{self, ModuleDef},
    hir_def::{AdtId, FunctionId},
    module::{Expr, ExprId, Field, Pattern, PatternId, Statement},
    resolver::{resolver_for_toplevel, ResolveResult, Resolver},
    resolver_for_expr,
};

use super::{display::next_letter, union_find::UnionFind, TyDatabase};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TyVar(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    Unknown {
        idx: u32,
    },
    Generic {
        name: SmolStr,
    },
    Bool,
    Int,
    Float,
    String,
    List {
        of: TyVar,
    },
    Adt {
        adt_id: AdtId,
        generic_params: Vec<TyVar>,
    },
    Function {
        params: Vec<TyVar>,
        return_: TyVar,
    },
    Tuple {
        fields: Vec<TyVar>,
    },
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
    field_resolution: HashMap<ExprId, FieldResolution>,
    pub fn_ty: super::Ty,
}

impl InferenceResult {
    pub fn ty_for_pattern(&self, pattern: PatternId) -> super::Ty {
        tracing::info!("{:#?} {:?}", self, pattern);
        self.pattern_ty_map[pattern].clone()
    }

    pub fn ty_for_expr(&self, expr: ExprId) -> super::Ty {
        self.expr_ty_map[expr].clone()
    }

    pub fn resolve_field(&self, expr: ExprId) -> Option<FieldResolution> {
        self.field_resolution.get(&expr).cloned()
    }
}

pub(crate) fn infer_function_query(db: &dyn TyDatabase, fn_id: FunctionId) -> Arc<InferenceResult> {
    let fun = db.lookup_intern_function(fn_id);
    let deps = db.dependency_order(fun.file_id);
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
    let mut idx = 0;
    let mut table = UnionFind::new(group.len(), |_| {
        idx += 1;
        Ty::Unknown { idx }
    });

    let mut fn_to_ctx = HashMap::new();
    let mut fn_to_ty_var = HashMap::new();

    for (i, f) in group.iter().enumerate() {
        let body = db.body(*f);
        let func = db.lookup_intern_function(*f);
        let resolver = resolver_for_toplevel(db.upcast(), func.file_id);
        let mut ctx = InferCtx {
            db,
            table: &mut table,
            idx,
            group: &group,
            fn_id: *f,
            resolver,
            body: body.deref(),
            body_ctx: BodyCtx::default(),
        };
        let ty = ctx.infer_function(&body);
        fn_to_ty_var.insert(*f, ty);
        ctx.unify_var(TyVar(i as u32), ty);

        let inferred_ctx = mem::replace(&mut ctx.body_ctx, BodyCtx::default());
        fn_to_ctx.insert(*f, inferred_ctx);
    }

    finish_infer(db, table, fn_to_ctx, fn_to_ty_var)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldResolution {
    Field(Field),
    ModuleDef(ModuleDef),
}

#[derive(Default)]
struct BodyCtx {
    pattern_to_ty: ArenaMap<PatternId, TyVar>,
    expr_to_ty: ArenaMap<ExprId, TyVar>,

    field_resolution: HashMap<ExprId, FieldResolution>,
}
struct InferCtx<'db> {
    db: &'db dyn TyDatabase,
    body_ctx: BodyCtx,
    idx: u32,
    fn_id: FunctionId,
    // The resolver is not kept up to date always, it's there to allow infer_pattern to work comfortably.
    // If it's important to have a new resolver, use a new one
    resolver: Resolver,
    group: &'db Vec<FunctionId>,
    body: &'db Body,
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

    fn make_type(&mut self, ty: super::Ty, env: &mut HashMap<SmolStr, TyVar>) -> TyVar {
        let ty = match ty {
            super::Ty::Unknown => return self.new_ty_var(),
            super::Ty::Generic { name } => {
                tracing::info!("making generic type: {}", name);
                return match env.get(&name) {
                    Some(var) => *var,
                    None => {
                        let var = self.new_ty_var();
                        self.unify_var_ty(var, Ty::Generic { name: name.clone() });
                        env.insert(name, var);
                        var
                    }
                };
            }
            super::Ty::Bool => Ty::Bool,
            super::Ty::Int => Ty::Int,
            super::Ty::Float => Ty::Float,
            super::Ty::String => Ty::String,
            super::Ty::List { of } => {
                let ty = self.make_type(of.deref().clone(), env);
                Ty::List { of: ty }
            }
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
            super::Ty::Tuple { fields: _ } => todo!(),
            super::Ty::Adt { name, params } => {
                let mut pars = Vec::new();

                for param in params.deref().into_iter() {
                    let par_var = self.new_ty_var();
                    let par_ty = self.make_type(param.clone(), env);
                    self.unify_var(par_var, par_ty);
                    pars.push(par_var);
                }
                match self.resolver.resolve_type(&name) {
                    Some(adt_id) => Ty::Adt {
                        generic_params: pars,
                        adt_id,
                    },
                    None => {
                        self.idx += 1;
                        Ty::Unknown { idx: self.idx }
                    }
                }
            }
        };
        TyVar(self.table.push(ty))
    }

    fn infer_function(&mut self, body: &Body) -> TyVar {
        let mut param_tys = Vec::new();
        for (param, ty) in &body.params {
            let pat_ty = self.ty_for_pattern(*param);
            tracing::info!("inferring function param ty {:?}", ty);
            if let Some(ty) = ty {
                let mut env = HashMap::new();
                let param_ty = self.make_type(ty.clone(), &mut env);
                self.unify_var(pat_ty, param_ty);
            }
            param_tys.push(pat_ty);
        }
        let body_ty = self.infer_expr(body.body_expr);
        Ty::Function {
            params: param_tys,
            return_: body_ty,
        }
        .intern(self)
    }

    fn infer_stmts(&mut self, stmts: Vec<Statement>) -> TyVar {
        let iter = stmts.into_iter();
        self.infer_stmts_iter(iter)
    }

    fn infer_stmts_iter(&mut self, mut stmts: impl Iterator<Item = Statement>) -> TyVar {
        let mut last = self.new_ty_var();
        while let Some(stmt) = stmts.next() {
            match stmt {
                Statement::Let { pattern, body } => {
                    //infer pat
                    let infered = self.infer_expr(body);
                    last = self.infer_pattern(pattern, infered);
                    // let pat_var = self.ty_for_pattern(pattern);
                    self.unify_var(last, infered);
                }
                Statement::Expr { expr } => last = self.infer_expr(expr),
                Statement::Use { patterns, expr } => {
                    let mut params = Vec::new();
                    for pat in patterns {
                        let new_var = self.new_ty_var();
                        let pat_ty = self.infer_pattern(pat, new_var);
                        self.unify_var(last, pat_ty);
                        params.push(pat_ty);
                    }
                    let cb_tail = self.infer_stmts(stmts.collect());
                    let cb_ty = Ty::Function {
                        params,
                        return_: cb_tail,
                    };
                    let cb_tyvar = self.new_ty_var();
                    self.unify_var_ty(cb_tyvar, cb_ty);
                    match &self.body[expr] {
                        Expr::Call { func, args } => {
                            let mut arg_tys = Vec::new();
                            for arg in args {
                                let param_ty = self.new_ty_var();
                                arg_tys.push(param_ty);
                                let arg_ty = self.infer_expr(*arg);
                                self.unify_var(arg_ty, param_ty)
                            }
                            arg_tys.push(cb_tyvar);
                            let ret_ty = self.new_ty_var();
                            let fun_ty = self.infer_expr(*func);

                            self.unify_var_ty(
                                fun_ty,
                                Ty::Function {
                                    params: arg_tys,
                                    return_: ret_ty,
                                },
                            );
                            last = ret_ty
                        }
                        _ => {
                            let mut arg_tys = Vec::new();

                            arg_tys.push(cb_tyvar);
                            let ret_ty = self.new_ty_var();
                            let fun_ty = self.infer_expr(expr);

                            self.unify_var_ty(
                                fun_ty,
                                Ty::Function {
                                    params: arg_tys,
                                    return_: ret_ty,
                                },
                            );
                            last = ret_ty
                        }
                    }

                    break;
                }
            }
        }
        last
    }

    fn infer_expr(&mut self, e: ExprId) -> TyVar {
        let placeholder_ty = self.ty_for_expr(e);
        let ty = self.infer_expr_inner(e);
        self.unify_var(placeholder_ty, ty);
        ty
    }

    fn infer_expr_inner(&mut self, tgt_expr: ExprId) -> TyVar {
        tracing::info!("inferring {:?}", &self.body[tgt_expr]);
        match &self.body[tgt_expr] {
            Expr::Missing => self.new_ty_var(),
            Expr::Literal(lit) => match lit {
                LiteralKind::Int => Ty::Int,
                LiteralKind::Float => Ty::Float,
                LiteralKind::String => Ty::String,
                LiteralKind::Bool => Ty::Bool,
            }
            .intern(self),
            Expr::Block { stmts } => {
                let old_resolver = mem::replace(
                    &mut self.resolver,
                    resolver_for_expr(self.db.upcast(), self.fn_id, tgt_expr),
                );
                let ty = self.infer_stmts(stmts.clone());
                self.resolver = old_resolver;
                ty
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
                    BinaryOpKind::Eq => {
                        let bool = self.new_ty_var();
                        self.unify_var_ty(bool, Ty::Bool);
                        self.unify_var(lhs_ty, rhs_ty);
                        bool
                    }
                }
            }
            Expr::Spread { expr } => {
                let sp_ty = self.infer_expr(*expr);
                let of = self.new_ty_var();
                self.unify_var_ty(sp_ty, Ty::List { of });
                of
            }
            Expr::Pipe { left, right } => {
                let arg_ty = self.infer_expr(*left);

                let ret_ty = self.new_ty_var();
                let fun_ty = self.infer_expr(*right);

                self.unify_var_ty(
                    fun_ty,
                    Ty::Function {
                        params: vec![arg_ty],
                        return_: ret_ty,
                    },
                );
                ret_ty
            }
            Expr::Call { func, args } => {
                let mut arg_tys = Vec::new();
                for arg in args {
                    arg_tys.push(self.infer_expr(*arg));
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
            Expr::Variable(name) => {
                let db = self.db;
                //
                let resolver = resolver_for_expr(db.upcast(), self.fn_id, tgt_expr);
                match resolver.resolve_name(name) {
                    Some(ResolveResult::Local(local)) => {
                        let pattern = self.body_ctx.pattern_to_ty.get(local.pat_id);
                        match pattern {
                            Some(ty_var) => *ty_var,
                            None => self.new_ty_var(),
                        }
                    }
                    Some(ResolveResult::Function(func)) => {
                        // check if resolved funcion is in group being inferred to avoid cycles
                        let group_ty = self.group.iter().enumerate().find(|f| *f.1 == func.id);
                        match group_ty {
                            Some(grp) => TyVar(grp.0 as u32),
                            None => {
                                let inferred_ty = func.ty(db);
                                let mut env = HashMap::new();
                                self.make_type(inferred_ty.clone(), &mut env)
                            }
                        }
                    }
                    // No resolution found, user error!?! report diagnostic!
                    _ => self.new_ty_var(),
                }
            }
            Expr::Case { subject, clauses } => {
                // let subj_tys = subjects.iter().map(|s| self.infer_expr(*s)).collect::<Vec<_>>();

                // let last = self.new_ty_var();
                // for clause in clauses.into_iter() {
                //     subj_tys.iter().zip(clause.patterns.iter()).for_each(|(sub, pat)| {
                //         self.infer_pattern(*pat, *sub);
                //         let expr_ty = self.infer_expr(clause.expr);
                //         self.unify_var(last, expr_ty);
                //     })
                // }
                // last
                let ret = self.new_ty_var();
                let ty = self.infer_expr(*subject);
                for clause in clauses.iter() {
                    self.infer_pattern(clause.pattern, ty);
                    let expr_ty = self.infer_expr(clause.expr);
                    self.unify_var(expr_ty, ret);
                }
                ret
            }
            Expr::FieldAccess {
                base_string,
                base: container,
                label: _,
                label_name,
            } => {
                let ty = self.infer_expr(*container);

                let field_var = self.new_ty_var();
                let field_ty = match self.table.get_mut(ty.0) {
                    Ty::Adt {
                        adt_id,
                        generic_params: params,
                    } => {
                        self.body_ctx.field_resolution.insert(
                            tgt_expr,
                            FieldResolution::ModuleDef(ModuleDef::Adt(hir::Adt::from(*adt_id))),
                        );
                        match params.iter().next() {
                            Some(var) => *var,
                            None => self.new_ty_var(),
                        }
                    }
                    _ => {
                        tracing::info!("INFERRING FIELD_RESOLUTION NON ADT");
                        let map = self.db.module_map();
                        let file = map
                            .iter()
                            .find(|(_, name)| name.ends_with(base_string.as_str()));
                        tracing::info!("fileid for modulename {:?} {}", file, base_string);
                        if let Some(res) = file
                            .map(|f| resolver_for_toplevel(self.db.upcast(), f.0))
                            .and_then(|r| r.resolve_name(label_name))
                        {
                            match res {
                                ResolveResult::Local(_) => {
                                    unreachable!("This is a compiler bug")
                                }
                                ResolveResult::Function(it) => {
                                    let fn_ty = it.ty(self.db);
                                    let mut env = HashMap::new();

                                    self.body_ctx.field_resolution.insert(
                                        tgt_expr,
                                        FieldResolution::ModuleDef(ModuleDef::Function(it)),
                                    );
                                    return self.make_type(fn_ty.clone(), &mut env);
                                }
                                // ToDo: Add type to adt in hir and fix this.
                                ResolveResult::Variant(it) => {
                                    let var = self.new_ty_var();
                                    self.unify_var_ty(
                                        var,
                                        Ty::Adt {
                                            generic_params: Vec::new(),
                                            adt_id: it.parent,
                                        },
                                    );
                                    return var;
                                }
                            }
                        }
                        self.new_ty_var()
                    }
                };
                self.unify_var(field_ty, field_var);
                field_ty
            }
            Expr::VariantLiteral { name, fields } => {
                let mut params = Vec::new();
                // also unify fields
                for field in fields {
                    params.push(self.infer_expr(*field));
                }
                let (ty, _params) = self.resolve_variant(name);
                ty
            }
            Expr::List { elements } => {
                let of_ty = self.new_ty_var();
                for elem in elements {
                    let elem_ty = self.infer_expr(*elem);
                    self.unify_var(elem_ty, of_ty);
                }
                Ty::List { of: of_ty }.intern(self)
            }
            Expr::Lambda { body, params } => {
                let mut param_tys = Vec::new();
                for param in params.clone() {
                    let pat_ty = self.ty_for_pattern(param);
                    param_tys.push(pat_ty);
                }
                let body_ty = self.infer_expr(*body);
                Ty::Function {
                    params: param_tys,
                    return_: body_ty,
                }
                .intern(self)
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

    fn infer_pattern(&mut self, pattern: PatternId, expected_ty_var: TyVar) -> TyVar {
        tracing::info!(
            "Inferring pattern {:?} {:?}",
            &self.body[pattern],
            self.table.get_mut(expected_ty_var.0)
        );
        let pat_var = self.ty_for_pattern(pattern);
        match &self.body[pattern] {
            Pattern::VariantRef {
                name,
                module: _,
                fields,
            } => {
                let (pat_ty, mut field_tys) = self.resolve_variant(name);

                // Tried with reserve / fill_with, but that didnt seem to work
                while field_tys.len() < fields.len() {
                    field_tys.push(self.new_ty_var());
                }
                tracing::info!("inferring fields {:?} {:?}", field_tys.capacity(), fields);
                for (field, field_ty) in fields.iter().zip(field_tys.iter()) {
                    // Unify fields with patterns
                    // self.unify_var_ty(ty, Ty::Int);
                    self.infer_pattern(*field, *field_ty);
                }
                self.unify_var(pat_ty, pat_var);
            }
            Pattern::AlternativePattern { patterns } => {
                for pat in patterns.iter() {
                    self.infer_pattern(*pat, expected_ty_var);
                }
            }
            Pattern::Literal { kind } => match kind {
                LiteralKind::Int => self.unify_var_ty(pat_var, Ty::Int),
                LiteralKind::Float => self.unify_var_ty(pat_var, Ty::Float),
                LiteralKind::String => self.unify_var_ty(pat_var, Ty::String),
                LiteralKind::Bool => self.unify_var_ty(pat_var, Ty::Bool),
            },
            Pattern::Spread { pattern } => {
                let pat = self.ty_for_pattern(*pattern);
                let of = self.new_ty_var();
                self.unify_var_ty(pat, Ty::List { of });
                self.unify_var(pat_var, of);
            }
            Pattern::List { elements } => {
                let of = self.new_ty_var();
                for elem in elements.into_iter() {
                    self.infer_pattern(*elem, of);
                }
                self.unify_var_ty(pat_var, Ty::List { of });
            }
            _ => {}
        }
        self.unify_var(expected_ty_var, pat_var);
        pat_var
    }

    fn resolve_variant(&mut self, name: &SmolStr) -> (TyVar, Vec<TyVar>) {
        match self.resolver.resolve_name(name) {
            // return field types aswell for unification
            Some(ResolveResult::Variant(variant)) => {
                let mut ty_env = HashMap::new();
                let params = variant
                    .fields(self.db.upcast())
                    .iter()
                    .map(|l| self.make_type(l.type_ref.clone(), &mut ty_env))
                    .collect();

                let ty = Ty::Adt {
                    adt_id: variant.parent,
                    generic_params: Vec::new(),
                }
                .intern(self);
                (ty, params)
            }

            _ => {
                // ToDo: Add diagnostics
                (self.new_ty_var(), Vec::new())
            }
        }
    }

    fn unify(&mut self, lhs: Ty, rhs: Ty) -> Ty {
        match (lhs, rhs) {
            (Ty::Unknown { idx }, Ty::Unknown { idx: idx2 }) => Ty::Unknown {
                idx: min(idx, idx2),
            },
            (Ty::Unknown { .. }, other) | (other, Ty::Unknown { .. }) => other,
            (Ty::List { of: of1 }, Ty::List { of: of2 }) => {
                self.unify_var(of1, of2);
                Ty::List { of: of1 }
            }
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
}

fn finish_infer(
    db: &dyn TyDatabase,
    mut table: UnionFind<Ty>,
    body_ctx: HashMap<FunctionId, BodyCtx>,
    fn_ty: HashMap<FunctionId, TyVar>,
) -> HashMap<FunctionId, InferenceResult> {
    let mut i = Collector::new(db, &mut table);

    let mut inference_map = HashMap::new();

    for (fn_id, ctx) in body_ctx.into_iter() {
        let mut pattern_ty_map = ArenaMap::new();
        let mut expr_ty_map = ArenaMap::new();

        for (pattern, ty) in ctx.pattern_to_ty.iter() {
            pattern_ty_map.insert(pattern, i.collect(*ty));
        }

        for (expr, ty) in ctx.expr_to_ty.iter() {
            expr_ty_map.insert(expr, i.collect(*ty));
        }

        inference_map.insert(
            fn_id,
            InferenceResult {
                fn_ty: i.collect(*fn_ty.get(&fn_id).expect("Compiler error")),

                field_resolution: ctx.field_resolution,
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
    db: &'a dyn TyDatabase,
    env: HashMap<u32, SmolStr>,
    uid: u32,
}

impl<'a> Collector<'a> {
    fn new(db: &'a dyn TyDatabase, table: &'a mut UnionFind<Ty>) -> Self {
        Self {
            cache: vec![None; table.len()],
            table,
            db,
            env: HashMap::new(),
            uid: 0,
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
            Ty::Unknown { idx } => {
                let lookup = self.env.get(idx);
                let name = match lookup.cloned() {
                    Some(name) => name,
                    None => {
                        let new_letter = next_letter(self.uid);
                        self.uid += 1;
                        self.env.insert(*idx, new_letter.clone());
                        new_letter
                    }
                };
                super::Ty::Generic { name }
            }
            Ty::Bool => super::Ty::Bool,
            Ty::Int => super::Ty::Int,
            Ty::Float => super::Ty::Float,
            Ty::String => super::Ty::String,
            Ty::List { of } => {
                let of = self.collect(*of);
                super::Ty::List { of: Arc::new(of) }
            }
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
            Ty::Adt {
                adt_id,
                generic_params: _,
            } => {
                let adt = self.db.lookup_intern_adt(*adt_id);
                let module = self.db.module_items(adt.file_id);
                let adt_data = &module[adt.value];
                super::Ty::Adt {
                    name: adt_data.name.clone(),
                    params: Arc::new(Vec::new()),
                }
            }
            Ty::Generic { name } => super::Ty::Generic { name: name.clone() },
        }
    }
}
