use std::{cmp::min, collections::HashMap, mem, ops::Deref, sync::Arc};

use la_arena::ArenaMap;
use smol_str::SmolStr;
use syntax::ast::{BinaryOpKind, LiteralKind};

use crate::{
    def::{
        body::Body,
        hir::{self, Adt, Field, ModuleDef, TypeAlias},
        hir_def::{AdtId, FunctionId},
        module::{Expr, ExprId, Pattern, PatternId, Statement},
        resolver::{resolver_for_toplevel, ResolveResult, Resolver},
        resolver_for_expr,
    },
    FileId,
};

use super::{display::next_letter, union_find::UnionFind, TyDatabase};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TyVar(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    Unknown {
        idx: u32,
    },
    Nil,
    Bool,
    Int,
    Float,
    String,
    Result {
        ok: TyVar,
        err: TyVar,
    },
    List {
        of: TyVar,
    },
    Adt {
        adt_id: AdtId,
        generic_params: Vec<TyVar>,
    },
    Function {
        params: Vec<(Option<SmolStr>, TyVar)>,
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
    module_resolution: HashMap<ExprId, FileId>,
    pub fn_ty: super::Ty,
}

impl InferenceResult {
    pub fn ty_for_pattern(&self, pattern: PatternId) -> super::Ty {
        self.pattern_ty_map[pattern].clone()
    }

    pub fn ty_for_expr(&self, expr: ExprId) -> super::Ty {
        self.expr_ty_map[expr].clone()
    }

    pub fn resolve_field(&self, expr: ExprId) -> Option<FieldResolution> {
        self.field_resolution.get(&expr).cloned()
    }

    pub fn resolve_module(&self, expr: ExprId) -> Option<FileId> {
        self.module_resolution.get(&expr).cloned()
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

        let inferred_ctx = std::mem::take(&mut ctx.body_ctx);
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
    module_resolution: HashMap<ExprId, FileId>,
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

    // ToDo: Add context file_id to make proper resolver!
    fn make_type(&mut self, ty: super::Ty, env: &mut HashMap<SmolStr, TyVar>) -> TyVar {
        let ty = match ty {
            super::Ty::Unknown => return self.new_ty_var(),
            super::Ty::Hole => return self.new_ty_var(),
            // this is not correct, same name obviously doesnt have same type e.g fn a(a:a) {a} and fn b(b: a} {b} dont have the same type!
            super::Ty::Generic { name } => {
                return match env.get(&name) {
                    Some(var) => *var,
                    None => {
                        let var = self.new_ty_var();
                        self.unify_var_ty(var, Ty::Unknown { idx: var.0 });
                        env.insert(name, var);
                        var
                    }
                };
            }
            super::Ty::Nil => Ty::Nil,
            super::Ty::Bool => Ty::Bool,
            super::Ty::Int => Ty::Int,
            super::Ty::Float => Ty::Float,
            super::Ty::String => Ty::String,
            super::Ty::Result { ok, err } => {
                let ok = self.make_type(ok.deref().clone(), env);
                let err = self.make_type(err.deref().clone(), env);
                Ty::Result { ok, err }
            }
            super::Ty::List { of } => {
                let ty = self.make_type(of.deref().clone(), env);
                Ty::List { of: ty }
            }
            super::Ty::Function { params, return_ } => {
                let mut pars = Vec::new();
                for param in params.deref().iter() {
                    let par_var = self.new_ty_var();
                    let par_ty = self.make_type(param.clone(), env);
                    self.unify_var(par_var, par_ty);
                    pars.push((None, par_var));
                }
                let ret = self.new_ty_var();
                let ret_ty = self.make_type(return_.deref().clone(), env);
                self.unify_var(ret, ret_ty);
                Ty::Function {
                    params: pars,
                    return_: ret,
                }
            }
            super::Ty::Tuple { fields } => {
                let mut field_tys = Vec::new();
                for field in fields.deref().iter() {
                    let field_ty = self.make_type(field.clone(), env);
                    field_tys.push(field_ty)
                }
                Ty::Tuple { fields: field_tys }
            }
            super::Ty::Adt {
                module,
                name,
                params,
            } => {
                let mut pars = Vec::new();

                for param in params.deref().iter() {
                    let par_var = self.new_ty_var();
                    let par_ty = self.make_type(param.clone(), env);
                    self.unify_var(par_var, par_ty);
                    pars.push(par_var);
                }
                let module_def = match module {
                    Some(module) => self.resolver.resolve_module(&module).and_then(|m| {
                        // ToDo: use something like Module.exports().get instead
                        let resolver = resolver_for_toplevel(self.db.upcast(), m);
                        resolver.resolve_type(&name)
                    }),
                    None => self.resolver.resolve_type(&name),
                };
                match module_def {
                    Some(type_) => match type_ {
                        ResolveResult::Adt(adt) => Ty::Adt {
                            generic_params: pars,
                            adt_id: adt.id,
                        },
                        ResolveResult::TypeAlias(alias) => {
                            let data = TypeAlias { id: alias.id }.data(self.db.upcast());
                            let type_alias = self.db.lookup_intern_type_alias(alias.id);
                            if let Some(ty) = data.body {
                                let resolver = std::mem::replace(
                                    &mut self.resolver,
                                    resolver_for_toplevel(self.db.upcast(), type_alias.file_id),
                                );
                                let ty = self.make_type(ty, env);
                                let _ = std::mem::replace(&mut self.resolver, resolver);
                                return ty;
                            }
                            self.idx += 1;
                            Ty::Unknown { idx: self.idx }
                        }
                        _ => {
                            self.idx += 1;
                            Ty::Unknown { idx: self.idx }
                        }
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
        let mut env = HashMap::new();
        for (param, ty, label) in &body.params {
            let pat_ty = self.ty_for_pattern(*param);
            if let Some(ty) = ty {
                let param_ty = self.make_type(ty.clone(), &mut env);
                self.unify_var(pat_ty, param_ty);
            }
            param_tys.push((label.clone(), pat_ty));
        }
        let return_ = body
            .return_
            .as_ref()
            .map(|r| self.make_type(r.clone(), &mut env));
        let body_ty = self.infer_expr(body.body_expr);
        if let Some(ret) = return_ {
            self.unify_var(ret, body_ty);
        }

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
                        params.push((None, pat_ty));
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
                            for (_, arg) in args {
                                let param_ty = self.new_ty_var();
                                arg_tys.push((None, param_ty));
                                let arg_ty = self.infer_expr(*arg);
                                self.unify_var(arg_ty, param_ty)
                            }
                            arg_tys.push((None, cb_tyvar));
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
                            let arg_tys = vec![(None, cb_tyvar)];
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
        match &self.body[tgt_expr] {
            Expr::Missing => self.new_ty_var(),
            Expr::Hole => self.new_ty_var(),
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
                let Some(op) = op else {
                    return self.new_ty_var();
                };
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
                    BinaryOpKind::Concat => {
                        self.unify_var_ty(lhs_ty, Ty::String);
                        self.unify_var(lhs_ty, rhs_ty);
                        lhs_ty
                    }
                }
            }
            Expr::Tuple { fields } => {
                let mut field_ty = Vec::new();
                for field in fields.iter() {
                    field_ty.push(self.infer_expr(*field));
                }
                Ty::Tuple { fields: field_ty }.intern(self)
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

                // Ugly hack to notify Expr call of
                match &self.body[*right] {
                    Expr::Call { func, args } => {
                        let func_ty = self.infer_expr(*func);
                        let ty = self.table.get_mut(func_ty.0).clone();

                        for (_, arg) in args {
                            self.infer_expr(*arg);
                        }

                        match ty {
                            Ty::Function { params, return_ } => {
                                if params.len() > args.len() && !params.is_empty() {
                                    self.unify_var(arg_ty, params[0].1)
                                }
                                return return_;
                            }
                            _ => {
                                //ToDo: Diagnostics
                            }
                        }
                        //infer func expr, check if amount of params is the same as args, then unify left
                    }
                    _ => {
                        //infer right, and unify like int he old times
                    }
                }

                let fun_ty = self.infer_expr(*right);

                self.unify_var_ty(
                    fun_ty,
                    Ty::Function {
                        params: vec![(None, arg_ty)],
                        return_: ret_ty,
                    },
                );
                ret_ty
            }
            Expr::Call { func, args } => {
                let mut arg_tys = Vec::new();
                let mut hole = None;
                // add(_, 1) => fn(_hole) { add(_hole, 1) }
                let ret_ty = self.new_ty_var();
                let fun_ty = self.infer_expr(*func);

                for (_, arg) in args {
                    match self.body[*arg] {
                        Expr::Hole => {
                            let var = self.infer_expr(*arg);
                            hole = Some(var);
                            arg_tys.push((None, var));
                        }
                        // ..object
                        Expr::Spread { expr } => {
                            let expr = self.infer_expr(expr);
                            self.unify_var(ret_ty, expr);
                        }
                        _ => {
                            arg_tys.push((None, self.infer_expr(*arg)));
                        }
                    }
                }

                self.unify_var_ty(
                    fun_ty,
                    Ty::Function {
                        params: arg_tys,
                        return_: ret_ty,
                    },
                );

                if let Some(hole) = hole {
                    let new_ty = self.new_ty_var();
                    self.unify_var_ty(
                        new_ty,
                        Ty::Function {
                            params: vec![(None, hole)],
                            return_: ret_ty,
                        },
                    );
                    return new_ty;
                }
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
                        // check if resolved function is in group being inferred to avoid cycles
                        let group_ty = self.group.iter().enumerate().find(|f| *f.1 == func.id);
                        match group_ty {
                            Some(grp) => TyVar(grp.0 as u32),
                            None => {
                                let inferred_ty = func.ty(db);
                                let mut env = HashMap::new();
                                // ToDo: analogous to inference for adt in field.access, this and every other resolved type / value
                                let func_loc = self.db.lookup_intern_function(func.id); //       needs to have a local resolver to make the correct type
                                let resolver = std::mem::replace(
                                    &mut self.resolver,
                                    resolver_for_toplevel(self.db.upcast(), func_loc.file_id),
                                );
                                let ty = self.make_type(inferred_ty, &mut env);
                                let _ = std::mem::replace(&mut self.resolver, resolver);
                                ty
                            }
                        }
                    }
                    // No resolution found, user error!?! report diagnostic!
                    _ => self.new_ty_var(),
                }
            }
            Expr::Case { subjects, clauses } => {
                let mut subject_tys = Vec::new();
                for subject in subjects.iter() {
                    subject_tys.push(self.infer_expr(*subject));
                }
                let ret = self.new_ty_var();

                for clause in clauses.iter() {
                    for (pat, ty) in clause.patterns.iter().zip(subject_tys.iter()) {
                        self.infer_pattern(*pat, *ty);
                    }
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
                let field_var = self.new_ty_var();
                let base_ty = self.infer_expr(*container);
                // ToDo: This is wrong, since it might also be a module_access
                if let Ty::Adt { adt_id, .. } = self.table.get_mut(base_ty.0) {
                    let adt = Adt { id: *adt_id };
                    if let Some(field) = adt.common_fields(self.db.upcast()).get(label_name) {
                        let mut env = HashMap::new();
                        self.body_ctx
                            .field_resolution
                            .insert(tgt_expr, FieldResolution::Field(*field));
                        return self.make_type(field.ty(self.db.upcast()), &mut env);
                    }
                }

                if let Some(file) = self.resolver.resolve_module(base_string) {
                    self.body_ctx.module_resolution.insert(*container, file);
                    let resolver = resolver_for_toplevel(self.db.upcast(), file);
                    if let Some(res) = resolver.resolve_name(label_name) {
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
                                let func_loc = self.db.lookup_intern_function(it.id); //       needs to have a local resolver to make the correct type
                                let resolver = std::mem::replace(
                                    &mut self.resolver,
                                    resolver_for_toplevel(self.db.upcast(), func_loc.file_id),
                                );
                                let ty = self.make_type(fn_ty, &mut env);
                                let _ = std::mem::replace(&mut self.resolver, resolver);
                                return ty;
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
                                self.body_ctx.field_resolution.insert(
                                    tgt_expr,
                                    FieldResolution::ModuleDef(ModuleDef::Variant(it)),
                                );
                                return var;
                            }
                            ResolveResult::BuiltIn(_) => {}
                            ResolveResult::Module(_) => {}
                            ResolveResult::Adt(_) => {}
                            ResolveResult::TypeAlias(_) => {}
                        }
                    }
                }
                field_var
            }
            Expr::VariantLiteral { name } => {
                let (ty, params) = self.resolve_variant(name);
                if !params.is_empty() {
                    Ty::Function {
                        params,
                        return_: ty,
                    }
                    .intern(self)
                } else {
                    ty
                }
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
                    param_tys.push((None, pat_ty));
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
        let pat_var = self.ty_for_pattern(pattern);
        match &self.body[pattern] {
            Pattern::VariantRef {
                name,
                module,
                fields,
            } => {
                let (pat_ty, mut field_tys) = match module {
                    None => self.resolve_variant(name),
                    Some(module) => self
                        .resolver
                        .resolve_module(module)
                        .and_then(|file| {
                            // ToDo: refactor to something nicer and more correct, e.g. FileId -> Moodule.exports().get(name)
                            let resolver = resolver_for_toplevel(self.db.upcast(), file);
                            resolver.resolve_name(name)
                        })
                        .map(|res| self.variant_from_resolve_result(res))
                        .unwrap_or_else(|| (self.new_ty_var(), Vec::new())),
                };

                // Tried with reserve / fill_with, but that didnt seem to work
                while field_tys.len() < fields.len() {
                    field_tys.push((None, self.new_ty_var()));
                }
                for (field, field_ty) in fields.iter().zip(field_tys.clone().iter()) {
                    // Unify fields with patterns

                    self.infer_pattern(*field, field_ty.1);
                }
                self.unify_var(pat_ty, pat_var);
            }
            Pattern::AlternativePattern { patterns } => {
                for pat in patterns.iter() {
                    self.infer_pattern(*pat, expected_ty_var);
                }
            }
            Pattern::AsPattern { pattern, as_name } => {
                let sub_pat = self.infer_pattern(*pattern, expected_ty_var);
                if let Some(as_name) = as_name {
                    self.infer_pattern(*as_name, expected_ty_var);
                }
                self.unify_var(pat_var, sub_pat)
            }
            Pattern::Literal { kind } => match kind {
                LiteralKind::Int => self.unify_var_ty(pat_var, Ty::Int),
                LiteralKind::Float => self.unify_var_ty(pat_var, Ty::Float),
                LiteralKind::String => self.unify_var_ty(pat_var, Ty::String),
                LiteralKind::Bool => self.unify_var_ty(pat_var, Ty::Bool),
            },
            Pattern::Spread { name: _ } => {
                self.unify_var_ty(
                    pat_var,
                    Ty::List {
                        of: expected_ty_var,
                    },
                );
                return pat_var;
            }
            Pattern::Tuple { fields } => {
                let mut field_ty = Vec::new();
                for field in fields.iter() {
                    let new_ty = self.new_ty_var();
                    self.infer_pattern(*field, new_ty);
                    field_ty.push(new_ty);
                }
                self.unify_var_ty(pat_var, Ty::Tuple { fields: field_ty })
            }
            Pattern::List { elements } => {
                let of = self.new_ty_var();
                for elem in elements.iter() {
                    self.infer_pattern(*elem, of);
                }
                self.unify_var_ty(pat_var, Ty::List { of });
            }
            Pattern::Concat { pattern } => {
                let expected_ty = self.new_ty_var();
                self.unify_var_ty(expected_ty, Ty::String);
                self.infer_pattern(*pattern, expected_ty);
            }
            Pattern::Missing => {}
            Pattern::Hole => {}
            Pattern::Variable { .. } => {}
        }
        self.unify_var(expected_ty_var, pat_var);
        pat_var
    }

    fn resolve_variant(&mut self, name: &SmolStr) -> (TyVar, Vec<(Option<SmolStr>, TyVar)>) {
        match self.resolver.resolve_name(name) {
            Some(res) => self.variant_from_resolve_result(res),
            None => (self.new_ty_var(), Vec::new()),
        }
    }

    fn variant_from_resolve_result(
        &mut self,
        result: ResolveResult,
    ) -> (TyVar, Vec<(Option<SmolStr>, TyVar)>) {
        match result {
            ResolveResult::Variant(variant) => {
                let mut ty_env = HashMap::new();
                let params = variant
                    .fields(self.db.upcast())
                    .into_iter()
                    .map(|field| {
                        (
                            field.label(self.db.upcast()),
                            self.make_type(field.ty(self.db.upcast()), &mut ty_env),
                        )
                    })
                    .collect();

                let adt_data = Adt::from(variant.parent).data(self.db.upcast());
                let mut generic_params = Vec::new();
                for param in adt_data.params.iter() {
                    generic_params.push(self.make_type(param.clone(), &mut ty_env));
                }

                let ty = Ty::Adt {
                    adt_id: variant.parent,
                    generic_params,
                }
                .intern(self);
                (ty, params)
            }
            ResolveResult::BuiltIn(it) => match it {
                hir::BuiltIn::Nil => (Ty::Nil.intern(self), Vec::new()),
                hir::BuiltIn::Ok => {
                    let ok = self.new_ty_var();
                    (
                        Ty::Result {
                            ok,
                            err: self.new_ty_var(),
                        }
                        .intern(self),
                        vec![(None, ok)],
                    )
                }
                hir::BuiltIn::Error => {
                    let err = self.new_ty_var();
                    (
                        Ty::Result {
                            ok: self.new_ty_var(),
                            err,
                        }
                        .intern(self),
                        vec![(None, err)],
                    )
                }
            },
            _ => (self.new_ty_var(), Vec::new()),
        }
    }

    fn unify(&mut self, lhs: Ty, rhs: Ty) -> Ty {
        match (lhs, rhs) {
            (Ty::Unknown { idx }, Ty::Unknown { idx: idx2 }) => Ty::Unknown {
                idx: min(idx, idx2),
            },
            (Ty::Unknown { .. }, other) | (other, Ty::Unknown { .. }) => other,
            (Ty::Result { ok, err }, Ty::Result { ok: okr, err: errr }) => {
                self.unify_var(ok, okr);
                self.unify_var(err, errr);
                Ty::Result { ok, err }
            }
            (
                Ty::Adt {
                    adt_id: id1,
                    generic_params: params1,
                },
                Ty::Adt {
                    adt_id: id2,
                    generic_params: params2,
                },
            ) => {
                // diagnostic if adt_id not the same!
                if id1 != id2 {
                    tracing::info!("Different adts {:?} {:?}", id1, id2);
                }
                for (p1, p2) in params1.clone().into_iter().zip(params2.into_iter()) {
                    self.unify_var(p1, p2)
                }
                Ty::Adt {
                    adt_id: id1,
                    generic_params: params1,
                }
            }
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
                // reorder
                for (p1, p2) in params1.clone().into_iter().zip(params2.into_iter()) {
                    self.unify_var(p1.1, p2.1)
                }
                self.unify_var(ret1, ret2);
                Ty::Function {
                    params: params1,
                    return_: ret1,
                }
            }
            (Ty::Tuple { fields: fields1 }, Ty::Tuple { fields: fields2 }) => {
                for (f1, f2) in fields1.clone().into_iter().zip(fields2.into_iter()) {
                    self.unify_var(f1, f2)
                }
                Ty::Tuple { fields: fields1 }
            }
            (lhs, rhs) => {
                if lhs != rhs {
                    tracing::debug!("ERROR: {:?} {:?}", lhs, rhs);
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
                module_resolution: ctx.module_resolution,
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
            Ty::Nil => super::Ty::Nil,
            Ty::Bool => super::Ty::Bool,
            Ty::Int => super::Ty::Int,
            Ty::Float => super::Ty::Float,
            Ty::String => super::Ty::String,
            Ty::Result { ok, err } => {
                let ok = self.collect(*ok);
                let err = self.collect(*err);
                super::Ty::Result {
                    ok: Arc::new(ok),
                    err: Arc::new(err),
                }
            }
            Ty::List { of } => {
                let of = self.collect(*of);
                super::Ty::List { of: Arc::new(of) }
            }
            Ty::Function { params, return_ } => {
                let mut super_params = Vec::new();
                for param in params {
                    super_params.push(self.collect(param.1));
                }
                let super_return = self.collect(*return_);
                super::Ty::Function {
                    params: Arc::new(super_params),
                    return_: Arc::new(super_return),
                }
            }
            Ty::Tuple { fields } => {
                let mut super_fields = Vec::new();
                for field in fields {
                    super_fields.push(self.collect(*field));
                }
                super::Ty::Tuple {
                    fields: Arc::new(super_fields),
                }
            }
            Ty::Adt {
                adt_id,
                generic_params,
            } => {
                let adt = self.db.lookup_intern_adt(*adt_id);
                let module = self.db.module_items(adt.file_id);
                let adt_data = &module[adt.value];
                let mut params = Vec::new();
                for param in generic_params {
                    params.push(self.collect(*param));
                }

                super::Ty::Adt {
                    // try to use module from adt
                    module: None,
                    name: adt_data.name.clone(),
                    params: Arc::new(params),
                }
            }
        }
    }
}
