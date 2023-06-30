use std::{mem, sync::Arc};

use la_arena::ArenaMap;
use syntax::ast::BinaryOpKind;

use crate::{
    def::{
        module::{Expr, ExprId, Function, Literal, ModuleData, NameId, Statement},
        NameResolution, ResolveResult,
    },
    FileId, TyDatabase,
};

use super::union_find::UnionFind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TyVar(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    Unknown,
    Int,
    Float,
    String,
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
    name_ty_map: ArenaMap<NameId, super::Ty>,
    expr_ty_map: ArenaMap<ExprId, super::Ty>,
    // field_resolution: HashMap<ExprId, FieldId>,
}

impl InferenceResult {
    pub fn ty_for_name(&self, name: NameId) -> super::Ty {
        self.name_ty_map[name].clone()
    }

    pub fn ty_for_expr(&self, expr: ExprId) -> super::Ty {
        self.expr_ty_map[expr].clone()
    }
}

pub(crate) fn infer_query(db: &dyn TyDatabase, file: FileId) -> Arc<InferenceResult> {
    infer(db, file)
}

pub(crate) fn infer(db: &dyn TyDatabase, file: FileId) -> Arc<InferenceResult> {
    let module = db.module(file);
    let nameres = db.name_resolution(file);
    let table = UnionFind::new(module.names().len() + module.exprs().len(), |_| Ty::Unknown);

    let mut ctx = InferCtx {
        db,
        file_id: file,
        module: &module,
        nameres: &nameres,
        table,
    };

    for (_, func) in module.functions() {
        let placeholder_ty = ctx.ty_for_name(func.name);
        let ty = ctx.infer_function(func);
        ctx.unify_var(placeholder_ty, ty);
    }

    tracing::info!("{:#?}", ctx.table);
    Arc::new(ctx.finish())
}

struct InferCtx<'db> {
    db: &'db dyn TyDatabase,
    module: &'db ModuleData,
    nameres: &'db NameResolution,

    file_id: FileId,

    /// The arena for both unification and interning.
    /// First `module.names().len() + module.exprs().len()` elements are types of each names and
    /// exprs, to allow recursive definition.
    table: UnionFind<Ty>,
}

impl<'db> InferCtx<'db> {
    fn new_ty_var(&mut self) -> TyVar {
        TyVar(self.table.push(Ty::Unknown))
    }

    fn ty_for_name(&self, i: NameId) -> TyVar {
        TyVar(u32::from(i.into_raw()))
    }

    fn ty_for_expr(&self, i: ExprId) -> TyVar {
        TyVar(self.module.names().len() as u32 + u32::from(i.into_raw()))
    }

    fn infer_function(&mut self, func: &Function) -> TyVar {
        let mut param_tys = Vec::new();
        for param in &func.params {
            let param_ty = self.new_ty_var();
            param_tys.push(param_ty);
            self.unify_var(param_ty, self.ty_for_name(param.name));
        }
        let body_ty = self.infer_expr(func.body);
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
            Statement::Use { patterns: _, expr: _ } => todo![]
        }
    }

    fn infer_expr(&mut self, e: ExprId) -> TyVar {
        let ty = self.infer_expr_inner(e);
        let placeholder_ty = self.ty_for_expr(e);
        self.unify_var(placeholder_ty, ty);
        ty
    }

    fn infer_expr_inner(&mut self, e: ExprId) -> TyVar {
        match &self.module[e] {
            Expr::Missing => self.new_ty_var(),
            Expr::Literal(lit) => {
                match lit {
                    Literal::Int(_) => Ty::Int,
                    Literal::Float(_) => Ty::Float,
                    Literal::String(_) => Ty::String,
                }
                .intern(self)
            }
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
                    BinaryOpKind::Add
                    | BinaryOpKind::Sub
                    | BinaryOpKind::Mul
                    | BinaryOpKind::Div => {
                        // TODO: Arguments have type: int | float.
                        self.unify_var(lhs_ty, rhs_ty);
                        lhs_ty
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
                tracing::info!("{:?}", fun_ty);
                self.unify_var_ty(fun_ty, Ty::Function{params: arg_tys, return_: ret_ty});
                ret_ty
            },
            Expr::NameRef(_) => match self.nameres.get(e) {
                None => self.new_ty_var(),
                Some(res) => match res {
                    &ResolveResult((name, _file)) => 
                    {
                        self.ty_for_name(name)
                        // if file == self.file_id {
                        //     self.ty_for_name(name)
                        // } else {
                        //     let inference = self.db.infer(file);
                        //     let var = self.new_ty_var();
                        //     self.unify_var_ty(var, inference.ty_for_name(name));
                        //     var
                        // }
                    }
                },
            },
        }
    }

    fn unify_var_ty(&mut self, var: TyVar, rhs: Ty) {
        let lhs = mem::replace(self.table.get_mut(var.0), Ty::Unknown);
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
            (Ty::Unknown, other) | (other, Ty::Unknown) => other,
            (Ty::Function{params: params1, return_: ret1} ,Ty::Function { params: params2, return_:ret2 }) => {
                for (p1, p2) in params1.clone().into_iter().zip(params2.into_iter()) {
                    self.unify_var(p1, p2)
                }
                self.unify_var(ret1.clone(), ret2);
                Ty::Function { params: params1, return_: ret1 }
            }
            (lhs, rhs) => {
                if lhs != rhs {
                    tracing::info!("ERROR: {:?} {:?}", lhs, rhs);
                };
                lhs
            },
        }
    }

    fn finish(mut self) -> InferenceResult {
        let mut i = Collector::new(&mut self.table);

        let name_cnt = self.module.names().len();
        let expr_cnt = self.module.exprs().len();
        let mut name_ty_map = ArenaMap::with_capacity(name_cnt);
        let mut expr_ty_map = ArenaMap::with_capacity(expr_cnt);
        for (name, _) in self.module.names() {
            let ty = TyVar(u32::from(name.into_raw()));
            name_ty_map.insert(name, i.collect(ty));
        }
        for (expr, _) in self.module.exprs() {
            let ty = TyVar(name_cnt as u32 + u32::from(expr.into_raw()));
            expr_ty_map.insert(expr, i.collect(ty));
        }

        InferenceResult {
            name_ty_map,
            expr_ty_map,
        }
    }
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

        // Prevent cycles.
        self.cache[i as usize] = Some(super::Ty::Unknown);
        let ret = self.collect_uncached(i);
        self.cache[i as usize] = Some(ret.clone());
        ret
    }

    fn collect_uncached(&mut self, i: u32) -> super::Ty {
        let ty = mem::replace(self.table.get_mut(i), Ty::Unknown);
        match ty {
            Ty::Unknown => super::Ty::Unknown,
            Ty::Int => super::Ty::Int,
            Ty::Float => super::Ty::Float,
            Ty::String => super::Ty::String,
            Ty::Function { params, return_ } => {
                let mut super_params = Vec::new();
                for param in params {
                    super_params.push(self.collect(param));
                }
                let super_return = self.collect(return_);
                super::Ty::Function {
                    params: Arc::new(super_params),
                    return_: Arc::new(super_return),
                }
            }
            Ty::Tuple { fields: _ } => todo!(),
        }
    }
}
