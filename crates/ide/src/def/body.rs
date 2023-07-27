use std::{collections::HashMap, ops};

use la_arena::{Arena, ArenaMap, Idx, IdxRange, RawIdx};
use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode, LiteralKind, StatementExpr},
    AstPtr,
};

use crate::{DefDatabase, Diagnostic, FileId, InFile};

use super::{
    module::{Clause, Expr, ExprId, Literal, Pattern, PatternId, Statement},
    FunctionId,
};

pub type ExprPtr = AstPtr<ast::Expr>;
pub type ExprSource = InFile<ExprPtr>;

pub type PatternPtr = AstPtr<ast::Pattern>;
pub type PatternSource = InFile<PatternPtr>;

/// The item tree of a source file.
#[derive(Debug, Eq, PartialEq)]
pub struct Body {
    pub patterns: Arena<Pattern>,
    pub exprs: Arena<Expr>,

    pub params: Vec<PatternId>,

    pub body_expr: ExprId,
}

impl Default for Body {
    fn default() -> Self {
        Self {
            patterns: Arena::default(),
            exprs: Arena::default(),
            params: Vec::default(),
            body_expr: dummy_expr_id(),
        }
    }
}
/// FIXME: very hacky to allow body to implementDefault and avoid body_expr being optional
pub(crate) fn dummy_expr_id() -> ExprId {
    ExprId::from_raw(RawIdx::from(u32::MAX))
}

impl Body {
    pub fn shink_to_fit(&mut self) {
        self.patterns.shrink_to_fit();
        self.exprs.shrink_to_fit();
    }

    pub fn exprs(&self) -> impl Iterator<Item = (ExprId, &'_ Expr)> + ExactSizeIterator + '_ {
        self.exprs.iter()
    }

    pub fn patterns(
        &self,
    ) -> impl Iterator<Item = (PatternId, &'_ Pattern)> + ExactSizeIterator + '_ {
        self.patterns.iter()
    }
}

impl ops::Index<ExprId> for Body {
    type Output = Expr;
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index]
    }
}

impl ops::Index<PatternId> for Body {
    type Output = Pattern;
    fn index(&self, index: PatternId) -> &Self::Output {
        &self.patterns[index]
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct BodySourceMap {
    expr_map: HashMap<ExprSource, ExprId>,
    expr_map_rev: ArenaMap<ExprId, ExprSource>,

    pattern_map: HashMap<PatternSource, PatternId>,
    pattern_map_rev: ArenaMap<PatternId, PatternSource>,

    pub diagnostics: Vec<Diagnostic>,
}

impl BodySourceMap {
    pub fn shrink_to_fit(&mut self) {
        self.expr_map.shrink_to_fit();
        self.expr_map_rev.shrink_to_fit();

        self.pattern_map.shrink_to_fit();
        self.pattern_map_rev.shrink_to_fit();
    }

    pub fn expr_for_node(&self, node: InFile<&ast::Expr>) -> Option<ExprId> {
        let src = node.map(AstPtr::new);
        self.expr_map.get(&src).cloned()
    }

    pub fn node_for_expr(&self, expr_id: ExprId) -> Option<ExprSource> {
        self.expr_map_rev.get(expr_id).cloned()
    }

    pub fn pattern_for_node(&self, node: InFile<&ast::Pattern>) -> Option<PatternId> {
        let src = node.map(AstPtr::new);
        self.pattern_map.get(&src).cloned()
    }

    pub fn node_for_pattern(&self, pat_id: PatternId) -> Option<PatternSource> {
        self.pattern_map_rev.get(pat_id).cloned()
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

pub(super) fn lower(db: &dyn DefDatabase, function_id: FunctionId) -> (Body, BodySourceMap) {
    let loc = db.lookup_intern_function(function_id);
    let module = db.module_items(loc.file_id);
    let func = loc.value;
    let func = &module[func];
    let parse = db.parse(loc.file_id);
    let ast = func.ast_ptr.to_node(&parse.syntax_node());

    let mut ctx = BodyLowerCtx {
        db,
        file_id: loc.file_id,
        source_map: BodySourceMap::default(),
        body: Body::default(),
    };

    if let Some(param_list) = ast.param_list() {
        for param in param_list.params() {
            if let Some(pattern) = param.pattern() {
                let pat_id = ctx.lower_pattern(ast::Pattern::PatternVariable(pattern));
                ctx.body.params.push(pat_id);
            }
        }
    }

    let expr_id = ctx.lower_expr_opt(ast::Expr::cast(
        ast.body()
            .expect("Expected a body, this is a compiler bug!")
            .syntax()
            .clone(),
    ));
    ctx.body.body_expr = expr_id;

    (ctx.body, ctx.source_map)
}

struct BodyLowerCtx<'a> {
    db: &'a dyn DefDatabase,
    body: Body,
    source_map: BodySourceMap,
    file_id: FileId,
}

impl BodyLowerCtx<'_> {
    fn alloc_expr(&mut self, expr: Expr, ptr: AstPtr<ast::Expr>) -> ExprId {
        let ptr = InFile {
            file_id: self.file_id,
            value: ptr,
        };
        let id = self.body.exprs.alloc(expr);
        self.source_map.expr_map.insert(ptr.clone(), id);
        self.source_map.expr_map_rev.insert(id, ptr);
        id
    }

    fn alloc_pattern(&mut self, pattern: Pattern, ptr: AstPtr<ast::Pattern>) -> PatternId {
        let ptr = InFile {
            file_id: self.file_id,
            value: ptr,
        };
        let id = self.body.patterns.alloc(pattern);
        self.source_map.pattern_map.insert(ptr.clone(), id);
        self.source_map.pattern_map_rev.insert(id, ptr);
        id
    }

    fn missing_pat(&mut self) -> PatternId {
        self.body.patterns.alloc(Pattern::Missing)
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> ExprId {
        let ptr = AstPtr::new(&expr);
        match expr {
            ast::Expr::NameRef(e) => {
                let name = e
                    .token()
                    .map_or_else(Default::default, |tok| tok.text().into());
                self.alloc_expr(Expr::NameRef(name), ptr)
            }
            ast::Expr::Block(defs) => {
                let mut stmts = Vec::new();

                defs.expressions()
                    .into_iter()
                    .for_each(|a| self.lower_expr_stmt(&mut stmts, a));
                self.alloc_expr(Expr::Block { stmts: stmts }, ptr)
            }
            ast::Expr::ExprCall(call) => {
                let func = self.lower_expr_opt(call.func());
                let mut arg_ids = Vec::new();
                if let Some(args) = call.arguments() {
                    for arg in args.args() {
                        arg_ids.push(self.lower_expr_opt(arg.value()));
                    }
                }

                self.alloc_expr(
                    Expr::Call {
                        func,
                        args: arg_ids,
                    },
                    ptr,
                )
            }
            ast::Expr::BinaryOp(e) => {
                let left = self.lower_expr_opt(e.lhs());
                let op = e.op_kind();
                let right = self.lower_expr_opt(e.rhs());
                self.alloc_expr(Expr::Binary { op, left, right }, ptr)
            }
            ast::Expr::Literal(lit) => {
                let lit = self.lower_literal(lit);
                self.alloc_expr(lit.map_or(Expr::Missing, Expr::Literal), ptr)
            }
            ast::Expr::Case(case) => {
                let start = self.next_expr_idx();
                for subj in case.subjects() {
                    self.lower_expr(subj);
                }
                let end = self.next_expr_idx();

                let clauses = case.clauses().map(|clause| {
                    let start = self.next_pattern_idx();
                    for pattern in clause.patterns() {
                        self.lower_pattern(pattern);
                    }
                    let end = self.next_pattern_idx();
                    Clause {
                        expr: self.lower_expr_opt(clause.body()),
                        patterns: IdxRange::new(start..end),
                    }
                }).collect();

                self.alloc_expr(
                    Expr::Case {
                        subjects: IdxRange::new(start..end),
                        clauses
                    },
                    ptr,
                )
            }
            _ => self.alloc_expr(Expr::Missing, ptr),
        }
    }

    fn lower_expr_opt(&mut self, expr: Option<ast::Expr>) -> ExprId {
        if let Some(expr) = expr {
            return self.lower_expr(expr);
        }
        // Synthetic syntax has no coresponding text.
        self.body.exprs.alloc(Expr::Missing)
    }

    fn lower_expr_stmt(&mut self, statements: &mut Vec<Statement>, ast: ast::StatementExpr) {
        match ast {
            StatementExpr::StmtLet(stmt) => {
                if let (Some(expr), Some(pattern)) = (stmt.body(), stmt.pattern()) {
                    let expr_id = self.lower_expr(expr);
                    let pattern = self.lower_pattern(pattern);
                    statements.push(Statement::Let {
                        pattern,
                        body: expr_id,
                    });
                }
            }
            StatementExpr::StmtExpr(expr) => {
                if let Some(expr) = expr.expr() {
                    let expr_id = self.lower_expr(expr);
                    statements.push(Statement::Expr { expr: expr_id });
                }
            }
            StatementExpr::StmtUse(use_) => {
                if let Some(expr) = use_.expr() {
                    let mut patterns = Vec::new();
                    for assignment in use_.assignments() {
                        if let Some(pattern) = assignment.pattern() {
                            patterns.push(self.lower_pattern(pattern));
                        }
                    }
                    let expr_id = self.lower_expr(expr);
                    statements.push(Statement::Use {
                        patterns,
                        expr: expr_id,
                    });
                }
            }
        }
    }

    fn lower_literal(&mut self, lit: ast::Literal) -> Option<Literal> {
        let kind = lit.kind()?;
        let tok = lit.token().unwrap();
        let text = tok.text();

        Some(match kind {
            LiteralKind::Int => Literal::Int(text.parse::<i64>().ok()?),
            LiteralKind::Float => Literal::Float(text.parse::<f64>().unwrap().into()),
            LiteralKind::String => Literal::String(text.into()),
        })
    }

    fn lower_pattern(&mut self, pattern: ast::Pattern) -> PatternId {
        let ptr = AstPtr::new(&pattern);
        match pattern {
            ast::Pattern::PatternVariable(var) => {
                let name = var.text().unwrap_or(SmolStr::new_inline("[missing text]"));
                self.alloc_pattern(Pattern::Variable { name }, ptr)
            }
            ast::Pattern::Literal(_) => todo!(),
            ast::Pattern::TypeNameRef(_) => todo!(),
            ast::Pattern::PatternConstructorApplication(_) => todo!(),
            ast::Pattern::PatternTuple(pat) => {
                let mut pats = Vec::new();
                for pat in pat.field_patterns() {
                    pats.push(self.lower_pattern(pat));
                }
                self.alloc_pattern(Pattern::Tuple { fields: pats }, ptr)
            }
            ast::Pattern::AlternativePattern(pat) => {
                let mut pats = Vec::new();
                for pat in pat.patterns() {
                    pats.push(self.lower_pattern(pat));
                }
                self.alloc_pattern(Pattern::AlternativePattern{patterns: pats}, ptr)
            },
        }
    }

    fn lower_pat_opt(&mut self, pat: Option<ast::Pattern>) -> PatternId {
        match pat {
            Some(pat) => self.lower_pattern(pat),
            None => self.missing_pat(),
        }
    }

    fn next_expr_idx(&self) -> Idx<Expr> {
        Idx::from_raw(RawIdx::from(self.body.exprs.len() as u32))
    }

    fn next_pattern_idx(&self) -> Idx<Pattern> {
        Idx::from_raw(RawIdx::from(self.body.patterns.len() as u32))
    }
}
