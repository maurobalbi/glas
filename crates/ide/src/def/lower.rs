use crate::{base::Target, Diagnostic, DiagnosticKind};

use super::{
    module::{
        Expr, ExprId, Function, FunctionId, Label, ModuleData, ModuleSourceMap, ModuleStatementId,
        Name, NameId, NameKind, Param, Pattern, PatternId, Statement,
    },
    AstPtr, DefDatabase, FileId,
};
use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode, StatementExpr},
    Parse,
};

pub(super) fn lower(
    db: &dyn DefDatabase,
    file_id: FileId,
    parse: Parse,
) -> (ModuleData, ModuleSourceMap) {
    let mut ctx = LowerCtx {
        db,
        file_id,
        module_data: ModuleData::default(),
        source_map: ModuleSourceMap::default(),
    };

    ctx.lower_module(parse.root());
    (ctx.module_data, ctx.source_map)
}

struct LowerCtx<'a> {
    db: &'a dyn DefDatabase,
    file_id: FileId,
    module_data: ModuleData,
    source_map: ModuleSourceMap,
}

impl LowerCtx<'_> {
    fn alloc_module_statement(
        &mut self,
        function: Function,
        ptr: AstPtr<ast::Function>,
    ) -> FunctionId {
        let id = self.module_data.functions.alloc(function);
        self.source_map.fn_map.insert(ptr.clone(), id.into());
        self.source_map.fn_map_rev.insert(id, ptr);
        id
    }

    fn alloc_name(&mut self, text: SmolStr, kind: NameKind, ptr: AstPtr<ast::Name>) -> NameId {
        let id = self.module_data.names.alloc(Name { text, kind });
        self.source_map.name_map.insert(ptr.clone(), id);
        self.source_map.name_map_rev.insert(id, ptr);
        id
    }

    fn missing_name(&mut self, kind: NameKind) -> NameId {
        self.module_data.names.alloc(Name {
            text: "[missing name]".into(),
            kind,
        })
    }

    fn alloc_expr(&mut self, expr: Expr, ptr: AstPtr<ast::Expr>) -> ExprId {
        let id = self.module_data.exprs.alloc(expr);
        self.source_map.expr_map.insert(ptr.clone(), id);
        self.source_map.expr_map_rev.insert(id, ptr);
        id
    }
    fn alloc_pattern(&mut self, pattern: Pattern, ptr: AstPtr<ast::Pattern>) -> PatternId {
        let id = self.module_data.patterns.alloc(pattern);
        self.source_map.pattern_map.insert(ptr.clone(), id);
        self.source_map.pattern_map_rev.insert(id, ptr);
        id
    }

    fn diagnostic(&mut self, diag: Diagnostic) {
        self.source_map.diagnostics.push(diag);
    }

    fn lower_module(&mut self, module: ast::SourceFile) -> Vec<ModuleStatementId> {
        module
            .statements()
            .flat_map(|tg| self.lower_target_group(&tg))
            .collect()
    }

    fn lower_target_group(&mut self, tg: &ast::TargetGroup) -> Vec<ModuleStatementId> {
        let package_info = self.db.source_root_package_info(crate::SourceRootId(0));
        if let (Some(token), Some(package_info)) =
            (tg.target().and_then(|t| t.token()), package_info)
        {
            tracing::info!("TARGET {:?} {:?}", token.text(), package_info.target);
            if Target::from(token.text()) != package_info.target {
                self.diagnostic(Diagnostic::new(
                    tg.syntax().text_range(),
                    DiagnosticKind::InactiveTarget,
                ));
                return Vec::new();
            }
        }

        tg.statements()
            .flat_map(|stmt| self.lower_module_statement(&stmt))
            .collect()
    }

    fn lower_module_statement(
        &mut self,
        stmnt: &ast::ModuleStatement,
    ) -> Option<ModuleStatementId> {
        let item = match stmnt {
            // ast::ModuleStatement::ModuleConstant(_) => todo!(),
            // ast::ModuleStatement::Import(_) => todo!(),
            ast::ModuleStatement::Function(f) => self.lower_function(f)?.into(),
            _ => return None,
        };

        Some(item)
    }

    fn lower_function(&mut self, fun: &ast::Function) -> Option<FunctionId> {
        let ast_ptr = AstPtr::new(fun);

        let name = fun.name().and_then(|n| {
            Some(self.alloc_name(
                n.token()?.text().into(),
                NameKind::Function,
                AstPtr::new(&n),
            ))
        })?;

        let mut params = Vec::new();

        if let Some(param_list) = fun.param_list() {
            for param in param_list.params() {
                let param_name = self.alloc_name(
                    // ToDo: Refactor question marks with .map to not skip loops early
                    param.name()?.token()?.text().into(),
                    NameKind::Param,
                    AstPtr::new(&param.name()?),
                );
                params.push(Param {
                    name: param_name,
                    label: param
                        .label()
                        .and_then(|n| Some(Label(n.token()?.text().into()))),
                });
            }
        };

        let expr_id = self.lower_expr(ast::Expr::Block(fun.body()?));

        Some(
            self.alloc_module_statement(
                Function {
                    name,
                    params,
                    body: expr_id,
                    ast_ptr: ast_ptr.clone(),
                },
                ast_ptr,
            )
            .into(),
        )
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
            _ => self.alloc_expr(Expr::Missing, ptr),
        }
    }

    fn lower_expr_opt(&mut self, expr: Option<ast::Expr>) -> ExprId {
        if let Some(expr) = expr {
            return self.lower_expr(expr);
        }
        // Synthetic syntax has no coresponding text.
        self.module_data.exprs.alloc(Expr::Missing)
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
        }
    }

    fn lower_pattern(&mut self, pattern: ast::Pattern) -> PatternId {
        let ptr = AstPtr::new(&pattern);
        match pattern {
            ast::Pattern::PatternVariable(var) => {
                let name = self.lower_name_opt(NameKind::Pattern, var.name());
                self.alloc_pattern(Pattern::Variable { name }, ptr)
            }
            ast::Pattern::TypeNameRef(_) => todo!(),
            ast::Pattern::PatternConstructorApplication(_) => todo!(),
            ast::Pattern::PatternTuple(_) => todo!(),
            ast::Pattern::AlternativePattern(_) => todo!(),
        }
    }

    fn lower_name_opt(&mut self, kind: NameKind, name: Option<ast::Name>) -> NameId {
        match name {
            Some(name) => {
                let ptr = AstPtr::new(&name);
                let text = name
                    .token()
                    .map_or_else(|| "[missing text]".into(), |t| t.text().into());
                self.alloc_name(text, kind, ptr)
            }
            None => self.missing_name(kind),
        }
    }
}
