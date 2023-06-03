use super::{
    module::{
        ExprId, Function, FunctionId, Label, ModuleData, ModuleSourceMap, ModuleStatementId, Name,
        NameId, NameKind, Param, Expr,
    },
    AstPtr, DefDatabase, FileId,
};
use smol_str::SmolStr;
use syntax::{ast::{self, AstNode}, Parse};

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

   fn alloc_expr(&mut self, expr: Expr, ptr: AstPtr<ast::Expr>) -> ExprId {
        let id = self.module_data.exprs.alloc(expr);
        self.source_map.expr_map.insert(ptr.clone(), id);
        self.source_map.expr_map_rev.insert(id, ptr);
        id
    }

    fn lower_module(&mut self, module: ast::SourceFile) -> Vec<ModuleStatementId> {
        module
            .statements()
            .flat_map(|tg| self.lower_target_group(&tg))
            .collect()
    }

    fn lower_target_group(&mut self, tg: &ast::TargetGroup) -> Vec<ModuleStatementId> {
        // if let Some(target) = stmnt.target() {
        //     if target.syntax().to_string() == "javascript" {
        //         self.diagnostic(Diagnostic::new(
        //             stmnt.syntax().text_range(),
        //             DiagnosticKind::InactiveTarget,
        //         ));
        //     }
        // };
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

        let expr_id = self.lower_expr(fun.body()?);

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
            _ => self.alloc_expr(Expr::Missing, ptr)
        }
    }
}

