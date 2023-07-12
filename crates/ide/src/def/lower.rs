use std::ops::Index;

use crate::{base::Target, Diagnostic, DiagnosticKind};

use super::{
    module::{
        Expr, ExprId, Function, Import, ImportId, Label, Literal, Name, NameId, NameKind, Param,
        Pattern, PatternId, Statement,
    },
    AstPtr, DefDatabase,
};
use la_arena::{Arena, Idx};
use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode, LiteralKind, StatementExpr},
    Parse,
};

#[derive(Default, Debug, Eq, PartialEq)]
pub struct ModuleItemData {
    functions: Arena<Function>,
    imports: Arena<Import>,

    diagnostics: Vec<Diagnostic>,
}

impl Index<Idx<Function>> for ModuleItemData {
    type Output = Function;

    fn index(&self, index: Idx<Function>) -> &Self::Output {
        &self.functions[index]
    }
}

struct LowerCtx<'a> {
    db: &'a dyn DefDatabase,
    module_items: ModuleItemData,
}

pub(super) fn lower_module(db: &dyn DefDatabase, parse: Parse) -> ModuleItemData {
    let mut ctx = LowerCtx {
        db,
        module_items: ModuleItemData::default(),
    };
    ctx.lower_module(parse.root());
    ctx.module_items
}

impl<'a> LowerCtx<'a> {
    fn alloc_function(&mut self, function: Function, ptr: AstPtr<ast::Function>) -> Idx<Function> {
        let name = function.name;
        let id = self.module_items.functions.alloc(function);
        id
    }

    fn alloc_import(&mut self, import: Import) -> Idx<Import> {
        self.module_items.imports.alloc(import)
    }

    fn diagnostic(&mut self, diag: Diagnostic) {
        self.module_items.diagnostics.push(diag);
    }

    fn lower_module(&mut self, module: ast::SourceFile) {
        for tg in module.statements() {
            self.lower_target_group(&tg)
        }
    }

    fn lower_target_group(&mut self, tg: &ast::TargetGroup) {
        let package_info = self.db.source_root_package_info(crate::SourceRootId(0));
        if let (Some(token), Some(package_info)) =
            (tg.target().and_then(|t| t.token()), package_info)
        {
            if Target::from(token.text()) != package_info.target {
                self.diagnostic(Diagnostic::new(
                    tg.syntax().text_range(),
                    DiagnosticKind::InactiveTarget,
                ));
                return;
            }
        }
        for statement in tg.statements() {
            self.lower_module_statement(&statement)
        }
    }

    fn lower_module_statement(&mut self, stmnt: &ast::ModuleStatement) {
        match stmnt {
            // ast::ModuleStatement::ModuleConstant(_) => todo!(),
            // ast::ModuleStatement::Import(_) => todo!(),
            ast::ModuleStatement::Function(f) => {
                self.lower_function(f);
            }
            ast::ModuleStatement::Import(i) => {
                self.lower_import(i);
            }
            _ => return,
        };
    }
    /// Here were resolving the imports and allocating the
    fn lower_import(&mut self, i: &ast::Import) {
        let ast_ptr = AstPtr::new(i);

        let module_name: SmolStr = i
            .module_path()
            .into_iter()
            .filter_map(|t| Some(format!("{}", t.token()?.text())))
            .collect();

        for unqualified in i.unqualified() {
            if let Some(unqualified_name) = unqualified
                .name()
                .and_then(|t| t.token().map(|t| SmolStr::from(t.text())))
            {
                let unqualified_as_name: Option<SmolStr> = unqualified
                    .as_name()
                    .and_then(|t| t.token())
                    .map(|t| t.text().into());

                self.alloc_import(Import {
                    module: module_name.clone(),
                    unqualified_as_name,
                    unqualified_name,
                    ast_ptr,
                });
            }
        }
    }

    fn lower_function(&mut self, fun: &ast::Function) -> Option<Idx<Function>> {
        let ast_ptr = AstPtr::new(fun);

        let name = fun.name();

        let mut params = Vec::new();

        if let Some(param_list) = fun.param_list() {
            for param in param_list.params() {
                if let Some(param_name) = param.pattern()?.text() {
                    params.push(Param {
                        name: param_name.into(),
                        label: param.label().and_then(|n| Some(n.token()?.text().into())),
                    });
                }
            }
        };

        Some(self.alloc_function(
            Function {
                name: fun.name()?.token()?.text().into(),
                params,
                ast_ptr: ast_ptr.clone(),
            },
            ast_ptr,
        ))
    }
}
