use std::collections::HashMap;

use crate::{Diagnostic, DiagnosticKind, FileRange};

use super::{
    AstPtr, DefDatabase, FileId, Function, FunctionId, Module, ModuleSourceMap, Name, NameId,
    NameKind, Param,
};
use smol_str::SmolStr;
use syntax::{
    ast::{self},
    Parse,
};

pub(super) fn lower(
    db: &dyn DefDatabase,
    file_id: FileId,
    parse: Parse,
) -> (Module, ModuleSourceMap) {
    let mut ctx = LowerCtx {
        db,
        file_id,
        module: Module::default(),
        source_map: ModuleSourceMap::default(),
    };

    ctx.lower_module(parse.root());
    (ctx.module, ctx.source_map)
}

struct LowerCtx<'a> {
    db: &'a dyn DefDatabase,
    file_id: FileId,
    module: Module,
    source_map: ModuleSourceMap,
}

impl LowerCtx<'_> {
    fn alloc_name(&mut self, text: SmolStr, kind: NameKind, ptr: AstPtr<ast::Name>) -> NameId {
        let id = self.module.names.alloc(Name { text, kind });
        self.source_map.name_map.insert(ptr.clone(), id);
        self.source_map.name_map_rev.insert(id, ptr);
        id
    }

    fn alloc_function(&mut self, function: Function, ptr: AstPtr<ast::Function>) -> FunctionId {
        let id = self.module.functions.alloc(function);
        self.source_map.function_map.insert(ptr.clone(), id);
        self.source_map.function_map_rev.insert(id, ptr);
        id
    }

    fn diagnostic(&mut self, diag: Diagnostic) {
        self.source_map.diagnostics.push(diag);
    }

    fn lower_module(&mut self, module: ast::SourceFile) {
        for tg in module.statements() {
            self.lower_target_group(tg);
        }
    }

    fn lower_target_group(&mut self, tg: ast::TargetGroup) {
        // if let Some(target) = stmnt.target() {
        //     if target.syntax().to_string() == "javascript" {
        //         self.diagnostic(Diagnostic::new(
        //             stmnt.syntax().text_range(),
        //             DiagnosticKind::InactiveTarget,
        //         ));
        //     }
        // };
        for stmnt in tg.statements() {
            self.lower_module_statement(stmnt);
        }
    }

    fn lower_module_statement(&mut self, stmnt: ast::ModuleStatement) {
        let ptr = AstPtr::new(&stmnt);

        //Needs module item enum and into() calls
        match stmnt {
            // ast::ModuleStatement::ModuleConstant(_) => todo!(),
            // ast::ModuleStatement::Import(_) => todo!(),
            ast::ModuleStatement::Function(f) => self.lower_function(f),
            _ => None,
        };
    }

    fn lower_function(&mut self, fun: ast::Function) -> Option<FunctionId> {
        let ptr = AstPtr::new(&fun);
        let mut param_locs = HashMap::new();
        let mut lower_name = |this: &mut Self, node: ast::Name, kind: NameKind| -> NameId {
            let ptr = AstPtr::new(&node);
            let text = match node.token() {
                None => "".into(),
                Some(tok) => {
                    let text: SmolStr = tok.text().into();
                    if let Some(prev_loc) = param_locs.insert(text.clone(), ptr.text_range()) {
                        this.diagnostic(
                            Diagnostic::new(ptr.text_range(), DiagnosticKind::DuplicatedParam)
                                .with_note(
                                    FileRange::new(this.file_id, prev_loc),
                                    "Previously defined here",
                                ),
                        );
                    }
                    text
                }
            };
            this.alloc_name(text, kind, ptr)
        };

        // Assoc function name
        // let name = self.alloc_name(fun, )?

        // let params = fun.param_list().map(|param_list| {
        //     param_list.params().filter_map(|param| {
        //         let name = param.name().map(|n| lower_name(self, n, NameKind::Param))?;
        //         Some(Param::Normal(name))
        //     })
        // }).iter();
        let name = self.alloc_name(
            fun.name()?.token()?.text().into(),
            NameKind::Function,
            AstPtr::new(&fun.name()?),
        );

        let mut params = Vec::new();

        if let Some(param_list) = fun.param_list() {
            for param in param_list.params() {
                let param_name = lower_name(self, param.name()?, NameKind::Param);
                params.push(Param::Normal(param_name));
            }
        };
        Some(self.alloc_function(Function { name, params }, ptr))
    }
}
