use crate::{Diagnostic, DiagnosticKind};

use super::{AstPtr, DefDatabase, FileId, Module, ModuleSourceMap, Name, NameId, NameKind};
use la_arena::Arena;
use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode, Target},
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
        module: Module {
            names: Arena::new(),
            statements: Arena::new(),
            // Placeholder.
        },
        source_map: ModuleSourceMap::default(),
    };

    let _entry = ctx.lower_module(parse.root());
    let mut module = ctx.module;
    (module, ctx.source_map)
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
        self.source_map.name_map_rev.insert(id, vec![ptr]);
        id
    }

    fn diagnostic(&mut self, diag: Diagnostic) {
        self.source_map.diagnostics.push(diag);
    }

    fn lower_module(&mut self, module: ast::SourceFile) {
        for stmnt in module.statements() {
            self.lower_target_group(stmnt);
        }
    }

    fn lower_target_group(&mut self, stmnt: ast::TargetGroup) {
        if let Some(target) = stmnt.target() {
            if target.syntax().to_string() == "javascript" {
                self.diagnostic(Diagnostic::new(
                    stmnt.syntax().text_range(),
                    DiagnosticKind::InactiveTarget,
                ));
            }
        };
    }
}
