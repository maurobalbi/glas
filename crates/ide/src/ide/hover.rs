use crate::DefDatabase;
use smol_str::SmolStr;
use syntax::ast::AstNode;
use syntax::{ast, best_token_at_offset, TextRange};

use crate::def::hir_def::{ModuleDefId, VariantLoc};
use crate::def::resolver::resolver_for_toplevel;
use crate::def::source_analyzer::find_def;
use crate::def::{self, resolver_for_expr};
use crate::ty::display::TyDisplay;
use crate::ty::TyDatabase;
use crate::{FilePos, InFile};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoverResult {
    pub range: TextRange,
    pub markup: String,
}

pub(crate) fn hover(db: &dyn TyDatabase, FilePos { file_id, pos }: FilePos) -> Option<HoverResult> {
    let parse = db.parse(file_id);
    let tok = best_token_at_offset(&parse.syntax_node(), pos)?;
    if matches!(
        tok.parent()?.kind(),
        syntax::SyntaxKind::FIELD_ACCESS | syntax::SyntaxKind::TUPLE_INDEX
    ) {
        return None;
    }

    if ast::NameRef::can_cast(tok.parent()?.kind()) {
        let expr = ast::Expr::cast(tok.parent()?.parent()?)?;
        tracing::info!("HERERE {:?}", expr);

        let expr_ptr = InFile {
            file_id: file_id,
            value: &tok.parent()?,
        };

        let resolver = match find_def(db.upcast(), expr_ptr) {
            Some(ModuleDefId::FunctionId(id)) => {
                let source_map = db.body_source_map(id);
                let expr_ptr = expr_ptr.with_value(&expr);
                resolver_for_expr(db.upcast(), id, source_map.expr_for_node(expr_ptr)?)
            }
            _ => resolver_for_toplevel(db.upcast(), file_id),
        };

        // let ResolveResult((name, file_id)) = name_res.get(expr_id)?;

        // let source_map = db.source_map(*file_id);
        let name = SmolStr::from(tok.text());

        let hover: Option<HoverResult> = resolver.resolve_name(&name).map(|res| {
            match res {
                def::resolver::ResolveResult::LocalBinding(pattern) => {
                    let infer = db.infer_function(resolver.body_owner()?);
                    let ty = infer.ty_for_pattern(pattern);
                    Some(HoverResult {
                        range: tok.text_range(),
                        markup: format!("```gleam\n{}\n```", ty.display(db)),
                    })
                }
                def::resolver::ResolveResult::FunctionId(fn_id) => {
                    let infer = db.infer_function(fn_id);
                    let ty = &infer.fn_ty;
                    Some(HoverResult {
                        range: tok.text_range(),
                        markup: format!("```gleam\n{}\n```", ty.display(db)),
                    })
                }
                def::resolver::ResolveResult::VariantId(_) => todo!(),
            }
            // let full_node = name_node.ancestors().find(|n| {
            //     matches!(
            //         n.kind(),
            //         SyntaxKind::LAMBDA | SyntaxKind::ATTR_PATH_VALUE | SyntaxKind::INHERIT
            //     )
            // })?;
        })?;

        return hover;
    }
    Some(HoverResult {
        range: TextRange::new(pos, pos.checked_add(5.into()).unwrap()),
        markup: String::from("This is hover"),
    })
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(fixture: &str, full: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1);
        let ret = super::hover(&db, f[0]).expect("No hover");
        let src = db.file_content(f[0].file_id);
        assert_eq!(full, &src[ret.range]);
        let mut got = ret.markup.trim().to_string();
        if got.contains('\n') {
            got += "\n";
        }
        expect.assert_eq(&got);
    }

    #[track_caller]
    fn check_no(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1);
        assert_eq!(super::hover(&db, f[0]), None);
    }

    #[test]
    fn definition() {
        check(
            "fn main() { $0main() }",
            "main",
            expect![[r#"
                ```gleam
                fn() -> a
                ```
            "#]],
        );
    }

    #[test]
    fn generic() {
        check(
            "fn main(a, b) { $0b }",
            "b",
            expect![[r#"
                ```gleam
                b
                ```
            "#]],
        );
    }
}
