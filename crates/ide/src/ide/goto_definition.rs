use super::NavigationTarget;
use crate::def::hir_def::{AdtLoc, ModuleDefId, VariantLoc};
use crate::def::resolver::resolver_for_toplevel;
use crate::def::resolver_for_expr;
use crate::def::source_analyzer::{find_def, SourceAnalyzer};
use crate::ty::TyDatabase;
use crate::{DefDatabase, FilePos, InFile, VfsPath};
use smol_str::SmolStr;
use syntax::ast::{self, AstNode, FieldAccessExpr};
use syntax::{best_token_at_offset, TextRange, TextSize, match_ast};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GotoDefinitionResult {
    Path(VfsPath),
    Targets(Vec<NavigationTarget>),
}

pub(crate) fn goto_definition(
    db: &dyn TyDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<GotoDefinitionResult> {
    let parse = db.parse(file_id).syntax_node();
    let tok = best_token_at_offset(&parse, pos)?;
    // let module_data = db.module_items(file_id);
    // let source_map = db.source_map(file_id);

    //If tok.parent is field access or tuple access, it will be necessary to infer type first
    if let Some(name_ref_tok) = ast::NameRef::cast(tok.parent()?) {
        if let Some(tok) = FieldAccessExpr::for_label_name_ref(&name_ref_tok) {
            let expr_ptr = InFile {
                file_id: file_id,
                value: &tok.label()?,
            };
            
            match find_def(db.upcast(), expr_ptr.with_value(tok.syntax())) {
                Some(ModuleDefId::FunctionId(fn_id)) => {
                    tracing::info!("WERE IN FIELD ACES`s");
                    let analyzer = SourceAnalyzer::new_for_function(
                        db,
                        fn_id,
                        expr_ptr.with_value(tok.syntax()),
                    );
                    let ast = ast::FieldAccessExpr::cast(tok.clone().syntax().clone())?;
                    let adt_id = analyzer.resolve_field(db.upcast(), &ast)?;
                    let adtloc = db.lookup_intern_adt(adt_id.clone());
                    let full_range = db.module_items(adtloc.file_id)[adtloc.value]
                        .ast_ptr
                        .syntax_node_ptr()
                        .text_range();
                    return Some(GotoDefinitionResult::Targets(vec![NavigationTarget {
                        file_id: adtloc.file_id,
                        focus_range: full_range,
                        full_range: full_range,
                    }]));
                }
                _ => {}
            }
        }
    }

    // Resolver is not enough for goto definition, some expressions have to be inferred aswell eg. field access
    if ast::NameRef::can_cast(tok.parent()?.kind()) {
        
        let in_file = InFile {
            file_id: file_id,
            value: &tok.parent()?,
        };
        
        let resolver = match find_def(db.upcast(), in_file) {
            Some(ModuleDefId::FunctionId(id)) => {
                let source_map = db.body_source_map(id);
                let expr = &tok.parent_ancestors().find_map(ast::Expr::cast)?;
                tracing::info!("Resolving pattern {:#?}", expr);
                let expr_ptr = in_file.with_value(expr);
                resolver_for_expr(db.upcast(), id, source_map.expr_for_node(expr_ptr)?)
            }
            _ => resolver_for_toplevel(db.upcast(), file_id),
        };

        // let ResolveResult((name, file_id)) = name_res.get(expr_id)?;

        // let source_map = db.source_map(*file_id);
        let deps = db.dependency_order(file_id);
        let name = SmolStr::from(tok.text());

        let targets = resolver.resolve_name(&name).map(|ptr| {
            let (full_range, focus_range, file_id) = match ptr {
                crate::def::resolver::ResolveResult::LocalBinding(pattern) => {
                    let focus_node = db
                        .body_source_map(resolver.body_owner()?.clone())
                        .node_for_pattern(pattern)?
                        .value
                        .syntax_node_ptr();
                    let root = db.parse(file_id).syntax_node();
                    let full_range = focus_node.to_node(&root).parent().unwrap().text_range();
                    (full_range, focus_node.text_range(), file_id)
                }
                crate::def::resolver::ResolveResult::FunctionId(func_id) => {
                    let func = db.lookup_intern_function(func_id.clone());
                    let full_node = db.module_items(func.file_id)[func.value]
                        .ast_ptr
                        .syntax_node_ptr();
                    let root = db.parse(file_id).syntax_node();
                    let name = ast::Function::cast(full_node.to_node(&root))
                        .unwrap()
                        .name()
                        .unwrap();
                    (
                        full_node.text_range(),
                        name.token().unwrap().text_range(),
                        func.file_id,
                    )
                }
                crate::def::resolver::ResolveResult::VariantId(variant_id) => {
                    let VariantLoc { value, .. } = db.lookup_intern_variant(variant_id.clone());
                    let full_range = db.module_items(value.file_id)[value.value]
                        .ast_ptr
                        .syntax_node_ptr()
                        .text_range();
                    (full_range, full_range, value.file_id)
                }
            };

            // let full_node = name_node.ancestors().find(|n| {
            //     matches!(
            //         n.kind(),
            //         SyntaxKind::LAMBDA | SyntaxKind::ATTR_PATH_VALUE | SyntaxKind::INHERIT
            //     )
            // })?;
            Some(NavigationTarget {
                file_id,
                focus_range,
                full_range,
            })
        })?;

        return Some(GotoDefinitionResult::Targets(vec![targets?]));
    }
    // let ptr: AstPtr<ast::Literal> = tok.parent_ancestors().find_map(|node| {
    //     match_ast! {
    //         match node {
    //             ast::Variable(n) => Some(AstPtr::new(&n.into())),
    //             ast::Name(n) => Some(AstPtr::new(&n.into())),
    //             ast::Literal(n) => Some(AstPtr::new(&n.into())),
    //             _ => None,
    //         }
    //     }
    // })?;
    Some(GotoDefinitionResult::Targets(vec![]))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};
    use tracing_test::traced_test;

    #[track_caller]
    fn check_no(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1, "Missing markers");
        assert_eq!(goto_definition(&db, f[0]), None);
    }

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1, "Missing markers");
        let mut got = match goto_definition(&db, f[0]).expect("No definition") {
            GotoDefinitionResult::Path(path) => format!("file://{}", path.display()),
            GotoDefinitionResult::Targets(targets) => {
                assert!(!targets.is_empty());
                targets
                    .into_iter()
                    .map(|target| {
                        assert!(target.full_range.contains_range(target.focus_range));
                        let src = db.file_content(target.file_id);
                        let mut full = src[target.full_range].to_owned();
                        let relative_focus = target.focus_range - target.full_range.start();
                        full.insert(relative_focus.end().into(), '>');
                        full.insert(relative_focus.start().into(), '<');
                        full
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            }
        };
        // Prettify.
        if got.contains('\n') {
            got += "\n";
        }
        expect.assert_eq(&got);
    }

    #[test]
    fn let_expr() {
        check("fn main(a) { let c = 123 $0c }", expect!["let <c> = 123"]);
    }

    #[test]
    fn case() {
        check("fn wops() { case 1 { a -> $0a} a}", expect!["<a>"]);
        check_no("fn wops() { case 1 { a -> a} $0a}")
    }
    
    #[test]
    fn pattern_variant() {
        check("fn wops() { case Bla(1) { Bla(a) -> $0a} }", expect!["<a>"]);
    }

    #[test]
    fn variant_constructor() {
        check(
            "type Mogie { Mogie(name: Int) } fn wops() { $0Mogie(name: 1)}",
            expect!["<Mogie(name: Int)>"],
        );
    }

    #[test]
    fn field_resolution() {
        check(
            "type Mogie { Mogie(name: Int) } fn wops() { Mogie(name: 1).$0name}",
            expect!["<type Mogie { Mogie(name: Int) }>"],
        );
    }
    
    #[test]
    fn case_pattern_variant_ref() {
        check(
            "type Mogie { Mogie(name: Int) } fn wops() { case Mogie { $0Mogie -> 1 } }",
            expect!["<Mogie(name: Int)>"],
        );
    }

    #[traced_test]
    #[test]
    fn field_access() {
        // check(
        //     "type Mogie { Mogie(name: Int) } fn wops() { let bobo = Mogie(name: 1) $0bobo.name}",
        //     expect!["let <bobo> = Mogie(name: 1)"],
        // );
        check(
            "type Mogie { Mogie(name: Int) } fn wops() { let bobo = Mogie(name: 1) bobo.$0name}",
            expect!["<type Mogie { Mogie(name: Int) }>"],
        );
    }

    #[test]
    fn module_res() {
        check(
            r#"
#-test.gleam
fn main() {
    1
}

#-test2.gleam
import test

fn bla() {
    $0main()
}
"#,
            expect![
                r#"
fn <main>() {
    1
}
"#
            ],
        );
    }
}
