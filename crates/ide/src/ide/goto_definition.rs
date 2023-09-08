use super::NavigationTarget;

use crate::def::hir::BuiltIn;
use crate::def::semantics;
use crate::def::source::HasSource;
use crate::def::Semantics;
use crate::ty::TyDatabase;
use crate::{DefDatabase, FilePos, VfsPath};

use syntax::ast::AstNode;
use syntax::{best_token_at_offset, TextRange};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GotoDefinitionResult {
    Path(VfsPath),
    Targets(Vec<NavigationTarget>),
}

pub(crate) fn goto_definition(
    db: &dyn TyDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<GotoDefinitionResult> {
    let sema = Semantics::new(db);

    let parse = sema.parse(file_id);
    let tok = best_token_at_offset(&parse.syntax(), pos)?;

    match semantics::classify_node(sema, &tok.parent()?)? {
        semantics::Definition::Adt(it) => {
            let src = it.source(db.upcast())?;
            let full_range = src.value.syntax().text_range();
            let focus_range = src
                .value
                .name()
                .map(|n| n.syntax().text_range())
                .unwrap_or_else(|| full_range);
            Some(GotoDefinitionResult::Targets(vec![NavigationTarget {
                file_id: src.file_id,
                focus_range,
                full_range,
            }]))
        }
        semantics::Definition::Function(it) => {
            let src = it.source(db.upcast())?;
            let full_range = src.value.syntax().text_range();
            let focus_range = src
                .value
                .name()
                .map(|n| n.syntax().text_range())
                .unwrap_or_else(|| full_range);
            Some(GotoDefinitionResult::Targets(vec![NavigationTarget {
                file_id: src.file_id,
                focus_range,
                full_range,
            }]))
        }
        semantics::Definition::Variant(it) => {
            let src = it.source(db.upcast())?;
            let full_range = src.value.syntax().text_range();
            Some(GotoDefinitionResult::Targets(vec![NavigationTarget {
                file_id: src.file_id,
                focus_range: full_range,
                full_range,
            }]))
        }
        semantics::Definition::Module(module) => {
            let full_range = TextRange::new(0.into(), 0.into());
            Some(GotoDefinitionResult::Targets(vec![NavigationTarget {
                file_id: module.id,
                focus_range: full_range,
                full_range,
            }]))
        }
        semantics::Definition::Field(_) => todo!(),
        semantics::Definition::Local(it) => {
            let focus_node = it.source(db.upcast());
            let focus_range = focus_node.syntax().text_range();
            let full_range = focus_node
                .syntax()
                .parent()
                .map(|p| p.text_range())
                .unwrap_or(focus_range);
            Some(GotoDefinitionResult::Targets(vec![NavigationTarget {
                file_id,
                focus_range,
                full_range,
            }]))
        }
        semantics::Definition::BuiltIn(_) => None,
    }
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
        tracing::info!("{:#?}", f);
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
            expect!["type <Mogie> { Mogie(name: Int) }"],
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
    fn pattern_spread() {
        check(
            "fn spread() { case [] { [..name] -> $0name }",
            expect!["[<..name>]"],
        );
    }

    #[test]
    fn field_access() {
        check(
            "type Mogie { Mogie(name: Int) } fn wops() { let bobo = Mogie(name: 1) $0bobo.name}",
            expect!["let <bobo> = Mogie(name: 1)"],
        );
        check(
            "type Mogie { Mogie(name: Int) } fn wops() { let bobo = Mogie(name: 1) bobo.$0name}",
            expect!["type <Mogie> { Mogie(name: Int) }"],
        );
    }

    #[test]
    fn module_res() {
        check_no(
            r#"
#- /test.gleam
fn main() {
    1
}

#- /test2.gleam
fn bla() {
    $0main()
}
"#);
    }

    #[test]
    fn module_field_access_res() {
        check(
            r#"
#- /asdf/test.gleam
fn main() {
    1
}

#- /test2.gleam
import asdf/test

fn bla() {
    test.$0main()
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
