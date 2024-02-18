use super::NavigationTarget;

use crate::def::semantics;
use crate::def::Semantics;
use crate::ty::TyDatabase;
use crate::{FilePos, VfsPath};

use syntax::ast::AstNode;
use syntax::best_token_at_offset;

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
    let tok = best_token_at_offset(parse.syntax(), pos)?;

    semantics::classify_node(&sema, &tok.parent()?)
        .and_then(|def| def.to_nav(db))
        .map(|navs| GotoDefinitionResult::Targets(vec![navs]))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

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
    // Temporarily broken!
    fn field_resolution() {
        check(
            "type Mogie { Mogie(name: Int) } fn wops() { Mogie(name: 1).$0name}",
            expect!["Mogie(<name: Int>)"],
        );
    }

    #[test]
    fn case_pattern_variant_ref() {
        check(
            "type Mogie { Mogie(name: Int) } fn wops() { case Mogie { $0Mogie -> 1 } }",
            expect!["<Mogie(name: Int)>"],
        );
    }

    #[test]
    fn pattern_spread() {
        check(
            "fn spread() { case [] { [..name] -> $0name }",
            expect!["[<..name>]"],
        );
    }

    // Temporarily broken!
    #[test]
    fn field_access() {
        check(
            "type Mogie { Mogie(name: Int) } fn wops() { let bobo = Mogie(name: 1) $0bobo.name}",
            expect!["let <bobo> = Mogie(name: 1)"],
        );
        check(
            "type Mogie { Mogie(name: Int) } fn wops() { let bobo = Mogie(name: 1) bobo.$0name}",
            expect!["Mogie(<name: Int>)"],
        );
    }

    // #[test]
    // #[traced_test]
    // fn guards_scope() {
    //     check(
    //         r#"
    //     fn guards(a: String) {
    //         case 1 {
    //             b if b == $0a -> 1
    //         }
    //     }
    //     "#,
    //         expect!["<a>: String"],
    //     );

    //     check(
    //         r#"
    //     fn guards(a: String) {
    //         case 1 {
    //             b if $0b == a -> 1
    //         }
    //     }
    //     "#,
    //         expect![""],
    //     );
    // }

    #[test]
    fn module_res() {
        check_no(
            r#"
#- test.gleam
fn main() {
    1
}

#- test2.gleam
fn bla() {
    $0main()
}
"#,
        );
    }

    #[test]
    fn module_field_access_res() {
        check(
            r#"
#- asdf/test.gleam
fn main() {
    1
}

#- test2.gleam
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

    #[test]
    fn qualified_type() {
        check(
            r#"
#- asdf/test.gleam
pub type Wobble {
    Some
}

#- test2.gleam
import asdf/test

fn bla(a: test.$0Wobble) {
}
"#,
            expect![
                r#"
pub type <Wobble> {
    Some
}
"#
            ],
        );
    }

    #[test]
    fn module_field_access() {
        check(
            r#"
#- test.gleam
fn print() {
    1
}

#- test2.gleam
import test

type Internal {
    Internal(Int)
}

fn test(test: Internal) { test.$0print }"#,
            expect![
                r#"
fn <print>() {
    1
}
"#
            ],
        );
        check(
            r#"
#- test.gleam
fn print() {
    1
}

#- test2.gleam
import test

type Internal {
    Type(print: Int)
}

fn test(test: Internal) { test.$0print }"#,
            expect![r#"Type(<print: Int>)"#],
        )
    }

    #[test]
    fn import_qualified_as() {
        check(
            r#"
#- test.gleam
pub type Test

#- test2.gleam
import test.{type Test as Dodo}

type Local = $0Dodo"#,
            expect![
                r#"
pub type <Test>"#
            ],
        )
    }

    #[test]
    fn import_aliased_module() {
        check(
            r#"
#- test.gleam
pub type Test

#- test2.gleam
import test.{type $0Test} as t
"#,
            expect![
                r#"
pub type <Test>"#
            ],
        )
    }

    #[test]
    fn resolve_qualified() {
        check(
            r#"
#- test.gleam
pub type Test {
    Test
}

#- test2.gleam
import test as t

fn main() {
    t.$0Test
}
"#,
            expect![
                r#"
<Test>"#
            ],
        )
    }

    #[test]
    fn resolve_qualified_pattern() {
        check(
            r#"
#- test.gleam
pub type Test {
    Test
}

#- test2.gleam
import test as t

fn main() {
    case a {
        t.$0Test -> 1 
    }
}
"#,
            expect![
                r#"
<Test>"#
            ],
        )
    }

    #[test]
    fn resolve_imported_fn() {
        check(
            r#"
#- test.gleam
pub fn testfn() {}

#- test2.gleam
import test.{$0testfn}
"#,
            expect![
                r#"
                pub fn <testfn>() {}"#
            ],
        )
    }

    #[test]
    fn resolve_pattern_label() {
        check(
            r#"
#- test.gleam
type Sheep {
    Dolly(age: Int)
    Dodo(age: Int)
}

#- test2.gleam
import test.{Dolly, Dodo}

fn get_age(d: Dolly) {
    case a {
        Dolly(age: a) -> a
        Dodo($0age: a) -> a
    }
}
"#,
            expect![
                r#"
                Dolly(<age: Int>)"#
            ],
        )
    }

    #[test]
    fn resovle_constr_field() {
        check(
            r#"
    type Reader =
  fn(Int) -> Result(Read, Nil)

type Read {
  Chunk(a: Int, next: Reader)
  ReadingFinished
}

fn bla(a: read, b: next) {
  Chunk($0next: b, a: a)
}"#,
            expect!["Chunk(a: Int, <next: Reader>)"],
        )
    }
}
