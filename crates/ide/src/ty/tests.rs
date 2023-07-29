use crate::def::InternDatabase;
use crate::tests::TestDB;
use crate::DefDatabase;
use expect_test::{expect, Expect};
use tracing_test::traced_test;

use super::display::TyDisplay;
use super::TyDatabase;

#[track_caller]
fn check_all(src: &str, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let scope = db.module_scope(file);
    let mut output = Vec::new();
    for fun in scope.declarations() {
        match fun.0 {
            crate::def::hir_def::ModuleDefId::FunctionId(fn_id) => {
                let infer = db.infer_function(fn_id);
                let func = db.lookup_intern_function(fn_id);
                let func = &db.module_items(func.file_id)[func.value];
                output.push(format!("{}: {}", func.name, infer.fn_ty.display(&db)));
            }
            crate::def::hir_def::ModuleDefId::AdtId(_) => {}
            crate::def::hir_def::ModuleDefId::VariantId(_) => {}
        }
    }

    let got = output.join("\n");
    expect.assert_eq(&got);
}

#[traced_test]
#[test]
fn let_in() {
    check_all(
        "type Bla2 { Bla } fn bla(a, b) { Bla }",
        expect![[r#"
            bla: fn(a, b) -> Bla2"#]],
    )
}

#[test]
fn mutual_recursion() {
    check_all(
        "fn ding() { wobsie(1) } fn wobsie(a) { ding() + 1 }",
        expect![[r#"
            ding: fn() -> Int
            wobsie: fn(Int) -> Int"#]],
    );
    check_all(
        "fn wobsie(a) { ding() + 1 } fn ding() { wobsie(1) } ",
        expect![[r#"
            wobsie: fn(Int) -> Int
            ding: fn() -> Int"#]],
    )
}

#[test]
fn generic_let() {
    check_all(
        "fn woosa(a) { let a = a a }",
        expect![[r#"
        woosa: fn(a) -> a"#]],
    )
}

#[test]
fn lablelled_args() {
    check_all(
        "fn wobble(b, f a: Int) { a } fn main() { abc(1) }",
        expect![[r#"
    wobble: fn(a, b) -> Bla2"#]],
    )
}

#[test]
fn case_expr() {
    check_all(
        "type Massa { Much } fn bla() { case Massa {
            Much -> 1
        }  }",
        expect![[r#"
        bla: fn(Int, a, b, c) -> Int"#]],
    )
}

#[test]
fn binary() {
    check_all(
        "fn bla(a, b, c, d) { a + 1 }",
        expect![[r#"
        bla: fn(Int, a, b, c) -> Int"#]],
    )
}

#[test]
fn adt_resolve() {
    check_all(
        "fn bla(a, b, c, d) { a + 1 }",
        expect![[r#"
        bla: fn(Int, a, b, c) -> Int"#]],
    )
}

