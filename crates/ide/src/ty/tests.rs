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

#[test]
fn let_in() {
    check_all(
        "type Bla2 { Bla } fn bla(a, b) { Bla }",
        expect![r#"
            bla: fn(a, b) -> Bla2"#],
    )
}

#[test]
fn fn_params() {
    check_all(
        "fn main(a,b) {a + b} fn bla(a, b) { b }",
        expect![r#"
            main: fn(Int, Int) -> Int
            bla: fn(a, b) -> b"#],
    )
}

#[test]
fn let_infer_pattern() {
    check_all(
        "type Biboop {Biboop(Int)} fn biboob(a) {let Biboop(b) = a b}",
        expect!["biboob: fn(Biboop) -> Int"],
    )
}

#[traced_test]
#[test]
fn let_infer() {
    check_all(
        "type Biboop {Biboop(Int)} fn biboob(a) { let b = a b }",
        expect!["biboob: fn(a) -> a"],
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
            wobble: fn(a, b) -> b
            main: fn() -> a"#]],
    )
}

#[test]
fn case_expr() {
    check_all(
        "type Massa { Much } fn bla() { case Much {
            Much -> 1
        }  }",
        expect![[r#"
        bla: fn() -> Int"#]],
    )
}

#[traced_test]
#[test]
fn pattern() {
    check_all(
        "type Massa { Much } fn bla() { case Much {
            a -> a
        }  }",
        expect![[r#"
        bla: fn() -> Massa"#]],
    );

    check_all(
        "type Massa { Much(Int) } fn bla() { case Much(1) {
            Much(a) -> a
        }  }",
        expect![[r#"
        bla: fn() -> Int"#]],
    )
}

#[test]
fn function() {
    check_all(
        "fn func() {fn(a,b) {a + b}",
        expect![["func: fn() -> fn(Int, Int) -> Int"]],
    );
    check_all(
        "fn func() {fn(a,b) {a + b}(1, 1)",
        expect![["func: fn() -> Int"]],
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

#[test]
fn pipe() {
    check_all(
        "fn main(a) { 1 |> a(2) } ",
        expect![[r#"
        main: fn(fn(Int) -> fn(Int) -> a) -> a"#]],
    )
}

#[test]
fn pipe_sugar() {
    check_all(
        "fn main(a) { 1 |> a(2) } ",
        expect![[r#"
        main: fn(fn(Int) -> fn(Int) -> a) -> a"#]],
    );
    check_all(
        "fn main() { 1 |> a(2) } ",
        expect![[r#"
        main: fn() -> a"#]],
    )
}

#[test]
fn use_() {
    check_all(
        "fn main(a) { 
            use param <- a(1)
            param
         } ",
        expect![[r#"
        main: fn(fn(Int, fn(a) -> a) -> b) -> b"#]],
    );

}

#[test]
fn use_pattern() {
    check_all(
        "type BlaT { Bla(Int) } fn main(a) { 
            use Bla(b) <- a
            b
         } ",
        expect![[r#"
        main: fn(fn(fn(BlaT) -> Int) -> a) -> a"#]],
    )
}

#[test]
fn case_fn() {
    check_all(
        "fn main(b) { case b(1) { 1 -> 1 } }",
        expect![[r#"
        main: fn(fn(Int) -> Int) -> Int"#]],
    )
}

#[test]
fn case_infer() {
    check_all("fn do_reverse_acc(remaining) {
        case remaining {
          1 -> 1.1
          2 -> 1.2
        }
      }", expect!["do_reverse_acc: fn(Int) -> Float"])
}

#[test]
fn pattern_spread() {
    check_all("fn spread() {
        case [1,2] {
            [1, ..name] -> name
        }
      }", expect!["spread: fn() -> List(Int)"])
}