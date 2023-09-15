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

#[track_caller]
fn check_fix(src: &str, expect: Expect) {
    let (db, f) = TestDB::from_fixture(src).unwrap();
    tracing::info!("{:#?}", f);
    let scope = db.module_scope(f[0].file_id);
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
        expect![
            r#"
            bla: fn(a, b) -> Bla2"#
        ],
    )
}

#[test]
fn fn_params() {
    check_all(
        "fn main(a,b) {a + b} fn bla(a, b) { b }",
        expect![
            r#"
            main: fn(Int, Int) -> Int
            bla: fn(a, b) -> b"#
        ],
    )
}

#[test]
fn fn_params_annotation() {
    check_all(
        "fn bla(a: blabla, b: bubu) { b }",
        expect![
            r#"
        bla: fn(a, b) -> b"#
        ],
    )
}
#[test]
fn let_infer_pattern() {
    check_all(
        "type Biboop {Biboop(Int)} fn biboob(a) {let Biboop(b) = a b}",
        expect!["biboob: fn(Biboop) -> Int"],
    )
}

#[test]
fn unsaturated_constructor() {
    check_all(
        "type Snow { Snow(Int) } fn snow() {Snow}",
        expect!["snow: fn() -> fn(Int) -> Snow"],
    )
}

#[test]
fn let_infer() {
    check_all(
        "type Biboop {Biboop(Int)} fn biboob(a) { let b = a b }",
        expect!["biboob: fn(a) -> a"],
    )
}

#[test]
fn generic_params() {
    check_all(
        "fn biboob(a: a, b: a) { a + 1 }",
        expect!["biboob: fn(Int, Int) -> Int"],
    )
}

#[test]
fn generic_adt() {
    check_all(
        r#"type Animal(a, b) {
        Dog(a, String)
        Cat(b)
      }
      fn animais(b) {
        case b {
          Cat(a) -> Cat("kitty")
          Dog(1, "goofy") -> Dog(a, "goofy")
        }
      }"#,
        expect!["animais: fn(Animal(Int, a)) -> Animal(b, String)"],
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
        "fn wobble(b: generic, f a: Int) { a } fn main() { abc(1) }",
        expect![[r#"
            wobble: fn(a, Int) -> Int
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
fn as_pattern() {
    check_all(
        "type Massa { Much } fn bla() { case Much {
            Much as a -> a
        }  }",
        expect![[r#"
        bla: fn() -> Massa"#]],
    );
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
    check_all(
        "fn do_reverse_acc(remaining) {
        case remaining {
          1 -> 1.1
          2 -> 1.2
        }
      }",
        expect!["do_reverse_acc: fn(Int) -> Float"],
    )
}

#[test]
fn tuple() {
    check_all(
        "fn tuple(a) {
        case a {
            #(1, a) -> #(a, 5)
        }
      }",
        expect!["tuple: fn(#(Int, a)) -> #(a, Int)"],
    )
}

#[test]
fn case_multiple_subjects() {
    check_all(
        "type Dog(a, a) { Dog(a, a) } fn subjects(a, b) {
        case a, b {
            #(1, more), Dog(2.5, d) -> Dog(more, d)
        }
    }
    ",
        expect!["subjects: fn(#(Int, Float), Dog(Float, Float)) -> Dog(Float, Float)"],
    )
}

#[test]
#[traced_test]
fn pattern_spread() {
    check_all(
        "fn spread() {
        case [1,2] {
            [1, ..name] -> name
        }
      }",
        expect!["spread: fn() -> List(Int)"],
    )
}

#[test]
fn boolean() {
    check_all(
        "fn spread() {
        True
      }",
        expect!["spread: fn() -> Bool"],
    )
}

#[test]
fn let_block_let() {
    check_all(
        "fn spread() {
        let x = {
            let y = 1
        }
      }",
        expect!["spread: fn() -> Int"],
    )
}

#[test]
fn lambda_shorthand() {
    check_all(
        "fn add(a: Float, b: Int) {
        a
      }

      fn test() {
        add(_, 1)
      }

      fn test2() {
        add(1.1, _)
      }
      ",
        expect![
            r#"
        add: fn(Float, Int) -> Float
        test: fn() -> fn(Float) -> Float
        test2: fn() -> fn(Int) -> Float"#
        ],
    )
}

#[test]
fn annotations() {
    check_all(
        "fn ann(a: #(Int), b: fn(Int, Float) -> Int) -> Int { \"123\" } ",
        expect!["ann: fn(#(Int), fn(Int, Float) -> Int) -> Int"],
    )
}

#[test]
fn list_spread() {
    check_all(
        "pub fn prepend(list, this item) {
        [item, ..list]
      }",
        expect!["prepend: fn(List(a), a) -> List(a)"],
    )
}

#[test]
fn field_access() {
    check_fix(
        r#"
#- /test.gleam
fn main() {
    1
}

type Bla {
    Bla
}

#- /test2.gleam
import test

fn test() { "abc" }

fn bla() {
    test.$0Bla
}
"#,
        expect![
            r#"test: fn() -> String
bla: fn() -> Bla"#
        ],
    )
}

#[test]
fn spread_call() {
    check_all(
        "fn unique(list) {
            case list {
              [] -> []
              [x] -> [x, ..unique([])]
            }
          }",
        expect!["unique: fn(List(a)) -> List(a)"],
    )
}

#[test]
fn generic_params_naming() {
    check_all(
        "fn do_filter_map(
            list,
            fun,
            acc,
          ) {
            case list {
              [] -> acc
              [x, ..xs] -> {
                let new_acc = case fun(x) {
                  x -> [x, ..acc]
                  x -> acc
                }
                do_filter_map(xs, fun, new_acc)
              }
            }
          }",
        expect!["do_filter_map: fn(List(a), fn(a) -> b, List(b)) -> List(b)"],
    )
}
