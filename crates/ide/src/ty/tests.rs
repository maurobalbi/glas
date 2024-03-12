use crate::def::InternDatabase;
use crate::tests::TestDB;
use crate::DefDatabase;
use expect_test::{expect, Expect};
use tracing_test::traced_test;

use super::display::TyDisplay;
use super::TyDatabase;

#[track_caller]
fn check_fn(src: &str, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let scope = db.module_scope(file);
    let mut output = Vec::new();
    for fun in scope.declarations().flatten() {
        match fun.0 {
            crate::def::hir_def::ModuleDefId::FunctionId(fn_id) => {
                let infer = db.infer_function(fn_id);
                let func = db.lookup_intern_function(fn_id);
                let func = &db.module_items(func.file_id)[func.value];
                output.push(format!("{}: {}", func.name, infer.fn_ty.display(&db)));
            }
            crate::def::hir_def::ModuleDefId::AdtId(_) => {}
            crate::def::hir_def::ModuleDefId::ModuleConstant(_) => {}
            crate::def::hir_def::ModuleDefId::VariantId(_) => {}
            crate::def::hir_def::ModuleDefId::TypeAliasId(_) => {}
        }
    }

    let got = output.join("\n");
    expect.assert_eq(&got);
}

#[track_caller]
fn check_all(src: &str, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let scope = db.module_scope(file);
    let mut output = Vec::new();
    for fun in scope.declarations().flatten() {
        match fun.0 {
            crate::def::hir_def::ModuleDefId::FunctionId(fn_id) => {
                let infer = db.infer_function(fn_id);
                let func = db.lookup_intern_function(fn_id);
                let (body, src_map) = db.body_with_source_map(fn_id);
                let root = db.parse(func.file_id).syntax_node();
                let func = &db.module_items(func.file_id)[func.value];
                output.push(format!("{}: {}", func.name, infer.fn_ty.display(&db)));
                for (pat_id, _) in body.patterns() {
                    let source = src_map.node_for_pattern(pat_id);
                    if let Some(src) = source {
                        let text = src.value.syntax_node_ptr().to_node(&root);
                        output.push(format!(
                            "  {}: {}",
                            text,
                            infer.ty_for_pattern(pat_id).display(&db)
                        ))
                    }
                }
            }
            crate::def::hir_def::ModuleDefId::AdtId(_) => {}
            crate::def::hir_def::ModuleDefId::ModuleConstant(_) => {}
            crate::def::hir_def::ModuleDefId::VariantId(_) => {}
            crate::def::hir_def::ModuleDefId::TypeAliasId(_) => {}
        }
    }

    let got = output.join("\n");
    expect.assert_eq(&got);
}

#[track_caller]
fn check_fix(src: &str, expect: Expect) {
    let (db, f) = TestDB::from_fixture(src).unwrap();
    let scope = db.module_scope(f[0].file_id);
    let mut output = Vec::new();
    for fun in scope.declarations().flatten() {
        match fun.0 {
            crate::def::hir_def::ModuleDefId::FunctionId(fn_id) => {
                let infer = db.infer_function(fn_id);
                let func = db.lookup_intern_function(fn_id);
                let func = &db.module_items(func.file_id)[func.value];
                output.push(format!("{}: {}", func.name, infer.fn_ty.display(&db)));
            }
            crate::def::hir_def::ModuleDefId::AdtId(_) => {}
            crate::def::hir_def::ModuleDefId::ModuleConstant(_) => {}
            crate::def::hir_def::ModuleDefId::VariantId(_) => {}
            crate::def::hir_def::ModuleDefId::TypeAliasId(_) => {}
        }
    }

    let got = output.join("\n");
    expect.assert_eq(&got);
}

#[test]
fn let_in() {
    check_fn(
        "type Bla2 { Bla } fn bla(a, b) { Bla }",
        expect![
            r#"
            bla: fn(a, b) -> Bla2"#
        ],
    )
}

#[test]
fn fn_params() {
    check_fn(
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
    check_fn(
        "fn bla(a: blabla, b: bubu) { b }",
        expect![
            r#"
        bla: fn(a, b) -> b"#
        ],
    )
}
#[test]
fn let_infer_pattern() {
    check_fn(
        "type Biboop {Biboop(Int)} fn biboob(a) {let Biboop(b) = a b}",
        expect!["biboob: fn(Biboop) -> Int"],
    )
}

#[test]
fn unsaturated_constructor() {
    check_fn(
        "type Snow { Snow(Int) } fn snow() {Snow}",
        expect!["snow: fn() -> fn(Int) -> Snow"],
    )
}

#[test]
fn unsaturated_constructor_qualified() {
    check_fix(
        r#"
#- snow.gleam
pub type WaterKind { Snow(Int) }

#- ice.gleam
import snow

fn melt() {
    $0snow.Snow
}
        "#,
        expect![
            r#"
        melt: fn() -> fn(Int) -> WaterKind"#
        ],
    )
}

#[test]
fn let_infer() {
    check_fn(
        "type Biboop {Biboop(Int)} fn biboob(a) { let b = a b }",
        expect!["biboob: fn(a) -> a"],
    )
}

#[test]
fn generic_params() {
    check_fn(
        "fn biboob(a: a, b: a) { a + 1 }",
        expect!["biboob: fn(Int, Int) -> Int"],
    )
}

#[test]
fn generic_adt() {
    check_fn(
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
    check_fn(
        "fn ding() { wobsie(1) } fn wobsie(a) { ding() + 1 }",
        expect![[r#"
            ding: fn() -> Int
            wobsie: fn(Int) -> Int"#]],
    );
    check_fn(
        "fn wobsie(a) { ding() + 1 } fn ding() { wobsie(1) } ",
        expect![[r#"
            wobsie: fn(Int) -> Int
            ding: fn() -> Int"#]],
    )
}

#[test]
fn generic_let() {
    check_fn(
        "fn woosa(a) { let a = a a }",
        expect![[r#"
        woosa: fn(a) -> a"#]],
    )
}

#[test]
fn lablelled_args() {
    check_fn(
        "fn wobble(b: generic, f a: Int) { a } fn main() { abc(1) }",
        expect![[r#"
            wobble: fn(a, Int) -> Int
            main: fn() -> a"#]],
    )
}

#[test]
fn case_expr() {
    check_fn(
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
    check_fn(
        "type Massa { Much } fn bla() { case Much {
            a -> a
        }  }",
        expect![[r#"
        bla: fn() -> Massa"#]],
    );

    check_fn(
        "type Massa { Much(Int) } fn bla() { case Much(1) {
            Much(a) -> a
        }  }",
        expect![[r#"
        bla: fn() -> Int"#]],
    )
}

#[test]
fn as_pattern() {
    check_fn(
        "type Massa { Much } fn bla() { case Much {
            Much as a -> a
        }  }",
        expect![[r#"
        bla: fn() -> Massa"#]],
    );
}

#[test]
fn function() {
    check_fn(
        "fn func() {fn(a,b) {a + b}",
        expect![["func: fn() -> fn(Int, Int) -> Int"]],
    );
    check_fn(
        "fn func() {fn(a,b) {a + b}(1, 1)",
        expect![["func: fn() -> Int"]],
    )
}

#[test]
fn binary() {
    check_fn(
        "fn bla(a, b, c, d) { a + 1 }",
        expect![[r#"
        bla: fn(Int, a, b, c) -> Int"#]],
    )
}

#[test]
fn adt_resolve() {
    check_fn(
        "fn bla(a, b, c, d) { a + 1 }",
        expect![[r#"
        bla: fn(Int, a, b, c) -> Int"#]],
    )
}

#[test]
fn pipe() {
    check_fn(
        "fn main(a) { 1 |> a(2) } ",
        expect![[r#"
        main: fn(fn(Int) -> fn(Int) -> a) -> a"#]],
    )
}

#[test]
fn pipe_sugar() {
    check_fn(
        "fn main(a) { 1 |> a(2) } ",
        expect![[r#"
        main: fn(fn(Int) -> fn(Int) -> a) -> a"#]],
    );
    check_fn(
        "fn main() { 1 |> a(2) } ",
        expect![[r#"
        main: fn() -> a"#]],
    )
}

#[test]
fn use_() {
    check_fn(
        "fn main(a) { 
            use param <- a(1)
            param
         } ",
        expect![[r#"
        main: fn(fn(Int, fn(a) -> a) -> b) -> b"#]],
    );
}

#[test]
fn record_spread() {
    check_fn(
        "type Alias {
        Bla(name, dodo)
      }
      fn main(a) {
        Bla(..a)
      }",
        expect!["main: fn(Alias) -> Alias"],
    )
}

#[test]
fn use_pattern() {
    check_fn(
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
    check_fn(
        "fn main(b) { case b(1) { 1 -> 1 } }",
        expect![[r#"
        main: fn(fn(Int) -> Int) -> Int"#]],
    )
}

#[test]
fn case_infer() {
    check_fn(
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
    check_fn(
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
    check_fn(
        "type Dog(a) { Dog(a, a) } fn subjects(a, b) {
        case a, b {
            #(1, more), Dog(2.5, d) -> Dog(more, d)
        }
    }
    ",
        expect!["subjects: fn(#(Int, Float), Dog(Float)) -> Dog(Float)"],
    )
}

#[test]
fn pattern_spread() {
    check_fn(
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
    check_fn(
        "fn spread() {
        True
      }",
        expect!["spread: fn() -> Bool"],
    )
}

#[test]
fn let_block_let() {
    check_fn(
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
    check_fn(
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
#[traced_test]
fn annotations() {
    check_fn(
        "fn ann(a: #(Int), b: fn(Int, Float) -> Int) -> Int { \"123\" } ",
        expect!["ann: fn(#(Int), fn(Int, Float) -> Int) -> Int"],
    )
}

#[test]
#[traced_test]
fn annot() {
    check_fn(
        "fn ann(a: #(Int), b: fn(Int, Float) -> Int) -> Result(Int,Int) { Ok(\"123\") } ",
        expect!["ann: fn(#(Int), fn(Int, Float) -> Int) -> Result(Int, Int)"],
    );

    check_fn(
        "fn ann(a: #(Int), b: fn(Int, Float) -> Int) -> Int) { \"123\" } ",
        expect!["ann: fn(#(Int), fn(Int, Float) -> Int) -> Int"],
    )
}

#[test]
fn list_spread() {
    check_fn(
        "pub fn prepend(list, this item) {
        [item, ..list]
      }",
        expect!["prepend: fn(List(a), a) -> List(a)"],
    )
}

#[test]
fn field_access() {
    check_fn(
        "type Wobble{ Wobble(name: Int) } fn name() { let w = Wobble(5) w.name }",
        expect!["name: fn() -> Int"],
    )
}

#[test]
fn field_access_module() {
    check_fix(
        r#"
#- test.gleam
fn main() {
    1
}

type Bla {
    Bla
}

#- test2.gleam
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
    check_fn(
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
fn pipe_infer() {
    check_fn(
        "fn pipe(a: Int, b: Float) -> Int { 1 } 
            fn unique(a) {
            a |> pipe(1.1)
          }",
        expect!["pipe: fn(Int, Float) -> Int\nunique: fn(Int) -> Int"],
    )
}

#[test]
fn generic_params_naming() {
    check_fn(
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

#[test]
fn alias_infer() {
    check_fn(
        "type Alias = String 
    fn main() -> Alias {
    }",
        expect!["main: fn() -> String"],
    )
}

#[test]
#[traced_test]
fn aliased_import() {
    check_fix(
        r#"#- test.gleam
pub type Bla = String

#- test2.gleam
import test.{type Bla, main as dodo}

fn test(a: String) -> Bla { $0 }"#,
        expect!["test: fn(String) -> String"],
    )
}

#[test]
fn generic_alias() {
    check_fix(
        r#"#- test.gleam
pub type Wobble(name) {
    Wobble(name)
}

#- test2.gleam
import test.{type Wobble as Bobo, main as dodo}
pub opaque type Nasty {
    Nasty
}
type Alias = Bobo(Nasty)

fn test(a: String) -> Alias { $0 }"#,
        expect!["test: fn(String) -> Wobble(Nasty)"],
    )
}

#[test]
fn generic_field_access() {
    check_fn(
        r#"
            type Bla(a) { Bla (name: a, age: Int) } 
            fn access_name(b: Bla(Int)) { b.name }

            fn access_name_2() { Bla(name: 1).name }
        "#,
        expect![
            r#"
        access_name: fn(Bla(Int)) -> Int
        access_name_2: fn() -> Int"#
        ],
    )
}

#[test]
fn qualified_pattern() {
    check_fix(
        r#"#- test.gleam
pub type Wobble(name) {
    Wobble(name)
}

#- test2.gleam
import test

fn test(a) { case a {
    test.Wobble(_) -> $0"23"
} }"#,
        expect!["test: fn(Wobble(a)) -> String"],
    )
}

#[test]
fn qualified_type() {
    check_fix(
        r#"#- test.gleam
pub type Wobble(name) {
    Wobble(name)
}

#- test2.gleam
import test

fn test(a: test.Wobble(a)) { $0"" }"#,
        expect!["test: fn(Wobble(a)) -> String"],
    )
}

#[test]
fn qualified_type_alias() {
    check_fix(
        r#"#- test.gleam
pub type Wobble = Alias
type Alias = String

#- test2.gleam
import test

fn test(a: test.Wobble) { $0"" }"#,
        expect!["test: fn(String) -> String"],
    )
}

#[test]
fn annotation_infer() {
    check_all(
        r#"
        fn main() -> Result(Int,Int) {
            let b = Ok("123")
            b
        }"#,
        expect!["main: fn() -> Result(Int, Int)\n  b: Result(String, a)"],
    )
}

#[test]
fn labels_infer() {
    check_fn(
        r#"
        fn main(name name: a, age age: b) -> Result(a, b) {
            Ok(b)
        }
        
        fn inst() {
            main(age: 1, name: "Glas")
        }
  
        fn inst1() {
            main(age: 1, name: "Glas", 1)
        }

        fn inst2() {
            main("123", 1)
        }

        fn inst3() {
            main(5, name: "123")
        }

        fn inst4() {
            main(name: "123", age: 1)
        }
        "#,
        expect![
            r#"
        main: fn(a, b) -> Result(a, b)
        inst: fn() -> Result(String, Int)
        inst1: fn() -> Result(String, Int)
        inst2: fn() -> Result(String, Int)
        inst3: fn() -> Result(String, Int)
        inst4: fn() -> Result(String, Int)"#
        ],
    )
}

#[test]
fn labels_infer_pattern() {
    check_fn(
        r#"
        pub type Bla(a) {
            Bla(Int, name: a, age: Int)
          }
          
        pub fn blob(abc) {
            let Bla(1, 1, name: Bla(_, age: age, name: name)) = abc
            name +. 1
            age
        }

        type Bobo(a) {
            Bobo(name: a, age: Int)
        }

        fn inst(bobo: Bobo(String)) {
            let Bobo(name, age) = bobo
            name
        }
        
        fn inst1(bobo: Bobo(String)) {
            let Bobo(name: name, age: age) = bobo
            name
        }

        fn inst2(bobo: Bobo(String)) {
            let Bobo(age: age, name: name) = bobo
            name
        }
        "#,
        expect![
            r#"
        blob: fn(Bla(Bla(Float))) -> Int
        inst: fn(Bobo(String)) -> String
        inst1: fn(Bobo(String)) -> String
        inst2: fn(Bobo(String)) -> String"#
        ],
    )
}

#[test]
fn labels_variant() {
    check_fn(
        r#"
type Read(a) {
    Chunk(BitArray, next: a)
    ReadingFinished
  }
  
  fn read_body_loop(reader: Read(a)) {
    case reader {
      ReadingFinished -> 1
      Chunk(chunk, next: p) -> {
        p + 1
      }
    }
  }
  "#,
        expect!["read_body_loop: fn(Read(Int)) -> Int"],
    )
}

#[test]
fn tuple_index() {
    check_fn(
        r#"
fn index() {
    let b = #(1, "abc")
    b.1
}
  "#,
        expect!["index: fn() -> String"],
    )
}

#[test]
fn bit_array() {
    check_fn(
        r#"
fn index() {
    <<1>>
}
  "#,
        expect!["index: fn() -> BitArray"],
    )
}
