


use std::fmt::Write;

use crate::def::{ModuleItemData, InternDatabase};
use crate::tests::TestDB;
use crate::{
    DefDatabase,
};
use expect_test::{expect, Expect};
use tracing_test::traced_test;

use super::display::TyDisplay;
use super::{Ty, InferenceResult, TyDatabase};

// #[track_caller]
// fn check(src: &str, expect: Expect) {
//     let (db, file) = TestDB::single_file(src).unwrap();
//     let module = db.module(file);
//     let infer = db.infer(file);
//     let ty = infer.ty_for_expr(module.entry_expr());
//     let got = ty.debug().to_string();
//     expect.assert_eq(&got);
// }

// #[track_caller]
// fn check_name(name: &str, src: &str, expect: Expect) {
//     let (db, file) = TestDB::single_file(src).unwrap();
//     let module = db.module(file);
//     let name = module
//         .names()
//         .find(|(_, n)| n.text == name)
//         .expect("Name not found")
//         .0;
//     let infer = db.infer(file);
//     let ty = infer.ty_for_name(name);
//     let got = ty.debug().to_string();
//     expect.assert_eq(&got);
// }

// fn all_types(module: &ModuleItemData, infer: &InferenceResult) -> String {
//     module
//         .functions()
//         .map(|(i, func)| format!("{}: {:?}\n", func, infer.ty_for_name(i)))
//         .collect()
// }

#[track_caller]
fn check_all(src: &str, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let scope= db.module_scope(file);
    let mut output = Vec::new();
    for fun in scope.declarations() {
        match fun.0 {
            crate::def::hir_def::ModuleDefId::FunctionId(fn_id) => {
                let infer = db.infer_function(fn_id);
                let func = db.lookup_intern_function(fn_id);
                let func = &db.module_items(func.file_id)[func.value];
                output.push(format!("{}: {}", func.name, infer.fn_ty.display(&db)));
            },
            crate::def::hir_def::ModuleDefId::AdtId(_) => {
            },
            crate::def::hir_def::ModuleDefId::VariantId(_) => {},
            
        }
    }

    let got = output.join("\n");
    expect.assert_eq(&got);
            
}

// #[track_caller]
// fn check_all_expect(src: &str, _expect_ty: Ty, expect: Expect) {
//     let (db, file) = TestDB::single_file(src).unwrap();
//     let module = db.module(file);
//     let name = module.functions().nth(0).unwrap().1.name;
//     let infer = super::infer::infer(&db, name, file);
//     let got = all_types(&module, &infer.1);
//     expect.assert_eq(&got);
// }

#[traced_test]
#[test] 
fn let_in() {
    check_all("type Bla2 { Bla } fn bla(a, b) { a(b)}", expect![[r#"
    bla: fn(a, b) -> Bla2"#]])
}

#[traced_test]
#[test] 
fn use_() {
    check_all("fn bla(a, b, c, d) { a + 1 } fn main(a) { main2(bla) } fn main2(b) { b(1.1) }", expect![[r#"
        bla: fn(Int, a, b, c) -> Int
        main: fn(a) -> Int
        main2: fn(fn(Float) -> a) -> a"#]])
}