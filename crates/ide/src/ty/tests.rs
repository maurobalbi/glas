use std::collections::HashMap;
use std::sync::Arc;

use crate::tests::TestDB;
use crate::{
    DefDatabase, InferenceResult, ModuleData, SourceDatabase, TyDatabase,
};
use expect_test::{expect, Expect};
use tracing_test::traced_test;

use super::Ty;

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

fn all_types(module: &ModuleData, infer: &InferenceResult) -> String {
    module
        .names()
        .map(|(i, name)| format!("{}: {:?}\n", name.text, infer.ty_for_name(i)))
        .collect()
}

#[track_caller]
fn check_all(src: &str, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let module = db.module(file);
    let infer = db.infer(file);
    let got = all_types(&module, &infer);
    expect.assert_eq(&got);
}

#[track_caller]
fn check_all_expect(src: &str, expect_ty: Ty, expect: Expect) {
    let (db, file) = TestDB::single_file(src).unwrap();
    let module = db.module(file);
    let infer = super::infer::infer(&db, file);
    let got = all_types(&module, &infer);
    expect.assert_eq(&got);
}

#[traced_test]
#[test] 
fn let_in() {
    check_all("fn main(a, b) { a + 1 }", expect![["123"]])
}