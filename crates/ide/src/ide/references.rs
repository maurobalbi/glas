use std::collections::HashSet;

use crate::def::{semantics, SearchScope, Semantics};
use crate::ty::TyDatabase;
use crate::{FilePos, FileRange};
use itertools::Itertools;
use syntax::ast::AstNode;
use syntax::best_token_at_offset;

pub(crate) fn references(db: &dyn TyDatabase, fpos: FilePos) -> Option<Vec<FileRange>> {
    let sema = Semantics::new(db);
    let parse = sema.parse(fpos.file_id);
    let tok = best_token_at_offset(parse.syntax(), fpos.pos)?;
    // let source_map = db.souce_map(fpos.file_id);
    let mut res = HashSet::new();

    let def = semantics::classify_node(&sema, &tok.parent()?)?;

    def.clone()
        .usages(&sema)
        .in_scope(&SearchScope::package_graph(db.upcast()))
        .all()
        .into_iter()
        .for_each(|(file_id, ranges)| {
            ranges.into_iter().for_each(|range| {
                res.insert(FileRange { file_id, range });
            })
        });

    Some(res.into_iter().unique().collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestDB;
    use crate::{base::SourceDatabase, FileId};
    use expect_test::{expect, Expect};
    use indexmap::IndexMap;
    use syntax::TextRange;

    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1, "Missing markers");
        let refs = references(&db, f[0]).expect("No definition");

        let mut file_set: IndexMap<FileId, Vec<TextRange>> = IndexMap::new();

        for refs in refs.into_iter() {
            file_set
                .entry(refs.file_id)
                .and_modify(|src| {
                    src.push(refs.range);
                })
                .or_insert_with(|| vec![refs.range]);
        };

        let mut actual = String::new();
        for (id, mut ranges) in file_set.into_iter().sorted_by(|a,b| a.0.cmp(&b.0)) {
            ranges.sort_by(|a, b| b.start().cmp(&a.start()));
            let mut content = db.file_content(id).to_string();
            for range in ranges {
                content.insert_str(usize::from(range.end()), ">");
                content.insert_str(usize::from(range.start()), "<");
            }

            actual += &format!("--- {:?}", id);
            actual += "\n\n";

            actual += &content;

            actual += "\n";
        }

        expect.assert_eq(actual.trim_start())
    }

    #[test]
    fn module_field_access() {
        check(
            r#"
#- test.gleam
fn $0print() {
    1
}

#- test2.gleam
import test

fn test() { test.print }
fn test2() { test.print }
"#,
            expect![
                r#"--- FileId(0)

fn <print>() {
    1
}
--- FileId(1)

import test

fn test() { test.<print> }
fn test2() { test.<print> }
"#
            ],
        );
    }

    #[test]
    fn import_ref() {
        check(
            r#"
#- test.gleam
pub fn $0testfn() {}

#- test2.gleam
import test.{testfn}"#,
            expect![
                r#"--- FileId(0)

pub fn <testfn>() {}
--- FileId(1)

import test.{<testfn>}
"#
            ],
        );
    }
}
