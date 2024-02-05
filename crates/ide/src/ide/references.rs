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

    def.to_nav(db).map(|nav| {
        res.insert(FileRange {
            file_id: nav.file_id,
            range: nav.focus_range,
        })
    });

    Some(res.into_iter().unique().collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1, "Missing markers");
        let refs = references(&db, f[0]).expect("No definition");

        let mut actual = String::new();
        for refs in refs {
            actual += "\n\n";

            let src = db.file_content(refs.file_id);
            let full = src[refs.range].to_owned();

            let file = format!("{:?} {:?} ", refs.file_id, refs.range);
            actual += &full;
            actual += &file;
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
fn test2() { test.print }"#,
            expect![
                r#"
fn <print>() {
    1
}
"#
            ],
        );
    }
}
