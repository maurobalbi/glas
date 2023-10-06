use std::collections::HashSet;

use crate::{
    def::{semantics, SearchScope, Semantics},
    ty::TyDatabase,
     FilePos,
};

use syntax::{ast::AstNode, best_token_at_offset, TextRange};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HlRelated {
    pub range: TextRange,
    pub is_definition: bool,
}

pub(crate) fn highlight_related(db: &dyn TyDatabase, fpos: FilePos) -> Option<Vec<HlRelated>> {
    let sema = Semantics::new(db);
    let parse = sema.parse(fpos.file_id);
    let tok = best_token_at_offset(&parse.syntax(), fpos.pos)?;
    // let source_map = db.souce_map(fpos.file_id);
    let mut res = HashSet::new();

    let def = semantics::classify_node(&sema, &tok.parent()?)?;
    def.clone()
        .usages(&sema)
        .in_scope(&SearchScope::single_file(fpos.file_id))
        .all()
        .references
        .remove(&fpos.file_id)
        .map(|t| {
            t.into_iter().for_each(|range| {
                res.insert(HlRelated {
                    range,
                    is_definition: false,
                });
            })
        });

    def.to_nav(db).map(|nav| {
        if fpos.file_id == nav.file_id {
            res.insert(HlRelated {
                range: nav.focus_range,
                is_definition: true,
            });
        }
    });

    Some(res.into_iter().collect())
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;
    use crate::SourceDatabase;
    use expect_test::{expect, Expect};
    use tracing_test::traced_test;

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1);
        let mut hls = super::highlight_related(&db, f[0]).unwrap_or_default();
        tracing::info!("{:?}", hls);
        hls.sort_by_key(|hl| hl.range.start());
        assert!(!hls.is_empty(), "No highlights");

        let mut src = db.file_content(f[0].file_id).to_string();
        for hl in hls.iter().rev() {
            let (open, close) = if hl.is_definition {
                ("<<", ">>")
            } else {
                ("<", ">")
            };
            src.insert_str(usize::from(hl.range.end()), close);
            src.insert_str(usize::from(hl.range.start()), open);
        }
        expect.assert_eq(&src);
    }

    #[test]
    #[traced_test]
    fn definition() {
        check(
            "fn case_hl() { case 1 { a -> $0a } }",
            expect!["fn case_hl() { case 1 { <<a>> -> <a> } }"],
        );
        check(
            "fn highlight(a) { $0a }",
            expect!["fn highlight(<<a>>) { <a> }"],
        );
    }
}
