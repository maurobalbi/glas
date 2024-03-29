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
    let tok = best_token_at_offset(parse.syntax(), fpos.pos)?;
    // let source_map = db.souce_map(fpos.file_id);
    let mut res = HashSet::new();

    let def = semantics::classify_node(&sema, &tok.parent()?)?;
    if let Some(t) = def
        .clone()
        .usages(&sema)
        .in_scope(&SearchScope::single_file(fpos.file_id))
        .all()
        .references
        .remove(&fpos.file_id)
    {
        t.into_iter().for_each(|range| {
            res.insert(HlRelated {
                range,
                is_definition: false,
            });
        })
    }

    Some(res.into_iter().collect())
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;
    use crate::SourceDatabase;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1);
        let mut hls = super::highlight_related(&db, f[0]).unwrap_or_default();
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
    fn definition() {
        check(
            "fn case_hl() { case 1 { a -> $0a } }",
            expect!["fn case_hl() { case 1 { <a> -> <a> } }"],
        );
        check(
            "fn highlight(a) { $0a }",
            expect!["fn highlight(<a>) { <a> }"],
        );
    }

    #[test]
    fn hl_field() {
        check(
            "type Wobblie { Variant1(name: Int) Variant2(name: Int) } fn hl(a: Wobblie) { a.$0name }",
            expect!["type Wobblie { Variant1(<name>: Int) Variant2(<name>: Int) } fn hl(a: Wobblie) { a.<name> }"],
        );
    }
}
