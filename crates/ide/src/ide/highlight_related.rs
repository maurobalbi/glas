use crate::def::{AstPtr, ResolveResult};
use crate::{DefDatabase, FilePos};
use syntax::ast::{self, AstNode};
use syntax::{best_token_at_offset, TextRange, T};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HlRelated {
    pub range: TextRange,
    pub is_definition: bool,
}

pub(crate) fn highlight_related(db: &dyn DefDatabase, fpos: FilePos) -> Option<Vec<HlRelated>> {
  let parse = db.parse(fpos.file_id);
  // let source_map = db.souce_map(fpos.file_id);
  None
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

    // #[test]
    // fn definition() {
    //     check("const b = 1 const a = $0b", expect!["const <b> = 1 const a = <<b>>"]);
    
    // }

}
