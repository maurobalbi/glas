use smol_str::SmolStr;
use syntax::{TextRange, best_token_at_offset, SyntaxKind, T};

use crate::{ty::TyDatabase, FilePos};

/// A single completion variant in the editor pop-up.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionItem {
    /// The label to show in the completion menu.
    pub label: SmolStr,
    /// Range of identifier that is being completed.
    pub source_range: TextRange,
    /// What content replaces the source range when user selects this item.
    pub replace: SmolStr,
    /// What item (struct, function, etc) are we completing.
    pub kind: CompletionItemKind,
    /// Type signature.
    pub signature: Option<String>,
    /// A brief description.
    pub description: Option<String>,
    /// The detailed documentation.
    pub documentation: Option<String>,
}

/// The type of the completion item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CompletionItemKind {
    Keyword,
    Param,
    LetBinding,
    Field,
}

pub(crate) fn completions(
    db: &dyn TyDatabase,
    fpos @ FilePos { file_id, pos }: FilePos,
    trigger_char: Option<char>,
) -> Option<Vec<CompletionItem>> {
    let parse = db.parse(file_id);


    let tok = best_token_at_offset(&parse.syntax_node(), pos)?;
    let source_range = match tok.kind() {
        T!["."] => TextRange::empty(pos),
        SyntaxKind::IDENT => tok.text_range(),
        _ => return None,
    };

    Some(vec![CompletionItem {
        label: "blabal".into(),
        source_range: source_range,
        replace: "blabal($0)".into(),
        kind: CompletionItemKind::Keyword,
        signature: None,
        description: None,
        documentation: None,
    }])
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check_no(fixture: &str, label: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        if let Some(compes) = super::completions(&db, f[0], None) {
            assert_eq!(compes.iter().find(|item| item.label == label), None);
        }
    }

    #[track_caller]
    fn check_trigger(fixture: &str, trigger_char: Option<char>, label: &str, expect: Expect) {
        let (mut db, f) = TestDB::from_fixture(fixture).unwrap();

        let compes = super::completions(&db, f[0], trigger_char).expect("No completion");
        let item = compes
            .iter()
            .find(|item| item.label == label)
            .expect("No expected completion");

        let source_range =
            usize::from(item.source_range.start())..usize::from(item.source_range.end());
        let mut completed = db.file_content(f[0].file_id).to_string();
        completed.replace_range(source_range, &item.replace);
        let got = format!("({:?}) {}", item.kind, completed);
        expect.assert_eq(&got);
    }
    
    #[track_caller]
    fn check(fixture: &str, label: &str, expect: Expect) {
        check_trigger(fixture, None, label, expect);
    }

    #[test]
    fn keyword() {
        check("i$0", "import", expect!["(Keyword) import"]);
    }
}
