use crate::{DefDatabase, Diagnostic, FileId};

pub(crate) fn diagnostics(db: &dyn DefDatabase, file: FileId) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    // Parsing.
    let parse = db.parse(file);
    diags.extend(parse.errors().iter().map(|&err| Diagnostic::from(err)));

    diags
}

#[cfg(test)]
mod tests {
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    fn check(fixture: &str, expect: Expect) {
        let (db, file_id) = TestDB::single_file(fixture).unwrap();
        let diags = super::diagnostics(&db, file_id);
        assert!(!diags.is_empty());
        let mut got = diags
            .iter()
            .map(|d| d.debug_display().to_string())
            .collect::<Vec<_>>()
            .join("\n");
        if got.contains('\n') {
            got.push('\n');
        }
        expect.assert_eq(&got);
    }

    #[test]
    fn syntax_error() {
        check("bla = bla", expect!["7..9: SyntaxError(MultipleNoAssoc)"]);
    }
}
