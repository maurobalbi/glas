use crate::{DefDatabase, Diagnostic, FileId};

pub(crate) fn diagnostics(db: &dyn DefDatabase, file: FileId) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    // Parsing.
    let parse = db.parse(file);
    diags.extend(parse.errors().iter().map(|&err| Diagnostic::from(err)));

    // let source_map = db.source_map(file);
    // diags.extend(source_map.diagnostics().iter().cloned());

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
        check(
            "bla = bla",
            expect![[r#"
            0..3: SyntaxError(ExpectedStatement)
            4..5: SyntaxError(ExpectedStatement)
            6..9: SyntaxError(ExpectedStatement)
        "#]],
        );
    }

    // #[test]
    // fn duplicated_param() {
    //     check("fn bla(a, a) {}", expect![[r#"
    //         10..11: DuplicatedParam
    //             7..8: Previously defined here
    //     "#]]);
    // }

    #[test]
    fn unused_target() {
        check(
            "if javascript {} const a",
            expect![[r#"
            24..24: SyntaxError(ExpectToken(EQ))
            24..24: SyntaxError(ExpectedConstantExpression)
        "#]],
        );
    }
}
