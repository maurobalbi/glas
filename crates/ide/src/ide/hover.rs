use std::fmt::Write;
use syntax::ast::{self, AstNode};
use syntax::{best_token_at_offset, match_ast, AstPtr, TextRange, TextSize};

use crate::def::source_analyzer::SourceAnalyzer;
use crate::ty::TyDatabase;
use crate::FilePos;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoverResult {
    pub range: TextRange,
    pub markup: String,
}

pub(crate) fn hover(db: &dyn TyDatabase, FilePos { file_id, pos }: FilePos) -> Option<HoverResult> {
    let parse = db.parse(file_id);
    let tok = best_token_at_offset(&parse.syntax_node(), pos)?;

    Some(HoverResult {
        range: TextRange::new(pos, pos.checked_add(5.into()).unwrap()),
        markup: String::from("This is hover"),
    })
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(fixture: &str, full: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1);
        let ret = super::hover(&db, f[0]).expect("No hover");
        let src = db.file_content(f[0].file_id);
        assert_eq!(full, &src[ret.range]);
        let mut got = ret.markup.trim().to_string();
        if got.contains('\n') {
            got += "\n";
        }
        expect.assert_eq(&got);
    }

    #[track_caller]
    fn check_no(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1);
        assert_eq!(super::hover(&db, f[0]), None);
    }

    #[test]
    fn definition() {
        check(
            "let $0a = 1; in a",
            "a",
            expect![[r#"
                Let binding `a`
                `int`
            "#]],
        );
        check(
            "let a.$0a = 1; in a",
            "a",
            expect![[r#"
                Attrset attribute `a`
                `int`
            "#]],
        );
        check(
            "{ $0a = 1; }",
            "a",
            expect![[r#"
                Attrset attribute `a`
                `int`
            "#]],
        );
        check(
            "rec { $0a = 1; }",
            "a",
            expect![[r#"
                Rec-attrset attribute `a`
                `int`
            "#]],
        );
        check(
            "$0a: a",
            "a",
            expect![[r#"
                Parameter `a`
                `?`
            "#]],
        );
        check(
            "{$0a}: a",
            "a",
            expect![[r#"
                Field parameter `a`
                `?`
            "#]],
        );
    }

    #[test]
    fn reference() {
        check(
            "let a = 1; in $0a",
            "a",
            expect![[r#"
                Let binding `a`
                `int`
            "#]],
        );
        check(
            "let a = 1; in { inherit $0a; }",
            "a",
            expect![[r#"
                Let binding `a`
                `int`
            "#]],
        );
        check(
            "let a = 1; in rec { inherit $0a; }",
            "a",
            expect![[r#"
                Let binding `a`
                `int`
            "#]],
        );
        check(
            "a: $0a",
            "a",
            expect![[r#"
                Parameter `a`
                `?`
            "#]],
        );
        check(
            "{a}: $0a",
            "a",
            expect![[r#"
                Field parameter `a`
                `?`
            "#]],
        );
    }

    #[test]
    fn with() {
        check(
            "with 1; $0a",
            "a",
            expect![[r#"
                `with` attribute `a`
                `?`
                Environments:
                1. `with 1;`
            "#]],
        );
        check(
            "with 1; with 2; $0a",
            "a",
            expect![[r#"
                `with` attribute `a`
                `?`
                Environments:
                1. `with 2;`
                2. `with 1;`
            "#]],
        );
    }

    #[test]
    fn builtin_global() {
        check(
            "$0true",
            "true",
            expect![[r#"
                `builtins.true`
                `bool`

                `builtins.true`
                (No documentation from Nix)
            "#]],
        );
        check(
            "$0map",
            "map",
            expect![[r#"
                `builtins.map`
                `(? → ?) → [?] → [?]`

                `builtins.map f list`
                Apply the function *f* to each element in the list *list*. For
                example,

                ```nix
                map (x: "foo" + x) [ "bar" "bla" "abc" ]
                ```

                evaluates to `[ "foobar" "foobla" "fooabc" ]`.
            "#]],
        );
    }

    #[test]
    fn builtin_with() {
        check(
            "with { }; with builtins; head$0",
            "head",
            expect![[r#"
                `builtins.head`
                `[?] → ?`

                `builtins.head list`
                Return the first element of a list; abort evaluation if the argument
                isn’t a list or is an empty list. You can test whether a list is
                empty by comparing it with `[]`.
            "#]],
        );
    }

    #[test]
    fn builtin_alias() {
        check(
            "let inherit (builtins) head; in head$0",
            "head",
            expect![[r#"
                `builtins.head`
                `[?] → ?`

                `builtins.head list`
                Return the first element of a list; abort evaluation if the argument
                isn’t a list or is an empty list. You can test whether a list is
                empty by comparing it with `[]`.
            "#]],
        );
    }

    #[test]
    fn builtin_attrpath() {
        check(
            "builtins.head$0",
            "builtins.head",
            expect![[r#"
                `builtins.head`
                `[?] → ?`

                `builtins.head list`
                Return the first element of a list; abort evaluation if the argument
                isn’t a list or is an empty list. You can test whether a list is
                empty by comparing it with `[]`.
            "#]],
        );

        check(
            "builtins.true$0.trailing",
            "builtins.true",
            expect![[r#"
                `builtins.true`
                `bool`

                `builtins.true`
                (No documentation from Nix)
            "#]],
        );

        // Invalid builtins.
        check_no("builtins.not_exist$0");
        // But the first part still works.
        check(
            "builtins$0.not_exist",
            "builtins",
            expect![[r#"
                `builtins.builtins`
                `{ abort: string → ?, add: float → float → float, addErrorContext: string → ? → ?, all: (? → bool) → [?] → bool, … }`

                `builtins.builtins`
                (No documentation from Nix)
            "#]],
        );
    }

    #[test]
    fn attrpath() {
        check(
            "let foo.$0bar = 1; in foo.bar",
            "bar",
            expect![[r#"
                Attrset attribute `bar`
                `int`
            "#]],
        );
        check(
            "let foo.bar = 1; in foo.$0bar",
            "bar",
            expect![[r#"
                Field `bar`
                `int`
            "#]],
        );
        check(
            "let foo.bar = 1; in foo?$0bar",
            "bar",
            expect![[r#"
                Field `bar`
                `int`
            "#]],
        );
    }
}
