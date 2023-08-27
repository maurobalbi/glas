use crate::DefDatabase;

use syntax::ast::AstNode;
use syntax::{best_token_at_offset, TextRange};

use crate::def::{semantics, Semantics};
use crate::ty::display::TyDisplay;
use crate::ty::TyDatabase;
use crate::FilePos;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoverResult {
    pub range: TextRange,
    pub markup: String,
}

pub(crate) fn hover(db: &dyn TyDatabase, FilePos { file_id, pos }: FilePos) -> Option<HoverResult> {
    let sema = Semantics::new(db);

    let parse = sema.parse(file_id);
    let tok = best_token_at_offset(&parse.syntax(), pos)?;

    match semantics::classify_node(sema, &tok.parent()?)? {
        semantics::Definition::Adt(it) => Some(HoverResult {
            range: tok.text_range(),
            markup: format!("```gleam\ntype {}\n```", it.name(db.upcast())),
        }),
        semantics::Definition::Function(it) => {
            let ty = it.ty(db);
            Some(HoverResult {
                range: tok.text_range(),
                markup: format!("```gleam\n{}\n```", ty.display(db)),
            })
        }
        semantics::Definition::Variant(it) => Some(HoverResult {
            range: tok.text_range(),
            markup: format!("```gleam\n{}\n```", it.name(db.upcast())),
        }),
        semantics::Definition::Field(_) => todo!(),
        semantics::Definition::Local(it) => {
            let ty = it.ty(db);
            Some(HoverResult {
                range: tok.text_range(),
                markup: format!("```gleam\n{}\n```", ty.display(db)),
            })
        }
        semantics::Definition::Module(it) => Some(HoverResult {
            range: tok.text_range(),
            markup: format!("```gleam\nimport {}\n```", it.name(db.upcast())),
        }),
    }
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
            "fn main() { $0main() }",
            "main",
            expect![[r#"
                ```gleam
                fn() -> a
                ```
            "#]],
        );
    }

    #[test]
    fn pattern_variant() {
        check(
            "fn main(a, b) { let Dog(a) = Dog(1) $0a }",
            "a",
            expect![[r#"
                ```gleam
                c
                ```
            "#]],
        );

        check(
            "type Animal { Dog(Int) } fn main(a, b) { let Dog(a) = Dog(1) $0a }",
            "a",
            expect![[r#"
                ```gleam
                Int
                ```
            "#]],
        );
    }
    
    #[test]
    fn module() {
        check(
            r#"
            #- test.gleam
            fn bobo() {}

            #- main.gleam
            import test

            fn use_func(a) {
                a(1)
            }
            
            fn test(test) {
                use test <- use_func
                test |>
                $0test.bobo()
            }
            "#,
            "test",
            expect![[r#"
                ```gleam
                fn() -> a
                ```
            "#]],
        );
    }
}
