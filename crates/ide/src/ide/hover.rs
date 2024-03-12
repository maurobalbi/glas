use crate::def::hir::Function;
use crate::ty;

use syntax::ast::{self, AstNode};
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
    let tok = best_token_at_offset(parse.syntax(), pos)?;
    if let Some(it) = tok.parent()?.parent().and_then(ast::FieldAccessExpr::cast) {
        let analyzer = sema.analyze(it.base()?.syntax())?;
        if let Some(resolved) = sema.resolve_field(it.clone()) {
            match resolved {
                ty::FieldResolution::Field(_) => {}
                ty::FieldResolution::ModuleDef(def) => match def {
                    crate::def::hir::ModuleDef::Function(it) => {
                        return render_function(db, tok, it)
                    }
                    crate::def::hir::ModuleDef::ModuleConstant(it) => {
                        return render_constant(db, tok, it)
                    }
                    crate::def::hir::ModuleDef::Variant(it) => return render_variant(db, tok, it),
                    crate::def::hir::ModuleDef::Adt(it) => return render_adt(db, tok, it),
                    crate::def::hir::ModuleDef::TypeAlias(it) => {
                        return render_type_alias(db, tok, it)
                    }
                },
            }
        };
        let ty = analyzer.type_of_expr(&ast::Expr::from(it))?;
        return Some(HoverResult {
            range: tok.text_range(),
            markup: format!("```gleam\n{}\n```", ty.display(db)),
        });
    };

    match semantics::classify_node(&sema, &tok.parent()?)? {
        semantics::Definition::Adt(it) => return render_adt(db, tok, it),
        semantics::Definition::Function(it) => return render_function(db, tok, it),
        semantics::Definition::Variant(it) => return render_variant(db, tok, it),
        semantics::Definition::ModuleConstant(it) => render_constant(db, tok, it),
        semantics::Definition::Field(it) => {
            let ty = it.ty(db.upcast());
            Some(HoverResult {
                range: tok.text_range(),
                markup: format!("```gleam\n{}\n```", ty),
            })
        }
        semantics::Definition::Local(it) => {
            let ty = it.ty(db);
            Some(HoverResult {
                range: tok.text_range(),
                markup: format!("```gleam\n{}\n```", ty.display(db)),
            })
        }
        semantics::Definition::Module(it) => {
            let docs = it.docs(db.upcast());
            Some(HoverResult {
                range: tok.text_range(),
                markup: format!(
                    "```gleam\nimport {}\n```\n___\n{docs}",
                    it.name(db.upcast())
                ),
            })
        }
        semantics::Definition::BuiltIn(it) => Some(HoverResult {
            range: tok.text_range(),
            markup: format!("```gleam\n{:?}\n```", it),
        }),
        semantics::Definition::TypeAlias(it) => return render_type_alias(db, tok, it),
    }
}

fn render_adt(
    db: &dyn TyDatabase,
    tok: syntax::rowan::SyntaxToken<syntax::GleamLanguage>,
    it: crate::def::hir::Adt,
) -> Option<HoverResult> {
    let docs = it.docs(db.upcast());
    Some(HoverResult {
        range: tok.text_range(),
        markup: format!("```gleam\ntype {}\n```\n___\n{docs}", it.name(db.upcast())),
    })
}

fn render_constant(
    db: &dyn TyDatabase,
    tok: syntax::rowan::SyntaxToken<syntax::GleamLanguage>,
    it: crate::def::hir::ModuleConstant,
) -> Option<HoverResult> {
    let docs = it.docs(db.upcast());
    Some(HoverResult {
        range: tok.text_range(),
        markup: format!("```gleam\nconst {}\n```\n___\n{docs}", it.name(db.upcast())),
    })
}

fn render_type_alias(
    db: &dyn TyDatabase,
    tok: syntax::rowan::SyntaxToken<syntax::GleamLanguage>,
    it: crate::def::hir::TypeAlias,
) -> Option<HoverResult> {
    Some(HoverResult {
        range: tok.text_range(),
        markup: format!("```gleam\ntype {}\n```", it.name(db.upcast())),
    })
}

fn render_variant(
    db: &dyn TyDatabase,
    tok: syntax::rowan::SyntaxToken<syntax::GleamLanguage>,
    it: crate::def::hir::Variant,
) -> Option<HoverResult> {
    let docs = it.docs(db.upcast());
    Some(HoverResult {
        range: tok.text_range(),
        markup: format!("```gleam\n{}\n```\n___\n{docs}", it.name(db.upcast())),
    })
}

fn render_function(
    db: &dyn TyDatabase,
    tok: syntax::SyntaxToken,
    it: Function,
) -> Option<HoverResult> {
    let name = it.name(db.upcast());
    let docs = it.docs(db.upcast());
    match it.ty(db) {
        ty::Ty::Function { params, return_ } => {
            let params = params
                .iter()
                .map(|(_, ty)| format!("{}", ty.display(db)))
                .collect::<Vec<_>>()
                .join(", ");
            Some(HoverResult {
                range: tok.text_range(),
                markup: format!(
                    "```gleam\nfn {}({}) -> {}\n```\n___\n{docs}",
                    name,
                    params,
                    return_.display(db)
                ),
            })
        }
        _ => None,
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

    #[allow(dead_code)]
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
                fn main() -> a
                ```
                ___
            "#]],
        );
    }

    #[test]
    fn generic_field() {
        check(
            "type Bla(a) { Bla( name: a) } fn main(b: Bla(String)) { b.$0name }",
            "name",
            expect![[r#"
                ```gleam
                String
                ```
            "#]],
        );
    }

    #[test]
    fn module_constant() {
        check(
            "///Very large constant\nconst big = 1\nfn main() { $0big }",
            "big",
            expect![[r#"
            ```gleam
            const big
            ```
            ___
            Very large constant
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
    fn adt_fields() {
        check(
            r#"type Bobo {
                Bobo(age: Int, na$0me: String)
            }"#,
            "name",
            expect![[r#"
                ```gleam
                String
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
                import test
                ```
                ___
            "#]],
        );
    }
}
