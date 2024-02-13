use std::collections::{HashMap, HashSet};

use itertools::Either;
use smol_str::SmolStr;
use syntax::{ast::AstNode, best_token_at_offset, lexer::GleamLexer, TextRange};

use crate::{
    def::{
        semantics::{self, Definition},
        SearchScope, Semantics,
    },
    text_edit::WorkspaceEdit,
    ty::TyDatabase,
    DefDatabase, FileId, FilePos, FileRange, TextEdit,
};

pub type RenameResult<T> = Result<T, String>;

pub(crate) fn prepare_rename(
    db: &dyn TyDatabase,
    fpos: FilePos,
) -> RenameResult<(TextRange, SmolStr)> {
    let sema = Semantics::new(db);

    let (range, def) =
        match find_def(&sema, fpos).ok_or_else(|| "No references found".to_owned())? {
            Either::Left(val) => val,
            Either::Right(str) => return Err(str),
        };

    let name = match def {
        Definition::Module(_) | Definition::BuiltIn(_) => {
            return Err(String::from("No references found"))
        }
        _ => def.name(db.upcast()).ok_or_else(|| "No references found")?,
    };

    Ok((range, name))
}

pub(crate) fn rename(
    db: &dyn TyDatabase,
    fpos: FilePos,
    new_name: &str,
) -> RenameResult<WorkspaceEdit> {
    let sema = Semantics::new(db);
    let (range, def) =
        match find_def(&sema, fpos).ok_or_else(|| "No references found".to_owned())? {
            Either::Left(val) => val,
            Either::Right(str) => return Err(str),
        };

    let new_token = GleamLexer::new(new_name)
        .next()
        .ok_or_else(|| "No valid identifier")?
        .kind;

    match def {
        Definition::Module(_) | Definition::BuiltIn(_) => {
            return Err(String::from("No references found"))
        }
        Definition::Adt(_) | Definition::Variant(_) | Definition::TypeAlias(_)
            if new_token != syntax::SyntaxKind::U_IDENT =>
        {
            return Err(String::from("Expected an uppercase identifier"))
        }
        Definition::Function(_) | Definition::Field(_) | Definition::Local(_)
            if new_token != syntax::SyntaxKind::IDENT =>
        {
            return Err(String::from("Expected a lowercase identifier"))
        }
        _ => {}
    };

    let mut edits: HashMap<FileId, HashSet<TextEdit>> = HashMap::new();

    def.clone()
        .usages(&sema)
        .in_scope(&SearchScope::package_graph(db.upcast()))
        .all()
        .into_iter()
        .for_each(|(file_id, ranges)| {
            ranges.into_iter().for_each(|range| {
                let text_edit = TextEdit {
                    delete: range,
                    insert: SmolStr::new(&new_name),
                };
                edits
                    .entry(file_id)
                    .and_modify(|set| {
                        set.insert(text_edit.clone());
                    })
                    .or_insert_with(|| HashSet::from([text_edit]));
            })
        });

    Ok(WorkspaceEdit {
        content_edits: edits,
    })
    // let (_, name) = find_name(db, fpos).ok_or_else(|| "No references found".to_owned())?;
}

fn find_def(
    sema: &'_ Semantics,
    FilePos { file_id, pos }: FilePos,
) -> Option<Either<(TextRange, Definition), String>> {
    let parse = sema.parse(file_id);
    let tok = best_token_at_offset(parse.syntax(), pos)?;
    // let source_map = db.souce_map(fpos.file_id);

    let def = semantics::classify_node(&sema, &tok.parent()?)?;

    if SmolStr::from(tok.text()) != SmolStr::from(def.name(sema.db.upcast())?) {
        return Some(Either::Right(String::from("Can't rename aliased names")));
    }
    Some(Either::Left((tok.text_range(), def)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestDB;
    use crate::{base::SourceDatabase, FileId};
    use expect_test::{expect, Expect};
    use itertools::Itertools;

    fn check(fixture: &str, new_name: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1, "Missing markers");
        let edits = rename(&db, f[0], new_name).expect("No definition");

        let mut actual = String::new();
        for (id, ranges) in edits
            .content_edits
            .into_iter()
            .sorted_by(|a, b| a.0.cmp(&b.0))
        {
            let iter = ranges
                .into_iter()
                .sorted_by(|a, b| b.delete.start().cmp(&a.delete.start()));
            let mut content = db.file_content(id).to_string();
            for edit in iter {
                let start: usize = edit.delete.start().into();
                let end: usize = edit.delete.end().into();
                content.replace_range(start..end, &edit.insert);
            }

            actual += &format!("--- {:?}", id);
            actual += "\n\n";

            actual += &content;

            actual += "\n";
        }

        expect.assert_eq(actual.trim_start())
    }

    #[test]
    fn rename_function() {
        check(
            r#"
#- test.gleam
fn $0print() {
    1
}

#- test2.gleam
import test.{print as print2}

fn test() { test.print }
fn test2() { test.print2 }
"#,
            "print_again",
            expect![
                r#"--- FileId(0)

fn print_again() {
    1
}
--- FileId(1)

import test.{print_again as print2}

fn test() { test.print_again }
fn test2() { test.print2 }
"#
            ],
        );
    }

    #[test]
    fn rename_type() {
        check(
            r#"
#- test.gleam
type $0Bobo {
    Bobobo
}

#- test2.gleam
import test as t

fn test(a: t.Bobo) { Bobobo }
"#,
            "Bobele",
            expect![
                r#"--- FileId(0)

type Bobele {
    Bobobo
}
--- FileId(1)

import test as t

fn test(a: t.Bobele) { Bobobo }
"#
            ],
        );
    }
}
