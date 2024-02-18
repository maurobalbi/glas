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
    FileId, FilePos, TextEdit,
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

    let is_local = def
        .module(sema.db.upcast())
        .ok_or_else(|| "No references found".to_owned())?
        .package(sema.db.upcast())
        .is_local(sema.db.upcast());

    if !is_local {
        return Err("Cannot rename a definition from an external dependency".to_owned());
    }

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
    let (_range, def) =
        match find_def(&sema, fpos).ok_or_else(|| "No references found".to_owned())? {
            Either::Left(val) => val,
            Either::Right(str) => return Err(str),
        };

    let mut lexer = GleamLexer::new(new_name);

    let new_token = lexer.next().ok_or_else(|| "Not a valid identifier")?.kind;
    if !lexer.next().is_none() {
        return Err("Not a valid identifier".to_owned());
    }

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
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
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

    fn check_fail(fixture: &str, new_name: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1, "Missing markers");
        let Err(e) = rename(&db, f[0], new_name) else {
            return expect.assert_eq("Expected no rename results");
        };
        expect.assert_eq(e.trim_start())
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

    #[test]
    fn rename_alias() {
        check(
            r#"
#- test.gleam
type Bobo {
    Bobobo
}

type $0Alias = Bobo

#- test2.gleam
import test

fn main(a: test.Alias) {
    Bobobo
}
"#,
            "Bobele",
            expect![
                r#"--- FileId(0)

type Bobo {
    Bobobo
}

type Bobele = Bobo
--- FileId(1)

import test

fn main(a: test.Bobele) {
    Bobobo
}
"#
            ],
        );
    }

    #[test]
    fn rename_type_wo_alias() {
        check(
            r#"
#- test.gleam
type $0Bobo {
    Bobobo
}

type Alias = Bobo

fn main(a: Alias) {
    Bobobo
}
"#,
            "Bobele",
            expect![
                r#"--- FileId(0)

type Bobele {
    Bobobo
}

type Alias = Bobele

fn main(a: Alias) {
    Bobobo
}
"#
            ],
        );
    }

    #[test]
    fn rename_field() {
        check(
            r#"
#- test.gleam
type Bobele {
    Bobobo(name: String)
    Second(name: String)
    Dudu(name: String)
}

fn name(a: Bobele) {
    a.$0name
}

#- test2.gleam
import test.{type Bobele, Dudu, Bobobo} as t

fn name(a: Bobele) {
    case a {
        Bobobo(name: a) -> a
        t.Second(name: b) -> b
        Dudu(name) -> name
    }
}
"#,
            "new_name",
            expect![
                r#"--- FileId(0)

type Bobele {
    Bobobo(new_name: String)
    Second(new_name: String)
    Dudu(new_name: String)
}

fn name(a: Bobele) {
    a.new_name
}
--- FileId(1)

import test.{type Bobele, Dudu, Bobobo} as t

fn name(a: Bobele) {
    case a {
        Bobobo(new_name: a) -> a
        t.Second(new_name: b) -> b
        Dudu(name) -> name
    }
}
"#
            ],
        );
    }

    #[test]
    fn rename_bound_pat() {
        check(
            r#"
#- test.gleam
type Bobele {
    Bobobo(alpha: Int, name: String)
    Dudu(name: String)
}

fn name(a: Bobele) {
    case a {
        Bobobo(name: a) -> a
        Dudu($0name) -> name
    }
}

"#,
            "new_name",
            expect![
                r#"--- FileId(0)

type Bobele {
    Bobobo(alpha: Int, name: String)
    Dudu(name: String)
}

fn name(a: Bobele) {
    case a {
        Bobobo(name: a) -> a
        Dudu(new_name) -> new_name
    }
}
"#
            ],
        );
    }

    #[test]
    fn rename_variant_field() {
        check(
            r#"
#- test.gleam
type Bobele {
    Bobobo(name: String)
    Second(alpha: Int, $0dodo: String)
}

#- test2.gleam
import test as t

fn name(a: Bobele) {
    case a {
        Bobobo(name: name) -> name
        t.Second(alpha: Int, dodo: name) -> name
    }
}
"#,
            "new_name",
            expect![
                r#"--- FileId(0)

type Bobele {
    Bobobo(name: String)
    Second(alpha: Int, new_name: String)
}
--- FileId(1)

import test as t

fn name(a: Bobele) {
    case a {
        Bobobo(name: name) -> name
        t.Second(alpha: Int, new_name: name) -> name
    }
}
"#
            ],
        );
    }

    #[test]
    fn rename_variant_label() {
        check(
            r#"
type Read {
    Chunk(a: BitArray, $0next: Reader)
    ReadingFinished
}

fn read_body_loop(
reader: Read,
) -> BitArray {
    case reader {
        ReadingFinished -> <<1>>
        Chunk(chunk, next: asb) -> {
            chunk
        }
    }   
}
"#,
            "new_name",
            expect![
                r#"--- FileId(0)

type Read {
    Chunk(a: BitArray, new_name: Reader)
    ReadingFinished
}

fn read_body_loop(
reader: Read,
) -> BitArray {
    case reader {
        ReadingFinished -> <<1>>
        Chunk(chunk, new_name: asb) -> {
            chunk
        }
    }   
}
"#
            ],
        );
    }

    #[test]
    fn rename_variant_constructor_field() {
        check(
            r#"
type Read {
    Chunk(a: BitArray, $0next: Reader)
    ReadingFinished
}

fn read_body_loop(
reader: Read,
) -> Read {
    Chunk(chunk, next: asb) 
}
"#,
            "new_name",
            expect![
                r#"--- FileId(0)

type Read {
    Chunk(a: BitArray, new_name: Reader)
    ReadingFinished
}

fn read_body_loop(
reader: Read,
) -> Read {
    Chunk(chunk, new_name: asb) 
}
"#
            ],
        );
    }

    #[test]
    fn rename_variant_constructor_field_no_label() {
        check(
            r#"
type Read {
    Chunk(BitArray, $0next: Read)
    ReadingFinished
}

fn read_body_loop(
reader: Read,
) -> Read {
    Chunk(chunk, next: Read) 
}
"#,
            "new_name",
            expect![
                r#"--- FileId(0)

type Read {
    Chunk(BitArray, new_name: Read)
    ReadingFinished
}

fn read_body_loop(
reader: Read,
) -> Read {
    Chunk(chunk, new_name: Read) 
}
"#
            ],
        );
    }

    #[test]
    fn check_rename() {
        check_fail(
            r#"
            fn $0function() {}
        "#,
            "new_Name",
            expect!["Expected a lowercase identifier"],
        );
    }

    #[test]
    fn check_rename2() {
        check_fail(
            r#"
            fn $0function() {}
        "#,
            "ne Name",
            expect!["Not a valid identifier"],
        );
    }
}
