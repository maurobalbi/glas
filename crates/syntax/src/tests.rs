use crate::{parse_module, GleamLanguage};
use expect_test::expect_file;
use itertools::Itertools;
use rowan::ast::AstNode;
use std::fmt::Write;
use std::{fs, iter};
use std::path::Path;

#[track_caller]
pub fn parse<N: AstNode<Language = GleamLanguage>>(src: &str) -> N {
    let parse = crate::parse_module(src);
    assert!(parse.errors().is_empty());
    parse.syntax_node().descendants().find_map(N::cast).unwrap()
}

fn run_test(dir: &Path, ok: bool) {
    let mut test_files = dir
        .read_dir()
        .unwrap()
        .filter_map(|entry| {
            let path = entry.unwrap().path();
            if path.extension().map_or(false, |ext| ext == "gleam") {
                Some(path)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    test_files.sort();

    for path in test_files {
        let src = fs::read_to_string(&path).unwrap();

        println!("Parsing {}", path.display());

        let ast = parse_module(&src);
        let mut got = String::new();
        for err in ast.errors() {
            writeln!(got, "{:?}: {:?}", err.range, err.kind).unwrap();
        }
        write!(got, "{:#?}", ast.syntax_node()).unwrap();

        if ok != ast.errors().is_empty() {
            println!("--------\n{}\n--------", got);
            panic!("Unexpected test result for {}", path.display());
        }

        let expect_path = path.with_extension("ast");
        expect_file![expect_path].assert_eq(&got);

        let out = iter::successors(ast.syntax_node().first_token(), |t| t.next_token()).map(|t| t.to_string()).join("");
        assert_eq!(src, out);
    }
}

#[test]
fn parser() {
    let dir = Path::new("test_data").canonicalize().unwrap();
    run_test(&dir.join("ok"), true);
    run_test(&dir.join("err"), false);
}
