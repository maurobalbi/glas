use rowan::{ast::AstNode};

use crate::{GleamLanguage};

#[track_caller]
pub fn parse<N: AstNode<Language = GleamLanguage>>(src: &str) -> N {
    let parse = crate::parse_file(src);
    assert!(parse.errors().is_empty());
    parse.syntax_node().descendants().find_map(N::cast).unwrap()
}
