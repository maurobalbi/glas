use super::NavigationTarget;
use crate::def::{Expr, Literal, ResolveResult};
use crate::{DefDatabase, FileId, FilePos, VfsPath};
use syntax::ast::{ self, AstNode};
use syntax::{AstPtr, best_token_at_offset, match_ast, SyntaxKind, SyntaxToken};

pub(crate) fn goto_definition(
    db: &dyn DefDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<Vec<NavigationTarget>> {
    let parse = db.parse(file_id);
    let tok = best_token_at_offset(&parse.syntax_node(), pos)?;

    // let ptr: AstPtr<ast::Literal> = tok.parent_ancestors().find_map(|node| {
    //     match_ast! {
    //         match node {
    //             ast::Variable(n) => Some(AstPtr::new(&n.into())),
    //             ast::Name(n) => Some(AstPtr::new(&n.into())),
    //             ast::Literal(n) => Some(AstPtr::new(&n.into())),
    //             _ => None,
    //         }
    //     }
    // })?;
    None
}