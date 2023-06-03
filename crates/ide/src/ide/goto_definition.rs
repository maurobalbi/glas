use super::NavigationTarget;
use crate::{DefDatabase, FilePos};
use syntax::ast::{ self, AstNode};
use syntax::{AstPtr, best_token_at_offset};

pub(crate) fn goto_definition(
    db: &dyn DefDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<Vec<NavigationTarget>> {
    let parse = db.parse(file_id).syntax_node();
    let tok = best_token_at_offset(&parse, pos)?;
    let source_map = db.source_map(file_id);

    //If tok.parent is field access or tuple access, it will be necessary to infer type first
    if matches!(
        tok.parent()?.kind(), syntax::SyntaxKind::FIELD_ACCESS | syntax:: SyntaxKind::TUPLE_INDEX
    ) {
        return None
    }


    // Refactor to build make module map / data with exprs / other definitions. Its way to much work to use defbodies for no reason!
 

    if ast::NameRef::can_cast(tok.parent()?.kind()) {
        let expr_ptr = ast::Expr::cast(tok.parent()?)?;
        let ptr = AstPtr::new(&expr_ptr);
        let expr_id = source_map.expr_for_node(ptr)?;

        let name_res = db.name_resolution(file_id);
        
    }
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

