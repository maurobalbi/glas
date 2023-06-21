use super::NavigationTarget;
use crate::def::ResolveResult;
use crate::{DefDatabase, FilePos, VfsPath};
use syntax::ast::{self, AstNode};
use syntax::{best_token_at_offset, AstPtr, TextRange, TextSize};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GotoDefinitionResult {
    Path(VfsPath),
    Targets(Vec<NavigationTarget>),
}

pub(crate) fn goto_definition(
    db: &dyn DefDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<GotoDefinitionResult> {
    let parse = db.parse(file_id).syntax_node();
    let tok = best_token_at_offset(&parse, pos)?;
    let source_map = db.source_map(file_id);

    tracing::info!("Module name: {:?}", db.module_map().module_name_for_file(file_id));
    //If tok.parent is field access or tuple access, it will be necessary to infer type first
    if matches!(
        tok.parent()?.kind(),
        syntax::SyntaxKind::FIELD_ACCESS | syntax::SyntaxKind::TUPLE_INDEX
    ) {
        return None;
    }

    // Refactor to build make module map / data with exprs / other definitions. Its way to much work to use defbodies for no reason!

    if ast::NameRef::can_cast(tok.parent()?.kind()) {
        let expr_ptr = ast::Expr::cast(tok.parent()?)?;
        let ptr = AstPtr::new(&expr_ptr);
        let expr_id = source_map.expr_for_node(ptr)?;
        let name_res = db.name_resolution(file_id);
        let targets = match name_res.get(expr_id)? {
            ResolveResult::Definition(name) => {
                source_map.node_for_name(*name).map(|ptr| {
                    let name_node = ptr.to_node(&parse);
                    // let full_node = name_node.ancestors().find(|n| {
                    //     matches!(
                    //         n.kind(),
                    //         SyntaxKind::LAMBDA | SyntaxKind::ATTR_PATH_VALUE | SyntaxKind::INHERIT
                    //     )
                    // })?;
                    NavigationTarget {
                        file_id,
                        focus_range: name_node.syntax().text_range(),
                        full_range: name_node.syntax().text_range(),
                    }
                })
            }
        };
        return Some(GotoDefinitionResult::Targets(vec![targets?]));
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
    Some(GotoDefinitionResult::Targets(vec![NavigationTarget {
        file_id,
        focus_range: TextRange::new(TextSize::from(0), TextSize::from(5)),
        full_range: TextRange::new(TextSize::from(0), TextSize::from(5)),
    }]))
}
