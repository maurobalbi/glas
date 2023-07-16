use super::NavigationTarget;
use crate::def::hir_def::ModuleDefId;
use crate::def::{resolver_for_expr};
use crate::{DefDatabase, FilePos, VfsPath};
use smol_str::SmolStr;
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
    // let module_data = db.module_items(file_id);
    // let source_map = db.source_map(file_id);

    tracing::info!(
        "Module name: {:?}",
        db.module_map().module_name_for_file(file_id)
    );
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

        let expr_ptr = crate::InFile { file_id: file_id, value: &expr_ptr };

        let item_scope = db.module_scope(file_id);

        let (expr_id, fn_id) = item_scope.values().find_map(|v| {
            let fn_id = match v.clone() {
                ModuleDefId::FunctionId(fn_id) => fn_id
            };
            let source_map = db.source_map(fn_id.clone());
            return source_map.expr_for_node(expr_ptr).map(|v| (v, fn_id))
        })?;

        let resolver =  resolver_for_expr(db, fn_id.clone() , expr_id);
        tracing::info!("Name_res: {:#?}", resolver);
        // let ResolveResult((name, file_id)) = name_res.get(expr_id)?;

        // let source_map = db.source_map(*file_id);
        let deps = db.dependency_order(file_id);
        tracing::info!("WERE DOING SOMETHING {:?}", deps);
        let name = SmolStr::from(tok.text());

        let targets = resolver.resolve_name(&name).map(|ptr| {
            let (node, file_id) = match ptr {
                crate::def::resolver::ResolveResult::LocalBinding(pattern) => (db.source_map(fn_id.clone()).pattern_map_rev.get(pattern)?.value.syntax_node_ptr(), file_id),
                crate::def::resolver::ResolveResult::FunctionId(func_id) => {
                    let func =  db.lookup_intern_function(func_id.clone());
                     (db.module_items(func.file_id)[func.value].ast_ptr.syntax_node_ptr(), func.file_id)
                },
            };
            
            // let full_node = name_node.ancestors().find(|n| {
            //     matches!(
            //         n.kind(),
            //         SyntaxKind::LAMBDA | SyntaxKind::ATTR_PATH_VALUE | SyntaxKind::INHERIT
            //     )
            // })?;
            Some(NavigationTarget {
                file_id,
                focus_range: node.text_range(),
                full_range: node.text_range(),
            })
        })?;

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
