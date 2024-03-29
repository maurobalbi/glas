//! This is actually so-called "semantic highlighting".
use crate::def::semantics::Definition;
use crate::def::{classify_node, Semantics};
use crate::ty::{Ty, TyDatabase};
use crate::FileId;
use syntax::ast::AstNode;
use syntax::{ast, match_ast, SyntaxToken, TextRange};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HlRange {
    pub range: TextRange,
    pub tag: HlTag,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum HlTag {
    Function,
    Module,
    Constructor,
}

pub(crate) fn highlight(
    db: &dyn TyDatabase,
    file: FileId,
    range: Option<TextRange>,
) -> Vec<HlRange> {
    let sema = Semantics::new(db);
    let parse = sema.parse(file);
    let root_node = parse.syntax();

    let token_tag = |tok: &SyntaxToken| -> Option<HlTag> {
        match_ast! {
            match (tok.parent()?) {
                ast::NameRef(node) => {
                    let def = classify_node(&sema, node.syntax())?;
                    if let Definition::Function(_) = def {
                        return Some(HlTag::Function);
                    };

                    if let Definition::Local(local) = def {
                        let ty = local.ty(db);
                        if let Ty::Function {..} = ty {
                            return Some(HlTag::Function);
                        }
                    };

                    if let Definition::Variant(_) = def {
                        return Some(HlTag::Constructor);
                    }
                },
                ast::Name(node) => {
                    if let Some(_) = ast::Variant::cast(node.syntax().parent()?) {
                        return Some(HlTag::Constructor)
                    }
                },
                _ => return None,
            }
        };
        return None;
    };

    let (first_tok, end_pos) = match range {
        None => (root_node.first_token(), u32::MAX.into()),
        Some(range) => (
            root_node.token_at_offset(range.start()).right_biased(),
            range.end(),
        ),
    };

    std::iter::successors(first_tok, |tok| tok.next_token())
        .take_while(|tok| tok.text_range().start() < end_pos)
        .filter_map(|tok| {
            Some(HlRange {
                range: tok.text_range(),
                tag: token_tag(&tok)?,
            })
        })
        .collect()
}
