use std::collections::HashSet;

use crate::def::{semantics, SearchScope, Semantics};
use crate::ty::TyDatabase;
use crate::{FilePos, FileRange};
use itertools::Itertools;
use syntax::ast::AstNode;
use syntax::best_token_at_offset;

pub(crate) fn references(db: &dyn TyDatabase, fpos: FilePos) -> Option<Vec<FileRange>> {
    let sema = Semantics::new(db);
    let parse = sema.parse(fpos.file_id);
    let tok = best_token_at_offset(&parse.syntax(), fpos.pos)?;
    // let source_map = db.souce_map(fpos.file_id);
    let mut res = HashSet::new();

    let def = semantics::classify_node(&sema, &tok.parent()?)?;
    def.clone()
        .usages(&sema)
        .in_scope(&SearchScope::package_graph(db.upcast()))
        .all()
        .into_iter()
        .for_each(|(file_id, ranges)| {
            ranges.into_iter().for_each(|range| {
                res.insert(FileRange { file_id, range });
            })
        });

    def.to_nav(db).map(|nav| {
        res.insert(FileRange {
            file_id: nav.file_id,
            range: nav.focus_range,
        })
    });

    Some(res.into_iter().unique().collect())
}
