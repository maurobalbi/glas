use std::collections::HashMap;

use la_arena::{Arena, ArenaMap};
use syntax::{ast, AstPtr};

use super::{ExprId,Expr};

#[derive(Debug, Eq, PartialEq)]
pub struct Body {
  pub exprs: Arena<Expr>,
  pub body_exr: ExprId,
}

pub type ExprPtr = AstPtr<ast::Expression>;

#[derive(Default, Debug, Eq, PartialEq)]
pub struct BodySourceMap {
    expr_map: HashMap<ExprPtr, ExprId>,
    expr_map_back: ArenaMap<ExprId, ExprPtr>
}