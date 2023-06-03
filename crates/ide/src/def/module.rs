use std::{collections::{HashMap}, ops};

use la_arena::{Arena, Idx, ArenaMap};
use smol_str::SmolStr;
use syntax::{AstPtr, ast::{self, BinaryOpKind}};

use crate::impl_from;

/// The item tree of a source file.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct ModuleData {
    pub functions: Arena<Function>,
    pub params: Arena<Param>,
    pub exprs: Arena<Expr>,
    pub names: Arena<Name>,
}

impl ModuleData {
    pub fn shink_to_fit(&mut self) {
        self.functions.shrink_to_fit();
        self.params.shrink_to_fit();
        self.exprs.shrink_to_fit();
        self.names.shrink_to_fit();
    }
}

impl ops::Index<FunctionId> for ModuleData {
    type Output = Function;
    fn index(&self, index: FunctionId) -> &Self::Output {
        &self.functions[index]
    }
}

impl ops::Index<NameId> for ModuleData {
    type Output = Name;
    fn index(&self, index: NameId) -> &Self::Output {
        &self.names[index]
    }
}

impl ops::Index<ExprId> for ModuleData {
    type Output = Expr;
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index]
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ModuleSourceMap {
    pub expr_map: HashMap<AstPtr<ast::Expr>, ExprId>,
    pub expr_map_rev: ArenaMap<ExprId, AstPtr<ast::Expr>>,

    pub fn_map: HashMap<AstPtr<ast::Function>, FunctionId>,
    pub fn_map_rev: HashMap<FunctionId, AstPtr<ast::Function>>,

    pub name_map: HashMap<AstPtr<ast::Name>, NameId>,
    pub name_map_rev: ArenaMap<NameId, AstPtr<ast::Name>>,
}

impl ModuleSourceMap {
    pub fn shrink_to_fit(&mut self) {
        self.expr_map.shrink_to_fit();
        self.expr_map_rev.shrink_to_fit();

        self.fn_map.shrink_to_fit();
        self.fn_map_rev.shrink_to_fit();
        
        self.name_map.shrink_to_fit();
        self.name_map_rev.shrink_to_fit();
    }

    pub fn expr_for_node(&self, node: AstPtr<ast::Expr>) -> Option<ExprId> {
        self.expr_map.get(&node).copied()
    }

    pub fn node_for_expr(&self, expr_id: ExprId) -> Option<AstPtr<ast::Expr>> {
        self.expr_map_rev.get(expr_id).cloned()
    }

    pub fn name_for_node(&self, node: AstPtr<ast::Name>) -> Option<NameId> {
        self.name_map.get(&node).copied()
    }

    pub fn fn_to_def(&self, ptr: AstPtr<ast::Function>) -> Option<FunctionId> {
        self.fn_map.get(&ptr).cloned()
    }

    pub fn expr_to_def(&self, ptr: AstPtr<ast::Expr>) -> Option<ExprId> {
        self.expr_map.get(&ptr).cloned()
    }
}

#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ModuleStatementId {
    FunctionId(FunctionId)
}

impl_from!(
    FunctionId for ModuleStatementId
);

pub type FunctionId = Idx<Function>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub name: NameId,
    pub params: Vec<Param>,

    pub body: ExprId,
 
    pub ast_ptr: AstPtr<ast::Function>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Param {
    pub name: NameId,
    pub label: Option<Label>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Literal(Literal),
    Binary {
        left: ExprId,
        right: ExprId,
        op: BinaryOpKind,
    },
    NameRef(SmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64),
    Float(SmolStr),
    String(SmolStr),
}

pub type NameId = Idx<Name>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub text: SmolStr,
    pub kind: NameKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameKind {
    Function,
    LetIn,
    Param,
    PatField,
}


#[derive(Debug, Clone, PartialEq, Eq )]
pub struct Label(pub SmolStr);