use std::{collections::HashMap, ops};

use la_arena::{Arena, ArenaMap, Idx};
use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use syntax::{
    ast::{self, BinaryOpKind},
    AstPtr,
};

use crate::{impl_from, Diagnostic};

/// The item tree of a source file.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct ModuleData {
    pub functions: Arena<Function>,
    pub imports: Arena<Import>,
    pub params: Arena<Param>,
    pub patterns: Arena<Pattern>,
    pub exprs: Arena<Expr>,
    pub names: Arena<Name>,

    pub expr_to_owner: HashMap<ExprId, NameId>,
    pub name_to_func: HashMap<NameId, FunctionId>,
}

impl ModuleData {
    pub fn shink_to_fit(&mut self) {
        self.functions.shrink_to_fit();
        self.imports.shrink_to_fit();
        self.params.shrink_to_fit();
        self.patterns.shrink_to_fit();
        self.exprs.shrink_to_fit();
        self.names.shrink_to_fit();
        self.expr_to_owner.shrink_to_fit();
        self.name_to_func.shrink_to_fit();
    }

    pub fn functions(
        &self,
    ) -> impl Iterator<Item = (FunctionId, &'_ Function)> + ExactSizeIterator + '_ {
        self.functions.iter()
    }
    pub fn imports(&self) -> impl Iterator<Item = (ImportId, &'_ Import)> + ExactSizeIterator + '_ {
        self.imports.iter()
    }

    pub fn exprs(&self) -> impl Iterator<Item = (ExprId, &'_ Expr)> + ExactSizeIterator + '_ {
        self.exprs.iter()
    }

    pub fn names(&self) -> impl Iterator<Item = (NameId, &'_ Name)> + ExactSizeIterator + '_ {
        self.names.iter()
    }

    pub fn patterns(
        &self,
    ) -> impl Iterator<Item = (PatternId, &'_ Pattern)> + ExactSizeIterator + '_ {
        self.patterns.iter()
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

impl ops::Index<PatternId> for ModuleData {
    type Output = Pattern;
    fn index(&self, index: PatternId) -> &Self::Output {
        &self.patterns[index]
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

    pub pattern_map: HashMap<AstPtr<ast::Pattern>, PatternId>,
    pub pattern_map_rev: ArenaMap<PatternId, AstPtr<ast::Pattern>>,

    pub diagnostics: Vec<Diagnostic>,
}

impl ModuleSourceMap {
    pub fn shrink_to_fit(&mut self) {
        self.expr_map.shrink_to_fit();
        self.expr_map_rev.shrink_to_fit();

        self.fn_map.shrink_to_fit();
        self.fn_map_rev.shrink_to_fit();

        self.name_map.shrink_to_fit();
        self.name_map_rev.shrink_to_fit();

        self.pattern_map.shrink_to_fit();
        self.pattern_map_rev.shrink_to_fit();
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

    pub fn node_for_name(&self, name_id: NameId) -> Option<AstPtr<ast::Name>> {
        self.name_map_rev.get(name_id).cloned()
    }

    pub fn fn_to_def(&self, ptr: AstPtr<ast::Function>) -> Option<FunctionId> {
        self.fn_map.get(&ptr).cloned()
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ModuleStatementId {
    FunctionId(FunctionId),
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

pub type ImportId = Idx<Import>;
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Import {
    pub module: SmolStr, // e.g. import >>one/wobble<<
    pub name: NameId, // Name as it's allocated in module, Should be AsName if available otherwise Name

    pub unqualified_as_name: Option<SmolStr>, // e.g. {* as >>AsName<<}
    pub unqualifed_name: SmolStr,             // e.g. { >>Name<< as Wobble }
}

impl Import {
    pub fn local_name(&self) -> SmolStr {
        self.unqualified_as_name
            .as_ref()
            .unwrap_or(&self.unqualifed_name)
            .clone()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Param {
    pub name: NameId,
    pub label: Option<Label>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let {
        pattern: PatternId, // Should be pattern
        // type_ann: Type
        body: ExprId,
    },
    Use {
        patterns: Vec<PatternId>,
        expr: ExprId,
    },
    Expr {
        expr: ExprId,
    },
}
pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Literal(Literal),
    Block {
        stmts: Vec<Statement>,
    },
    Binary {
        left: ExprId,
        right: ExprId,
        op: Option<BinaryOpKind>,
    },
    Call {
        func: ExprId,
        args: Vec<ExprId>,
    },
    NameRef(SmolStr),
}

pub type PatternId = Idx<Pattern>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    // Missing,
    Variable { name: NameId },
    Record { args: Vec<PatternId> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64),
    Float(OrderedFloat<f64>),
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
    Pattern,
    Param,
    PatField,
    Import,
    Module,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label(pub SmolStr);
