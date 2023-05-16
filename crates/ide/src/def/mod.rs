mod body;
mod lower;

use std::{
    hash::Hash,
    sync::Arc,
};

use crate::{base::SourceDatabase, impl_from};
use crate::{Diagnostic, FileId};
use la_arena::{Arena, ArenaMap, Idx};
use smol_str::SmolStr;
use std::collections::{HashMap};

use syntax::{ast, AstPtr, Parse };

pub use syntax::ast::{AstNode, BinaryOpKind as BinaryOp, Expression, UnaryOpKind as UnaryOp};

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    fn parse(&self, file_id: FileId) -> Parse;

    fn module_with_source_map(&self, file_id: FileId) -> (Arc<Module>, Arc<ModuleSourceMap>);

    fn module(&self, file_id: FileId) -> Arc<Module>;

    fn source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap>;

    // fn body_with_source_map(&self, def: FunctionId) -> (Arc<Body>, Arc<BodySourceMap>);
}

fn parse(db: &dyn DefDatabase, file_id: FileId) -> Parse {
    let content = db.file_content(file_id);
    syntax::parse_file(&content)
}

fn module_with_source_map(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> (Arc<Module>, Arc<ModuleSourceMap>) {
    let parse = db.parse(file_id);
    let (mut module, mut source_map) = lower::lower(db, file_id, parse);
    module.shrink_to_fit();
    source_map.shrink_to_fit();
    (Arc::new(module), Arc::new(source_map))
}

fn module(db: &dyn DefDatabase, file_id: FileId) -> Arc<Module> {
    db.module_with_source_map(file_id).0
}

fn source_map(db: &dyn DefDatabase, file_id: FileId) -> Arc<ModuleSourceMap> {
    db.module_with_source_map(file_id).1
}

// fn body_with_source_map(
//     db: &dyn DefDatabase,
//     function_id: FunctionId,
// ) -> (Arc<Module>, Arc<ModuleSourceMap>) {
//     let (mut module, mut source_map) = body::lower(db, function_id, parse);
//     module.shrink_to_fit();
//     source_map.shrink_to_fit();
//     (Arc::new(module), Arc::new(source_map))
// }

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Module {
    names: Arena<Name>,
    functions: Arena<Function>,
}

impl Module {
    pub fn shrink_to_fit(&mut self) {
        self.names.shrink_to_fit();
    }

    pub fn names(&self) -> impl Iterator<Item = (NameId, &'_ Name)> + ExactSizeIterator + '_ {
        self.names.iter()
    }

    pub fn functions(
        &self,
    ) -> impl Iterator<Item = (FunctionId, &'_ Function)> + ExactSizeIterator + '_ {
        self.functions.iter()
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleSourceMap {
    name_map: HashMap<AstPtr<ast::Name>, NameId>,
    name_map_rev: ArenaMap<NameId, AstPtr<ast::Name>>,

    function_map: HashMap<AstPtr<ast::Function>, FunctionId>,
    function_map_rev: HashMap<FunctionId, AstPtr<ast::Function>>,

    // This contains locations, thus is quite volatile.
    diagnostics: Vec<Diagnostic>,
}

impl ModuleSourceMap {
    pub fn shrink_to_fit(&mut self) {
        self.name_map.shrink_to_fit();
        self.name_map_rev.shrink_to_fit();
        self.function_map.shrink_to_fit();
        self.function_map_rev.shrink_to_fit();
        self.diagnostics.shrink_to_fit();
    }

    pub fn name_for_node(&self, node: AstPtr<ast::Name>) -> Option<NameId> {
        self.name_map.get(&node).copied()
    }

    pub fn node_for_name(&self, name_id: NameId) -> Option<AstPtr<ast::Name>> {
        self.name_map_rev.get(name_id).cloned()
    }

    pub fn node_for_statement(&self, stmnt_id: FunctionId) -> Option<AstPtr<ast::Function>> {
        self.function_map_rev.get(&stmnt_id).cloned()
    }

    pub fn statement_for_node(&self, node: AstPtr<ast::Function>) -> Option<FunctionId> {
        self.function_map.get(&node).copied()
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

pub type ModuleStatementId = Idx<ModuleStatement>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleStatement {
    Function(Function),
}

impl_from!(Function for ModuleStatement);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub text: SmolStr,
    pub kind: NameKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameKind {
    LetDef,
    Param,
    Function,
}

pub type NameId = Idx<Name>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolveResult(NameId);

pub type FunctionId = Idx<Function>;
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub name: NameId,
    pub params: Vec<Param>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Param {
    Normal(NameId)
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64),
    Float(SmolStr),
    String(SmolStr),
}
