mod lower; 
mod body;

use std::{sync::Arc, fs::File, marker::PhantomData, hash::{Hash, Hasher}};

use crate::base::SourceDatabase;
use crate::{Diagnostic, FileId};
use la_arena::{Arena, ArenaMap, Idx};
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};

use syntax::{Parse, SyntaxNode, TextRange, AstPtr, ast};

pub use syntax::ast::{Expression, AstNode, BinaryOpKind as BinaryOp, UnaryOpKind as UnaryOp};

use self::body::{Body, BodySourceMap};

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    fn parse(&self, file_id: FileId) -> Parse;

    fn module_with_source_map(&self, file_id: FileId) -> (Arc<Module>, Arc<ModuleSourceMap>);

    fn module(&self, file_id: FileId) -> Arc<Module>;

    fn source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap>;

    fn body_with_source_map(&self, def: FunctionId) -> (Arc<Body>, Arc<BodySourceMap>);
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

fn body_with_source_map(
    db: &dyn DefDatabase,
    function_id: FunctionId,
) -> (Arc<Module>, Arc<ModuleSourceMap>) {
    let (mut module, mut source_map) = body::lower(db, function_id, parse);
    module.shrink_to_fit();
    source_map.shrink_to_fit();
    (Arc::new(module), Arc::new(source_map))
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    names: Arena<Name>,
    statements: Arena<ModuleStatement>,
}

impl Module {
    pub fn shrink_to_fit(&mut self) {
        self.names.shrink_to_fit();
    }
    
    pub fn names(&self) -> impl Iterator<Item = (NameId, &'_ Name)> + ExactSizeIterator + '_ {
        self.names.iter()
    }
  }


#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleSourceMap {
    name_map: HashMap<AstPtr<ast::Name>, NameId>,
    name_map_rev: ArenaMap<NameId, Vec<AstPtr<ast::Name>>>,


    // This contains locations, thus is quite volatile.
    diagnostics: Vec<Diagnostic>,
}

impl ModuleSourceMap {
    pub fn shrink_to_fit(&mut self) {
        self.name_map.shrink_to_fit();
        self.name_map_rev.shrink_to_fit();
        self.diagnostics.shrink_to_fit();
    }

    pub fn name_for_node(&self, node: AstPtr<ast::Name>) -> Option<NameId> {
        self.name_map.get(&node).copied()
    }

    pub fn nodes_for_name(&self, name_id: NameId) -> impl Iterator<Item = AstPtr<ast::Name>> + '_ {
        self.name_map_rev
            .get(name_id)
            .into_iter()
            .flatten()
            .cloned()
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleStatement {
  Function(Function),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub text: SmolStr,
    pub kind: NameKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameKind {
    LetDef,
    Param,
    PatField,
    TypeCon,
    Module,
}

pub type NameId = Idx<Name>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolveResult(NameId);

pub type FunctionId = Idx<Function>;
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub name: Name,
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