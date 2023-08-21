use std::{cell::RefCell, collections::HashMap};

use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode},
    match_ast, AstPtr, SyntaxNode,
};

use crate::{
    impl_from,
    ty::{FieldResolution, TyDatabase, self},
    DefDatabase, FileId, InFile,
};

use super::{
    hir::{Adt, Function, Local, Module, Variant},
    hir_def::ModuleDefId,
    module::Field,
    resolver::{resolver_for_toplevel, ResolveResult},
    source_analyzer::SourceAnalyzer,
};

pub enum Definition {
    Adt(Adt),
    Function(Function),
    Variant(Variant),
    Field(Field),
    Local(Local),
    Module(Module),
}

impl_from!(
    Adt, Local, Function, Field, Variant, Module
    for Definition
);

impl From<FieldResolution> for Definition {
    fn from(value: FieldResolution) -> Self {
        match value {
            FieldResolution::Field(it) => it.into(),
            FieldResolution::ModuleDef(def) => match def {
                super::hir::ModuleDef::Function(it) => it.into(),
                super::hir::ModuleDef::Variant(it) => it.into(),
                super::hir::ModuleDef::Adt(it) => it.into(),
            },
        }
    }
}

impl From<ResolveResult> for Definition {
    fn from(value: ResolveResult) -> Self {
        match value {
            ResolveResult::Local(it) => it.into(),
            ResolveResult::Function(it) => it.into(),
            ResolveResult::Variant(it) => it.into(),
            ResolveResult::Module(it) => it.into(),
        }
    }
}

pub fn classify_node(sema: Semantics, node: &SyntaxNode) -> Option<Definition> {
    match_ast! {
        match node {
            ast::NameRef(name_ref) => classify_name_ref(sema, &name_ref),
            _ => None,
        }
    }
}

fn classify_name_ref(sema: Semantics, name_ref: &ast::NameRef) -> Option<Definition> {
    let parent = name_ref.syntax().parent()?;

    match_ast! {
        match parent {
            ast::FieldAccessExpr(expr) => return sema.resolve_field(expr).map(Into::into),
            _ => {},
        }
    }

    return sema.resolve_name(name_ref.clone()).map(Into::into);
}

pub struct Semantics<'db> {
    pub db: &'db dyn TyDatabase,
    cache: RefCell<HashMap<SyntaxNode, FileId>>,
}

impl<'db> Semantics<'db> {
    pub fn new(db: &'db dyn TyDatabase) -> Self {
        Semantics {
            db,
            cache: Default::default(),
        }
    }

    pub fn parse(&self, file_id: FileId) -> ast::SourceFile {
        let root = self.db.parse(file_id).root();
        self.cache(root.syntax().clone(), file_id.into());
        root
    }

    pub fn resolve_field(&self, field: ast::FieldAccessExpr) -> Option<FieldResolution> {
        self.analyze(field.syntax())?.resolve_field(&field)
    }

    pub fn resolve_name(&self, name: ast::NameRef) -> Option<ResolveResult> {
        let analyzer = self.analyze(name.syntax())?;
        if let Some(module) = name
            .syntax()
            .parent()
            .and_then(ast::Expr::cast)
            .and_then(|expr| analyzer.resolve_module(&expr))
        {
            return Some(ResolveResult::Module(module.into()));
        }

        self.analyze(name.syntax())?
            .resolver
            .resolve_name(&SmolStr::from(name.text()?))
    }

    pub fn ty_of_expr(&self, expr: &ast::Expr) -> Option<ty::Ty> {
       self.analyze(expr.syntax())?
            .type_of_expr(self.db.upcast(), expr)
    }

    fn analyze(&self, node: &SyntaxNode) -> Option<SourceAnalyzer> {
        let node = self.find_file(node);
        let resolver = match find_def(self.db.upcast(), node) {
            Some(ModuleDefId::FunctionId(id)) => {
                return Some(SourceAnalyzer::new_for_function(self.db, id, node));
            }
            _ => resolver_for_toplevel(self.db.upcast(), node.file_id),
        };

        Some(SourceAnalyzer::new_for_resolver(resolver, node))
    }

    fn cache(&self, root_node: SyntaxNode, file_id: FileId) {
        assert!(root_node.parent().is_none());
        let mut cache = self.cache.borrow_mut();
        let prev = cache.insert(root_node, file_id);
        assert!(prev == None || prev == Some(file_id))
    }

    /// Wraps the node in a [`InFile`] with the file id it belongs to.
    fn find_file<'node>(&self, node: &'node SyntaxNode) -> InFile<&'node SyntaxNode> {
        let root_node = find_root(node);
        let file_id = self.lookup(&root_node).unwrap_or_else(|| {
            panic!(
                "\n\nFailed to lookup {:?} in this Semantics.\n\
                 Make sure to use only query nodes, derived from this instance of Semantics.\n\
                 root node:   {:?}\n\
                 known nodes: {}\n\n",
                node,
                root_node,
                self.cache
                    .borrow()
                    .keys()
                    .map(|it| format!("{:?}", it))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        });
        InFile::new(file_id, node)
    }

    fn lookup(&self, root_node: &SyntaxNode) -> Option<FileId> {
        let cache = self.cache.borrow();
        cache.get(root_node).copied()
    }
}

fn find_root(node: &SyntaxNode) -> SyntaxNode {
    node.ancestors().last().unwrap()
}

// This is very inefficient
// ToDo: build a source_map during lowering...
pub fn find_def(db: &dyn DefDatabase, node: InFile<&SyntaxNode>) -> Option<ModuleDefId> {
    let module = db.module_scope(node.file_id);
    for node in node.ancestors() {
        if let Some(def) = ast::ModuleStatement::cast(node.value) {
            match def {
                ast::ModuleStatement::Function(it) => {
                    for (id, _) in module.declarations() {
                        match id {
                            ModuleDefId::FunctionId(func_id) => {
                                let func = db.lookup_intern_function(*func_id);
                                let data = &db.module_items(node.file_id)[func.value];
                                if data.ast_ptr == AstPtr::new(&it) {
                                    return Some(id.clone());
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
    }
    None
}
