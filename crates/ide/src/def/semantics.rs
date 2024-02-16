use std::{cell::RefCell, collections::HashMap};

use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode},
    match_ast, GleamLanguage, SyntaxNode, TextRange,
};

use crate::{
    ide::NavigationTarget,
    impl_from,
    ty::{FieldResolution, TyDatabase},
    DefDatabase, FileId, InFile,
};

use super::{
    hir::{Adt, BuiltIn, Field, Function, Import, Local, Module, TypeAlias, Variant},
    hir_def::ModuleDefId,
    resolver::{resolver_for_toplevel, ResolveResult},
    source::HasSource,
    source_analyzer::SourceAnalyzer,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Definition {
    Adt(Adt),
    Function(Function),
    Variant(Variant),
    Field(Field),
    Local(Local),
    Module(Module),
    BuiltIn(BuiltIn),
    TypeAlias(TypeAlias),
}

impl_from!(
    Adt, Local, Function, Field, Variant, Module, BuiltIn, TypeAlias
    for Definition
);

impl Definition {
    pub fn module(&self, db: &dyn DefDatabase) -> Option<Module> {
        let module = match self {
            Definition::Adt(it) => it.module(db),
            Definition::Function(it) => it.module(db),
            Definition::Variant(it) => Adt::from(it.parent).module(db),
            Definition::Field(it) => Variant::from(it.parent).module(db),
            Definition::Local(it) => Function::from(it.parent).module(db),
            Definition::Module(it) => *it,
            Definition::BuiltIn(_) => return None,
            Definition::TypeAlias(it) => it.module(db),
        };
        Some(module)
    }

    pub fn name(&self, db: &dyn DefDatabase) -> Option<SmolStr> {
        let name = match self {
            Definition::Adt(it) => it.name(db),
            Definition::Function(it) => it.name(db),
            Definition::Variant(it) => it.name(db),
            // ToDo: Fixme
            Definition::Field(it) => return it.label(db),
            Definition::Local(it) => it.name(db),
            Definition::Module(_) => return None,
            // ToDo: Fixme
            Definition::BuiltIn(_) => return None,
            Definition::TypeAlias(it) => it.name(db),
        };
        Some(name)
    }

    pub fn to_nav(&self, db: &dyn TyDatabase) -> Option<NavigationTarget> {
        match self {
            Definition::Adt(it) => {
                let src = it.source(db.upcast())?;
                let full_range = src.value.syntax().text_range();
                let focus_range = src
                    .value
                    .name()
                    .map(|n| n.syntax().text_range())
                    .unwrap_or_else(|| full_range);
                Some(NavigationTarget {
                    file_id: src.file_id,
                    focus_range,
                    full_range,
                })
            }
            Definition::Function(it) => {
                let src = it.source(db.upcast())?;
                let full_range = src.value.syntax().text_range();
                let focus_range = src
                    .value
                    .name()
                    .map(|n| n.syntax().text_range())
                    .unwrap_or_else(|| full_range);
                Some(NavigationTarget {
                    file_id: src.file_id,
                    focus_range,
                    full_range,
                })
            }
            Definition::Variant(it) => {
                let src = it.source(db.upcast())?;
                let full_range = src.value.syntax().text_range();
                Some(NavigationTarget {
                    file_id: src.file_id,
                    focus_range: full_range,
                    full_range,
                })
            }
            Definition::Field(it) => {
                let src = it.source(db.upcast())?;
                let variant: Variant = it.parent.into();
                let full_range = variant.source(db.upcast())?.value.syntax().text_range();
                let focus_range = src.value.syntax().text_range();
                Some(NavigationTarget {
                    file_id: src.file_id,
                    full_range,
                    focus_range,
                })
            }
            Definition::Module(module) => {
                let full_range = TextRange::new(0.into(), 0.into());
                Some(NavigationTarget {
                    file_id: module.id,
                    focus_range: full_range,
                    full_range,
                })
            }
            Definition::Local(it) => {
                let focus_node = it.source(db.upcast());
                let focus_range = focus_node.value.syntax().text_range();
                let full_range = focus_node
                    .value
                    .syntax()
                    .parent()
                    .map(|p| p.text_range())
                    .unwrap_or(focus_range);
                Some(NavigationTarget {
                    file_id: focus_node.file_id,
                    focus_range,
                    full_range,
                })
            }
            Definition::BuiltIn(_) => None,
            Definition::TypeAlias(it) => {
                let src = it.source(db.upcast())?;
                let full_range = src.value.syntax().text_range();
                let focus_range = src
                    .value
                    .name()
                    .map(|n| n.syntax().text_range())
                    .unwrap_or_else(|| full_range);
                Some(NavigationTarget {
                    file_id: src.file_id,
                    focus_range,
                    full_range,
                })
            }
        }
    }
}

impl From<FieldResolution> for Definition {
    fn from(value: FieldResolution) -> Self {
        match value {
            FieldResolution::Field(it) => it.into(),
            FieldResolution::ModuleDef(def) => match def {
                super::hir::ModuleDef::Function(it) => it.into(),
                super::hir::ModuleDef::Variant(it) => it.into(),
                super::hir::ModuleDef::Adt(it) => it.into(),
                super::hir::ModuleDef::TypeAlias(it) => it.into(),
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
            ResolveResult::BuiltIn(it) => it.into(),
            ResolveResult::Adt(it) => it.into(),
            ResolveResult::TypeAlias(it) => it.into(),
        }
    }
}

pub fn classify_node(sema: &Semantics, node: &SyntaxNode) -> Option<Definition> {
    match_ast! {
        match node {
            ast::NameRef(name_ref) => classify_name_ref(sema, &name_ref),
            ast::Name(name) => classify_name(sema, &name),
            ast::TypeName(type_name) => classify_type_name(sema, &type_name),
            ast::Label(label) => classify_label(sema, &label),
            // ast::PatternVariable(type_name) => classify_pattern_variable(sema, &type_name),
            _ => None,
        }
    }
}

fn classify_name(sema: &Semantics, name: &ast::Name) -> Option<Definition> {
    let parent = name.syntax().parent()?;

    match_ast! {
        match parent {
            ast::Function(it) => return sema.to_def(&it).map(From::from),
            ast::PatternVariable(it) => {
                let pattern = ast::Pattern::cast(it.syntax().clone())?;
                let def = sema.to_def(&pattern).map(From::from);

                return def
            },
            ast::Variant(it) => {
               return sema.to_def(&it).map(From::from)
            },
            ast::UnqualifiedImport(it) => {
                let src = sema
                    .find_file(it.syntax())
                    .with_value(&it)
                    .cloned();
                let map = sema.db.module_source_map(src.file_id);
                let import_id = map.node_to_import(&src.value);
                let import: Import = import_id.map(From::from)?;
                return import.definition(sema.db.upcast());
            },
            ast::VariantField(it) => {
                let def = sema.to_def(&it).map(From::from);
                return def;
            },
            _ => {},
        }
    }

    None
}

fn classify_name_ref(sema: &Semantics, name_ref: &ast::NameRef) -> Option<Definition> {
    let parent = name_ref.syntax().parent()?;

    // Handle pattern!

    if let Some(expr) = ast::FieldAccessExpr::cast(parent.clone()) {
        return sema.resolve_field(expr).map(Into::into);
    }

    sema.resolve_nameref(name_ref.clone()).map(Into::into)
}

fn classify_type_name(sema: &Semantics, type_name: &ast::TypeName) -> Option<Definition> {
    let parent = type_name.syntax().parent()?;
    if let Some(it) = ast::UnqualifiedImport::cast(parent.clone()) {
        let src = sema.find_file(it.syntax()).with_value(&it).cloned();
        let map = sema.db.module_source_map(src.file_id);
        let import_id = map.node_to_import(&src.value);
        let import: Import = import_id.map(From::from)?;
        return import.definition(sema.db.upcast());
    };

    ast::TypeNameRef::cast(parent)
        .and_then(|t| {
            let module: SmolStr = t.module()?.text()?;
            let file_id = sema
                .analyze(type_name.syntax())?
                .resolver
                .resolve_module(&module)?;
            let res =
                resolver_for_toplevel(sema.db.upcast(), file_id).resolve_type(&type_name.text()?);
            res?.into()
        })
        .or_else(|| sema.resolve_type(type_name))
        .map(Into::into)
}

fn classify_label(sema: &Semantics, label: &ast::Label) -> Option<Definition> {
    let label_text = SmolStr::from(label.text()?);

    let parent = label.syntax().parent()?;

    let ref_field = ast::VariantRefField::cast(parent.clone())?;
    let field_list = ast::VariantRefFieldList::cast(ref_field.syntax().parent()?)?;
    let variant_ref = ast::VariantRef::cast(field_list.syntax().parent()?)?;
    let variant_name = variant_ref.variant()?;

    let variant = match classify_name_ref(sema, &variant_name)? {
        Definition::Variant(variant) => variant,
        _ => return None,
    };

    let adt = variant.parent();

    // This is a bit of a hack to get renaming to work with common fields and pattern matching!
    if let Some(field) = adt.common_fields(sema.db.upcast()).get(&label_text) {
        return Some(Definition::Field(*field));
    }

    for field in variant.fields(sema.db.upcast()).iter() {
        if field.label(sema.db.upcast())? == label_text {
            return Some(Definition::Field(*field));
        }
    }

    None
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
        self.cache(root.syntax().clone(), file_id);
        root
    }

    pub fn resolve_field(&self, field: ast::FieldAccessExpr) -> Option<FieldResolution> {
        self.analyze(field.syntax())?.resolve_field(&field)
    }

    pub fn resolve_module(&self, expr: ast::Expr) -> Option<FileId> {
        self.analyze(expr.syntax())?.resolve_module(&expr)
    }

    pub fn resolve_nameref(&self, name: ast::NameRef) -> Option<ResolveResult> {
        let analyzer = self.analyze(name.syntax())?;
        if let Some(module) = name
            .syntax()
            .parent()
            .and_then(ast::Expr::cast)
            .and_then(|expr| analyzer.resolve_module(&expr))
        {
            return Some(ResolveResult::Module(module.into()));
        }

        if let Some((module, name)) = name
            .syntax()
            .parent()
            .and_then(ast::VariantRef::cast)
            .and_then(|p| {
                Some((
                    analyzer
                        .resolver
                        .resolve_module(&p.module()?.name()?.text()?)?,
                    p.variant()?.text()?,
                ))
            })
        {
            let result = resolver_for_toplevel(self.db.upcast(), module).resolve_name(&name)?;
            return Some(result);
        }

        analyzer.resolver.resolve_name(&(name.text()?))
    }

    pub fn resolve_type(&self, type_name: &ast::TypeName) -> Option<ResolveResult> {
        self.analyze(type_name.syntax())?
            .resolver
            .resolve_type(&(type_name.text()?))
    }

    pub(crate) fn analyze(&self, node: &SyntaxNode) -> Option<SourceAnalyzer> {
        let node = self.find_file(node);
        let resolver = match find_container(self.db.upcast(), node) {
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
        assert!(prev.is_none() || prev == Some(file_id))
    }

    /// Wraps the node in a [`InFile`] with the file id it belongs to.
    pub fn find_file<'node>(&self, node: &'node SyntaxNode) -> InFile<&'node SyntaxNode> {
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

    fn to_def<T: ToDef>(&self, src: &T) -> Option<T::Def> {
        let src = self.find_file(src.syntax()).with_value(src).cloned();
        T::to_def(self, src)
    }

    fn lookup(&self, root_node: &SyntaxNode) -> Option<FileId> {
        let cache = self.cache.borrow();
        cache.get(root_node).copied()
    }
}

fn find_root(node: &SyntaxNode) -> SyntaxNode {
    node.ancestors().last().unwrap()
}
pub trait ToDef: AstNode<Language = GleamLanguage> + Clone {
    type Def;

    fn to_def(sema: &Semantics<'_>, src: InFile<Self>) -> Option<Self::Def>;
}

impl ToDef for ast::Function {
    type Def = Function;

    fn to_def(sema: &Semantics<'_>, src: InFile<Self>) -> Option<Self::Def> {
        let map = sema.db.module_source_map(src.file_id);
        let fn_id = map.node_to_function(&src.value);
        fn_id.map(From::from)
    }
}

impl ToDef for ast::UnqualifiedImport {
    type Def = Import;

    fn to_def(sema: &Semantics<'_>, src: InFile<Self>) -> Option<Self::Def> {
        let map = sema.db.module_source_map(src.file_id);
        let import_id = map.node_to_import(&src.value);
        import_id.map(From::from)
    }
}

impl ToDef for ast::Variant {
    type Def = Variant;

    fn to_def(sema: &Semantics<'_>, src: InFile<Self>) -> Option<Self::Def> {
        let map = sema.db.module_source_map(src.file_id);
        let variant_id = map.node_to_variant(&src.value);
        variant_id.map(From::from)
    }
}

impl ToDef for ast::VariantField {
    type Def = Field;

    fn to_def(sema: &Semantics<'_>, src: InFile<Self>) -> Option<Self::Def> {
        let syntax = src.value.syntax();
        let container = find_container(sema.db.upcast(), src.with_value(syntax))?;

        if let ModuleDefId::VariantId(it) = container {
            let text = src.value.label()?.text()?;

            let variant = Variant { id: it.local_id, parent: it.parent };
            let adt = variant.parent();

            if let Some(field) = adt.common_fields(sema.db.upcast()).get(&text) {
                return Some(*field)
            }

            for field in variant.fields(sema.db.upcast()) {
                if field.label(sema.db.upcast())? == text {
                    return Some(field)
                }
            }
        }

        None
    }
}

impl ToDef for ast::Pattern {
    type Def = Local;

    fn to_def(sema: &Semantics<'_>, src: InFile<Self>) -> Option<Self::Def> {
        let syntax = src.value.syntax();
        let container = find_container(sema.db.upcast(), src.with_value(syntax))?;
        match container {
            ModuleDefId::FunctionId(it) => {
                let (_, source_map) = sema.db.body_with_source_map(it);
                let pat = source_map.pattern_for_node(src.as_ref())?;
                Some(Local {
                    parent: it,
                    pat_id: pat,
                })
            }
            _ => None,
        }
    }
}

pub fn find_container(db: &dyn DefDatabase, node: InFile<&SyntaxNode>) -> Option<ModuleDefId> {
    let map = db.module_source_map(node.file_id);
    for node in node.ancestors() {
        if let Some(ast::ModuleStatement::Function(it)) =
            ast::ModuleStatement::cast(node.value.clone())
        {
            let fn_id = map.node_to_function(&it);
            return fn_id.map(From::from);
        }

        if let Some(it) = ast::Adt::cast(node.value.clone()) {
            let adt_id = map.node_to_adt(&it);
            return adt_id.map(From::from);
        }

        if let Some(it) = ast::Variant::cast(node.value) {
            let variant_id = map.node_to_variant(&it);
            return variant_id.map(From::from);
        }
    }
    None
}
