use std::sync::Arc;

use memchr::memmem::Finder;
use nohash_hasher::IntMap;
use once_cell::unsync::Lazy;
use syntax::{
    ast::{self, AstNode},
    TextRange, TextSize,
};
use tracing::Level;

use crate::{DefDatabase, FileId};

use super::{classify_node, semantics::Definition, Semantics};

#[derive(Clone, Debug)]
pub struct SearchScope {
    entries: IntMap<FileId, Option<TextRange>>,
}

impl SearchScope {
    fn new(entries: IntMap<FileId, Option<TextRange>>) -> SearchScope {
        SearchScope { entries }
    }

    /// Build an empty search scope.
    pub fn empty() -> SearchScope {
        SearchScope::new(IntMap::default())
    }

    /// Build a empty search scope spanning the given file.
    pub fn single_file(file: FileId) -> SearchScope {
        SearchScope::new(std::iter::once((file, None)).collect())
    }

    pub fn package_graph(db: &dyn DefDatabase) -> SearchScope {
        let mut entries = IntMap::default();
        for (file, _) in db.module_map().iter() {
            entries.insert(file, None);
        }
        SearchScope { entries }
    }
}

impl Definition {
    fn search_scope(&self, db: &dyn DefDatabase) -> SearchScope {
        // ToDo: This is a hack to get started
        match self.module(db) {
            Some(_) => SearchScope::package_graph(db),
            None => SearchScope::empty(),
        }
    }

    pub fn usages<'a>(self, sema: &'a Semantics<'_>) -> FindUsages<'a> {
        FindUsages {
            def: self,
            sema,
            scope: None,
        }
    }
}

#[derive(Clone)]
pub struct FindUsages<'a> {
    def: Definition,
    sema: &'a Semantics<'a>,
    scope: Option<&'a SearchScope>,
}

impl<'a> FindUsages<'a> {
    /// Limit the search to a given [`SearchScope`].
    pub fn in_scope(self, scope: &'a SearchScope) -> Self {
        self.set_scope(Some(scope))
    }

    /// Limit the search to a given [`SearchScope`].
    pub fn set_scope(mut self, scope: Option<&'a SearchScope>) -> Self {
        assert!(self.scope.is_none());
        self.scope = scope;
        self
    }

    pub fn all(self) -> UsageSearchResult {
        let mut res = UsageSearchResult::default();
        self.search(&mut |file_id, reference| {
            res.references.entry(file_id).or_default().push(reference);
            false
        });
        res
    }

    pub fn search(&self, sink: &mut dyn FnMut(FileId, TextRange) -> bool) {
        let _p = tracing::span!(Level::TRACE, "FindUsages:search");
        let sema = self.sema;
        let base = self.def.search_scope(sema.db.upcast());

        let name = match self.def {
            Definition::Module(_) => None,
            _ => self.def.name(sema.db.upcast()),
        };

        let name = match &name {
            Some(s) => s.as_str(),
            None => return,
        };
        let finder = &Finder::new(name);

        // for<'a> |text: &'a str, name: &'a str, search_range: TextRange| -> impl Iterator<Item = TextSize> + 'a { ... }
        fn match_indices<'a>(
            text: &'a str,
            finder: &'a Finder<'a>,
            search_range: TextRange,
        ) -> impl Iterator<Item = TextSize> + 'a {
            finder.find_iter(text.as_bytes()).filter_map(move |idx| {
                let offset: TextSize = idx.try_into().unwrap();
                if !search_range.contains_inclusive(offset) {
                    return None;
                }
                Some(offset)
            })
        }

        // for<'a> |scope: &'a SearchScope| -> impl Iterator<Item = (Arc<String>, FileId, TextRange)> + 'a { ... }
        fn scope_files<'a>(
            sema: &'a Semantics<'a>,
            scope: &'a SearchScope,
        ) -> impl Iterator<Item = (Arc<str>, FileId, TextRange)> + 'a {
            scope.entries.iter().map(|(&file_id, &search_range)| {
                let text = sema.db.file_content(file_id);
                let search_range =
                    search_range.unwrap_or_else(|| TextRange::up_to(TextSize::of(&*text)));

                (text, file_id, search_range)
            })
        }

        let find_nodes = move |name: &str, node: &syntax::SyntaxNode, offset: TextSize| {
            node.token_at_offset(offset)
                .find(|t| t.text() == name)
                .and_then(|it| it.parent())
        };

        for (text, file_id, search_range) in scope_files(sema, &base) {
            let tree = Lazy::new(move || sema.parse(file_id).syntax().clone());
            // Search for occurrences of the items name
            for offset in match_indices(&text, finder, search_range) {
                if let Some(name) =
                    find_nodes(name, &tree, offset).and_then(ast::TypeNameOrName::cast)
                {
                    if match name {
                        ast::TypeNameOrName::Name(name) => self.found_name(&name, sink),
                        ast::TypeNameOrName::TypeName(type_name) => {
                            self.found_type_name(&type_name, sink)
                        }
                        ast::TypeNameOrName::NameRef(name_ref) => {
                            self.found_name_ref(&name_ref, sink)
                        }
                    } {
                        return;
                    }
                }
            }
        }
    }

    fn found_name_ref(
        &self,
        name_ref: &ast::NameRef,
        sink: &mut dyn FnMut(FileId, TextRange) -> bool,
    ) -> bool {
        if let Some(def) = classify_node(self.sema, name_ref.syntax()) {
            if self.def == def {
                let file_id = self.sema.find_file(name_ref.syntax()).file_id;
                return sink(file_id, name_ref.syntax().text_range());
            }
        }
        false
    }

    fn found_type_name(
        &self,
        name_ref: &ast::TypeName,
        sink: &mut dyn FnMut(FileId, TextRange) -> bool,
    ) -> bool {
        if let Some(def) = classify_node(self.sema, name_ref.syntax()) {
            if self.def == def {
                let file_id = self.sema.find_file(name_ref.syntax()).file_id;
                return sink(file_id, name_ref.syntax().text_range());
            }
        }
        false
    }

    // This is currently handled by the last step in highlight related
    fn found_name(
        &self,
        _name_ref: &ast::Name,
        _sink: &mut dyn FnMut(FileId, TextRange) -> bool,
    ) -> bool {
        // if let Some(def) = classify_node(self.sema, name_ref.syntax()) {
        //     if self.def == def {
        //         let file_id = self.sema.find_file(name_ref.syntax()).file_id;
        //         return sink(file_id, name_ref.syntax().text_range())
        //     }
        // }
        false
    }
}

#[derive(Debug, Default, Clone)]
pub struct UsageSearchResult {
    pub references: IntMap<FileId, Vec<TextRange>>,
}

impl IntoIterator for UsageSearchResult {
    type Item = (FileId, Vec<TextRange>);
    type IntoIter = <IntMap<FileId, Vec<TextRange>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.references.into_iter()
    }
}
