mod completion;
mod diagnostics;
mod goto_definition;
mod highlight_related;
mod hover;
mod references;
mod rename;
mod semantic_highlighting;
mod syntax_tree;

use crate::base::SourceDatabaseStorage;
use crate::def::{DefDatabaseStorage, InternDatabaseStorage};
use crate::text_edit::WorkspaceEdit;
use crate::ty::{TyDatabase, TyDatabaseStorage};
use crate::{
    Change, DefDatabase, Diagnostic, FileId, FilePos, FileRange, FileSet, SourceDatabase,
    SourceRoot, VfsPath,
};
use salsa::{Database, Durability, ParallelDatabase};
use smol_str::SmolStr;
use std::fmt;
use syntax::TextRange;

pub use completion::{CompletionItem, CompletionItemKind, CompletionRelevance};
pub use goto_definition::GotoDefinitionResult;
pub use highlight_related::HlRelated;
pub use hover::HoverResult;
pub use semantic_highlighting::{HlRange, HlTag};

pub const DEFAULT_LRU_CAP: usize = 128;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NavigationTarget {
    pub file_id: FileId,
    pub full_range: TextRange,
    pub focus_range: TextRange,
}

pub use salsa::Cancelled;

use self::rename::RenameResult;

pub type Cancellable<T> = Result<T, Cancelled>;

#[salsa::database(
    InternDatabaseStorage,
    SourceDatabaseStorage,
    DefDatabaseStorage,
    TyDatabaseStorage
)]

pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for RootDatabase {}

impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(RootDatabase {
            storage: self.storage.snapshot(),
        })
    }
}

impl fmt::Debug for RootDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootDatabase").finish_non_exhaustive()
    }
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

impl Upcast<dyn TyDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn TyDatabase + 'static) {
        self
    }
}

impl Upcast<dyn SourceDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn SourceDatabase + 'static) {
        self
    }
}

impl Upcast<dyn DefDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn DefDatabase + 'static) {
        self
    }
}

impl Default for RootDatabase {
    fn default() -> Self {
        let mut db = Self {
            storage: salsa::Storage::default(),
        };

        crate::def::ParseQuery
            .in_db_mut(&mut db)
            .set_lru_capacity(DEFAULT_LRU_CAP);
        db.set_package_graph_with_durability(Default::default(), Durability::MEDIUM);
        db
    }
}

#[derive(Default, Debug)]
pub struct AnalysisHost {
    db: RootDatabase,
}

impl AnalysisHost {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_single_file(src: &str) -> (Self, FileId) {
        let mut change = Change::default();
        let file = FileId(0);
        change.change_file(file, src.into());
        let mut file_set = FileSet::default();
        file_set.insert(file, VfsPath::new("/main.gleam")); // Hack
        change.set_roots(vec![SourceRoot::new(file_set, "/".into())]);
        let mut this = Self::new();
        this.apply_change(change);
        (this, file)
    }

    pub fn snapshot(&self) -> Analysis {
        Analysis {
            db: self.db.snapshot(),
        }
    }

    pub fn request_cancellation(&mut self) {
        self.db.salsa_runtime_mut().synthetic_write(Durability::LOW);
    }

    pub fn apply_change(&mut self, change: Change) {
        self.request_cancellation();
        change.apply(&mut self.db);
    }
}

#[derive(Debug)]
pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

impl Analysis {
    fn with_db<F, T>(&self, f: F) -> Cancellable<T>
    where
        F: FnOnce(&RootDatabase) -> T + std::panic::UnwindSafe,
    {
        Cancelled::catch(|| f(&self.db))
    }

    //// LSP standard ////
    pub fn hover(&self, fpos: FilePos) -> Cancellable<Option<HoverResult>> {
        self.with_db(|db| hover::hover(db, fpos))
    }

    pub fn completions(
        &self,
        pos: FilePos,
        trigger_char: Option<char>,
    ) -> Cancellable<Option<Vec<CompletionItem>>> {
        self.with_db(|db| completion::completions(db, pos, trigger_char))
    }

    pub fn diagnostics(&self, file: FileId) -> Cancellable<Vec<Diagnostic>> {
        self.with_db(|db| diagnostics::diagnostics(db, file))
    }

    pub fn highlight_related(&self, fpos: FilePos) -> Cancellable<Vec<HlRelated>> {
        self.with_db(|db| highlight_related::highlight_related(db, fpos).unwrap_or_default())
    }

    pub fn goto_definition(&self, pos: FilePos) -> Cancellable<Option<GotoDefinitionResult>> {
        self.with_db(|db| goto_definition::goto_definition(db, pos))
    }

    pub fn references(&self, pos: FilePos) -> Cancellable<Option<Vec<FileRange>>> {
        self.with_db(|db| references::references(db, pos))
    }

    pub fn prepare_rename(&self, fpos: FilePos) -> Cancellable<RenameResult<(TextRange, SmolStr)>> {
        self.with_db(|db| rename::prepare_rename(db, fpos))
    }

    pub fn rename(
        &self,
        fpos: FilePos,
        new_name: &str,
    ) -> Cancellable<RenameResult<WorkspaceEdit>> {
        self.with_db(|db| rename::rename(db, fpos, new_name))
    }

    pub fn syntax_tree(&self, file_id: FileId) -> Cancellable<String> {
        self.with_db(|db| syntax_tree::syntax_tree(db, file_id))
    }

    pub fn syntax_highlight(
        &self,
        file: FileId,
        range: Option<TextRange>,
    ) -> Cancellable<Vec<HlRange>> {
        self.with_db(|db| semantic_highlighting::highlight(db, file, range))
    }
}
