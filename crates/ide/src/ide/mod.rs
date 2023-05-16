mod diagnostics;
mod highlight_related;
mod goto_definition;

use crate::base::SourceDatabaseStorage;
use crate::def::DefDatabaseStorage;
use crate::{Change, Diagnostic, FileId, FileSet, FilePos, SourceRoot, VfsPath};
use salsa::{Database, Durability, ParallelDatabase};
use std::fmt;
use syntax::TextRange;

pub use highlight_related::HlRelated;

pub const DEFAULT_LRU_CAP: usize = 128;
use crate::DEFAULT_IMPORT_FILE;


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NavigationTarget {
    pub file_id: FileId,
    pub full_range: TextRange,
    pub focus_range: TextRange,
}

pub use salsa::Cancelled;

pub type Cancellable<T> = Result<T, Cancelled>;

#[salsa::database(SourceDatabaseStorage, DefDatabaseStorage)]

struct RootDatabase {
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

impl Default for RootDatabase {
    fn default() -> Self {
        use crate::SourceDatabase;

        let mut db = Self {
            storage: salsa::Storage::default(),
        };

        crate::def::ParseQuery
            .in_db_mut(&mut db)
            .set_lru_capacity(DEFAULT_LRU_CAP);
        db.set_module_graph_with_durability(Default::default(), Durability::MEDIUM);
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
        file_set.insert(file, VfsPath::new(format!("/{DEFAULT_IMPORT_FILE}")));
        change.set_roots(vec![SourceRoot::new_local(file_set, Some(file))]);
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

    pub fn diagnostics(&self, file: FileId) -> Cancellable<Vec<Diagnostic>> {
        self.with_db(|db| diagnostics::diagnostics(db, file))
    }

    pub fn highlight_related(&self, fpos: FilePos) -> Cancellable<Vec<HlRelated>> {
        self.with_db(|db| highlight_related::highlight_related(db, fpos).unwrap_or_default())
    }

   pub fn goto_definition(&self, pos: FilePos) -> Cancellable<Option<Vec<NavigationTarget>>> {
        self.with_db(|db| goto_definition::goto_definition(db, pos))
    }
}
