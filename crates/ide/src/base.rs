use salsa::Durability;
use smol_str::SmolStr;
use std::collections::HashMap;
use std::{fmt, iter};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use syntax::{TextRange, TextSize, SyntaxNode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub u32);

// The location of a gleam.toml
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceRootId(pub u32);

/// An path in the virtual filesystem.
#[cfg(unix)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VfsPath {
    Path(PathBuf),
    Virtual(String),
}

impl VfsPath {
    /// Construct a new filesystem path.
    #[must_use]
    pub fn new(path: impl AsRef<Path>) -> Self {
        Self::Path(path.as_ref().to_path_buf())
    }

    /// Return a reference to the underlying `Path`.
    #[must_use]
    pub fn as_path(&self) -> Option<&Path> {
        match self {
            Self::Path(path) => Some(path),
            Self::Virtual(_) => None,
        }
    }

    /// Extends `self` with `path`.
    ///
    /// Returns `None` if the path is not extentable, otherwise returns `Some(())`.
    #[must_use]
    pub fn push(&mut self, path: &str) -> Option<()> {
        match self {
            Self::Path(this) => {
                this.push(path);
                Some(())
            }
            Self::Virtual(_) => None,
        }
    }

    /// Creates a new `VfsPath` with `path` adjoined to self.
    #[must_use]
    pub fn join(&self, path: &str) -> Option<Self> {
        match self {
            Self::Path(this) => Some(Self::Path(this.join(path))),
            Self::Virtual(_) => None,
        }
    }

    /// Truncates `self` to the parent of it.
    ///
    /// Returns `false` and does nothing if `self` has no parent,
    /// otherwise, return `true`.
    pub fn pop(&mut self) -> bool {
        match self {
            Self::Path(this) => this.pop(),
            Self::Virtual(_) => false,
        }
    }

    /// Returns an `impl Display` struct for human.
    #[must_use]
    pub fn display(&self) -> impl fmt::Display + '_ {
        struct Display<'a>(&'a VfsPath);

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.0 {
                    VfsPath::Path(path) => path.display().fmt(f),
                    VfsPath::Virtual(id) => write!(f, "(virtual path {id})"),
                }
            }
        }

        Display(self)
    }
}

impl From<PathBuf> for VfsPath {
    fn from(path: PathBuf) -> Self {
        Self::Path(path)
    }
}

impl From<&'_ Path> for VfsPath {
    fn from(path: &'_ Path) -> Self {
        Self::Path(path.to_path_buf())
    }
}

/// A set of [`VfsPath`]s identified by [`FileId`]s.
#[derive(Default, Clone, PartialEq, Eq)]
pub struct FileSet {
    files: HashMap<VfsPath, FileId>,
    paths: HashMap<FileId, VfsPath>,
}

impl FileSet {
    pub fn insert(&mut self, file: FileId, path: VfsPath) {
        self.files.insert(path.clone(), file);
        self.paths.insert(file, path);
    }

    pub fn remove_file(&mut self, file: FileId) {
        if let Some(path) = self.paths.remove(&file) {
            self.files.remove(&path);
        }
    }

    pub fn file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.files.get(path).copied()
    }

    pub fn path_for_file(&self, file: FileId) -> &VfsPath {
        &self.paths[&file]
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &'_ VfsPath)> + ExactSizeIterator + '_ {
        self.paths.iter().map(|(&file, path)| (file, path))
    }
}

impl fmt::Debug for FileSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(&self.paths).finish()
    }
}

/// A workspace unit, typically a Gleam package.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceRoot {
    file_set: FileSet,
    pub is_library: bool,
}

impl SourceRoot {
    pub fn new_local(file_set: FileSet) -> Self {
        Self { file_set, is_library: false}
    }

    pub fn new_library(file_set: FileSet) -> SourceRoot {
        SourceRoot { is_library: true, file_set }
    }

    pub fn file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.file_set.file_for_path(path)
    }

    pub fn path_for_file(&self, file: FileId) -> &VfsPath {
        self.file_set.path_for_file(file)
    }

    pub fn files(&self) -> impl Iterator<Item = (FileId, &'_ VfsPath)> + ExactSizeIterator + '_ {
        self.file_set.iter()
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct PackageGraph {
    pub nodes: HashMap<SourceRootId, PackageInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageInfo {
    // pub root_file: FileId,
    // pub version: Option<String>,
    pub target: Target,
    pub display_name: String,
    pub dependencies: Vec<Dependency>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Target {
    #[default]
    Erlang,
    Javascript
}

impl From<&str> for Target {
    fn from(value: &str) -> Self {
        match value {
            "javascript" => Self::Javascript,
            _ => Self::default()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    pub package_root: SourceRootId,
    pub name: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}

impl<T> InFile<T> {
    pub fn new(file_id: FileId, value: T) -> Self {
        Self { file_id, value }
    }
    
    pub fn with_value<U>(&self, value: U) -> InFile<U> {
        InFile::new(self.file_id, value)
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> InFile<U> {
        InFile {
            file_id: self.file_id,
            value: f(self.value),
        }
    }

    pub fn as_ref(&self) -> InFile<&T> {
        self.with_value(&self.value)
    }
}

impl<T: Clone> InFile<&T> {
    pub fn cloned(&self) -> InFile<T> {
        self.with_value(self.value.clone())
    }
}

impl<'a> InFile<&'a SyntaxNode> {
    pub fn ancestors(self)-> impl Iterator<Item = InFile<SyntaxNode>> + Clone {
        iter::successors(Some(self.cloned()), move |node| match node.value.parent() {
            Some(parent) => Some(node.with_value(parent)),
            None => None,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FilePos {
    pub file_id: FileId,
    pub pos: TextSize,
}

impl FilePos {
    pub fn new(file_id: FileId, pos: TextSize) -> Self {
        Self { file_id, pos }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FileRange {
    pub file_id: FileId,
    pub range: TextRange,
}

impl FileRange {
    pub fn new(file_id: FileId, range: TextRange) -> Self {
        Self { file_id, range }
    }

    pub fn empty(pos: FilePos) -> Self {
        Self::new(pos.file_id, TextRange::empty(pos.pos))
    }

    pub fn span(start: FilePos, end: FilePos) -> Self {
        assert_eq!(start.file_id, end.file_id);
        Self::new(start.file_id, TextRange::new(start.pos, end.pos))
    }
}

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn file_content(&self, file_id: FileId) -> Arc<str>;

    #[salsa::input]
    fn source_root(&self, sid: SourceRootId) -> Arc<SourceRoot>;

    fn source_root_package_info(&self, sid: SourceRootId) -> Option<Arc<PackageInfo>>;

    #[salsa::input]
    fn file_source_root(&self, file_id: FileId) -> SourceRootId;

    #[salsa::input]
    fn package_graph(&self) -> Arc<PackageGraph>;

}

fn source_root_package_info(db: &dyn SourceDatabase, sid: SourceRootId) -> Option<Arc<PackageInfo>> {
    db.package_graph().nodes.get(&sid).cloned().map(Arc::new)
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Change {
    pub package_graph: Option<PackageGraph>,
    pub roots: Option<Vec<SourceRoot>>,
    pub file_changes: Vec<(FileId, Arc<str>)>,
}

impl Change {
    pub fn is_empty(&self) -> bool {
        self.roots.is_none() && self.file_changes.is_empty()
    }

    pub fn set_package_graph(&mut self, graph: PackageGraph) {
        self.package_graph = Some(graph);
    }

    pub fn set_roots(&mut self, roots: Vec<SourceRoot>) {
        self.roots = Some(roots);
    }

    pub fn change_file(&mut self, file_id: FileId, content: Arc<str>) {
        self.file_changes.push((file_id, content));
    }

    pub(crate) fn apply(self, db: &mut dyn SourceDatabase) {
        if let Some(package_graph) = self.package_graph {
            db.set_package_graph_with_durability(Arc::new(package_graph), Durability::MEDIUM);
        }
        if let Some(roots) = self.roots {
            u32::try_from(roots.len()).expect("Length overflow");
            for (sid, root) in (0u32..).map(SourceRootId).zip(roots) {
                for (fid, _) in root.files() {
                    db.set_file_source_root_with_durability(fid, sid, Durability::HIGH);
                }
                db.set_source_root_with_durability(sid, Arc::new(root), Durability::HIGH);
            }
        }
        for (file_id, content) in self.file_changes {
            db.set_file_content_with_durability(file_id, content, Durability::LOW);
        }
    }
}

impl fmt::Debug for Change {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let modified = self
            .file_changes
            .iter()
            .filter(|(_, content)| !content.is_empty())
            .count();
        let cleared = self.file_changes.len() - modified;
        f.debug_struct("Change")
            .field("roots", &self.roots.as_ref().map(|roots| roots.len()))
            .field("modified", &modified)
            .field("cleared", &cleared)
            .finish_non_exhaustive()
    }
}
