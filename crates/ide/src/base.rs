use la_arena::{Arena, Idx};
use salsa::Durability;
use smol_str::SmolStr;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::{fmt, iter, ops};
use syntax::{SyntaxNode, TextRange, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub u32);
/// safe because `FileId` is a newtype of `u32`
impl nohash_hasher::IsEnabled for FileId {}

// The location of a gleam.toml
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceRootId(pub u32);

/// An path in the virtual filesystem.
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

#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct ModuleMap {
    module_names: HashMap<FileId, SmolStr>,
    files: HashMap<SmolStr, FileId>,
}

impl ModuleMap {
    pub fn insert(&mut self, file: FileId, module_name: SmolStr) -> Option<FileId> {
        self.module_names.insert(file, module_name.clone());
        self.files.insert(module_name, file)
    }

    pub fn file_for_module_name(&self, name: &SmolStr) -> Option<FileId> {
        self.files.get(name).copied()
    }

    pub fn module_name_for_file(&self, file: FileId) -> Option<SmolStr> {
        self.module_names.get(&file).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &'_ SmolStr)> + ExactSizeIterator + '_ {
        self.module_names.iter().map(|(&file, path)| (file, path))
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PackageRoot {
    pub path: PathBuf,
}

/// A workspace unit, typically a Gleam package.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceRoot {
    file_set: FileSet,
    root_path: PathBuf,
}

impl SourceRoot {
    pub fn new(file_set: FileSet, root_path: PathBuf) -> Self {
        Self {
            file_set,
            root_path,
        }
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

    pub fn module_files(&self) -> impl Iterator<Item = (FileId, &'_ VfsPath)> + '_ {
        self.files().filter(|(_, path)| {
            let Some(ext) = path.as_path().and_then(|p| p.extension()) else {
                return false;
            };
            ext == "gleam"
        })
    }
}

pub fn module_name(root_path: &PathBuf, module_path: &Path) -> Option<SmolStr> {
    // my/module.gleam
    let mut module_path = module_path
        .strip_prefix(root_path)
        .expect("Stripping package prefix from module path")
        .to_path_buf();

    if module_path.extension()? != "gleam" {
        return None;
    }

    // my/module
    let _ = module_path.set_extension("");

    let module_path: PathBuf = module_path.components().skip(1).collect();

    // Stringify
    let name = module_path
        .to_str()
        .expect("Module name path to str")
        .to_string();

    // normalise windows paths
    Some(name.replace('\\', "/").into())
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct PackageGraph {
    target: Target,
    pub arena: Arena<PackageInfo>,
}

impl PackageGraph {
    pub fn set_target(&mut self, target: Target) {
        self.target = target;
    }

    pub fn add_package(
        &mut self,
        display_name: SmolStr,
        gleam_toml: FileId,
        is_local: bool,
    ) -> PackageId {
        let info = PackageInfo {
            gleam_toml,
            display_name,
            dependencies: Vec::new(),
            is_local,
        };
        self.arena.alloc(info)
    }

    pub fn add_dep(&mut self, from: PackageId, dep: Dependency) {
        self.arena[from].dependencies.push(dep)
    }

    pub fn iter(&self) -> impl Iterator<Item = PackageId> + '_ {
        self.arena.iter().map(|(idx, _)| idx)
    }
}

impl ops::Index<PackageId> for PackageGraph {
    type Output = PackageInfo;
    fn index(&self, crate_id: PackageId) -> &PackageInfo {
        &self.arena[crate_id]
    }
}

pub type PackageId = Idx<PackageInfo>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageInfo {
    pub gleam_toml: FileId,
    // pub version: Option<String>,
    pub display_name: SmolStr,
    pub dependencies: Vec<Dependency>,
    pub is_local: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Target {
    #[default]
    Erlang,
    Javascript,
}

impl From<&str> for Target {
    fn from(value: &str) -> Self {
        match value {
            "javascript" => Self::Javascript,
            _ => Self::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    pub package: PackageId,
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
    pub fn ancestors(self) -> impl Iterator<Item = InFile<SyntaxNode>> + Clone {
        iter::successors(Some(self.cloned()), move |node| {
            node.value.parent().map(|parent| node.with_value(parent))
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

    // fn source_root_package_info(&self, sid: SourceRootId) -> Option<Arc<PackageInfo>>;

    #[salsa::input]
    fn file_source_root(&self, file_id: FileId) -> SourceRootId;

    fn source_root_package(&self, source_root_id: SourceRootId) -> Arc<PackageId>;

    #[salsa::input]
    fn package_graph(&self) -> Arc<PackageGraph>;

    #[salsa::input]
    fn module_map(&self, source: SourceRootId) -> Arc<ModuleMap>;
}

fn source_root_package(db: &dyn SourceDatabase, id: SourceRootId) -> Arc<PackageId> {
    let graph = db.package_graph();
    let mut iter = graph.iter().filter(|&package| {
        let root_file = graph[package].gleam_toml;
        db.file_source_root(root_file) == id
    });
    let res = iter.next().expect("should always find a package!");
    Arc::new(res)
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Change {
    pub package_graph: Option<PackageGraph>,
    pub roots: Option<Vec<SourceRoot>>,
    pub file_changes: Vec<(FileId, Arc<str>)>,
    pub is_structural_change: bool,
    pub is_workspace_change: bool,
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

    pub fn set_structural_change(&mut self) {
        self.is_structural_change = true;
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
                let mut module_map = ModuleMap::default();
                for (fid, vpath) in root.files() {
                    tracing::info!("Setting source root for: {:?} {:?}", fid, vpath);
                    db.set_file_source_root_with_durability(fid, sid, Durability::HIGH);
                    if let Some(module_name) = vpath
                        .as_path()
                        .and_then(|mpath| module_name(&root.root_path, mpath))
                    {
                        module_map.insert(fid, module_name); // Todo: Report error if insert returns an fileid
                    }
                }
                db.set_module_map_with_durability(sid, Arc::new(module_map), Durability::HIGH);
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
