mod base;
mod def;
mod diagnostic;
mod ide;

#[cfg(test)]
mod tests;

pub const DEFAULT_IMPORT_FILE: &str = "gleam.toml";

pub use self::ide::{Analysis, AnalysisHost, Cancelled};
pub use base::{
    Change, FileId, FilePos, FileRange, FileSet, InFile, ModuleGraph, ModuleInfo, SourceDatabase,
    SourceRoot, SourceRootId, VfsPath,
};
pub use def::DefDatabase;
pub use diagnostic::{Diagnostic, DiagnosticKind, Severity};
