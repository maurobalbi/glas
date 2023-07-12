mod base;
mod def;
mod diagnostic;
mod ide;
mod ty;

#[cfg(test)]
mod tests;

pub const DEFAULT_IMPORT_FILE: &str = "gleam.toml";

pub use self::ide::{Analysis, AnalysisHost, Cancelled, GotoDefinitionResult};
pub use base::{
    Change, FileId, FilePos, FileRange, FileSet, InFile, PackageGraph, PackageInfo, SourceDatabase,
    SourceRoot, SourceRootId, VfsPath, ModuleMap
};
pub use def::DefDatabase;
pub use diagnostic::{Diagnostic, DiagnosticKind, Severity};
pub use ty::{InferenceResult, TyDatabase};

/// Generates `From` impls for `Enum E { Foo(Foo), Bar(Bar) }` enums
#[macro_export]
macro_rules! impl_from {
    ($($variant:ident $(($($sub_variant:ident),*))?),* for $enum:ident) => {
        $(
            impl From<$variant> for $enum {
                fn from(it: $variant) -> $enum {
                    $enum::$variant(it)
                }
            }
            $($(
                impl From<$sub_variant> for $enum {
                    fn from(it: $sub_variant) -> $enum {
                        $enum::$variant($variant::$sub_variant(it))
                    }
                }
            )*)?
        )*
    };
    ($($variant:ident$(<$V:ident>)?),* for $enum:ident) => {
        $(
            impl$(<$V>)? From<$variant$(<$V>)?> for $enum$(<$V>)? {
                fn from(it: $variant$(<$V>)?) -> $enum$(<$V>)? {
                    $enum::$variant(it)
                }
            }
        )*
    }
}