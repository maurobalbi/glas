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

/// Generates `From` impls for `Enum E { Foo(Foo), Bar(Bar) }` enums
///
/// # Example
///
/// ```rust
/// impl_from!(Struct, Union, Enum for Adt);
/// ```
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