use std::{collections::HashMap, fmt};

use smol_str::SmolStr;

use crate::def::hir_def::ModuleDefId;

use super::{Ty, TyDatabase};

pub trait TyWrite: fmt::Write {
    fn start_location_link(&mut self, location: ModuleDefId);
    fn end_location_link(&mut self);
}

// String will ignore link metadata
impl TyWrite for String {
    fn start_location_link(&mut self, _: ModuleDefId) {}

    fn end_location_link(&mut self) {}
}

// `core::Formatter` will ignore metadata
impl TyWrite for fmt::Formatter<'_> {
    fn start_location_link(&mut self, _: ModuleDefId) {}
    fn end_location_link(&mut self) {}
}
pub struct TyFormatter<'a> {
    pub db: &'a dyn TyDatabase,
    fmt: &'a mut dyn TyWrite,
    buf: String,
    display_target: DisplayTarget,
    env: HashMap<u32, SmolStr>,
    uid: u32,
}

pub trait TyDisplay {
    fn ty_fmt(&self, f: &mut TyFormatter<'_>) -> Result<(), TyDisplayError>;

    /// Returns a `Display`able type that is human-readable.
    /// Use this for showing types to the user (e.g. diagnostics)
    fn display<'a>(&'a self, db: &'a dyn TyDatabase) -> TyDisplayWrapper<'a, Self>
    where
        Self: Sized,
    {
        TyDisplayWrapper {
            db,
            t: self,
            max_size: None,
            omit_verbose_types: false,
            display_target: DisplayTarget::Diagnostics,
        }
    }
}

impl<'a> TyFormatter<'a> {
    pub fn write_joined<T: TyDisplay>(
        &mut self,
        iter: impl IntoIterator<Item = T>,
        sep: &str,
    ) -> Result<(), TyDisplayError> {
        let mut first = true;
        for e in iter {
            if !first {
                write!(self, "{sep}")?;
            }
            first = false;

            e.ty_fmt(self)?;
        }
        Ok(())
    }

    /// This allows using the `write!` macro directly with a `HirFormatter`.
    pub fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> Result<(), TyDisplayError> {
        // We write to a buffer first to track output size
        self.buf.clear();
        fmt::write(&mut self.buf, args)?;

        // Then we write to the internal formatter from the buffer
        self.fmt.write_str(&self.buf).map_err(TyDisplayError::from)
    }

    pub fn write_str(&mut self, s: &str) -> Result<(), TyDisplayError> {
        self.fmt.write_str(s)?;
        Ok(())
    }

    pub fn write_char(&mut self, c: char) -> Result<(), TyDisplayError> {
        self.fmt.write_char(c)?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum DisplaySourceCodeError {
    PathNotFound,
    UnknownType,
    Closure,
    Generator,
}

pub enum TyDisplayError {
    /// Errors that can occur when generating source code
    DisplaySourceCodeError(DisplaySourceCodeError),
    /// `FmtError` is required to be compatible with std::fmt::Display
    FmtError,
}
impl From<fmt::Error> for TyDisplayError {
    fn from(_: fmt::Error) -> Self {
        Self::FmtError
    }
}

#[derive(Clone, Copy)]
pub enum DisplayTarget {
    /// Display types for inlays, doc popups, autocompletion, etc...
    /// Showing `{unknown}` or not qualifying paths is fine here.
    /// There's no reason for this to fail.
    Diagnostics,
    /// Display types for inserting them in source files.
    /// The generated code should compile, so paths need to be qualified.
    // SourceCode { module_id: ModuleId },
    /// Only for test purpose to keep real types
    Test,
}

impl DisplayTarget {
    // fn is_source_code(&self) -> bool {
    //     matches!(self, Self::SourceCode { .. })
    // }
    fn is_test(&self) -> bool {
        matches!(self, Self::Test)
    }
}

pub struct TyDisplayWrapper<'a, T> {
    db: &'a dyn TyDatabase,
    t: &'a T,
    max_size: Option<usize>,
    omit_verbose_types: bool,
    display_target: DisplayTarget,
}

impl<T: TyDisplay> TyDisplayWrapper<'_, T> {
    pub fn write_to<F: TyWrite>(&self, f: &mut F) -> Result<(), TyDisplayError> {
        self.t.ty_fmt(&mut TyFormatter {
            db: self.db,
            fmt: f,
            buf: String::with_capacity(20),
            display_target: self.display_target,
            uid: 0,
            env: HashMap::new(),
        })
    }
}

impl<'a, T> fmt::Display for TyDisplayWrapper<'a, T>
where
    T: TyDisplay,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.write_to(f) {
            Ok(()) => Ok(()),
            Err(TyDisplayError::FmtError) => Err(fmt::Error),
            Err(TyDisplayError::DisplaySourceCodeError(_)) => {
                // This should never happen
                panic!("HirDisplay::hir_fmt failed with DisplaySourceCodeError when calling Display::fmt!")
            }
        }
    }
}

impl TyDisplay for Ty {
    fn ty_fmt(
        &self,
        f @ &mut TyFormatter { db: _, .. }: &mut TyFormatter<'_>,
    ) -> Result<(), TyDisplayError> {
        match self {
            Ty::Unknown => write!(f, "?"),
            Ty::Nil => write!(f, "Nil"),
            Ty::Bool => write!(f, "Bool"),
            Ty::Int => write!(f, "Int"),
            Ty::Float => write!(f, "Float"),
            Ty::String => write!(f, "String"),
            Ty::Result { ok, err } => {
                write!(f, "Result(")?;
                ok.ty_fmt(f)?;
                write!(f, ", ")?;
                err.ty_fmt(f)?;
                write!(f, ")")
            }
            Ty::Function { params, return_ } => {
                write!(f, "fn(")?;
                f.write_joined(params.as_ref().clone().into_iter(), ", ")?;

                write!(f, ") -> ")?;
                return_.ty_fmt(f)
            }
            Ty::Hole => {
                write!(f, "_")
            }
            Ty::List { of } => {
                write!(f, "List(")?;
                of.ty_fmt(f)?;
                write!(f, ")")
            }
            Ty::Generic { name } => {
                // let lookup = f.env.get(idx);
                // match lookup.cloned() {
                //     Some(name) => write!(f, "{}", name),
                //     None => {
                //         let new_letter = next_letter(f.uid);
                //         f.env.insert(*idx, new_letter.clone());
                //         f.uid += 1;
                //         write!(f, "{}", new_letter)
                //     }
                // }
                write!(f, "{}", name)
            }
            Ty::Tuple { fields } => {
                write!(f, "#(")?;
                f.write_joined(fields.as_ref().clone().into_iter(), ", ")?;
                write!(f, ")")
            }
            Ty::Adt {
                module,
                name,
                params,
            } => {
                // let adt = db.lookup_intern_adt(*adt_id);
                // let adt = &db.module_items(adt.file_id)[adt.value];
                // if params.len() > 0 {
                //     write!(f, "{}", adt.name)?;
                //     write!(f, "(")?;
                //     f.write_joined(params.as_ref().clone().into_iter(), ", ")?;
                //     write!(f, ")")
                // } else {
                //     write!(f, "{}", adt.name)
                // }
                if params.len() > 0 {
                    write!(f, "{}", name)?;
                    write!(f, "(")?;
                    f.write_joined(params.as_ref().clone().into_iter(), ", ")?;
                    write!(f, ")")
                } else {
                    write!(f, "{}", name)
                }
            }
        }
    }
}

pub fn next_letter(uid: u32) -> SmolStr {
    let alphabet_length = 26;
    let char_offset = 97;
    let mut chars = vec![];
    let mut n;
    let mut rest = uid;

    loop {
        n = rest % alphabet_length;
        rest /= alphabet_length;
        chars.push((n as u8 + char_offset) as char);

        if rest == 0 {
            break;
        }
        rest -= 1
    }

    chars.into_iter().rev().collect()
}
