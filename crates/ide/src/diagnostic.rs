use crate::FileRange;
use core::fmt;
use syntax::{ErrorKind as SynErrorKind, TextRange};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub range: TextRange,
    pub kind: DiagnosticKind,
    pub notes: Vec<(FileRange, String)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    // Syntax.
    SyntaxError(SynErrorKind),
    // Lowering.

    // Name resolution.
    InactiveTarget,

    // Liveness.
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    IncompleteSyntax,
}

impl Diagnostic {
    pub fn new(range: TextRange, kind: DiagnosticKind) -> Self {
        Self {
            range,
            kind,
            notes: Vec::new(),
        }
    }

    pub fn with_note(mut self, frange: FileRange, message: impl Into<String>) -> Self {
        self.notes.push((frange, message.into()));
        self
    }

    pub fn code(&self) -> &'static str {
        match self.kind {
            DiagnosticKind::SyntaxError(_) => "syntax_error",
            DiagnosticKind::InactiveTarget => "inactive_target",
        }
    }

    pub fn severity(&self) -> Severity {
        match self.kind {
            DiagnosticKind::SyntaxError(_) => Severity::Error,
            DiagnosticKind::InactiveTarget => Severity::Info,
        }
    }

    pub fn message(&self) -> String {
        match self.kind {
            DiagnosticKind::SyntaxError(kind) => return kind.to_string(),
            DiagnosticKind::InactiveTarget => "Inactive Target",
        }
        .into()
    }

    // pub fn is_unnecessary(&self) -> bool {
    //     matches!(
    //         self.kind,
    //         DiagnosticKind::EmptyInherit
    //             | DiagnosticKind::UnusedBinding
    //             | DiagnosticKind::UnusedWith
    //             | DiagnosticKind::UnusedRec
    //     )
    // }

    // pub fn is_deprecated(&self) -> bool {
    //     matches!(
    //         self.kind,
    //         DiagnosticKind::LetAttrset | DiagnosticKind::UriLiteral
    //     )
    // }

    pub fn debug_display(&self) -> impl fmt::Display + '_ {
        struct Wrapper<'a>(&'a Diagnostic);
        impl fmt::Display for Wrapper<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:?}: {:?}", self.0.range, self.0.kind)?;
                for (frange, msg) in &self.0.notes {
                    // Currently all related information is in the same file.
                    // Ignore the FileId here.
                    write!(f, "\n    {:?}: {}", frange.range, msg)?;
                }
                Ok(())
            }
        }
        Wrapper(self)
    }
}

impl From<syntax::Error> for Diagnostic {
    fn from(err: syntax::Error) -> Self {
        Self::new(err.range, DiagnosticKind::SyntaxError(err.kind))
    }
}
