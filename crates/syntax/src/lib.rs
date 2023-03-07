#[macro_use]
mod kind;

pub mod ast;
pub mod lexer;
pub mod parser;

use core::fmt;

#[cfg(test)]
mod tests;

pub use rowan::{self, NodeOrToken, TextRange, TextSize};

pub type SyntaxNode = rowan::SyntaxNode<GleamLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<GleamLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<GleamLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<GleamLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<GleamLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<GleamLanguage>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<GleamLanguage>;

pub use self::kind::SyntaxKind;
pub use self::parser::{parse_file, Parse};

pub fn whatever() {
  println!("{}", Error{range:TextRange::new(1.into(),2.into()),  kind: ErrorKind::NestTooDeep})
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Error {
    pub range: TextRange,
    pub kind: ErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    NestTooDeep,
    UnexpectedEof,
    ExpectToken(SyntaxKind),
    ExpectedTarget,
    ExpectedConstantExpression,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NestTooDeep => "Nest too deep",
            Self::UnexpectedEof => "Did not expect end of file.",
            Self::ExpectToken(tok) => return write!(f, "Expecting {}", tok),
            Self::ExpectedTarget => "Expected target javascript or erlang",
            Self::ExpectedConstantExpression => "Expected constant expression"
        }
        .fmt(f)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.kind,
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )
    }
}

impl std::error::Error for ErrorKind {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GleamLanguage {}

impl rowan::Language for GleamLanguage {
    type Kind = SyntaxKind;

    #[inline(always)]
    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        raw.into()
    }

    #[inline(always)]
    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        kind.into()
    }
}
