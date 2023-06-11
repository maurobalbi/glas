#[macro_use]
mod kind;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod ptr;
mod token_set;

use core::fmt;

#[cfg(test)]
mod tests;

use rowan::TokenAtOffset;

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
pub use self::ptr::{AstPtr};

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
    ExpectedExpression,
    ExpectedArgument,
    ExpectedTarget,
    ExpectedConstantExpression,
    MultipleNoAssoc,
    ExpectedStatement,
    ExpectedType,
    ExpectedIdentifier,
    UnexpectedImport,
    ExpectedParameter,
    OpaqueAlias,
    TrailingComma,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NestTooDeep => "Nest too deep",
            Self::UnexpectedEof => "Did not expect end of file.",
            Self::ExpectToken(tok) => return write!(f, "Expecting {}", tok),
            Self::ExpectedTarget => "Expected target javascript or erlang",
            Self::ExpectedArgument => "Expected an argument",
            Self::ExpectedIdentifier => "Expected an identifier",
            Self::MultipleNoAssoc => "No-associative operators cannot be chained",
            Self::ExpectedConstantExpression => "Expected constant expression",
            Self::ExpectedStatement => "Expected statement",
            Self::ExpectedType => "Expected type",
            Self::ExpectedExpression => "Expected Expression",
            Self::UnexpectedImport => "Did not expect an import here",
            Self::ExpectedParameter => "Expected a parameter",
            Self::OpaqueAlias => "Type alias can't be opaque",
            Self::TrailingComma => "Trailing comma is not allowed",
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

/// Matches a `SyntaxNode` against an `ast` type.
///
/// # Example:
///
/// ```
/// # use syntax::{SyntaxNode, match_ast, ast};
/// # fn main() {
/// # let node: SyntaxNode = return;
/// match_ast! {
///     match node {
///         ast::AttrpathValue(it) => {},
///         ast::PatField(it) => {},
///         _ => {},
///     }
/// }
/// # }
/// ```
#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => {
        match_ast!(match ($node) { $($tt)* })
    };
    (match ($node:expr) {
        $( $( $path:ident )::+ ($it:pat) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = <$($path)::* as $crate::rowan::ast::AstNode>::cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}

/// Pick the most meaningful token at given cursor offset.
pub fn best_token_at_offset(node: &SyntaxNode, offset: TextSize) -> Option<SyntaxToken> {
    fn score(tok: SyntaxKind) -> u8 {
        match tok {
            SyntaxKind::ERROR | SyntaxKind::WHITESPACE => 0,
            SyntaxKind::COMMENT => 1,
            k if k.is_symbol() => 4,
            k if k.is_keyword() => 5,
            // IDENT, INT, and etc.
            _ => 6,
        }
    }

    match node.token_at_offset(offset) {
        TokenAtOffset::None => None,
        TokenAtOffset::Single(tok) => Some(tok),
       TokenAtOffset::Between(lhs, rhs) => {
            // Slightly prefer RHS.
            if score(lhs.kind()) > score(rhs.kind()) {
                Some(lhs)
            } else {
                Some(rhs)
            }
        }
    }
}