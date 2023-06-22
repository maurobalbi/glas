use crate::lexer::lex_string;
pub use logos::{Lexer, Logos};
use std::fmt;

macro_rules! def {
  (
    $(
      $(#[$meta:meta])*
      $variant:ident $(= [$($tt:tt)*])? $(@ $anchor:ident)?,
    )*
  ) => {
    #[allow(bad_style)]
    #[derive(Logos, Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
    #[repr(u16)]
    pub enum SyntaxKind {
      $(
        $(#[$meta])*
        $variant,
      )*
    }

    #[macro_export]
    macro_rules! T {
        $($(
            ($($tt)*) => { $crate::SyntaxKind::$variant };
        )?)*
    }

    impl SyntaxKind {
        $($(const $anchor: Self = Self::$variant;)?)*
    }

    impl fmt::Display for SyntaxKind {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                $(Self::$variant => f.write_str(to_str!($variant, $($($tt)*)?)),)*
            }
        }
    }
  };
}

macro_rules! to_str {
    // IDENT
    ($variant:tt, ) => {
        concat!('"', stringify!($variant), '"')
    };
    // Special case.
    ($variant:tt, '"') => {
        r#"'"'"#
    };
    // This breaks `literal` fragment.
    ($variant:tt, -) => {
        r#""-""#
    };
    // '['
    ($variant:tt, $s:literal) => {
        concat!('"', $s, '"')
    };
    // &&
    ($variant:tt, $($tt:tt)+) => {
        concat!('"', stringify!($($tt)+), '"')
    };
}

def! {
    #[regex(r"([ \t\n])+")]
    WHITESPACE @WHITESPACE_FIRST,

    #[regex(r"//[^\n\r]*")]
    COMMENT @WHITESPACE_LAST,

    #[regex(r"///[^\n\r]*")]
    COMMENT_STATEMENT @DOC_COMMENT_FIRST,

    #[regex(r"////[^\n\r]*")]
    COMMENT_MODULE @DOC_COMMENT_LAST,

    #[regex("[a-z][_a-z0-9]*")]
    IDENT,

    #[regex("_[_a-z0-9]*")]
    DISCARD_IDENT,

    #[regex("[A-Z][0-9a-zA-Z]*")]
    U_IDENT ,

    #[regex(r"([0-9][0-9_]*(\.[0-9_]+)([eE][+-]?[0-9_]+)?)")]
    FLOAT,

    #[regex("(0[xXbBoO])?[0-9_]+")]
    INTEGER,

    #[regex(r#"""#, lex_string)]
    STRING,

    #[token("[")]
    L_SQUARE = ["["] @SYMBOL_FIRST,

    #[token("]")]
    R_SQUARE = ["]"],

    #[token("{")]
    L_BRACE = ["{"],

    #[token("}")]
    R_BRACE = ["}"],

    #[token("(")]
    L_PAREN = ["("],

    #[token(")")]
    R_PAREN = [")"],

    #[token("+")]
    PLUS = ["+"],

    #[token("-")]
    MINUS = ["-"],

    #[token("*")]
    STAR = ["*"],

    #[token("/")]
    SLASH = ["/"],

    #[token("<")]
    LESS = ["<"],

    #[token(">")]
    GREATER = [">"],

    #[token("<=")]
    LESS_EQ = ["<="],

    #[token(">=")]
    GREATER_EQ = [">="],

    #[token("+.")]
    PLUS_DOT = ["+."],

    #[token("-.")]
    MINUS_DOT = ["-."],

    #[token("*.")]
    STAR_DOT = ["*."],

    #[token("/.")]
    SLASH_DOT = ["/."],

    #[token("%")]
    PERCENT = ["%"],

    #[token("<.")]
    LESS_DOT = ["<."],

    #[token(">.")]
    GREATER_DOT = [">."],

    #[token("<=.")]
    LESS_EQ_DOT = ["<=."],

    #[token(">=.")]
    GREATER_EQ_DOT = [">=."],

    #[token("<>")]
    LT_GT = ["<>"],

    #[token(":")]
    COLON = [":"],

    #[token(",")]
    COMMA = [","],

    #[token("#")]
    HASH = ["#"],

    #[token("!")]
    BANG = ["!"],

    #[token("=")]
    EQ = ["="],

    #[token("==")]
    EQ_EQ = ["=="],

    #[token("!=")]
    NOT_EQ = ["!="],

    #[token("|")]
    VBAR = ["|"],

    #[token("||")]
    VBAR_VBAR = ["||"],

    #[token("&&")]
    AMPER_AMPER = ["&&"],

    #[token("<<")]
    LT_LT = ["<<"],

    #[token(">>")]
    GT_GT = [">>"],

    #[token("|>")]
    VBAR_GT = ["|>"],

    #[token(".")]
    DOT = ["."],

    #[token("->")]
    R_ARROW = ["->"],

    #[token("<-")]
    L_ARROW = ["<-"],

    #[token("..")]
    DOT_DOT = [".."] @SYMBOL_LAST,

    #[token("as")]
    AS_KW = ["as"] @KEYWORD_FIRST,

    #[token("assert")]
    ASSERT_KW = ["assert"],

    #[token("case")]
    CASE_KW = ["case"],

    #[token("const")]
    CONST_KW = ["const"],

    #[token("external")]
    EXTERNAL_KW = ["external"],

    #[token("fn")]
    FN_KW = ["fn"],

    #[token("if")]
    IF_KW = ["if"],

    #[token("import")]
    IMPORT_KW = ["import"],

    #[token("let")]
    LET_KW = ["let"],

    #[token("opaque")]
    OPAQUE_KW = ["opaque"],

    #[token("panic")]
    PANIC_KW = ["panic"],

    #[token("pub")]
    PUB_KW = ["pub"],

    #[token("todo")]
    TODO_KW = ["todo"],

    #[token("try")]
    TRY_KW = ["try"],

    #[token("type")]
    TYPE_KW = ["type"],

    #[token("use")]
    USE_KW = ["use"] @KEYWORD_LAST,

    #[error]
    ERROR,

    EOF,

    // Nodes
    ANNOTATION,
    STMT_EXPR,
    TUPLE,
    BLOCK,
    EXPR_CALL,
    ARG,
    ARG_LIST,
    UNARY_OP,
    BINARY_OP,
    FIELD_ACCESS,
    TUPLE_INDEX,
    FIELD_NAME,
    NAME_REF,
    LITERAL,
    STMT_LET,
    IMPORT,
    FUNCTION,
    FN_BODY,
    LABEL,
    HOLE,
    SOURCE_FILE,
    MODULE_NAME,
    MODULE_CONSTANT,
    NAME,
    TYPE_NAME,
    PATH,
    PARAM,
    PARAM_LIST,
    GENERIC_PARAM_LIST,
    CUSTOM_TYPE_VARIANT,
    TARGET,
    TARGET_GROUP,
    FN_TYPE,
    TYPE_APPLICATION,
    TYPE_ARG_LIST,
    TYPE_ARG,
    PARAM_TYPE_LIST,
    VAR_TYPE,
    TUPLE_TYPE,
    HOLE_TYPE,
    UNQUALIFIED_IMPORT,
    CUSTOM_TYPE,
    TYPE_NAME_REF,
    QUALIFIED_TYPE_REF,
    CUSTOM_TYPE_ALIAS,
    VARIANT_FIELD_LIST,
    VARIANT_FIELD,
    STATEMENTS,
    CONSTANT_TUPLE,
    CONSTANT_LIST,
    PIPE,
    CASE,
    CLAUSE,
    ALTERNATIVE_PATTERN,
    PATTERN_VARIABLE,
    PATTERN_TUPLE,
    PATTERN_CONSTRUCTOR_APPLICATION,
    PATTERN_CONSTRUCTOR_ARG_LIST,
    PATTERN_CONSTRUCTOR_ARG,
    LIST,
    RECORD,
    BITSTRING,

    __LAST,
}

impl SyntaxKind {
    #[inline(always)]
    pub fn is_whitespace(self) -> bool {
        (Self::WHITESPACE_FIRST as u16..=Self::WHITESPACE_LAST as u16).contains(&(self as u16))
    }

    #[inline(always)]
    pub fn is_module_doc(self) -> bool {
        self.is_whitespace() || self == Self::COMMENT_MODULE
    }

    #[inline(always)]
    pub fn is_trivia(self) -> bool {
        (Self::WHITESPACE_FIRST as u16..=Self::DOC_COMMENT_LAST as u16).contains(&(self as u16))
    }

    #[inline(always)]
    pub fn is_stmt_doc(self) -> bool {
        self.is_whitespace() || self == Self::COMMENT_MODULE
    }

    #[inline(always)]
    pub fn is_doc(self) -> bool {
        (Self::DOC_COMMENT_FIRST as u16..=Self::DOC_COMMENT_LAST as u16).contains(&(self as u16))
    }

    #[inline(always)]
    pub fn is_keyword(self) -> bool {
        (Self::KEYWORD_FIRST as u16..=Self::KEYWORD_LAST as u16).contains(&(self as u16))
    }

    #[inline(always)]
    pub fn is_symbol(self) -> bool {
        (Self::SYMBOL_FIRST as u16..=Self::SYMBOL_LAST as u16).contains(&(self as u16))
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    #[inline]
    fn from(k: SyntaxKind) -> Self {
        Self(k as u16)
    }
}

impl From<rowan::SyntaxKind> for SyntaxKind {
    #[inline(always)]
    fn from(k: rowan::SyntaxKind) -> Self {
        assert!(k.0 <= SyntaxKind::__LAST as u16);
        // SAFETY: Guarded by the assert.
        unsafe { std::mem::transmute::<u16, SyntaxKind>(k.0 as u16) }
    }
}

#[test]
fn is_trivia() {
    assert!(SyntaxKind::COMMENT.is_trivia());
    assert!(SyntaxKind::COMMENT.is_whitespace());
}
