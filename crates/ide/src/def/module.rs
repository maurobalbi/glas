use la_arena::Idx;
use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use syntax::{
    ast::{self, BinaryOpKind},
    AstPtr,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub name: SmolStr,
    pub params: Vec<Param>,

    pub visibility: Visibility,
    pub ast_ptr: AstPtr<ast::Function>,
}

pub type ImportId = Idx<Import>;
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Import {
    pub module: SmolStr, // e.g. import >>one/wobble<<

    pub unqualified_as_name: Option<SmolStr>, // e.g. {* as >>AsName<<}
    pub unqualified_name: SmolStr,            // e.g. { >>Name<< as Wobble }

    pub ast_ptr: AstPtr<ast::Import>,
}

impl Import {
    pub fn local_name(&self) -> SmolStr {
        self.unqualified_as_name
            .as_ref()
            .unwrap_or(&self.unqualified_name)
            .clone()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Param {
    pub name: SmolStr,
    pub label: Option<SmolStr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let {
        pattern: PatternId, // Should be pattern
        // type_ann: Type
        body: ExprId,
    },
    Use {
        patterns: Vec<PatternId>,
        expr: ExprId,
    },
    Expr {
        expr: ExprId,
    },
}
pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Literal(Literal),
    Block {
        stmts: Vec<Statement>,
    },
    Binary {
        left: ExprId,
        right: ExprId,
        op: Option<BinaryOpKind>,
    },
    Call {
        func: ExprId,
        args: Vec<ExprId>,
    },
    NameRef(SmolStr),
}

pub type PatternId = Idx<Pattern>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Missing,
    Variable { name: SmolStr },
    Record { args: Vec<PatternId> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64),
    Float(OrderedFloat<f64>),
    String(SmolStr),
}

pub type NameId = Idx<Name>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub text: SmolStr,
    pub kind: NameKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameKind {
    Function,
    Pattern,
    Param,
    PatField,
    Import,
    Module,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label(pub SmolStr);
