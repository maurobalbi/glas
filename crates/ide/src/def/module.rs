use la_arena::{Idx, IdxRange};
use smol_str::SmolStr;
use syntax::{
    ast::{self, BinaryOpKind, LiteralKind},
    AstPtr,
};

use crate::ty;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AdtData {
    pub name: SmolStr,

    pub variants: IdxRange<VariantData>,

    pub params: Vec<TypeParam>,
    pub visibility: Visibility,

    pub ast_ptr: AstPtr<ast::Adt>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariantData {
    pub name: SmolStr,

    pub fields: Vec<Field>,

    pub ast_ptr: AstPtr<ast::Variant>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field {
    pub label: Option<SmolStr>,
    pub type_ref: ty::Ty,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeParam(SmolStr);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionData {
    pub name: SmolStr,
    pub params: Vec<Param>,

    pub visibility: Visibility,
    pub ast_ptr: AstPtr<ast::Function>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ImportData {
    pub module: SmolStr, // e.g. import >>one/wobble<<

    pub unqualified_as_name: Option<SmolStr>, // e.g. {* as >>AsName<<}
    pub unqualified_name: SmolStr,            // e.g. { >>Name<< as Wobble }

    pub ast_ptr: AstPtr<ast::Import>,
}

impl ImportData {
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Clause {
    // patterns are lowered into #(pat, pat) tuples to make type inference easier
    pub pattern: PatternId,
    // pub guard: Option<ExprId>,
    pub expr: ExprId,
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
    Literal(LiteralKind),
    Block {
        stmts: Vec<Statement>,
    },
    Binary {
        left: ExprId,
        right: ExprId,
        op: Option<BinaryOpKind>,
    },
    FieldAccess {
        base_string: SmolStr,
        base: ExprId,
        label: ExprId,
        label_name: SmolStr,
    },
    VariantLiteral {
        name: SmolStr,
        fields: Vec<ExprId>,
    },
    Call {
        func: ExprId,
        args: Vec<ExprId>,
    },
    List {
        elements: Vec<ExprId>,
    },
    Pipe {
        left: ExprId,
        right: ExprId,
    },
    Lambda {
        body: ExprId,
        params: IdxRange<Pattern>,
    },
    Spread {
        expr: ExprId,
    },
    Case {
        // subjects are lowered into tuple to make type inference easier
        subject: ExprId,
        clauses: Vec<Clause>,
    },
    Variable(SmolStr),
}

pub type PatternId = Idx<Pattern>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Missing,
    Hole,
    Variable {
        name: SmolStr,
    },
    Tuple {
        fields: Vec<PatternId>,
    },
    Literal {
        kind: LiteralKind,
    },
    Spread {
        pattern: PatternId,
    },
    List {
        elements: Vec<PatternId>,
    },
    VariantRef {
        name: SmolStr,
        module: Option<SmolStr>,
        fields: Vec<PatternId>,
    },
    AlternativePattern {
        patterns: Vec<PatternId>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label(pub SmolStr);
