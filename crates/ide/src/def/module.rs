use std::{collections::HashMap, sync::Arc};

use crate::ty;
use la_arena::{Idx, IdxRange};
use smol_str::SmolStr;
use syntax::{
    ast::{self, BinaryOpKind, LiteralKind},
    AstPtr,
};

use super::hir_def::LocalFieldId;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AdtData {
    pub name: SmolStr,

    pub variants: IdxRange<VariantData>,
    pub common_fields: HashMap<SmolStr, TypeRef>,

    pub params: Vec<TypeRef>,
    pub visibility: Visibility,

    pub ast_ptr: AstPtr<ast::Adt>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeAliasData {
    pub name: SmolStr,

    pub body: Option<TypeRef>,

    pub params: Vec<TypeRef>,
    pub visibility: Visibility,

    pub ast_ptr: AstPtr<ast::TypeAlias>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariantData {
    pub name: SmolStr,

    pub fields: IdxRange<FieldData>,

    pub ast_ptr: AstPtr<ast::Variant>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FieldData {
    pub label: Option<SmolStr>,
    pub type_ref: TypeRef,
    pub ast_ptr: AstPtr<ast::VariantField>,
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
pub struct ModuleImport {
    pub name: SmolStr,
    pub accessor: SmolStr,

    pub ast_ptr: AstPtr<ast::Import>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ImportData {
    pub is_type_import: bool,      // .{ type ... }
    pub module: Idx<ModuleImport>, // e.g. import >>one/wobble<<

    pub unqualified_as_name: Option<SmolStr>, // e.g. {* as >>AsName<<}
    pub unqualified_name: SmolStr,            // e.g. { >>Name<< as Wobble }

    pub ast_ptr: AstPtr<ast::UnqualifiedImport>,
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
    pub patterns: Vec<PatternId>,
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
    Hole,
    Literal(LiteralKind),
    Block {
        stmts: Vec<Statement>,
    },
    Binary {
        left: ExprId,
        right: ExprId,
        op: Option<BinaryOpKind>,
    },
    Tuple {
        fields: Vec<ExprId>,
    },
    FieldAccess {
        base_string: SmolStr,
        base: ExprId,
        label: ExprId,
        label_name: SmolStr,
    },
    VariantLiteral {
        name: SmolStr,
    },
    Call {
        func: ExprId,
        args: Vec<(Option<SmolStr>, ExprId)>,
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
        subjects: Vec<ExprId>,
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
        name: Option<SmolStr>,
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
    AsPattern {
        pattern: PatternId,
        as_name: Option<PatternId>,
    },
    Concat {
        pattern: PatternId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label(pub SmolStr);


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeRef {
    Hole,
    Unknown,
    Generic {
        name: SmolStr
    },
    Function {
        params: Arc<Vec<TypeRef>>,
        return_: Arc<TypeRef>,
    },
    Adt {
        // ToDo: refactor, since this is not quite accurate enough: other types might be qualified also
        module: Option<SmolStr>,
        name: SmolStr,
        params: Arc<Vec<TypeRef>>,
    },
    Tuple {
        fields: Arc<Vec<TypeRef>>,
    },
}

pub fn typeref_from_ast_opt(type_ast: Option<ast::TypeExpr>) -> Option<TypeRef> {
    type_ast.map(typeref_from_ast)
}

pub fn typeref_from_ast(ast_expr: ast::TypeExpr) -> TypeRef {
    match ast_expr {
        ast::TypeExpr::FnType(it) => {
            let mut fn_params = Vec::new();
            if let Some(params) = it.param_list() {
                for ty in params.params() {
                    fn_params.push(typeref_from_ast(ty));
                }
            };
            let ret = it.return_().map_or_else(|| TypeRef::Unknown, typeref_from_ast);
            TypeRef::Function {
                params: Arc::new(fn_params),
                return_: Arc::new(ret),
            }
        }
        ast::TypeExpr::TupleType(it) => {
            let mut fields = Vec::new();
            for ty in it.field_types() {
                fields.push(typeref_from_ast(ty));
            }
            TypeRef::Tuple {
                fields: Arc::new(fields),
            }
        }
        ast::TypeExpr::TypeNameRef(t) => {
            if let Some((ty, token)) = t
                .constructor_name()
                .and_then(|t| Some((t.text()?, t.token()?)))
            {
                match ty.as_str() {
                    a if token.kind() == syntax::SyntaxKind::U_IDENT => {
                        return TypeRef::Adt {
                            module: t.module().and_then(|m| m.text()),
                            name: a.into(),
                            params: Arc::new(Vec::new()),
                        }
                    }
                    a => return TypeRef::Generic { name: a.into() },
                }
            }
            TypeRef::Unknown
        }
        ast::TypeExpr::TypeApplication(ty) => {
            let type_constr = ty.type_constructor();
            let name = type_constr
                .clone()
                .and_then(|t| t.constructor_name())
                .and_then(|c| c.text());
            let Some(name) = name else { return TypeRef::Unknown };

            let mut arguments = Vec::new();
            if let Some(args) = ty.arg_list() {
                for arg in args.args() {
                    let arg = typeref_from_ast_opt(arg.arg());
                    if let Some(arg) = arg {
                        arguments.push(arg);
                    }
                }
            }
            TypeRef::Adt {
                module: type_constr.and_then(|t| t.module()).and_then(|m| m.text()),
                name,
                params: Arc::new(arguments),
            }
        }
        ast::TypeExpr::Hole(_) => TypeRef::Hole,
    }
}