use crate::SyntaxKind::{self, *};
use crate::{GleamLanguage, SyntaxElementChildren, SyntaxNode, SyntaxToken};
use itertools::Itertools;
use rowan::ast::support::{child, children};
use rowan::NodeOrToken;
use smol_str::SmolStr;

pub use rowan::ast::{AstChildren, AstNode};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOpKind {
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntMod,
    IntGT,
    IntLT,
    IntGTE,
    IntLTE,
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatGT,
    FloatLT,
    FloatGTE,
    FloatLTE,
    Eq,

    //String
    Concat,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOpKind {
    Not,
    Negate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LiteralKind {
    Int,
    Float,
    String,
}

trait NodeWrapper {
    const KIND: SyntaxKind;
}

macro_rules! enums {
    ($($name:ident { $($variant:ident$(<$gen:ident>)?,)* },)*) => {
        $(
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        pub enum $name {
            $($variant($variant$(<$gen>)*),)*
        }

        impl AstNode for $name {
            type Language = GleamLanguage;

            fn can_cast(kind: SyntaxKind) -> bool
                where Self: Sized
            {
                matches!(kind, $(<$variant as NodeWrapper>::KIND)|*)
            }

            fn cast(node: SyntaxNode) -> Option<Self>
            where
                Self: Sized
            {
                match node.kind() {
                    $(<$variant as NodeWrapper>::KIND => Some(Self::$variant$(::<$gen>)*($variant(node))),)*
                    _ => None,
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$variant(e) => &e.0,)*
                }
            }
        }
        )*
    };
}

macro_rules! asts {
    (
        $(
            $kind:ident = $name:ident $([$trait:tt])?
            { $($impl:tt)* },
        )*
    ) => {
        $(
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        pub struct $name (SyntaxNode);

        impl $name {
            ast_impl!($($impl)*);
        }

        $(impl $trait for $name {})*

        impl NodeWrapper for $name{
            const KIND: SyntaxKind = SyntaxKind::$kind;
        }

        impl AstNode for $name {
            type Language = GleamLanguage;

            fn can_cast(kind: SyntaxKind) -> bool
                where Self: Sized
            {
                kind == SyntaxKind::$kind
            }

            fn cast(node: SyntaxNode) -> Option<Self>
            where
                Self: Sized
            {
                if node.kind() == SyntaxKind::$kind {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
        )*
    };
}

macro_rules! ast_impl {
    () => {};
    ($field:ident: $ast:ident, $($tt:tt)*) => {
        pub fn $field(&self) -> Option<$ast> { child(&self.0) }
        ast_impl!($($tt)*);
    };
    ($field:ident[$k:tt]: $ast:ident, $($tt:tt)*) => {
        pub fn $field(&self) -> Option<$ast> { children(&self.0).nth($k) }
        ast_impl!($($tt)*);
    };
    ($field:ident: [$ast:ident], $($tt:tt)*) => {
        pub fn $field(&self) -> AstChildren<$ast> { children(&self.0) }
        ast_impl!($($tt)*);
    };
    ($field:ident: T![$tok:tt], $($tt:tt)*) => {
        pub fn $field(&self) -> Option<SyntaxToken> {
            token(&self.0, T![$tok])
        }
        ast_impl!($($tt)*);
    };
    ($field:ident[$k:tt]: T![$tok:tt], $($tt:tt)*) => {
        pub fn $field(&self) -> Option<SyntaxToken> {
            self.0
                .children_with_tokens()
                .filter_map(|it| it.into_token())
                .filter(|it| it.kind() == T![$tok])
                .nth($k)
        }
        ast_impl!($($tt)*);
    };
    ($($item:item)*) => {
        $($item)*
    };
}

enums! {
    ModuleStatement {
        ModuleConstant,
        Import,
        Function,
        Adt,
        TypeAlias,
    },
    ConstantExpr {
        Literal,
        ConstantTuple,
        ConstantList,
    },
    StatementExpr {
        StmtLet,
        StmtExpr,
        StmtUse,
    },
    Expr {
        Case,
        BitArray,
        Literal,
        Block,
        Variable,
        Lambda,
        BinaryOp,
        Hole,
        Tuple,
        Pipe,
        UnaryOp,
        List,
        ExprCall,
        VariantConstructor,
        FieldAccessExpr,
        TupleIndex,
        ExprSpread,
    },
    TypeExpr {
        FnType,
        TupleType,
        TypeNameRef,
        TypeApplication,
        Hole,
    },
    Pattern {
        PatternVariable,
        VariantRef,
        PatternTuple,
        Literal,
        PatternList,
        Hole,
        PatternSpread,
        AsPattern,
        PatternConcat,
    },
    TypeNameOrName {
        Name,
        TypeName,
        NameRef,
        Label,
    },
}

impl Variable {
    pub fn text(&self) -> Option<SmolStr> {
        self.name().and_then(|n| n.text())
    }
}

impl From<FieldAccessExpr> for Expr {
    fn from(field: FieldAccessExpr) -> Self {
        Expr::FieldAccessExpr(field)
    }
}

impl From<Variable> for Expr {
    fn from(name: Variable) -> Self {
        Expr::Variable(name)
    }
}

impl TypeNameOrName {
    pub fn token(&self) -> Option<SyntaxToken> {
        match self {
            TypeNameOrName::Name(name) => name.token(),
            TypeNameOrName::TypeName(type_name) => type_name.token(),
            TypeNameOrName::NameRef(it) => it.token(),
            TypeNameOrName::Label(it) => it.token(),
        }
    }

    pub fn text(&self) -> Option<SmolStr> {
        self.token().map(|t| t.text().into())
    }
}

impl FieldAccessExpr {
    pub fn for_label_name_ref(label: &NameRef) -> Option<FieldAccessExpr> {
        let syn = label.syntax();
        let candidate = syn.parent().and_then(FieldAccessExpr::cast)?;
        if candidate.label().as_ref() == Some(label) {
            Some(candidate)
        } else {
            None
        }
    }
}

asts! {
    BLOCK = Block {
        expressions: [StatementExpr],
    },
    BIT_ARRAY = BitArray {

    },
    BINARY_OP = BinaryOp {
        lhs: Expr,
        rhs[1]: Expr,

        pub fn op_details(&self) -> Option<(SyntaxToken, BinaryOpKind)> {
            self.syntax().children_with_tokens().find_map(|n| {
                let tok = n.into_token()?;
                let op = match tok.kind() {
                    T!["+"] => BinaryOpKind::IntAdd,
                    T!["-"] => BinaryOpKind::IntSub,
                    T!["*"] => BinaryOpKind::IntMul,
                    T!["/"] => BinaryOpKind::IntDiv,
                    T!["%"] => BinaryOpKind::IntMod,
                    T![">"] => BinaryOpKind::IntGT,
                    T!["<"] => BinaryOpKind::IntLT,
                    T![">="] => BinaryOpKind::IntGTE,
                    T!["<="] => BinaryOpKind::IntLTE,

                    T!["+."] => BinaryOpKind::FloatAdd,
                    T!["-."] => BinaryOpKind::FloatSub,
                    T!["*."] => BinaryOpKind::FloatMul,
                    T!["/."] => BinaryOpKind::FloatDiv,
                    T![">."] => BinaryOpKind::FloatGT,
                    T!["<."] => BinaryOpKind::FloatLT,
                    T![">=."] => BinaryOpKind::FloatGTE,
                    T!["<=."] => BinaryOpKind::FloatGTE,
                    T!["=="] => BinaryOpKind::Eq,
                    T!["<>"] => BinaryOpKind::Concat,
                    _ => return None,
                };
                Some((tok, op))
            })
        }
        pub fn op_token(&self) -> Option<SyntaxToken> {
            self.op_details().map(|t| t.0)
        }
        pub fn op_kind(&self) -> Option<BinaryOpKind> {
            self.op_details().map(|t| t.1)
        }
    },
    PIPE = Pipe {
        lhs: Expr,
        rhs[1]: Expr,
    },
    CONSTANT_LIST = ConstantList {
        elements: [ConstantExpr],
    },
    LITERAL = Literal {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }

        pub fn text(&self) -> Option<SmolStr>{
            self.token().map(|t| t.text().into())
        }

        pub fn kind(&self) -> Option<LiteralKind> {
            Some(match self.token()?.kind() {
                INTEGER => LiteralKind::Int,
                FLOAT => LiteralKind::Float,
                STRING => LiteralKind::String,
                _ => return None,
            })
        }
    },
    ADT = Adt [HasDocParts] {
        name: TypeName,
        constructors: [Variant],
        generic_params: GenericParamList,

        pub fn is_public(&self) -> bool {
            self.syntax().children_with_tokens().any(|it| it.kind() == T!["pub"])
        }
    },
    GENERIC_PARAM_LIST = GenericParamList {
        params: [TypeExpr],
    },
    TYPE_ALIAS = TypeAlias {
        name: TypeName,
        type_: TypeExpr,
        generic_params: GenericParamList,

        pub fn is_public(&self) -> bool {
            self.syntax().children_with_tokens().any(|it| it.kind() == T!["pub"])
        }

        pub fn is_opaque(&self) -> bool {
            self.0.children_with_tokens().any(|t| t.kind() == T!["opaque"])
        }
    },
    VARIANT = Variant [HasDocParts] {
        name: Name,
        field_list: VariantFieldList,
    },
    VARIANT_CONSTRUCTOR = VariantConstructor {
        name: NameRef,
    },
    VARIANT_FIELD_LIST = VariantFieldList {
        fields: [VariantField],
    },
    VARIANT_FIELD = VariantField {
        label: Name,
        type_: TypeExpr,
    },
    LAMBDA = Lambda {
        param_list: ParamList,
        return_type: TypeExpr,
        body: Block,
    },
    FUNCTION = Function [HasDocParts] {
        name: Name,
        param_list: ParamList,
        return_type: TypeExpr,
        body: Block,

        pub fn is_public(&self) -> bool {
            self.syntax().children_with_tokens().any(|it| it.kind() == T!["pub"])
        }
    },
    EXPR_CALL = ExprCall {
        func: Expr,
        arguments: ArgList,
    },
    ARG_LIST = ArgList {
        args: [Arg],
        pub fn l_paren_token(&self) -> Option<SyntaxToken> {
            self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find(|it| it.kind() == T!["{"])
        }
        pub fn r_paren_token(&self) -> Option<SyntaxToken> {
            self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find(|it| it.kind() == T!["}"])
        }
    },
    ARG = Arg {
        label: Label,
        value: Expr,
    },
    FIELD_ACCESS = FieldAccessExpr {
        base: Expr,
        label: NameRef,
    },
    TUPLE = Tuple {
        fields: [Expr],
    },
    TUPLE_INDEX = TupleIndex {
        base: Expr,
        index: Literal,
    },
    IMPORT = Import {
        module_path: ModulePath,
        as_name: Name,
        unqualified: [UnqualifiedImport],
    },
    MODULE_PATH = ModulePath {
        path: [Path],
    },
    VARIABLE = Variable {
        name: NameRef,
    },
    EXPR_SPREAD = ExprSpread {
        expr: Expr,
    },
    SOURCE_FILE = SourceFile [HasDocParts] {
        statements: [ModuleStatement],
    },
    MODULE_NAME_REF = ModuleNameRef {
        name: NameRef,
    },
    MODULE_CONSTANT = ModuleConstant [HasDocParts] {
        name: Name,
        value: ConstantExpr,
        annotation: TypeExpr,
        pub fn is_public(&self) -> bool {
            self.syntax().children_with_tokens().any(|it| it.kind() == T!["pub"])
        }
    },
    NAME = Name {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }

        pub fn text(&self) -> Option<SmolStr> {
            self.token().map(|t| t.text().into())
        }
    },
    TYPE_NAME = TypeName {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }

        pub fn text(&self) -> Option<SmolStr>{
            self.token().map(|t| t.text().into())
        }
    },
    LABEL = Label {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }

        pub fn text(&self) -> Option<SmolStr>{
            self.token().map(|t| t.text().into())
        }
    },
    TARGET = Target {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }
    },
    PATH = Path {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }
    },
    UNQUALIFIED_IMPORT = UnqualifiedImport {
        name: TypeNameOrName,
        as_name[1]: TypeNameOrName,

        pub fn is_type(&self) -> bool {
            self.syntax().children_with_tokens().any(|it| it.kind() == T!["type"])
        }
    },
    PARAM = Param {
        pattern: Pattern, // this is a pattern to make name resolution easier
        label: Label,
        ty: TypeExpr,
    },
    PARAM_LIST = ParamList {
        params: [Param],
    },
    UNARY_OP = UnaryOp {
        arg: Expr,

        pub fn op_details(&self) -> Option<(SyntaxToken, UnaryOpKind)> {
            self.syntax().children_with_tokens().find_map(|n| {
                let tok = n.into_token()?;
                let kind = match tok.kind() {
                    T!["!"] => UnaryOpKind::Not,
                    T!["-"] => UnaryOpKind::Negate,
                    _ => return None,
                };
                Some((tok, kind))
            })
        }
        pub fn op_token(&self) -> Option<SyntaxToken> {
            self.op_details().map(|t| t.0)
        }
        pub fn op_kind(&self) -> Option<UnaryOpKind> {
            self.op_details().map(|t| t.1)
        }
    },
    HOLE = Hole {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }
    },
    LIST = List {
        elements: [Expr],
    },
    STMT_EXPR = StmtExpr {
        expr: Expr,
    },
    STMT_LET = StmtLet {
        pattern: Pattern,
        annotation: TypeExpr,
        body: Expr,
    },
    STMT_USE = StmtUse {
        assignments: [UseAssignment],
        expr: Expr,
    },
    USE_ASSIGNMENT = UseAssignment {
        pattern: Pattern,
        annotation: TypeExpr,
    },
    CONSTANT_TUPLE = ConstantTuple {
        elements: [ConstantExpr],
    },
    TYPE_NAME_REF = TypeNameRef {
        module: Name,
        constructor_name: TypeName,
    },
    TYPE_APPLICATION = TypeApplication {
        type_constructor: TypeNameRef,
        arg_list: TypeArgList,
    },
    TYPE_ARG_LIST = TypeArgList {
        args: [TypeArg],
    },
    TYPE_ARG = TypeArg {
        arg: TypeExpr,
    },
    TUPLE_TYPE = TupleType{
      field_types: [TypeExpr],
    },
    FN_TYPE = FnType {
        param_list: ParamTypeList,
        return_: TypeExpr,
    },
    PARAM_TYPE_LIST = ParamTypeList {
        params: [TypeExpr],
    },
    NAME_REF = NameRef {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }

        pub fn text(&self) -> Option<SmolStr>{
            self.token().map(|t| t.text().into())
        }
    },
    CASE = Case {
        subjects: [Expr],
        clauses: [Clause],
    },
    CLAUSE = Clause {
        patterns: [AlternativePattern],
        body: Expr,
    },
    AS_PATTERN = AsPattern {
        pattern: Pattern,
        // Is a pattern to make scoping it easier!
        as_name[1]: Pattern,
    },
    ALTERNATIVE_PATTERN = AlternativePattern {
        patterns: [Pattern],
    },
    PATTERN_VARIABLE = PatternVariable {
        name: Name,
    },
    VARIANT_REF = VariantRef {
        module: ModuleNameRef,
        variant: NameRef,
        field_list: VariantRefFieldList,
    },
    VARIANT_REF_FIELD_LIST = VariantRefFieldList {
        fields: [VariantRefField],
    },
    VARIANT_REF_FIELD = VariantRefField {
        label: Label,
        field: Pattern,
    },
    PATTERN_TUPLE = PatternTuple {
        field_patterns: [Pattern],
    },
    PATTERN_SPREAD = PatternSpread {
       name: Name,
    },
    PATTERN_LIST = PatternList {
        elements: [Pattern],
    },
    PATTERN_GUARD = PatternGuard {
        expr: Expr,
    },
    PATTERN_CONCAT = PatternConcat {
        string: Literal,
        name[1]: Pattern,
    },
}

pub trait HasDocParts: AstNode<Language = GleamLanguage> {
    fn doc_parts(&self) -> DocPartIter {
        DocPartIter(self.syntax().children_with_tokens())
    }

    fn doc_text(&self) -> String {
        self.doc_parts()
            .filter_map(|docpart| {
                let slashes = number_of_slashes(&docpart).clone();
                Some(docpart.text()[slashes?..].to_string())
            })
            .join("\n")
    }
}

fn number_of_slashes(token: &SyntaxToken) -> Option<usize> {
    match token.kind() {
        COMMENT => Some(2),
        COMMENT_STATEMENT => Some(3),
        COMMENT_MODULE => Some(4),
        _ => None,
    }
}

pub struct DocPartIter(SyntaxElementChildren);

impl Iterator for DocPartIter {
    type Item = SyntaxToken;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.find_map(|nt| match (nt.kind(), nt) {
            (kind, NodeOrToken::Token(t)) if kind.is_doc() => Some(t),
            _ => None,
        })
    }
}

impl Name {
    pub fn missing() -> SmolStr {
        "[missing]".into()
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::tests::parse;

    trait HasSyntaxNode {
        fn has_syntax_node(&self) -> &SyntaxNode;
    }

    trait AstTest {
        fn should_eq(&self, expect: &str);
    }

    impl AstTest for SyntaxNode {
        #[track_caller]
        fn should_eq(&self, expect: &str) {
            assert_eq!(self.to_string().trim(), expect);
        }
    }

    impl AstTest for SyntaxToken {
        #[track_caller]
        fn should_eq(&self, expect: &str) {
            assert_eq!(self.to_string(), expect);
        }
    }

    #[test]
    fn apply() {
        let e = parse::<PatternTuple>("fn main() { let #(1,2) = #(1,2) }");
        println!("{:?}", e.field_patterns().next().unwrap().syntax());
        // println!("{:?}", e.statements().next().unwrap().syntax());
    }

    #[test]
    fn assert() {
        let e = crate::parse_module("pub fn pat(a) { let Pat(name: name, age: age) = a \n name }");
        for error in e.errors() {
            println!("{}", error);
        }
        println!("{:#?}", e.syntax_node())
    }

    #[test]
    fn pub_const() {
        let e = parse::<ModuleConstant>("pub const a = \"123\"");
        e.name().unwrap().syntax().should_eq("a");
        assert!(e.is_public());
    }

    #[test]
    fn const_tuple() {
        let e = parse::<Tuple>("const a = #(#(2,3),2)");
        let mut iter = e.fields();
        iter.next().unwrap().syntax().should_eq("#(2,3)");
        iter.next().unwrap().syntax().should_eq("2");
        assert!(iter.next().is_none())
    }

    #[test]
    fn module() {
        let e = parse::<SourceFile>(
            "@target(erlang)\nconst a = 1 const b = 2 @target(javascript) const c = 3",
        );
        let mut iter = e.statements();
        iter.next()
            .unwrap()
            .syntax()
            .should_eq("@target(erlang)\nconst a = 1");
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }

    #[test]
    fn fn_type_ann() {
        let e = parse::<FnType>("const a: fn(Int, String) -> Cat = 1");
        e.return_().unwrap().syntax().should_eq("Cat");
        let mut iter = e.param_list().unwrap().params();
        iter.next().unwrap().syntax().should_eq("Int");
        iter.next().unwrap().syntax().should_eq("String");
    }

    #[test]
    fn pattern_spread() {
        let e = parse::<Pattern>("fn spread() { case [] { [..name] -> name } }");
        e.syntax().should_eq("[..name]");
        match e {
            Pattern::PatternList(list) => match list.elements().next().unwrap() {
                Pattern::PatternSpread(spread) => spread.name().unwrap().syntax().should_eq("name"),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn type_variant() {
        let e = parse::<Adt>(
            "type Wobbles {
            Alot(name: Int, Int)
            Of(String)
        }",
        );
        e.name().unwrap().syntax().should_eq("Wobbles");
        let mut iter = e.constructors();
        let variant = iter.next().unwrap();
        variant.name().unwrap().syntax().should_eq("Alot");
        let mut fields = variant.field_list().unwrap().fields();
        let first_field = fields.next().unwrap();
        let _ = fields.next().is_some();
        first_field.label().unwrap().syntax().should_eq("name");
        first_field.type_().unwrap().syntax().should_eq("Int");
        let _ = fields.next().is_some();
        let _ = fields.next().is_none();
    }

    #[test]
    fn type_variant_generic() {
        let e = parse::<Adt>(
            "type Wobbles(a,b) {
            Alot(name: Int, Int)
            Of(String)
        }",
        );
        e.name().unwrap().syntax().should_eq("Wobbles");
    }

    #[test]
    fn opaque_type() {
        let e = parse::<TypeAlias>("pub type Bla = Bla");
        e.name().unwrap().syntax().should_eq("Bla");
    }

    #[test]
    fn tuple_type_ann() {
        let e = parse::<TupleType>("const a: #(Int, String) = 1");
        let mut iter = e.field_types();
        iter.next().unwrap().syntax().should_eq("Int");
        iter.next().unwrap().syntax().should_eq("String");
    }

    #[test]
    fn constructor_module_type() {
        let e = parse::<ModuleConstant>("const a: gleam.Int = 1");
        e.annotation().unwrap().syntax().should_eq("gleam.Int")
    }

    #[test]
    fn module_constructor_type() {
        let e = parse::<TypeNameRef>("const a : gleam.Int = 1");
        e.constructor_name().unwrap().syntax().should_eq("Int");
        e.module().unwrap().syntax().should_eq("gleam");
    }

    #[test]
    fn import_module() {
        let e = parse::<Import>("import aa/a");
        let module_path = e.module_path();
        let mut iter = module_path.unwrap().path();
        iter.next().unwrap().syntax().should_eq("aa");
        iter.next().unwrap().syntax().should_eq("a");
        assert!(iter.next().is_none());
    }

    #[test]
    fn import_unqualified() {
        let e = parse::<Import>("import aa/a.{m as a, M as A}");
        let mut iter = e.unqualified();
        let fst = iter.next().unwrap();
        let snd = iter.next().unwrap();

        fst.as_name().unwrap().syntax().should_eq("a");
        fst.name().unwrap().syntax().should_eq("m");
        snd.as_name().unwrap().syntax().should_eq("A");
        snd.name().unwrap().syntax().should_eq("M");
        assert!(iter.next().is_none());
    }

    #[test]
    fn import_qualified_as() {
        let e = parse::<Import>("import aa/a.{m as a, M as A} as e");

        let str = e
            .module_path()
            .unwrap()
            .path()
            .filter_map(|t| Some(t.token()?.text().to_string()))
            .collect::<Vec<_>>()
            .join("/");
        assert_eq!(str, "aa/a");
        e.as_name().unwrap().syntax().should_eq("e");
    }

    #[test]
    fn function_parameters() {
        let e = parse::<Function>("fn main(a b: Int) -> fn(Int) -> Int {}");
        e.name().unwrap().syntax().should_eq("main");
        e.return_type()
            .unwrap()
            .syntax()
            .should_eq("fn(Int) -> Int");
        let mut params = e.param_list().unwrap().params();
        let fst = params.next().unwrap();
        fst.label().unwrap().syntax().should_eq("a");
        fst.pattern().unwrap().syntax().should_eq("b");
        fst.ty().unwrap().syntax().should_eq("Int");
        assert!(params.next().is_none())
    }

    #[test]
    fn function_docs() {
        let e = parse::<Function>("///123\n \n ///abc\n fn main(a b: Int) -> fn(Int) -> Int {}");

        let mut doc_iter = e.doc_parts();
        doc_iter.next().unwrap().should_eq("///123");
        doc_iter.next().unwrap().should_eq("///abc");
    }

    #[test]
    fn name() {
        let e = parse::<Param>("fn bla(a b: Int) {}");
        e.pattern().unwrap().syntax().should_eq("b");
    }

    #[test]
    fn type_name() {
        let e = parse::<Variant>(
            "type Wobble(a) {
            Wobble1(a)
        }",
        );
        e.name().unwrap().syntax().should_eq("Wobble1");
    }

    #[test]
    fn constructor_field() {
        let e = parse::<VariantField>(
            "type Wobble(a) {
            Wobble1(a: int.Wobbles)
        }",
        );
        e.label().unwrap().syntax().should_eq("a");
        e.type_().unwrap().syntax().should_eq("int.Wobbles")
    }

    #[test]
    fn block() {
        let e = parse::<Block>("fn b() { 1 }");
        let mut exprs = e.expressions();
        exprs.next().unwrap().syntax().should_eq("1")
    }

    #[test]
    fn binary_op() {
        let e = parse::<BinaryOp>("fn b() { 1 * 2 + 3 }");
        e.lhs().unwrap().syntax().should_eq("1 * 2");
        e.rhs().unwrap().syntax().should_eq("3");

        let e2 = parse::<BinaryOp>("fn b() { 1 + 2 * 3 }");
        e2.lhs().unwrap().syntax().should_eq("1");
        e2.rhs().unwrap().syntax().should_eq("2 * 3")
    }

    #[test]
    fn type_application() {
        let e = parse::<Variant>("type Bla { Bla2(#(Int)) }");
        e.field_list()
            .iter()
            .next()
            .unwrap()
            .syntax()
            .should_eq("(#(Int))");
    }

    #[test]
    fn unary_op() {
        let e = parse::<UnaryOp>("fn a() { -1 }");
        assert_eq!(e.op_kind(), Some(UnaryOpKind::Negate));
        e.op_token().unwrap().should_eq("-");
        e.arg().unwrap().syntax().should_eq("1");
    }

    #[test]
    fn let_expr() {
        let e = parse::<StmtLet>("fn a() { let name:Int = 1}");
        e.annotation().unwrap().syntax().should_eq("Int");
        e.body().unwrap().syntax().should_eq("1")
    }

    #[test]
    fn call_expr() {
        let e = parse::<StmtExpr>("fn main() { abc(name: 1, 2) }");
        match e.expr().unwrap() {
            Expr::ExprCall(expr) => {
                expr.syntax().should_eq("abc(name: 1, 2)");
                expr.func().unwrap().syntax().should_eq("abc");
                let mut args = expr.arguments().unwrap().args();
                let first = args.next().unwrap();
                first.label().unwrap().syntax().should_eq("name");
                first.value().unwrap().syntax().should_eq("1");
                args.next().unwrap().syntax().should_eq("2");
            }
            _ => panic!(),
        }
    }

    #[test]
    fn literal() {
        let e = parse::<Literal>("fn a() { 1 }");
        assert_eq!(e.kind(), Some(LiteralKind::Int));
    }

    #[test]
    fn case_expr() {
        let e = parse::<Case>("fn a() { case wobble, 1 + 7 { Cat, Dog -> 1 }}");
        let mut subs = e.subjects();
        subs.next().unwrap().syntax().should_eq("wobble");
        subs.next().unwrap().syntax().should_eq("1 + 7");
        assert!(subs.next().is_none());
        let mut clauses = e.clauses();
        clauses.next().unwrap().syntax().should_eq("Cat, Dog -> 1")
    }

    #[test]
    fn clause() {
        let c = parse::<Clause>(
            "fn a() { 
                    case wobble, 1 + 7 
                    { 
                        Bird | Snake, a -> 2
                        Cat, Dog -> 1 
                    }}",
        );
        c.syntax().should_eq("Bird | Snake, a -> 2");
        c.patterns()
            .next()
            .unwrap()
            .syntax()
            .should_eq("Bird | Snake");
        c.body().unwrap().syntax().should_eq("2");
        let mut pats = c.patterns();
        pats.next().unwrap().syntax().should_eq("Bird | Snake");
        pats.next().unwrap().syntax().should_eq("a");
    }

    #[test]
    fn clause_guards() {
        let clause = parse::<Clause>(
            " fn guards(a: String) {
            case 1 {
                b if b == a -> 1
            }
        }",
        );
        let mut pats = clause.patterns();
        pats.next().unwrap().syntax().should_eq("b");
        assert!(pats.next().is_none());
        clause.body().unwrap().syntax().should_eq("1")
    }

    #[test]
    fn alt_pattern() {
        let a = parse::<AlternativePattern>(
            "fn a() { 
                    case wobble
                    { 
                        Bird | Snake -> 2
                    }}",
        );
        a.syntax().should_eq("Bird | Snake");
    }

    #[test]
    fn bit_array() {
        let _b = parse::<BitArray>("fn a() { <<a:size(0)>> <<a:8, rest:bit_array>> }");
    }

    #[test]
    fn pattern() {
        let p = parse::<AlternativePattern>(
            "fn a() { 
                    case wobble, 1 + 7 
                    { 
                        int.Bla(Some(a)), 1 -> 2
                    }}",
        );
        p.syntax().should_eq("int.Bla(Some(a))");
        let pattern = VariantRef::cast(p.patterns().next().unwrap().syntax().clone()).unwrap();
        pattern
            .field_list()
            .unwrap()
            .fields()
            .next()
            .unwrap()
            .syntax()
            .should_eq("Some(a)");
        pattern.module().unwrap().syntax().should_eq("int");
    }

    #[test]
    fn use_() {
        let p = parse::<StmtUse>(
            "fn a() { 
                use manager: Int, a <- result.try(
                    start_manager()
                )
            }",
        );
        p.assignments()
            .next()
            .unwrap()
            .syntax()
            .should_eq("manager: Int");
    }

    #[test]
    fn constructor_fiels() {
        let e = parse::<VariantRef>("fn pat(a) { let Pat(name_ref: name, age: age) = a \n name }");
        let mut fields = e.field_list().unwrap().fields().into_iter();

        fields
            .next()
            .unwrap()
            .label()
            .unwrap()
            .syntax()
            .should_eq("name_ref");
    }

    #[test]
    fn field_access() {
        let f = parse::<FieldAccessExpr>("fn wops() { Mogie(name: 1).name}");
        f.label().unwrap().syntax().should_eq("name");
        f.base().unwrap().syntax().should_eq("Mogie(name: 1)");

        let f = parse::<FieldAccessExpr>("fn wops() { base.label}");
        f.label().unwrap().syntax().should_eq("label");
        f.base().unwrap().syntax().should_eq("base")
    }

    #[test]
    fn variant_constructor() {
        let f = parse::<VariantConstructor>("fn fields() { Muddle(name: 5) }");
        // f.args().unwrap().syntax().should_eq("(name: 5)");
        f.name().unwrap().syntax().should_eq("Muddle");
    }

    #[test]
    fn todo_test() {
        let _p = parse::<Block>(
            "pub fn todoo() -> Nil {
            todo
        }",
        );
    }

    #[test]
    fn as_pattern() {
        let p = parse::<AsPattern>(
            "fn todoo() {
            case 1 {
                5 as b -> b
            }
        }",
        );
        p.as_name().unwrap().syntax().should_eq("b")
    }

    #[test]
    fn adt_generic_params() {
        let adt = parse::<Adt>("type List(a) { Cons(a) Nil }");

        let params = adt.generic_params().unwrap();
        params.syntax().should_eq("(a)");
        params.params().next().unwrap().syntax().should_eq("a");
    }

    #[test]
    fn pattern_concat() {
        let conc = parse::<PatternConcat>(
            r#"fn main() {
            case "" {
              "abc" <> asdf -> asdf
            }
          }"#,
        );
        conc.name().unwrap().syntax().should_eq("asdf");
    }

    #[test]
    fn alias_body() {
        let alias = parse::<TypeAlias>(r#"type Alias = String"#);
        alias.name().unwrap().syntax().should_eq("Alias");
        alias.type_().unwrap().syntax().should_eq("String");
    }

    #[test]
    fn allow_discard_argument() {
        let _ = parse::<Function>(
            r#"
            fn main(asdf _: a, age age: b) -> Result(a, b) {
                accepted()
            }"#,
        );
    }

    #[test]
    fn panic_as() {
        let _ = parse::<Function>(
            r#"
            fn main(b) -> Result(a, b) {
                panic as { "abc" <> "123" }
            }"#,
        );
    }

    // https://github.com/maurobalbi/glas/issues/10
    #[test]
    fn allow_true_false_constructor() {
        let _ = parse::<Adt>(
            r#"
        pub type Test {
            Foo
            True
            False
        }"#,
        );
    }
}
