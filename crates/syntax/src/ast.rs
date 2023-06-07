use crate::SyntaxKind::{self, *};
use crate::{GleamLanguage, SyntaxNode, SyntaxToken};
use rowan::ast::support::{child, children};
use rowan::NodeOrToken;

pub use rowan::ast::{AstChildren, AstNode};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
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
    },
    ConstantValue {
        Literal,
        ConstantTuple,
        ConstantList,
    },
    Statement {
        Assignment,
    },
    Expr {
        Literal,
        Block,
        NameRef,
        BinaryOp,
        UnaryOp,
    },
    Type {
        FnType,
        VarType,
        TupleType,
        ConstructorType,
    },
}

asts! {
    LET_EXPR = Assignment {
        //pattern
        annotation: Type,
        value: Expr,
    },
    BLOCK = Block {
        expressions: [Expr],
    },
    BINARY_OP = BinaryOp {
        lhs: Expr,
        rhs[1]: Expr,

        pub fn op_details(&self) -> Option<(SyntaxToken, BinaryOpKind)> {
            self.syntax().children_with_tokens().find_map(|n| {
                let tok = n.into_token()?;
                let op = match tok.kind() {
                    T!["+"] => BinaryOpKind::Add,
                    T!["-"] => BinaryOpKind::Sub,
                    T!["*"] => BinaryOpKind::Mul,
                    T!["/"] => BinaryOpKind::Div,
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
    CONSTANT_LIST = ConstantList {
        elements: [ConstantValue],
    },
    LITERAL = Literal {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
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
    FUNCTION = Function {
        name: Name,
        param_list: ParamList,
        return_type: Type,
        body: Expr,
    },
    FIELD_ACCESS = FieldAccess {
        label: NameRef,
        container: Expr,
    },
    TUPLE_INDEX = TupleIndex {
        index: Literal,
        container: Expr,
    },
    IMPORT = Import {
        module: ImportModule,
    },
    IMPORT_MODULE = ImportModule {
        module_path: [Path],
        as_name: Name,
        unqualified: [UnqualifiedImport],
    },
    SOURCE_FILE = SourceFile {
        statements: [TargetGroup],
    },
    MODULE_NAME = ModuleName {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }
    },
    // Change to body with expression to be able to reuse parser / collecting logic and validate constant during lowering
    MODULE_CONSTANT = ModuleConstant {
        name: Name,
        value: ConstantValue,
        annotation: Type,
        pub fn is_public(&self) -> bool {
            self.syntax().children_with_tokens().find(|it| it.kind() == T!["pub"]).is_some()
        }
    },
    NAME = Name {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }
    },
    LABEL = Label {
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
      name: Name,
      as_name[1]: Name,
    },
    PARAM = Param {
        // pat: Pat,
        name: Name,
        label: Label,
        ty: Type,
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
    TARGET = Target {
        name: Name,
    },
    TARGET_GROUP = TargetGroup {
        target: Target,
        statements: [ModuleStatement],
    },
    CONSTANT_TUPLE = ConstantTuple {
        elements: [ConstantValue],
    },
    CONSTRUCTOR_TYPE = ConstructorType {
      constructor: Name,
      module: ModuleName,
    },
    TUPLE_TYPE = TupleType{
      field_types: [Type],
    },
    FN_TYPE = FnType {
        param_list: ParamList,
        return_: Type,
    },
    VAR_TYPE = VarType {
        name: Name,
    },
    NAME_REF = NameRef {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }
    },
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
        let e = parse::<Param>("fn main(a b: Int) -> fn(Int) -> Int {}");
        println!("{:?}", e.name().unwrap().token().unwrap().text());
        // println!("{:?}", e.statements().next().unwrap().syntax());
    }

    #[test]
    fn assert() {
        let e = crate::parse_file(
            "
                //// asdfttt

                /// asdfasdfasdf
             
                /// asdf
                fn asdf() {}
            ",
        );
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
        let e = parse::<ConstantTuple>("const a = #(#(2,3),2)");
        let mut iter = e.elements();
        iter.next().unwrap().syntax().should_eq("#(2,3)");
        iter.next().unwrap().syntax().should_eq("2");
        assert!(iter.next().is_none())
    }

    #[test]
    fn module() {
        let e =
            parse::<SourceFile>("if erlang {const a = 1} const b = 2 if javascript {const c = 3}");
        let mut iter = e.statements();
        iter.next()
            .unwrap()
            .syntax()
            .should_eq("if erlang {const a = 1}");
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }

    #[test]
    fn target_group() {
        let e =
            parse::<TargetGroup>("if erlang {const a = 1} const b = 2 if javascript {const c = 3}");
        e.target().unwrap().syntax().should_eq("erlang");
        let mut iter = e.statements();
        iter.next().unwrap().syntax().should_eq("const a = 1");
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
        let e = parse::<ConstructorType>("const a : gleam.Int = 1");
        e.constructor().unwrap().syntax().should_eq("Int");
        e.module().unwrap().syntax().should_eq("gleam");
    }

    #[test]
    fn import_module() {
        let e = parse::<ImportModule>("import aa/a");
        let mut iter = e.module_path();
        iter.next().unwrap().syntax().should_eq("aa");
        iter.next().unwrap().syntax().should_eq("a");
        assert!(iter.next().is_none());
    }

    #[test]
    fn import_unqualified() {
        let e = parse::<ImportModule>("import aa/a.{m as a, M as A}");
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
        let e = parse::<ImportModule>("import aa/a.{m as a, M as A} as e");

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
        fst.name().unwrap().syntax().should_eq("b");
        fst.ty().unwrap().syntax().should_eq("Int");
        assert!(params.next().is_none())
    }

    #[test]
    fn name() {
        let e = parse::<Param>("fn bla(a b: Int) {}");
        e.name().unwrap().token().unwrap().should_eq("b");
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
    fn unary_op() {
        let e = parse::<UnaryOp>("fn a() { -1 }");
        assert_eq!(e.op_kind(), Some(UnaryOpKind::Negate));
        e.op_token().unwrap().should_eq("-");
        e.arg().unwrap().syntax().should_eq("1");
    }

    #[test]
    fn let_expr() {
        let e = parse::<Assignment>("fn a() { let name:Int = 1}");
        e.annotation().unwrap().syntax().should_eq("Int")
    }
}
