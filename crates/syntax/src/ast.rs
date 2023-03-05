use crate::SyntaxKind::{self};
use crate::{GleamLanguage, SyntaxNode, SyntaxToken};
use rowan::ast::support::{child, children};
use rowan::NodeOrToken;

pub use rowan::ast::{AstChildren, AstNode};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOpKind {
    Imply,
    Or,
    And,

    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    Update,
    Concat,

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
    Uri,
    Path,
    SearchPath,
}

trait NodeWrapper {
    const KIND: SyntaxKind;
}

macro_rules! enums {
    ($($name:ident { $($variant:ident,)* },)*) => {
        $(
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        pub enum $name {
            $($variant($variant),)*
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
                    $(<$variant as NodeWrapper>::KIND => Some(Self::$variant($variant(node))),)*
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
        pub struct $name(SyntaxNode);

        impl $name {
            ast_impl!($($impl)*);
        }

        $(impl $trait for $name {})*

        impl NodeWrapper for $name {
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
    Statement {
      ModuleConstant,
    },
}

asts! {
    MODULE = Module {
        statements: [TargetGroup],
    },
    TARGET_GROUP = TargetGroup {
      target: Target,
      statements: [Statement],
    },
    MODULE_CONSTANT = ModuleConstant {

    },
    TARGET = Target {
      name: Name,
    },
    NAME = Name {
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
        let e = parse::<Module>("1 2");
        println!("{:?}", e.syntax());
    }

    // #[test]
    // fn assert() {
    //     let e = parse::<Assert>("assert 1; 2");
    //     e.assert_token().unwrap().should_eq("assert");
    //     e.condition().unwrap().syntax().should_eq("1");
    //     e.semicolon_token().unwrap().should_eq(";");
    //     e.body().unwrap().syntax().should_eq("2");
    // }

    // #[test]
    // fn attr_path() {
    //     let e = parse::<Attrpath>(r#"{ foo."bar".${baz} = 1; }"#);
    //     let mut iter = e.attrs();
    //     iter.next().unwrap().syntax().should_eq("foo");
    //     iter.next().unwrap().syntax().should_eq(r#""bar""#);
    //     iter.next().unwrap().syntax().should_eq("${baz}");
    //     assert!(iter.next().is_none());
    // }

    // #[test]
    // fn attr_path_value() {
    //     let e = parse::<AttrpathValue>(r#"{ foo."bar".${baz} = 1; }"#);
    //     e.attrpath()
    //         .unwrap()
    //         .syntax()
    //         .should_eq(r#"foo."bar".${baz}"#);
    //     e.equal_token().unwrap().should_eq("=");
    //     e.value().unwrap().syntax().should_eq("1");
    //     e.semicolon_token().unwrap().should_eq(";");
    // }

    // #[test]
    // fn plain_attrset() {
    //     let e = parse::<AttrSet>("{ a = let { }; b = rec { }; }");
    //     assert!(e.let_token().is_none());
    //     assert!(e.rec_token().is_none());
    //     e.l_curly_token().unwrap().should_eq("{");
    //     e.r_curly_token().unwrap().should_eq("}");

    //     let mut iter = e.bindings();
    //     iter.next().unwrap().syntax().should_eq("a = let { };");
    //     iter.next().unwrap().syntax().should_eq("b = rec { };");
    // }
}
