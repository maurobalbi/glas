use crate::SyntaxKind::{self, *};
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
    String,
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
    ConstantValue {
      Literal,
      Tuple,
      List,
    },
}

asts! {
    ANNOTATION = Annotation {
        type_: Name,
    },
    LIST = List {
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
    MODULE = Module {
        statements: [TargetGroup],
    },
    MODULE_CONSTANT = ModuleConstant {
        name: Name,
        value: ConstantValue,
        pub fn is_public(&self) -> bool {
          self.syntax().children_with_tokens().find(|it| it.kind() == T!["pub"]).is_some()
        }
    },
    NAME = Name {
        pub fn token(&self) -> Option<SyntaxToken> {
            self.0.children_with_tokens().find_map(NodeOrToken::into_token)
        }
    },
    TARGET = Target {
      name: Name,
    },
    TARGET_GROUP = TargetGroup {
        target: Target,
        statements: [Statement],
    },
    TUPLE = Tuple {
        elements: [ConstantValue],
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
        let e = parse::<Module>("if erlang { const a = 1 } const b = 2");
        println!("{:#?}", e.syntax());
        // println!("{:?}", e.statements().next().unwrap().syntax());
    }

    #[test]
    fn assert() {
        let e = crate::parse_file("pub const a = #(1,2)");
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
        // e.equal_token().unwrap().should_eq("=");
        // e.value().unwrap().syntax().should_eq("1");
        // e.semicolon_token().unwrap().should_eq(";");
    }
    
    #[test]
    fn const_tuple() {
        let e = parse::<Tuple>("const a = #(#(2,3),2)");
        let mut iter = e.elements();
        // assert!(!// // // // // // // // );
        iter.next().unwrap().syntax().should_eq("#(2,3)");
        iter.next().unwrap().syntax().should_eq("2");
        assert!(iter.next().is_none())
        // e.equal_token().unwrap().should_eq("=");
        // e.value().unwrap().syntax().should_eq("1");
        // e.semicolon_token().unwrap().should_eq(";");
    }


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
