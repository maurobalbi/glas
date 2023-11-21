use ide::{HlTag};
use lsp_types::{SemanticTokenModifier, SemanticTokenType};

macro_rules! def_index {
    (
        $ty:ty, $array:ident, $enum:ident;
        $($ident:ident => $expr:expr,)*
    ) => {
        pub const $array: &[$ty] = &[$($expr),*];

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum $enum { $($ident),* }
    };
}

// coc.nvim's builtin highlighting groups:
// https://github.com/neoclide/coc.nvim/blob/347e33d77bf58fedd32ef4eb1dc982a11f5f0a22/plugin/coc.vim#L527
def_index! {
    SemanticTokenType, SEMANTIC_TOKEN_TYPES, TokenTypeIdx;

    Module => SemanticTokenType::NAMESPACE,
    Function => SemanticTokenType::FUNCTION,
}


def_index! {
    SemanticTokenModifier, SEMANTIC_TOKEN_MODIFIERS, TokenModIdx;

    Public => SemanticTokenModifier::new("public"),
}

impl TokenModIdx {
    pub fn to_bit(self) -> u32 {
        1 << self as u8
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TokenModSet(pub u32);

impl TokenModSet {
    pub fn insert(&mut self, i: TokenModIdx) {
        self.0 |= i.to_bit();
    }
}

pub(crate) fn to_semantic_type_and_modifiers(tag: HlTag) -> (TokenTypeIdx, TokenModSet) {
    let mut mods = TokenModSet::default();
    let ty = match tag {
        HlTag::Function => TokenTypeIdx::Function,
        HlTag::Module => TokenTypeIdx::Module,
    };
    (ty, mods)
}
