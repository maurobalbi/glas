//! A bit-set of `SyntaxKind`s.

use crate::SyntaxKind;

/// A bit-set of `SyntaxKind`s
#[derive(Clone, Copy)]
pub(crate) struct TokenSet(u128);

impl TokenSet {
    pub(crate) const EMPTY: TokenSet = TokenSet(0);

    pub(crate) const fn new(kinds: &[SyntaxKind]) -> TokenSet {
        let mut res = 0u128;
        let mut i = 0;
        while i < kinds.len() {
            res |= mask(kinds[i]);
            i += 1;
        }
        TokenSet(res)
    }

    pub(crate) const fn union(self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    /// Tokens contained in other TokenSet without Tokens contained in self TokenSet
    pub(crate) const fn difference(self, other: TokenSet) -> TokenSet {
        TokenSet(other.0 & !(self.0 & other.0))
    }

    pub(crate) const fn contains(&self, kind: SyntaxKind) -> bool {
        self.0 & mask(kind) != 0
    }
}

const fn mask(kind: SyntaxKind) -> u128 {
    1u128 << (kind as usize)
}

#[test]
fn token_set_works_for_tokens() {
    use crate::SyntaxKind::*;
    let ts = TokenSet::new(&[EOF]);
    assert!(ts.contains(EOF));
    assert!(!ts.contains(PLUS));
}

#[test]
fn token_set_difference() {
    use crate::SyntaxKind::*;
    let ts = TokenSet::new(&[IDENT, STRING]);
    let ts2 = TokenSet::new(&[INTEGER, IDENT]);
    let diff = ts.difference(ts2);
    assert!(diff.contains(INTEGER));
    assert!(!diff.contains(STRING));
    assert!(!diff.contains(IDENT));
    assert!(!diff.contains(EOF));
}
