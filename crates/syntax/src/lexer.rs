use crate::kind::SyntaxKind;
pub use logos::{Lexer, Logos};
use rowan::{TextRange, TextSize};
use std::ops::Range as StdRange;

pub fn lex_string(lex: &mut Lexer<SyntaxKind>) -> bool {
    let remainder: &str = lex.remainder();
    let mut escaped = false;

    let mut total_len = 0;

    for c in remainder.chars() {
        total_len += c.len_utf8();

        if c == '\\' {
            escaped = !escaped;
            continue;
        }

        if c == '"' && !escaped {
            lex.bump(remainder[0..total_len].as_bytes().len());
            return true;
        }

        escaped = false;
    }
    false
}

pub struct GleamLexer<'a> {
    inner: logos::Lexer<'a, SyntaxKind>,
}

impl<'a> GleamLexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: SyntaxKind::lexer(input),
        }
    }
}

impl<'a> Iterator for GleamLexer<'a> {
    type Item = LexToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        let range = {
            let StdRange { start, end } = self.inner.span();
            let start = TextSize::try_from(start).unwrap();
            let end = TextSize::try_from(end).unwrap();

            TextRange::new(start, end)
        };

        Some(Self::Item { kind, text, range })
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LexToken<'a> {
    pub kind: SyntaxKind,
    pub text: &'a str,
    pub range: TextRange,
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_lex(src: &str, expect: Expect) {
        let toks = GleamLexer::new(src);
        let out = toks
            .into_iter()
            .map(|LexToken { kind, text, .. }| format!("{:?} {:?}\n", kind, text))
            .collect::<Vec<_>>()
            .join("");
        expect.assert_eq(&out);
    }

    #[test]
    fn it_works() {
        const INPUT: &str = "Lexers.";
        let lex = SyntaxKind::lexer(INPUT);

        println!(
            "{:?}",
            lex.chain(std::iter::once(SyntaxKind::MODULE))
                .collect::<Vec<SyntaxKind>>()
        );

        #[cfg(feature = "comparison")]
        {
            use gleam_core::parse::lexer::make_tokenizer;
            println!("{:?}", make_tokenizer(INPUT).collect::<Vec<_>>());
        }
    }

    #[test]
    fn path() {
        check_lex(
            "<nixpkgs/pkgs> <nixpkgs/ > a/b a/ b a /b",
            expect![[r#"
                LESS "<"
                IDENT "nixpkgs"
                SLASH "/"
                IDENT "pkgs"
                GREATER ">"
                WHITESPACE " "
                LESS "<"
                IDENT "nixpkgs"
                SLASH "/"
                WHITESPACE " "
                GREATER ">"
                WHITESPACE " "
                IDENT "a"
                SLASH "/"
                IDENT "b"
                WHITESPACE " "
                IDENT "a"
                SLASH "/"
                WHITESPACE " "
                IDENT "b"
                WHITESPACE " "
                IDENT "a"
                WHITESPACE " "
                SLASH "/"
                IDENT "b"
            "#]],
        );
    }
}
