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
            .map(|LexToken { kind, text, .. }| format!("{kind:?} {text:?}\n"))
            .collect::<Vec<_>>()
            .join("");
        expect.assert_eq(&out);
    }

    #[test]
    fn it_works() {
        let _blu = 0b1;
        const INPUT: &str = "Lexers.";
        let lex = SyntaxKind::lexer(INPUT);

        println!(
            "{:?}",
            lex.chain(std::iter::once(SyntaxKind::SOURCE_FILE))
                .collect::<Vec<SyntaxKind>>()
        );

        #[cfg(feature = "comparison")]
        {
            use gleam_core::parse::lexer::make_tokenizer;
            println!("{:?}", make_tokenizer(INPUT).collect::<Vec<_>>());
        }
    }

    #[test]
    fn string() {
        check_lex(
            "\"abc 1 A",
            expect![[r#"
            ERROR "\""
            IDENT "abc"
            WHITESPACE " "
            INTEGER "1"
        "#]],
        )
    }

    #[test]
    fn target_group() {
        check_lex(
            "if erlang { pub const a = \"123\" }",
            expect![[r#"
                IF_KW "if"
                WHITESPACE " "
                IDENT "erlang"
                WHITESPACE " "
                L_BRACE "{"
                WHITESPACE " "
                PUB_KW "pub"
                WHITESPACE " "
                CONST_KW "const"
                WHITESPACE " "
                IDENT "a"
                WHITESPACE " "
                EQ "="
                WHITESPACE " "
                STRING "\"123\""
                WHITESPACE " "
                R_BRACE "}"
            "#]],
        );
    }
}
