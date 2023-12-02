#![no_main]
use libfuzzer_sys::fuzz_target;
use std::iter;

fuzz_target!(|text: &str| {
    let src = syntax::parse_module(text);
    let out = iter::successors(src.syntax_node().first_token(), |t| t.next_token())
        .map(|t| t.to_string())
        .collect::<Vec<_>>().join("");
    assert_eq!(text, out);
});
