use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use syntax::lexer::GleamLexer;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("lexer", |b| {
        b.iter(|| GleamLexer::new(black_box("This is a test file!!")).collect::<Vec<_>>())
    });

    #[cfg(feature = "comparison")]
    {
        use gleam_core::parse::lexer::make_tokenizer;
        let mut group = c.benchmark_group("lexers");
        for i in ["1//this ()is a comment\n//more comment\n()Create ridiculously fast Lexers.23123 1231 23123124 124 124 1", "fast ist the impossible"].iter() {
          group.bench_with_input(BenchmarkId::new("Logox Lexer", i), i, 
              |b, i| b.iter(|| lexer::LexToken::lexer(*i)));
          group.bench_with_input(BenchmarkId::new("Gleam hand-crafted lexer", i), i, 
              |b, i| b.iter(|| make_tokenizer(*i)));
      }
        group.finish();
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
