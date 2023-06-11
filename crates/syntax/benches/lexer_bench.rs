use criterion::{black_box, criterion_group, criterion_main, Criterion};
use syntax::lexer::GleamLexer;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("lexer", |b| {
        b.iter(|| GleamLexer::new(black_box("This is a test file!!")).collect::<Vec<_>>())
    });

}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
