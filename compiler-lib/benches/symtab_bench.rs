use criterion::{criterion_group, criterion_main, Criterion};

use compiler_lib::symtab::Scoped;

fn scope_walk(input: &'_ usize) {
    let defs: Vec<String> = (0..*input).map(|i| format!("scope#{}", i)).collect();
    let mut scoped = Scoped::new();
    for def in defs.iter() {
        scoped.define(def, ()).unwrap();
        scoped.enter_scope();
    }
    for depth in (0..*input).rev() {
        let def = &defs[depth];
        scoped.visible_definition(def);
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function_over_inputs(
        "scope_walk",
        |bencher, input| bencher.iter(|| scope_walk(input)),
        vec![1, 10, 50, 100, 500],
    );
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
