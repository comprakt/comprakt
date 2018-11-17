use criterion::{criterion_group, criterion_main, Criterion};

use compiler_lib::symtab::Scoped;

fn scope_walk(scoped: &Scoped<&String, ()>, defs: &[String]) {
    let rootdef = &defs[0];
    // ask for rootdef from innermost scope
    // a naive implementation would be in O(N) where N is number of nested scopes
    for _ in (0..defs.len()).rev() {
        let res = scoped.visible_definition(rootdef);
        assert!(res.is_some());
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function_over_inputs(
        "scope_walk",
        |bencher, input| {
            let defs: Vec<String> = (0..*input).map(|i| format!("scope#{}", i)).collect();
            let mut scoped = Scoped::new();
            for def in defs.iter() {
                scoped.define(def, ()).unwrap();
                scoped.enter_scope();
            }
            bencher.iter(|| scope_walk(&scoped, &defs));
        },
        vec![1, 10, 50, 100, 500],
    );
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
