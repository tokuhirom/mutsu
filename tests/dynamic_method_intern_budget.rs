//! #8998: a dynamic method spelling is interned once per dispatch, not once
//! per native probe or hyper target element.

fn interns_for(src: &str) -> u64 {
    let mut warm = mutsu::Interpreter::new();
    warm.run(src).expect("warm program runs");
    drop(warm);
    let before = mutsu::symbol::intern_calls();
    let mut interp = mutsu::Interpreter::new();
    interp.run(src).expect("program runs");
    mutsu::symbol::intern_calls() - before
}

fn slope(program: impl Fn(usize) -> String) -> f64 {
    let low = interns_for(&program(100));
    let high = interns_for(&program(1100));
    (high - low) as f64 / 1000.0
}

#[test]
fn dynamic_hyper_method_does_not_intern_per_element() {
    let per_element = slope(|n| {
        format!(
            "my $method = 'elems'; my @items = (1..{n}); my @out = @items>>.\"$method\"(); say @out.elems;"
        )
    });
    eprintln!("dynamic hyper method: {per_element:.3} interns per element");
    assert!(
        per_element <= 0.01,
        "dynamic hyper method re-interns its name per element ({per_element:.3}); see #8998"
    );
}

#[test]
fn dynamic_method_interns_its_spelling_once_per_dispatch() {
    let per_call = slope(|n| {
        format!(
            "my $method = 'elems'; my $hits = 0; for ^{n} {{ $hits = [1, 2].\"$method\"() }}; say $hits;"
        )
    });
    eprintln!("dynamic method: {per_call:.3} interns per call");
    assert!(
        per_call <= 1.01,
        "dynamic method re-interns its spelling within one dispatch ({per_call:.3}); see #8998"
    );
}
