//! #8302: invoking a `Callable` must not re-intern its own names once per call.
//!
//! A closure call named everything it touched through the *string*-keyed APIs:
//! the binder bound each parameter by name, the exit writeback rebuilt the
//! parameter-name set by interning every `SubData::params` entry, the
//! locals seed/flush loops probed `Env` by name, the defining file was interned
//! for the routine frame, and four `__mutsu_*` dispatcher-metadata probes were
//! spelled as literals. A `-> $a { $a }` called in a loop therefore performed
//! **18** `Symbol::intern` calls per invocation against a named sub's 2 —
//! callgrind put `Symbol::intern` plus its thread-local memo at ~16% of the
//! whole benchmark.
//!
//! The pin is deterministic rather than timed, the same way
//! `tests/regex_match_intern_budget.rs` pins the smartmatch path:
//! `symbol::intern_calls()` is an exact per-thread counter, so this measures
//! the *shape* of the cost with no dependence on machine speed or load. It is a
//! budget, not an equality — the point is that the per-call interning is gone,
//! not that every caller is frozen.

/// Interns performed while running `src`, measured after a warm-up run so that
/// one-time interning (parsing, compiling, first pass through each code path)
/// is not counted.
fn interns_for(src: &str) -> u64 {
    let mut warm = mutsu::Interpreter::new();
    warm.run(src).expect("program runs");
    drop(warm);

    let before = mutsu::symbol::intern_calls();
    let mut interp = mutsu::Interpreter::new();
    interp.run(src).expect("program runs");
    mutsu::symbol::intern_calls() - before
}

/// Interns attributable to one extra iteration of `body`, isolated from the
/// program's fixed startup cost by running the same loop at two lengths. The
/// preamble declares one of each callable shape so that the *creation* of the
/// closure — a different cost, and not what this file is about — happens once,
/// outside the loop.
fn interns_per_iteration(body: &str) -> f64 {
    const LOW: usize = 100;
    const HIGH: usize = 1100;
    let program = |n: usize| {
        format!(
            "my $s = 0; \
             sub named($a) {{ $a }}; \
             my $pointy = -> $a {{ $a }}; \
             my $bare = {{ $_ }}; \
             for ^{n} {{ {body} }}; \
             say $s;"
        )
    };
    let lo = interns_for(&program(LOW));
    let hi = interns_for(&program(HIGH));
    assert!(
        hi >= lo,
        "interning cannot shrink with more iterations: {lo} -> {hi}"
    );
    (hi - lo) as f64 / (HIGH - LOW) as f64
}

/// The loop body without the call: the same assignment, the same operand.
/// Subtracting it leaves the invocation's own interning.
const CONTROL: &str = "$s = $s;";

fn interns_per_call(body: &str) -> f64 {
    interns_per_iteration(body) - interns_per_iteration(CONTROL)
}

#[test]
fn pointy_block_call_does_not_intern_its_parameter_names() {
    let per_call = interns_per_call("$s = $pointy($s);");
    eprintln!("`$pointy($s)`: {per_call:.3} interns per call");
    // Measured: 18 before #8302, 2 after. The sixteen that went were the
    // parameter name `a` (interned eight times over: the binder, the readonly
    // mark, the writeback's parameter-name set, and three by-name `Env`
    // probes), the defining file, `@_`, and the four `__mutsu_*` dispatcher
    // metadata keys.
    // #7766's assertion round took it further: the remaining one was the
    // parameter name, re-interned by `baked_param_name_sym`'s own
    // `debug_assert!`. Measured 1.0 now, in debug and release alike.
    assert!(
        per_call <= 3.0,
        "a closure call re-interns its own names per call \
         ({per_call:.3} interns/call, budget 3, was 18 before #8302); see #7766"
    );
}

#[test]
fn closure_call_is_not_costlier_than_a_named_sub_call_in_interning() {
    let closure = interns_per_call("$s = $pointy($s);");
    let named = interns_per_call("$s = named($s);");
    eprintln!("closure {closure:.3} vs named sub {named:.3} interns per call");
    // The inversion this issue was filed for: rakudo makes a closure call
    // CHEAPER than a named sub call (it skips multi-dispatch), while mutsu made
    // it 9x dearer in interning alone. Holding the two within a couple of
    // interns of each other is what stops that regressing.
    assert!(
        closure <= named + 3.0,
        "a closure call interns {closure:.3} per call against a named sub's \
         {named:.3}; the two paths should cost the same in interning (#8302)"
    );
}

#[test]
fn bare_block_call_does_not_intern_its_topic_handling() {
    let per_call = interns_per_call("$s = $bare($s);");
    eprintln!("`$bare($s)`: {per_call:.3} interns per call");
    // A bare block binds `$_` rather than a named parameter, so it never paid
    // the parameter-name bucket — but it did pay the metadata probes and the
    // defining-file intern. Measured: 6 before, 1 after.
    // The last one was the defining-file intern, which was
    // `current_source_file_sym`'s own `debug_assert!` re-deriving `?FILE`.
    // Measured 0.0 now.
    assert!(
        per_call <= 2.0,
        "a bare-block call re-interns a fixed env key per call \
         ({per_call:.3} interns/call, budget 2, was 6 before #8302); see #7766"
    );
}
