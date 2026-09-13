//! #7766 unit 2: a by-name call into a compiled routine must not re-intern the
//! callsite name and package once per call.
//!
//! `call_compiled_function_named[_inner]` took `fn_package: &str, fn_name:
//! &str` and interned both on entry, and `find_compiled_function_inner` interned
//! the same callsite name again one layer below — while every caller already
//! held the interned forms. A callsite name is a string constant with a
//! `CompiledCode::const_sym` entry, a resolved `FunctionDef` carries `package` /
//! `name` as `Symbol`s, and the current package has an atomic `Symbol` mirror
//! (`current_package_sym`). Several callers additionally built a `String` — some
//! of them twice, `current_package().to_string()` — purely to produce the `&str`
//! those signatures asked for.
//!
//! The pin is deterministic rather than timed. `symbol::intern_calls()` is an
//! exact per-thread counter, so this measures the *shape* of the cost — how much
//! interning one additional call performs — with no dependence on machine speed
//! or load. Budgets, not equalities: the remaining interns belong to the
//! resolution layers #7766 unit 2 still leaves open (`user_method_overloads`,
//! `multi_arg_type_keys`, `resolve_function_multi_cached`), and this file's job
//! is to stop the callee's own *name* from coming back, not to freeze every
//! caller.
//!
//! Same method as `tests/regex_match_intern_budget.rs`; see that file for the
//! rationale behind the two-length slope.
//!
//! Each budget comes in two calibrations. A **debug** build interns strictly
//! more than a release one for reasons that have nothing to do with this
//! change: `current_source_file_sym`'s `debug_assert_eq!` calls
//! `source_file_sym_by_walk`, which interns `?FILE` and the whole declaring
//! path on every routine entry, and is compiled out in release (#7766 records
//! this trap). `cargo test` is a debug build everywhere in this repo, so the
//! debug budget is the one CI enforces; both were measured before and after the
//! change and both fail on the pre-change counts.

/// The measured pre-change count, then the post-change one, for one shape --
/// used only to document what each budget is protecting.
const fn budget(debug: f64, release: f64) -> f64 {
    if cfg!(debug_assertions) {
        debug
    } else {
        release
    }
}

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

/// Interns attributable to one extra iteration of a loop, isolated from the
/// program's fixed startup cost (and from any one-time declaration cost) by
/// running the same program at two loop lengths.
fn interns_per_iteration(program: impl Fn(usize) -> String) -> f64 {
    const LOW: usize = 100;
    const HIGH: usize = 1100;
    let lo = interns_for(&program(LOW));
    let hi = interns_for(&program(HIGH));
    assert!(
        hi >= lo,
        "interning cannot shrink with more iterations: {lo} -> {hi}"
    );
    (hi - lo) as f64 / (HIGH - LOW) as f64
}

/// A loop body that calls nothing: the same accumulation, the same literals.
/// Subtracting it leaves the call's own interning, so the budgets below are not
/// inflated by whatever the surrounding loop costs.
fn interns_per_call(preamble: &str, body: &str) -> f64 {
    let control = interns_per_iteration(|n| {
        format!("my $hits = 0;\nfor ^{n} {{ $hits = 1 + 2 }}\nsay $hits;")
    });
    let measured = interns_per_iteration(|n| {
        format!("{preamble}\nmy $hits = 0;\nfor ^{n} {{ {body} }}\nsay $hits;")
    });
    measured - control
}

/// A `where` constraint makes the call ineligible for the light entries, so it
/// takes `call_compiled_function_named` — the path this change is about — with
/// only one candidate to resolve.
#[test]
fn where_constrained_sub_call_does_not_intern_its_own_name() {
    let per_call = interns_per_call("sub w($a where * > 0) { $a }", r#"$hits = w(1);"#);
    eprintln!("where-constrained sub call: {per_call:.3} interns per call");
    // Measured: release 63 -> 61, debug 72 -> 70. The budget sits close to the
    // measurement because the rest is the `where`-constraint machinery, which
    // this change does not touch; it moves only when the call *name* starts
    // being re-hashed again.
    let limit = budget(71.0, 62.0);
    assert!(
        per_call <= limit,
        "a by-name call to a compiled sub re-interns its name/package per call \
         ({per_call:.3} interns/call, budget {limit}); see #7766"
    );
}

/// A `multi` is excluded from the name-keyed compiled-key cache, so every call
/// runs the full resolution chain and then the named entry.
#[test]
fn named_multi_call_does_not_intern_its_own_name() {
    let per_call = interns_per_call(
        "multi m(Int $a, Int $b) { $a * $b }\nmulti m(Str $a, Str $b) { $a ~ $b }",
        r#"$hits = m(2, 3);"#,
    );
    eprintln!("named multi call: {per_call:.3} interns per call");
    // Measured: release 25 -> 23, debug 38 -> 36.
    let limit = budget(37.0, 24.0);
    assert!(
        per_call <= limit,
        "a by-name call to a compiled multi re-interns its name/package per call \
         ({per_call:.3} interns/call, budget {limit}); see #7766"
    );
}

/// The shape #7766 is measured on: one `Test` assertion, which reaches the
/// named entry twice (`ok`, then `proclaim`) through `ExecCallPairs` — the
/// opcode that exists for exactly this call shape.
#[test]
fn test_assertion_does_not_intern_the_assertion_routine_names() {
    let per_assertion =
        interns_per_iteration(|n| format!("use Test;\nplan {n};\nfor ^{n} {{ ok 1, \"x\" }}\n"));
    eprintln!("Test assertion: {per_assertion:.3} interns per assertion");
    // Measured: release 21.0 -> 15.0 (the ticket's headline number), debug
    // 64.0 -> 58.0. The budget leaves room for the resolution layers unit 2
    // still has open, and tightens as those land.
    let limit = budget(61.0, 18.0);
    assert!(
        per_assertion <= limit,
        "a Test assertion re-interns its routine names per assertion \
         ({per_assertion:.3} interns/assertion, budget {limit}); see #7766"
    );
}
