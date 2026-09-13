//! #8269: a `~~` must not re-intern its fixed env keys once per match.
//!
//! The smartmatch path saved and restored the topic, cleared the pending `make`
//! payload, wrote `$/`, and wrote one `$0`..`$N` per positional capture — all
//! through the *string-keyed* `Env` API, which calls `Symbol::intern` on a
//! literal every time. A repeat intern is memoized but not free: it hashes and
//! compares the whole key string, and callgrind put `Symbol::intern` at 16.4% of
//! `"a" ~~ /a/`, twice what running the matcher itself cost.
//!
//! The pin is deterministic rather than timed. `symbol::intern_calls()` is an
//! exact per-thread counter, so this measures the *shape* of the cost — how much
//! interning one additional match performs — with no dependence on machine speed
//! or load. A budget, not an equality: the remaining interns are other paths'
//! (type-object names, the `Match` object's attribute names), and this file's job
//! is to stop the fixed *env keys* from coming back, not to freeze every caller.

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
/// program's fixed startup cost by running the same loop at two lengths.
fn interns_per_iteration(body: &str) -> f64 {
    const LOW: usize = 100;
    const HIGH: usize = 1100;
    let program = |n: usize| format!("my $hits = 0; for ^{n} {{ {body} }}; say $hits;");
    let lo = interns_for(&program(LOW));
    let hi = interns_for(&program(HIGH));
    assert!(
        hi >= lo,
        "interning cannot shrink with more iterations: {lo} -> {hi}"
    );
    (hi - lo) as f64 / (HIGH - LOW) as f64
}

/// The loop body without the match: the same `$hits++`, the same `if`, the same
/// string literal. Subtracting it leaves the smartmatch's own interning, so the
/// budgets below are not inflated by whatever the surrounding loop costs.
const CONTROL: &str = r#"$hits++ if "a";"#;

/// Interns one `~~` performs, over and above the loop that carries it.
fn interns_per_match(body: &str) -> f64 {
    interns_per_iteration(body) - interns_per_iteration(CONTROL)
}

#[test]
fn boolean_smartmatch_does_not_intern_its_fixed_env_keys() {
    let per_match = interns_per_match(r#"$hits++ if "a" ~~ /a/;"#);
    eprintln!("boolean `~~`: {per_match:.3} interns per match");
    // Measured: 16 before #8269, 4 after. The twelve that went were `$_`
    // (read and written by both the VM op and the runtime arm), `$/` (written
    // on the match path and read back to decide the op's result), and `made`.
    assert!(
        per_match <= 8.0,
        "a boolean smartmatch re-interns a fixed env key per match \
         ({per_match:.3} interns/match, budget 8, was 16 before the fix); see #8269"
    );
}

#[test]
fn capturing_smartmatch_does_not_intern_its_capture_indices() {
    let per_match = interns_per_match(r#"$hits++ if "abc" ~~ /(a)(b)(c)/;"#);
    eprintln!("capturing `~~`: {per_match:.3} interns per match");
    // Measured: 22 before, 4 after. Each positional capture used to be written
    // under an `i.to_string()` key — a fresh `String` allocation plus an intern,
    // per capture, per match, and twice over (once as a string, once upgraded to
    // a `Match`). Note this now costs the same as the capture-free case above,
    // which is the property worth keeping: captures no longer add interning.
    assert!(
        per_match <= 8.0,
        "a capturing smartmatch re-interns a capture-index env key per match \
         ({per_match:.3} interns/match, budget 8, was 22 before the fix); see #8269"
    );
}

#[test]
fn named_capture_smartmatch_does_not_intern_its_fixed_env_keys() {
    let per_match = interns_per_match(r#"$hits++ if "abc" ~~ /$<first>=(a)/;"#);
    eprintln!("named-capture `~~`: {per_match:.3} interns per match");
    // Measured: 18 before, 6 after. The budget is two wider than its siblings
    // on purpose: the `<name>` env keys are still built with `format!` and
    // interned per match. That is left as it stands — the name varies per
    // pattern, so it is a different fix from these fixed literals — and this
    // budget documents it rather than asserting it away.
    assert!(
        per_match <= 10.0,
        "a named-capture smartmatch interns more than its named keys per match \
         ({per_match:.3} interns/match, budget 10, was 18 before the fix); see #8269"
    );
}
