//! Pins "one function call, one multi resolution" (#7886).
//!
//! `dispatch_func_call_inner` used to run the full `resolve_function_with_types`
//! candidate walk **three** times for a single call:
//!
//! 1. `find_compiled_function_memo` -> `resolve_function_multi_cached_keyed`,
//!    which handed its answer forward only when the resolution was *type-keyed*;
//! 2. the `has_multi_candidates_cached` arm, re-resolving by name because the
//!    memo above was empty;
//! 3. `compile_and_call_function_def` -> `push_multi_dispatch_frame`, resolving
//!    by name again to work out which candidate it had just been handed.
//!
//! For a `multi` whose candidates carry a `where` clause (or a subset, or a
//! literal) none of that is cacheable — `func_multi_dispatch_type_cacheable`
//! correctly refuses a value-dependent multi — so each of the three walks ran
//! the constraint, i.e. **user code**. Rakudo resolves a call once.
//!
//! The memo is now filled for every resolution and the dispatch frame takes the
//! winner it is already holding, so a call resolves exactly once. A regression
//! shows up as `f=N` climbing back to a multiple of the call count in the
//! `function-full-resolve` vm-stats line.

use std::process::Command;

fn run(src: &str) -> (String, String, bool) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(src);
    cmd.env("MUTSU_VM_STATS", "1");
    let out = cmd.output().expect("failed to spawn mutsu");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

/// The `<name>=N` count in the `function-full-resolve` vm-stats line, or 0 when
/// the routine never reached a full candidate walk.
fn full_resolves(stderr: &str, name: &str) -> u64 {
    let key = format!("{name}=");
    stderr
        .lines()
        .filter(|l| l.contains("] function-full-resolve total="))
        .find_map(|l| {
            l.split_whitespace()
                .find_map(|w| w.strip_prefix(key.as_str()))
                .and_then(|v| v.parse().ok())
        })
        .unwrap_or(0)
}

#[test]
fn a_where_constrained_multi_resolves_once_per_call() {
    let src = "multi sub f(Int:D $x where { True }) { 1 }\n\
               multi sub f($x) { 2 }\n\
               say f(1);";
    let (out, err, ok) = run(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "1\n");
    assert_eq!(
        full_resolves(&err, "f"),
        1,
        "one call must resolve its candidates once: {err}"
    );
}

#[test]
fn a_where_constrained_multi_resolves_once_per_call_in_a_loop() {
    let src = "multi sub f(Int:D $x where { True }) { 1 }\n\
               multi sub f($x) { 2 }\n\
               my $s = 0; for ^5 { $s += f(1) }; say $s;";
    let (out, err, ok) = run(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "5\n");
    assert_eq!(
        full_resolves(&err, "f"),
        5,
        "five calls must resolve five times, not fifteen: {err}"
    );
}

#[test]
fn a_subset_constrained_multi_resolves_once_per_call() {
    // The other flavour of value-dependent dispatch the resolution cache has to
    // refuse: a subset's `where` is user code just as an inline one is.
    let src = "subset Pos of Int where * > 0;\n\
               multi sub f(Pos $x) { 1 }\n\
               multi sub f($x) { 2 }\n\
               my $s = 0; for ^5 { $s += f(1) }; say $s;";
    let (out, err, ok) = run(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "5\n");
    assert_eq!(
        full_resolves(&err, "f"),
        5,
        "five calls must resolve five times, not fifteen: {err}"
    );
}

#[test]
fn a_where_clause_runs_far_fewer_times_than_it_used_to() {
    // The user-visible consequence. Rakudo evaluates the winning candidate's
    // constraint once per call; mutsu still re-evaluates it while binding the
    // winner, but the count must no longer scale with a *re-resolution* of the
    // same call. Three resolutions of this two-candidate multi cost 16
    // evaluations before #7886.
    let src = "my $n = 0;\n\
               multi sub f(Int:D $x where { $n++; True }) { 'int' }\n\
               multi sub f($x where { $n++; True }) { 'any' }\n\
               my $r = f(1);\n\
               say \"$r $n\";";
    let (out, err, ok) = run(src);
    assert!(ok, "run failed: {err}");
    let n: u64 = out
        .trim()
        .strip_prefix("int ")
        .unwrap_or_else(|| panic!("wrong candidate won: {out}"))
        .parse()
        .unwrap_or_else(|_| panic!("unparseable count: {out}"));
    assert!(
        n <= 8,
        "the winning candidate's `where` ran {n} times for one call (was 16 \
         across three resolutions; rakudo runs it once): {out}{err}"
    );
}
