//! Pins the "literal parameter default binds directly" rule in
//! `Interpreter::eval_param_default`: an omitted parameter whose default is an
//! immutable scalar literal (`$desc = ''`, `$n = 1`) must bind the literal
//! itself, not re-enter `eval_block_value` on every call. The vendored
//! `Test.rakumod`'s `proclaim($cond, $desc, $unescaped-prefix = '')` paid that
//! evaluation once per assertion -- about 8% of an `ok` under the real
//! module -- for a value that can never be anything but `''`.
//!
//! A regression here shows up as the `evaluated=` count climbing back to one
//! per call in the `param-defaults` vm-stats line.

use std::process::Command;

fn run_with_stats(src: &str) -> (String, String, bool) {
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

/// `(evaluated, constant)` from the `param-defaults` vm-stats line.
fn param_default_counts(stderr: &str) -> (u64, u64) {
    let line = stderr
        .lines()
        .find(|l| l.contains("] param-defaults:"))
        .unwrap_or_else(|| panic!("no param-defaults vm-stats line in stderr: {stderr}"));
    let field = |key: &str| -> u64 {
        line.split_whitespace()
            .find_map(|w| w.strip_prefix(key))
            .and_then(|v| v.parse().ok())
            .unwrap_or_else(|| panic!("missing {key} in: {line}"))
    };
    (field("evaluated="), field("constant="))
}

#[test]
fn literal_default_is_bound_without_evaluating_it() {
    // `is copy` keeps `f` off the positional light path (which fills constant
    // defaults from its own table), so every call below goes through the
    // general binder and its `eval_param_default`.
    let src = "sub f($a, $s is copy, $d = '', $n = 1) { $s ~ $d ~ $n }; \
               my $r; for ^50 { $r = f(1, 'x') }; say $r";
    let (out, err, ok) = run_with_stats(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "x1\n");
    let (evaluated, constant) = param_default_counts(&err);
    assert_eq!(evaluated, 0, "a literal default was evaluated: {err}");
    assert_eq!(
        constant, 100,
        "two literal defaults per call over 50 calls: {err}"
    );
}

#[test]
fn non_literal_default_is_still_evaluated_per_call() {
    // A default that reads an earlier parameter is an expression; it must be
    // evaluated each call, with the earlier parameter in scope.
    let src = "sub g($a, $b is copy, $c = $a * 2) { $c }; \
               my @r; for 1..3 { @r.push(g($_, 0)) }; say @r";
    let (out, err, ok) = run_with_stats(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "[2 4 6]\n");
    let (evaluated, _) = param_default_counts(&err);
    assert_eq!(evaluated, 3, "one evaluation per call: {err}");
}

#[test]
fn container_literal_default_stays_fresh_per_call() {
    // `[]` is a container literal: each call must get its OWN array, so the
    // shortcut must not apply (a shared constant would accumulate pushes).
    let src = "sub h($a, $b is copy, $acc = []) { $acc.push($a); $acc.elems }; \
               say (h(1, 0), h(2, 0), h(3, 0))";
    let (out, err, ok) = run_with_stats(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "(1 1 1)\n");
}
