//! Pins how a listop-style statement call with a named argument resolves
//! (#7574, #9462).
//!
//! Every `Test` assertion takes that shape, because the parser injects a
//! `__mutsu_test_callsite_line` named argument into it. It used to run through
//! the dedicated `OpCode::ExecCallPairs`, whose entry once probed with the
//! marker still in the argument list and then resolved the same call a second
//! time in its carrier arm. The opcode is gone: the statement now compiles to
//! the expression form's `CallFuncNamed`, which strips the marker once and
//! resolves each call at most once (`multi_call_resolves_once.rs`).

use std::process::Command;

fn run(src: &str, real_test: bool) -> (String, String, bool) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(src);
    cmd.env("MUTSU_VM_STATS", "1");
    if real_test {
        cmd.env("MUTSU_REAL_TEST", "1");
    }
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
fn test_assertions_resolve_their_routine_at_most_once_per_call() {
    // Under the real `Test.rakumod` every assertion is a user-declared `multi`.
    let src = "use Test; plan 20; for ^20 { ok 1, \"x\" }";
    let (out, err, ok) = run(src, true);
    assert!(ok, "run failed: {err}");
    assert_eq!(out.lines().filter(|l| l.starts_with("ok ")).count(), 20);
    let n = full_resolves(&err, "ok");
    assert!(
        (1..=20).contains(&n),
        "20 assertions must resolve `ok` between once and 20 times, got {n}: {err}"
    );
}

#[test]
fn a_failing_assertion_still_reports_its_own_source_line() {
    // The callsite line rides on the marker pair the entry now strips before
    // the probes; it is threaded to the carrier explicitly instead. Losing it
    // would silently report the wrong line for every failure in the suite.
    let src = "use Test;\nplan 1;\nok 0, \"boom\";\n";
    for real_test in [false, true] {
        let (out, err, _) = run(src, real_test);
        assert!(
            out.contains("not ok 1 - boom"),
            "assertion did not run (real_test={real_test}): {out}{err}"
        );
        assert!(
            format!("{out}{err}").contains("line 3"),
            "failure did not report its callsite line (real_test={real_test}): {out}{err}"
        );
    }
}

#[test]
fn a_listop_statement_call_with_named_args_still_binds_them() {
    // The generic shape: an unqualified statement call carrying a named
    // argument.
    let src = "sub greet($who, :$loud) { say $loud ?? \"HI $who\" !! \"hi $who\" }; \
               greet 'bob', :loud;";
    let (out, err, ok) = run(src, false);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "HI bob\n");
}
