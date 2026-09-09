//! Pins the `OpCode::ExecCallPairs` entry arm (#7574).
//!
//! A listop-style statement call with a named argument -- the shape every
//! `Test` assertion takes, because the parser injects a
//! `__mutsu_test_callsite_line` named argument into it -- used to do two things
//! wrong at the opcode entry:
//!
//! 1. it ran its `find_compiled_function` / `try_native_function` probes
//!    against the *unsanitized* argument list, i.e. one carrying that internal
//!    marker pair, so the probes asked about a call shape that does not exist;
//! 2. it then threw away whatever resolution the probe had performed and let
//!    the carrier arm resolve the very same call a second time inside
//!    `exec_call`.
//!
//! Both are fixed: the entry sanitizes once (as `OpCode::ExecCall` always has)
//! and hands the type-keyed winner it resolved to the carrier. A regression
//! shows up as `execcallpairs:carrier` replacing
//! `execcallpairs:carrier-preresolved` in the vm-stats dispatch-entry line.

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

/// The count recorded under `execcallpairs:<outcome>` in the dispatch-entry
/// vm-stats line, or 0 when the key is absent.
fn outcome_count(stderr: &str, outcome: &str) -> u64 {
    let key = format!("execcallpairs:{outcome}=");
    stderr
        .lines()
        .filter(|l| l.contains("dispatch-entry"))
        .find_map(|l| {
            l.split_whitespace()
                .find_map(|w| w.strip_prefix(key.as_str()))
                .and_then(|v| v.parse().ok())
        })
        .unwrap_or(0)
}

#[test]
fn test_assertions_resolve_their_routine_once_per_call() {
    // Under the real `Test.rakumod` every assertion is a user-declared `multi`,
    // so the entry's compiled probe resolves the winner and the carrier must
    // reuse it rather than resolving again.
    let src = "use Test; plan 20; for ^20 { ok 1, \"x\" }";
    let (out, err, ok) = run(src, true);
    assert!(ok, "run failed: {err}");
    assert_eq!(out.lines().filter(|l| l.starts_with("ok ")).count(), 20);
    assert_eq!(
        outcome_count(&err, "carrier-preresolved"),
        20,
        "every assertion should reuse the entry's resolution: {err}"
    );
    assert_eq!(
        outcome_count(&err, "carrier"),
        0,
        "an assertion resolved its routine twice: {err}"
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
    // The generic shape this opcode exists for: an unqualified statement call
    // whose callee is not statically known, carrying a named argument.
    let src = "sub greet($who, :$loud) { say $loud ?? \"HI $who\" !! \"hi $who\" }; \
               greet 'bob', :loud;";
    let (out, err, ok) = run(src, false);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "HI bob\n");
}
