//! #14337: an `EVAL` in one routine reaches only that routine's own lexical
//! scope, so it must not make every argument-taking call of the program write
//! its parameters into the shared env (`env_deep_copies` once per call).

use std::process::Command;

/// Run a Raku snippet with `MUTSU_VM_STATS=1`; returns (stdout, stderr).
fn run_with_stats(src: &str) -> (String, String) {
    let out = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg("-e")
        .arg(src)
        .env("MUTSU_VM_STATS", "1")
        .output()
        .expect("failed to spawn mutsu");
    assert!(out.status.success(), "run failed: {:?}", out);
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

/// `env_deep_copies=N` from the dual-store stats line.
fn env_deep_copies(stderr: &str) -> u64 {
    stderr
        .split_whitespace()
        .find_map(|w| w.strip_prefix("env_deep_copies="))
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("no env_deep_copies= in: {stderr}"))
}

const CALLS: &str = "sub f($n) { $n + 1 }\nmy $s = 0; for ^1000 { $s = f($s) }\nsay $s;";

#[test]
fn eval_in_a_never_called_routine_does_not_copy_env_per_call() {
    let src = format!("sub ev($c) {{ EVAL $c }}\n{CALLS}");
    let (out, err) = run_with_stats(&src);
    assert_eq!(out, "1000\n");
    let copies = env_deep_copies(&err);
    assert!(
        copies < 10,
        "1000 calls of `f($s)` made {copies} env deep copies; an EVAL in an unrelated \
         routine must not force the parameter env write (#14337): {err}"
    );
}

#[test]
fn top_level_eval_does_not_copy_env_per_call_of_other_routines() {
    let src = format!("EVAL 'my $x = 1';\n{CALLS}");
    let (out, err) = run_with_stats(&src);
    assert_eq!(out, "1000\n");
    let copies = env_deep_copies(&err);
    assert!(copies < 10, "{copies} env deep copies: {err}");
}
