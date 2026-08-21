//! Pins the `news/2026-08/nested-sub-in-block-otf-recompile-fixed.md`
//! fix: a `sub` declared directly inside a bare block/closure body, invoked
//! through `call_compiled_closure` (the path `.()` / `call_sub_value`'s
//! compiled-code machinery reaches), used to miss the closure's own
//! `compiled_fns` fast-path lookup on EVERY call and fall through the slow
//! `user_function_matches_call` resolution ladder instead. The root cause
//! was a compile-time/runtime package mismatch: the block's nested `sub` was
//! keyed in `compiled_fns` by the closure's SYNTHETIC state-scope package
//! (`Pkg::&<closure>/N`), but no runtime lookup that reconstructs candidate
//! keys from the ACTUAL runtime package (`Interpreter::bare_name_packages`,
//! `find_compiled_function`) could ever match it, because a block never
//! pushes that synthetic name as `current_package()` at runtime.
//!
//! For a plain-bodied nested sub this "only" cost the resolution-ladder
//! overhead per call (the compiled body itself was still cached via
//! `FunctionDef::compiled`); for a `state`-declaring nested sub it was much
//! worse, because the OTF-compilable gate's `declares_state` exclusion
//! rejects that ladder branch entirely, degrading every call to full
//! `interpreter_fallbacks` tree-walk dispatch. A regression here reintroduces
//! both: either the "function-call" stats line no longer stays at
//! `interpreter_fallbacks=0`, or (for the state case) it climbs back to one
//! fallback per call.

use std::process::Command;

/// Run a Raku snippet through the built `mutsu` with `MUTSU_VM_STATS=1`.
/// Returns (stdout, stderr, success).
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

/// Extract `interpreter_fallbacks=N` from the "function-call" vm-stats line.
fn function_call_interpreter_fallbacks(stderr: &str) -> u64 {
    let line = stderr
        .lines()
        .find(|l| l.contains("] function-call opcodes="))
        .unwrap_or_else(|| panic!("no function-call vm-stats line in stderr: {stderr}"));
    line.split_whitespace()
        .find_map(|w| w.strip_prefix("interpreter_fallbacks="))
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("missing interpreter_fallbacks= in: {line}"))
}

#[test]
fn plain_nested_sub_in_block_hits_compiled_fast_path() {
    let src = "my $blk = { sub foo () { 42 }; my $r; for ^5 { $r = foo }; $r };\nsay $blk.();";
    let (out, err, ok) = run_with_stats(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "42\n");
    let fallbacks = function_call_interpreter_fallbacks(&err);
    assert_eq!(
        fallbacks, 0,
        "a plain-bodied `sub` declared inside a block called via `.()` is being \
         dispatched through the interpreter fallback again (see \
         news/2026-08/nested-sub-in-block-otf-recompile-fixed.md); \
         full stderr: {err}"
    );
}

#[test]
fn state_declaring_nested_sub_in_block_hits_compiled_fast_path() {
    // The anonymous `$` in `{$ = 42}` is a `state` variable -- the shape that
    // used to degrade to full tree-walk `interpreter_fallbacks` dispatch
    // because the OTF-compilable gate's `declares_state` exclusion rejected
    // the (already-broken) resolution-ladder fallback outright.
    let src = "my $blk = { sub foo () {$ = 42}; my $r; for ^5 { $r = foo }; $r };\nsay $blk.();";
    let (out, err, ok) = run_with_stats(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "42\n");
    let fallbacks = function_call_interpreter_fallbacks(&err);
    assert_eq!(
        fallbacks, 0,
        "a `state`-declaring `sub` declared inside a block called via `.()` is being \
         dispatched through the interpreter fallback again (see \
         news/2026-08/nested-sub-in-block-otf-recompile-fixed.md); \
         full stderr: {err}"
    );
}
