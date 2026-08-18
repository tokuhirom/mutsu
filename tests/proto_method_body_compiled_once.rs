//! Pins the `todo/tickets/adr0019-method-body-compile-dedup-remnants.md`
//! (item 2) fix: `Interpreter::run_proto_method` used to rebuild a fresh,
//! uncompiled `MethodDef` for a `proto method`/`proto submethod` body on
//! EVERY call, forcing `run_resolved_method_celled`'s on-demand-compile path
//! to recompile the same body from AST every time instead of once. A
//! recursive `proto method` dispatch (`t/where-named-param-sibling-ref.t`'s
//! binomial-triangle shape) used to trigger 265 recompiles; the fix caches
//! the compiled body on `Registry::proto_compiled_cache`, keyed by
//! `(owner, method_name)`. A regression here reintroduces the per-call
//! recompile.

use std::process::Command;

#[test]
fn recursive_proto_method_body_compiles_once_not_per_call() {
    let script = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("t")
        .join("where-named-param-sibling-ref.t");
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg(&script);
    cmd.env("MUTSU_VM_STATS", "1");
    let out = cmd.output().expect("failed to spawn mutsu");
    assert!(
        out.status.success(),
        "test file run failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    let stats_line = stderr
        .lines()
        .find(|l| l.contains("adr0019-d3-8:"))
        .unwrap_or_else(|| panic!("no adr0019-d3-8 stats line in stderr: {stderr}"));
    let compiles: u64 = stats_line
        .split_whitespace()
        .find_map(|w| w.strip_prefix("method_body_runtime_compiles="))
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("missing method_body_runtime_compiles= in: {stats_line}"));
    // Before the fix this was 265 (one recompile per recursive `.get` call).
    // A generous ceiling well under that catches a reintroduced per-call
    // recompile without pinning the exact count.
    assert!(
        compiles < 10,
        "method_body_runtime_compiles={compiles} — a proto method body is being \
         recompiled from AST on every call again (see \
         todo/tickets/adr0019-method-body-compile-dedup-remnants.md item 2); \
         full stats line: {stats_line}"
    );
}
