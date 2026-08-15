//! Pins the ADR-0016 P5 lazy-`Match` invariant at the method-dispatch layer
//! (`todo/tickets/yaml-parse-throughput.md` round 8): resolving a method's
//! owner chain (`Interpreter::dispatch_mro`/`dispatch_owner_chain`) and the
//! native-dispatch bypass gate (`try_native_method_raw`) must not force full
//! `Match` materialization (`MatchNode::force_attrs`) just to learn a
//! receiver's class/shape. Before the fix those three call sites decoded
//! `value.view()` unconditionally — cheap tag probes (`is_lazy_match_value`/
//! `is_mixin_value`) now gate them. A regression here reintroduces the ~12x
//! `match_materializations` blowup (1749 -> 20949) measured on
//! `benchmarks/bench-yaml-parse.raku` that motivated this test.

use std::process::Command;

#[test]
fn grammar_parse_does_not_eagerly_materialize_every_leaf_match() {
    let bench = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("benchmarks")
        .join("bench-yaml-parse.raku");
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg(&bench);
    cmd.env("MUTSU_VM_STATS", "1");
    let out = cmd.output().expect("failed to spawn mutsu");
    assert!(
        out.status.success(),
        "benchmark run failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    let stats_line = stderr
        .lines()
        .find(|l| l.contains("regex-captures:"))
        .unwrap_or_else(|| panic!("no regex-captures stats line in stderr: {stderr}"));
    let materializations: u64 = stats_line
        .split_whitespace()
        .find_map(|w| w.strip_prefix("match_materializations="))
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("missing match_materializations= in: {stats_line}"));
    // The legitimate, non-leaf materializations for this benchmark are ~1749
    // (grammar actions recursing into named/positional children that
    // genuinely need the attribute map). A generous ceiling well under the
    // pre-fix 20949 catches a reintroduction of an unguarded `view()` call on
    // the dispatch hot path without pinning the exact count.
    assert!(
        materializations < 5000,
        "match_materializations={materializations} — a method-dispatch call site is \
         forcing lazy Match materialization unconditionally again (see \
         todo/tickets/yaml-parse-throughput.md round 8); full stats line: {stats_line}"
    );
}
