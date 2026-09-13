//! Pins the #8265 fix: a proto with a mix of static and non-static `:sym<...>`
//! candidates no longer re-runs the expensive registry walk
//! (`resolve_token_patterns_static_in_pkg`, which formats every candidate's
//! pattern text from scratch) on EVERY `<subrule>` reference.
//!
//! Two bugs combined to cause that: `regex_pattern_is_static` misclassified
//! `$<sym>=[...]` (the lowering of a bare `<sym>` inside a `:sym<...>` token
//! body) as runtime variable interpolation, and
//! `resolve_parsed_token_candidates_in_pkg` declined its memo for the WHOLE
//! proto the moment any one candidate looked non-static. Even with the first
//! bug fixed, a proto with a genuinely dynamic sibling (this test's
//! `value:sym<dynv>`, which really does interpolate an outer `$dyn`) still
//! needs the second fix: the raw (pre-parse) candidate list is now cached
//! separately (`RAW_TOKEN_CANDIDATES`) from the fully-parsed, all-static form
//! (`PARSED_TOKEN_CANDIDATES`), so only the one dynamic candidate pays a
//! per-call re-parse — never the registry walk that finds and formats all of
//! them.
//!
//! A timing assertion would be flaky under load; the raw-cache hit/miss
//! counter is deterministic and exact, so it is the pin instead.

use std::process::Command;

/// A proto with two static `:sym<...>` candidates (each lowering to `$<sym>=[...]`)
/// and one genuinely dynamic candidate (`$dyn`, an outer lexical). Matched
/// against 4 tokens, so a per-reference registry-walk regression shows up as
/// `misses` tracking the reference count instead of staying at 1.
const PROGRAM: &str = r#"
my $dyn = 'zzz';
grammar G {
    token TOP { <value>+ % ' ' }
    proto token value {*}
    token value:sym<true>  { <sym> }
    token value:sym<false> { <sym> }
    token value:sym<dynv>  { $dyn }
}
say G.parse('true false zzz true') ?? "matched" !! "no match";
"#;

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

/// Extract `hits=N misses=M` from the "regex-raw-token-candidates-cache" line.
fn raw_token_candidates_misses(stderr: &str) -> u64 {
    let line = stderr
        .lines()
        .find(|l| l.contains("] regex-raw-token-candidates-cache:"))
        .unwrap_or_else(|| panic!("no regex-raw-token-candidates-cache line in stderr: {stderr}"));
    line.split_whitespace()
        .find_map(|w| w.strip_prefix("misses="))
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("missing misses= in: {line}"))
}

#[test]
fn mixed_static_dynamic_proto_reuses_raw_candidate_resolution() {
    let (out, err, ok) = run_with_stats(PROGRAM);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "matched\n");
    let misses = raw_token_candidates_misses(&err);
    // One miss per (pkg, name) resolution under a stable TOKEN_DEFS_GEN,
    // regardless of how many of the 4 tokens `<value>` matched. Before the
    // #8265 fix there was no raw-candidate cache at all, so every reference
    // paid a fresh, uncached registry walk -- this assertion catches a
    // regression back to that by refusing to let misses scale with the
    // reference count.
    assert_eq!(
        misses, 1,
        "the proto's raw candidate list is being re-resolved on more than one \
         reference (a regression to the pre-#8265 whole-proto cache decline); \
         full stderr: {err}"
    );
}
