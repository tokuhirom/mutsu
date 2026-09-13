//! Pins the #8247 invariant: an operation that scans ONE subject repeatedly
//! materializes that subject once, not once per match.
//!
//! `MatchTarget::new` copies the whole subject (an `Arc<String>` plus an
//! `Arc<[char]>`, ~5 bytes per character). `.split(rx)` used to build one per
//! separator and `.subst(rx, :g)` / `s:g///` one whole-subject `Vec<char>` per
//! match, making all three O(matches x subject): an 80 KB `split(/\s+/)` took
//! 11.9 s against rakudo's 0.26 s, and a 640 KB global substitution 23.2 s
//! against 0.79 s. Each now builds one shared target per call, which is what the
//! `match_targets` counter here counts.
//!
//! A timing assertion would be flaky, so this asserts the *shape* of the fix
//! instead. It cannot see a regression that reintroduces a raw per-match
//! `chars().collect()` bypassing `MatchTarget` entirely — `benchmarks/bench-regex-split-subst.raku`
//! and its deterministic instruction-count series are the guard for that.

use std::process::Command;

/// 500 separators and 500 matches over one subject each: a per-match target
/// would put this in the thousands.
const PROGRAM: &str = r#"
my $text = ("word$_ " xx 500).join;
my @fields = $text.split(/ \s+ /);
my $substituted = $text.subst(/ \d+ /, '#', :g);
my $operator = $text;
$operator ~~ s:g/ \d+ /#/;
say @fields.elems ~ " " ~ $substituted.chars ~ " " ~ $operator.chars;
"#;

#[test]
fn repeated_scanning_of_one_subject_materializes_it_once_per_call() {
    let out = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg("-e")
        .arg(PROGRAM)
        .env("MUTSU_VM_STATS", "1")
        .output()
        .expect("failed to spawn mutsu");
    assert!(
        out.status.success(),
        "run failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(
        stdout.trim(),
        "501 2500 2500",
        "the three operations no longer agree with each other"
    );

    let stderr = String::from_utf8_lossy(&out.stderr);
    let stats_line = stderr
        .lines()
        .find(|l| l.contains("regex-captures:"))
        .unwrap_or_else(|| panic!("no regex-captures stats line in stderr: {stderr}"));
    let targets: u64 = stats_line
        .split_whitespace()
        .find_map(|w| w.strip_prefix("match_targets="))
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("missing match_targets= in: {stats_line}"));
    // Three scanning operations, so three targets is the expected shape; the
    // ceiling leaves room for an unrelated incidental match elsewhere in the
    // program without admitting anything per-match.
    assert!(
        targets <= 16,
        "match_targets={targets} over 1500 matches — a repeated-scan path is \
         building one whole-subject MatchTarget per match again (#8247); \
         stats line: {stats_line}"
    );
}
