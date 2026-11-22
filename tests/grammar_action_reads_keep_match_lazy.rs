//! Pins that the reads a grammar action makes of its `$/` keep the Match lazy
//! (ADR-0016 P5, #12917). `~$/`, `~$<name>`, `make` and `@<name>».ast` each
//! used to reach a `view()` (the `prefix:<~>` coercion's Buf/code probes, the
//! named-capture read, a hyper call's env write-back sweep, the native method
//! table's receiver probes, ...) that materialized the Match's attribute map,
//! once per action. The parse below makes ~150 action calls (301
//! materializations before the fix); with every read answered from the
//! capture node, almost nothing is materialized.

use std::process::Command;

const SCRIPT: &str = r#"
grammar G {
    token TOP   { <entry>+ % ',' }
    token entry { <key> '=' <value> }
    token key   { \w+ }
    token value { \d+ }
}
class A {
    method TOP($/)   { make @<entry>».ast.hash }
    method entry($/) { make ~$<key> => ~$<value> }
    method key($/)   { make ~$/ }
    method value($/) { make ~$/ }
}
my $text = (1..50).map({ "k$_=$_" }).join(',');
my %h = G.parse($text, :actions(A)).made;
say %h.elems, ' ', %h<k7>, ' ', %h<k50>;
"#;

#[test]
fn grammar_action_reads_do_not_materialize_the_match() {
    let out = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg("-e")
        .arg(SCRIPT)
        .env("MUTSU_VM_STATS", "1")
        .output()
        .expect("failed to spawn mutsu");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "run failed: {stderr}");
    assert_eq!(stdout.trim(), "50 7 50", "wrong parse result; stderr: {stderr}");
    let stats_line = stderr
        .lines()
        .find(|l| l.contains("regex-captures:"))
        .unwrap_or_else(|| panic!("no regex-captures stats line in stderr: {stderr}"));
    let materializations: u64 = stats_line
        .split_whitespace()
        .find_map(|w| w.strip_prefix("match_materializations="))
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("missing match_materializations= in: {stats_line}"));
    // One materialization per action call (~150) is the regression; the one
    // left is the parse result's own `.made` read.
    assert!(
        materializations < 20,
        "match_materializations={materializations}: an action's read of `$/` \
         materialized the Match again; full stats line: {stats_line}"
    );
}
