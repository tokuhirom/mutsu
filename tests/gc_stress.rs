//! Integration coverage for the GC cycle collector running *during* real
//! execution (safepoint wiring, §11 step 8 second half). The collector's
//! algorithm is unit-tested in `src/gc/collect.rs`; these tests exercise it
//! end-to-end through the built interpreter under the `MUTSU_GC` stress modes.
//!
//! Default runs (no `MUTSU_GC`) never collect, so this is the only place the
//! collector meets real interpreter state — the key property being that
//! collecting aggressively must not disturb *live* data.

use std::process::Command;

/// Run a Raku snippet through the built `mutsu`, optionally with a GC stress
/// mode enabled. Returns (stdout, stderr, success).
fn run(src: &str, gc: &[(&str, &str)]) -> (String, String, bool) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(src);
    for (k, v) in gc {
        cmd.env(k, v);
    }
    let out = cmd.output().expect("failed to spawn mutsu");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

/// Collecting on every instruction must produce byte-identical output to not
/// collecting at all — i.e. the collector never reclaims reachable/live data.
#[test]
fn collect_every_instruction_preserves_live_data() {
    let src = r#"
        my %counts;
        my @data = (1..60).map({ $_ * 3 });
        for @data -> $n { my $k = ($n % 5).Str; %counts{$k} = (%counts{$k} // 0) + $n; }
        say %counts.sort.map({ .key ~ "=" ~ .value }).join(",");
        my %nested = a => { b => [1,2,3] }, c => { d => [4,5,6] };
        say %nested<a><b>.sum + %nested<c><d>.sum;
        say @data.grep(* > 100).sum;
    "#;

    let (off, _, ok_off) = run(src, &[]);
    let (on, on_err, ok_on) = run(
        src,
        &[("MUTSU_GC", "on"), ("MUTSU_GC_EVERY_SAFEPOINT", "1")],
    );

    assert!(ok_off, "GC-off run must succeed");
    assert!(ok_on, "GC-stress run must not crash; stderr:\n{on_err}");
    assert_eq!(
        off, on,
        "GC collecting every instruction changed program output"
    );
}

/// A program that repeatedly builds self-referential / mutual cycles and drops
/// them must actually get those cycles reclaimed (not just leak), and survive.
#[test]
fn cycles_are_reclaimed_during_execution() {
    let src = r#"
        for ^30 {
            my %h; %h<self> := %h;
            my %x; my %y; %x<y> := %y; %y<x> := %x;
        }
        say "done";
    "#;

    let (out, err, ok) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_SAFEPOINT", "1"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );

    assert!(ok, "cycle-stress run must not crash; stderr:\n{err}");
    assert!(out.contains("done"), "program must run to completion");

    // The `[mutsu vm-stats] gc: ... reclaimed_nodes=N ...` summary must report a
    // positive reclaim — proof the collector broke the cycles.
    let reclaimed = err
        .lines()
        .find(|l| l.contains("[mutsu vm-stats] gc:"))
        .and_then(|l| {
            l.split_whitespace()
                .find_map(|tok| tok.strip_prefix("reclaimed_nodes="))
        })
        .and_then(|n| n.parse::<u64>().ok())
        .unwrap_or(0);
    assert!(
        reclaimed > 0,
        "expected the collector to reclaim cycle nodes, got reclaimed_nodes={reclaimed}\nstderr:\n{err}"
    );
}
