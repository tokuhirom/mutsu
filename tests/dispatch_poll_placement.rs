//! Where the VM polls its safepoint consumers (#8821, `vm_poll::DispatchPolls`).
//!
//! With only the GC consumer armed (every default run), a poll runs on loop
//! back-edges, compound-loop iterations and calls — not on every executed
//! opcode. Two properties are pinned here, each against the
//! `[mutsu vm-stats] poll: polls=N` counter, by running a loop at two
//! iteration counts and looking at the difference so start-up polls cancel:
//!
//! - **at most one poll per iteration** (plus a small constant): the order
//!   goal of #8821. Per-opcode polling would show ~5 per iteration here.
//! - **at least one poll per iteration**: the stop-the-world bound. A loop
//!   shape that iterated without polling would let a collector wait on it
//!   for the whole loop — `nqp::while` in sink context is the case that
//!   repeats through a plain backward `Jump` rather than a loop opcode.

use std::process::Command;

fn polls(src: &str, n: u64) -> u64 {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(src).arg(n.to_string());
    for k in [
        "MUTSU_GC",
        "MUTSU_GC_EVERY_SAFEPOINT",
        "MUTSU_GC_EVERY_CANDIDATE",
        "MUTSU_GC_AT",
        "MUTSU_GC_RANDOM_RATE",
        "MUTSU_PROFILE",
    ] {
        cmd.env_remove(k);
    }
    cmd.env("MUTSU_VM_STATS", "1");
    let out = cmd.output().expect("failed to spawn mutsu");
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "run failed; stderr:\n{err}");
    err.lines()
        .find_map(|l| l.strip_prefix("[mutsu vm-stats] poll: polls="))
        .and_then(|n| n.trim().parse().ok())
        .unwrap_or_else(|| panic!("no poll counter in stderr:\n{err}"))
}

fn assert_one_poll_per_iteration(name: &str, src: &str) {
    const N: u64 = 2000;
    let small = polls(src, N);
    let large = polls(src, 2 * N);
    let per_n = large.saturating_sub(small);
    assert!(
        per_n >= N,
        "{name}: {per_n} polls for {N} extra iterations — some iterations never poll"
    );
    assert!(
        per_n <= N + 16,
        "{name}: {per_n} polls for {N} extra iterations — more than one per iteration"
    );
}

#[test]
fn while_loop_polls_once_per_iteration() {
    assert_one_poll_per_iteration(
        "while",
        "sub go($n) { my int $i = 0; my int $m = $n; while $i < $m { $i = $i + 1 } }; go(@*ARGS[0].Int)",
    );
}

#[test]
fn while_loop_with_empty_body_still_polls() {
    assert_one_poll_per_iteration(
        "while (empty body)",
        "my $m = @*ARGS[0].Int; my $i = 0; while $i++ < $m { }",
    );
}

#[test]
fn c_style_loop_polls_once_per_iteration() {
    assert_one_poll_per_iteration(
        "loop (;;)",
        "my $m = @*ARGS[0].Int; my $s = 0; loop (my $i = 0; $i < $m; $i++) { $s += $i }",
    );
}

#[test]
fn repeat_loop_polls_once_per_iteration() {
    assert_one_poll_per_iteration(
        "repeat while",
        "my $m = @*ARGS[0].Int; my $i = 0; repeat { $i++ } while $i < $m",
    );
}

#[test]
fn sunk_nqp_while_polls_on_its_backward_jump() {
    assert_one_poll_per_iteration(
        "nqp::while",
        "use nqp; my $m = @*ARGS[0].Int; my $i = 0; nqp::while($i < $m, $i++); say $i",
    );
}

#[test]
fn sunk_nqp_while_in_a_called_sub_polls_on_its_backward_jump() {
    // A sub body runs in a call path's own dispatch loop, not `run_inner`;
    // before #8821 that loop never polled, so this loop had no safepoint.
    assert_one_poll_per_iteration(
        "nqp::while in a sub",
        "use nqp; sub f($m) { my $i = 0; nqp::while($i < $m, $i++); $i }; say f(@*ARGS[0].Int)",
    );
}

#[test]
fn method_body_loop_polls_once_per_iteration() {
    assert_one_poll_per_iteration(
        "while in a method",
        "class C { method go($m) { my $i = 0; while $i < $m { $i++ } } }; C.go(@*ARGS[0].Int)",
    );
}
