//! Pins that a multi family's dispatch caches outlive another family's
//! registration (ADR-12529 §2.4).
//!
//! The multi caches used to be tagged with `fn_resolve_gen` alone, the version
//! of the whole functions map. A block that declares a routine of its own moves
//! that version on every entry and exit, so a loop around such a block re-ran
//! the full candidate walk of every *other* multi it called, once per
//! iteration. The caches now carry the family's own generation as well, and a
//! regression shows up as `f=N` climbing back to the iteration count in the
//! `function-full-resolve` vm-stats line.

use std::process::Command;

fn run(src: &str) -> (String, String, bool) {
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

/// The `<name>=N` count in the `function-full-resolve` vm-stats line, or 0 when
/// the routine never reached a full candidate walk.
fn full_resolves(stderr: &str, name: &str) -> u64 {
    let key = format!("{name}=");
    stderr
        .lines()
        .filter(|l| l.contains("] function-full-resolve total="))
        .find_map(|l| {
            l.split_whitespace()
                .find_map(|w| w.strip_prefix(key.as_str()))
                .and_then(|v| v.parse().ok())
        })
        .unwrap_or(0)
}

#[test]
fn another_familys_block_declaration_keeps_the_winner_cached() {
    let src = "multi sub f(Int $x) { $x + 1 }\n\
               multi sub f(Str $x) { $x }\n\
               my $s = 0;\n\
               for ^50 -> $i { { multi g(Int $z) { $z }; $s += f($i) + g(1) } }\n\
               say $s;";
    let (out, err, ok) = run(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "1325\n");
    assert_eq!(
        full_resolves(&err, "f"),
        1,
        "`g`'s per-iteration registration must not retire `f`'s caches: {err}"
    );
}

#[test]
fn the_familys_own_block_candidate_still_re_resolves() {
    let src = "multi sub f(Int $x) { 1 }\n\
               my $s = 0;\n\
               for ^5 -> $i { { multi f(Str $x) { 2 }; $s += f($i) } }\n\
               say $s;";
    let (out, err, ok) = run(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "5\n");
    assert!(
        full_resolves(&err, "f") >= 5,
        "a block adding a candidate of `f` must retire `f`'s caches: {err}"
    );
}
