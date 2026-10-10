//! Pins that a routine called from a method resolves once, not once per call
//! (ADR-12529 phase 2).
//!
//! A method body used to run in whichever compilation unit its caller was in,
//! so a call to a routine its own module declared or imported resolved
//! through the frame-anchored unit-private fallback, which the plain
//! resolution memo did not record. Every call then paid the whole resolution
//! walk. The assertion is a slope between two iteration counts, so module
//! loading cancels out.

use std::process::Command;

fn full_resolves(n: u32) -> u64 {
    let src = format!(
        "use lib 't/lib'; use OwnUnitCounterClass; \
         my $c = OwnUnitCounter.new; my $s = 0; \
         $s = $c.step($s) for ^{n}; say $s;"
    );
    let out = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg("-e")
        .arg(&src)
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .env("MUTSU_VM_STATS", "1")
        .output()
        .expect("failed to spawn mutsu");
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(out.status.success(), "run failed: {stderr}");
    assert_eq!(String::from_utf8_lossy(&out.stdout), format!("{n}\n"));
    stderr
        .lines()
        .find(|l| l.contains("] function-full-resolve"))
        .and_then(|l| l.split_whitespace().find_map(|w| w.strip_prefix("total=")))
        .map(|v| v.parse().expect("a count"))
        .unwrap_or(0)
}

#[test]
fn an_imported_routine_called_from_a_method_resolves_once() {
    let few = full_resolves(10);
    let many = full_resolves(200);
    assert_eq!(
        many, few,
        "190 more calls from the method added full resolutions ({few} -> {many})"
    );
}
