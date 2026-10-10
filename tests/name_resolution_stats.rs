//! Pins the `name-resolution` vm-stats line that ADR-12529 phase 0 added.
//!
//! ADR-12529 removes, phase by phase, the by-name resolution a call frame
//! does through its caller's env: the scoped overlay each call chains over
//! the caller (`scoped_overlays`, gone in phase 3), the lookups that miss the
//! frame's own overlay and walk that chain (`chain_walks` / `chain_hops`,
//! phases 1-3), and the closure capture copied out of it (`captures` /
//! `capture_own_entries` / `capture_layers`, gone in phase 4). Each phase
//! states its effect as a change in these counters, so they must keep
//! counting what they claim to.
//!
//! The assertions are slopes between two iteration counts, so start-up work
//! (module loading, the setting) cancels out. A phase that removes one of the
//! mechanisms changes the matching assertion here on purpose.

use std::process::Command;

struct Counts {
    scoped_overlays: u64,
    /// `None` from a release binary, which does not count walks.
    chain_walks: Option<u64>,
    chain_hops: Option<u64>,
    captures: u64,
    capture_own_entries: u64,
}

fn run(src: &str) -> (String, Counts) {
    let out = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg("-e")
        .arg(src)
        .env("MUTSU_VM_STATS", "1")
        .output()
        .expect("failed to spawn mutsu");
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(out.status.success(), "run failed: {stderr}");
    let line = stderr
        .lines()
        .find(|l| l.contains("] name-resolution:"))
        .unwrap_or_else(|| panic!("no name-resolution line in: {stderr}"));
    let raw = |name: &str| -> &str {
        let key = format!("{name}=");
        line.split_whitespace()
            .find_map(|w| w.strip_prefix(key.as_str()))
            .unwrap_or_else(|| panic!("no `{name}` in: {line}"))
    };
    let field = |name: &str| -> u64 {
        raw(name)
            .parse()
            .unwrap_or_else(|_| panic!("`{name}` is not a count in: {line}"))
    };
    let walk_field = |name: &str| -> Option<u64> {
        let v = raw(name);
        (v != "n/a").then(|| field(name))
    };
    let counts = Counts {
        scoped_overlays: field("scoped_overlays"),
        chain_walks: walk_field("chain_walks"),
        chain_hops: walk_field("chain_hops"),
        captures: field("captures"),
        capture_own_entries: field("capture_own_entries"),
    };
    // Present even when zero, so a reader can tell "none" from "not counted".
    field("capture_layers");
    (String::from_utf8_lossy(&out.stdout).into_owned(), counts)
}

/// A sub that returns a closure, called and its result called, `n` times.
fn closure_loop(n: u32) -> String {
    format!(
        "sub mk($a) {{ -> $x {{ $x eq $a }} }}\n\
         my $s = 0; for ^{n} {{ $s += mk('a')('a') }}; say $s;"
    )
}

#[test]
fn counters_track_calls_and_closure_creations() {
    let (out10, c10) = run(&closure_loop(10));
    let (out20, c20) = run(&closure_loop(20));
    assert_eq!(out10, "10\n");
    assert_eq!(out20, "20\n");

    // Ten more iterations create ten more closures, each captured once.
    assert_eq!(
        c20.captures - c10.captures,
        10,
        "one capture per closure creation"
    );
    // Each captured closure copies at least its one free variable (`$a`).
    assert!(
        c20.capture_own_entries - c10.capture_own_entries >= 10,
        "capture entries must be counted"
    );
    // Two calls per iteration (`mk` and the closure); each chains at least
    // one overlay over its caller today.
    assert!(
        c20.scoped_overlays - c10.scoped_overlays >= 20,
        "every call's overlay must be counted"
    );
    // The loop resolves names by walking the caller chain. Only a debug
    // binary counts walks (`Env::get_sym` says why); a release one prints
    // `n/a`, and the test binary is built in the same profile.
    let walks = (
        c10.chain_walks,
        c20.chain_walks,
        c10.chain_hops,
        c20.chain_hops,
    );
    if cfg!(debug_assertions) {
        let (Some(w10), Some(w20), Some(h10), Some(h20)) = walks else {
            panic!("a debug binary must count walks: {walks:?}");
        };
        assert!(w20 > w10, "chain walks must be counted");
        assert!(h20 >= h10, "hops never decrease");
    } else {
        assert_eq!(
            walks,
            (None, None, None, None),
            "a release binary counted walks"
        );
    }
}

#[test]
fn no_stats_line_without_the_switch() {
    // Without MUTSU_VM_STATS nothing is printed; the counters' gate is the
    // same one every other vm-stats counter uses.
    let out = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg("-e")
        .arg(closure_loop(3))
        .env_remove("MUTSU_VM_STATS")
        .output()
        .expect("failed to spawn mutsu");
    assert!(out.status.success());
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !stderr.contains("name-resolution:"),
        "stats printed without MUTSU_VM_STATS: {stderr}"
    );
}
