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
    capture_layers: u64,
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
        // Present even when zero, so a reader can tell "none" from "not counted".
        capture_layers: field("capture_layers"),
    };
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

/// A closure body that calls itself `depth` times, then creates two closures
/// at the bottom of that dynamic nesting.
fn closure_depth(depth: u32) -> String {
    format!(
        "my &run; &run = -> Int $d, &k {{ $d == 0 ?? k() !! run($d - 1, &k) }};\n\
         my $n = 0; for ^5 {{ $n += run({depth}, -> {{ my &c = -> {{ 1 }}; c() }}) }}; say $n;"
    )
}

#[test]
fn a_closure_capture_ignores_the_calling_closures() {
    // ADR-12529 phase 3 (#12519): a closure created while a closure body runs
    // captures that body's scope, not the closure frames that called it, so
    // its capture does not grow with the dynamic nesting.
    let (out5, c5) = run(&closure_depth(5));
    let (out40, c40) = run(&closure_depth(40));
    assert_eq!(out5, "5\n");
    assert_eq!(out40, "5\n");
    assert_eq!(c5.captures, c40.captures, "same closures created");
    assert_eq!(
        c5.capture_layers, c40.capture_layers,
        "capture layers followed the dynamic nesting"
    );
    assert!(
        c5.capture_layers <= c5.captures,
        "a capture took the callers' layers: {} layers for {} captures",
        c5.capture_layers,
        c5.captures
    );
}

/// A named sub that calls itself `depth` times through a closure, then
/// creates two closures at the bottom of that dynamic nesting.
fn named_sub_depth(depth: u32) -> String {
    format!(
        "sub run(Int $d, &k) {{ $d == 0 ?? k() !! (-> {{ run($d - 1, &k) }})() }}\n\
         sub make() {{ my &c = -> {{ 1 }}; c() }}\n\
         my $n = 0; for ^5 {{ $n += run({depth}, &make) }}; say $n;"
    )
}

#[test]
fn a_named_sub_capture_ignores_its_callers() {
    // ADR-12529 phase 3: a closure created in a named sub's frame captures
    // that frame and the program scope, not the frames that called the sub.
    let (out5, c5) = run(&named_sub_depth(5));
    let (out40, c40) = run(&named_sub_depth(40));
    assert_eq!(out5, "5\n");
    assert_eq!(out40, "5\n");
    assert!(
        c40.capture_layers <= c5.capture_layers,
        "capture layers followed the dynamic nesting: {} at depth 5, {} at depth 40",
        c5.capture_layers,
        c40.capture_layers
    );
}
