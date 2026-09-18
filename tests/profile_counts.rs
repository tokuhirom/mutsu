//! ADR-0106 §8 gates 3 and 4 for the exact counters (Slice 3).
//!
//! Gate 3 is attribution correctness: on a fixture whose trip count is known
//! by construction, the top self line is that line and its `hits` equals the
//! trip count. Gate 4 is JIT parity: the same fixture profiled with the JIT on
//! and off produces the same top line and the same `hits`.
//!
//! Both are asserted against the published JSON document (`docs/profiler.md`),
//! which is also what makes them a check on Slice 5's schema: `hits` has to be
//! exactly the trip count *and* has to be where the document says it is.
//! Together with `profile_samples.rs` they are what pins the JIT side at all —
//! the only other coverage builds a `CompiledCode` by hand and calls
//! `record_line` directly, which no codegen change can break.

mod profile_doc;

use profile_doc::{fixture_path, profile};

/// The `$i = $i + 1` and `$s = $s + $i` lines each run exactly `TRIPS` times;
/// the `while` condition runs once more (the test that ends the loop).
const TRIPS: u64 = 5000;

fn fixture() -> String {
    format!(
        "my $s = 0;\nmy $i = 0;\nwhile $i < {TRIPS} {{\n    $i = $i + 1;\n    $s = $s + $i;\n}}\nsay $s;\n"
    )
}

/// ADR-0106 §8 gate 3: the hot lines' hit counts are the trip counts, exactly.
#[test]
fn line_hits_are_the_trip_counts() {
    let path = fixture_path("counts-gate3", &fixture());
    let run = profile(&path, &[("MUTSU_JIT", "off")]);
    let _ = std::fs::remove_file(&path);
    assert_eq!(run.stdout, format!("{}\n", (TRIPS * (TRIPS + 1)) / 2));

    let hits = |line: u32| -> u64 {
        run.hits(line)
            .unwrap_or_else(|| panic!("line {line} is missing from {:?}", run.line_hits()))
    };
    // The loop body's two lines run once per trip; the condition runs once
    // more, the time it is false. The body lines are therefore the answer to
    // "which line costs the most", which is what a profile is opened for.
    assert_eq!(hits(4), TRIPS, "$i = $i + 1");
    assert_eq!(hits(5), TRIPS, "$s = $s + $i");
    assert_eq!(hits(3), TRIPS + 1, "the while condition");
    // The top row is the hottest line, and everything outside the loop is 1.
    assert_eq!(run.line_hits()[0].2, TRIPS + 1);
    assert_eq!(hits(1), 1);
    assert_eq!(hits(7), 1);
}

/// ADR-0106 §8 gate 4: the JIT-compiled body's emitted `profile_line` hooks
/// and the interpreter's per-dispatch hook produce the same counts, not merely
/// the same shape.
#[test]
fn jit_on_and_off_produce_the_same_counts() {
    let path = fixture_path("counts-gate4", &fixture());
    let off = profile(&path, &[("MUTSU_JIT", "off")]);
    let on = profile(&path, &[("MUTSU_JIT", "on"), ("MUTSU_JIT_THRESHOLD", "1")]);
    let _ = std::fs::remove_file(&path);

    assert_eq!(off.stdout, on.stdout, "the program's own output diverged");
    if cfg!(feature = "jit") {
        assert!(
            on.jit_entries() > 0,
            "the JIT never entered, so this proves nothing about JIT parity"
        );
    }
    assert_eq!(
        off.line_hits(),
        on.line_hits(),
        "line counts diverge between JIT off and on"
    );
    assert_eq!(off.routine_entries(), on.routine_entries());
    assert_eq!(off.callsite_calls(), on.callsite_calls());
}

/// Routine entries and call sites are counted exactly too, and a routine that
/// is hot enough to be JIT-compiled is counted the same either way.
#[test]
fn routine_and_callsite_counts_are_exact() {
    let path = fixture_path(
        "counts-sub",
        "sub add($a, $b) { return $a + $b }\nmy $s = 0;\nfor ^100 { $s = add($s, 1) }\nsay $s;\n",
    );
    let off = profile(&path, &[("MUTSU_JIT", "off")]);
    let on = profile(&path, &[("MUTSU_JIT", "on"), ("MUTSU_JIT_THRESHOLD", "1")]);
    let _ = std::fs::remove_file(&path);

    assert_eq!(off.stdout, "100\n");
    assert_eq!(on.stdout, off.stdout);
    let entries = off
        .routine_entries()
        .into_iter()
        .find(|(name, _)| name.ends_with("::add"))
        .unwrap_or_else(|| panic!("`add` is missing from {:?}", off.routine_entries()))
        .1;
    assert_eq!(entries, 100, "`add` is called exactly 100 times");
    assert_eq!(off.routine_entries(), on.routine_entries());
    assert_eq!(off.callsite_calls(), on.callsite_calls());
    // The caller edge carries the same exact count, keyed by the line that made
    // the call -- the column a flat line table cannot produce (ADR-0106 D3).
    assert!(
        off.callsite_calls()
            .iter()
            .any(|(edge, calls)| edge.ends_with(":3 -> GLOBAL::add") && *calls == 100),
        "the caller edge from line 3 is missing or miscounted: {:?}",
        off.callsite_calls()
    );
}
