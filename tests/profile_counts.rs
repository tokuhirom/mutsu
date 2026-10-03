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

use crate::profile_doc;

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

/// A loop whose body sits on one line still counts a hit per trip (#8737).
///
/// A hit is an arrival at a line. Coming back around a loop is an arrival even
/// when the line does not change: a compound loop entering its body for the
/// next trip, or an `nqp::while`'s backward jump. Before #8737 only a line
/// *change* counted, so every one-line loop below reported `hits 1`. A line
/// holding a whole loop is reached once from the line above and once per trip,
/// so it reports `TRIPS + 1` — the same `trips + 1` a multi-line `while`'s
/// condition line has always reported.
const ONE_LINE_TRIPS: u64 = 100;

fn one_line_fixture() -> String {
    let n = ONE_LINE_TRIPS;
    format!(
        "use nqp;\n\
         my $a = 0;\n\
         for 1..{n} {{ $a = $a + 1 }}\n\
         my $b = 0;\n\
         for 1..{n} {{\n\
         \x20   $b = $b + 1;\n\
         }}\n\
         my $c = 0; my $i = 0;\n\
         while $i < {n} {{ $c = $c + 1; $i = $i + 1 }}\n\
         my $d = 0;\n\
         loop (my $k = 0; $k < {n}; $k++) {{ $d = $d + 1 }}\n\
         my $e = 0;\n\
         repeat {{ $e = $e + 1 }} while $e < {n};\n\
         my $f = 0;\n\
         $f++ for ^{n};\n\
         my int $g = 0;\n\
         nqp::while($g < {n}, $g = $g + 1);\n\
         say \"$a $b $c $d $e $f $g\";\n"
    )
}

#[test]
fn one_line_loop_bodies_count_per_trip() {
    let path = fixture_path("counts-one-line", &one_line_fixture());
    let off = profile(&path, &[("MUTSU_JIT", "off")]);
    let on = profile(&path, &[("MUTSU_JIT", "on"), ("MUTSU_JIT_THRESHOLD", "1")]);
    let _ = std::fs::remove_file(&path);

    let n = ONE_LINE_TRIPS;
    assert_eq!(off.stdout, format!("{n} {n} {n} {n} {n} {n} {n}\n"));
    let hits = |line: u32| -> u64 {
        off.hits(line)
            .unwrap_or_else(|| panic!("line {line} is missing from {:?}", off.line_hits()))
    };
    assert_eq!(hits(3), n + 1, "one-line `for`");
    assert_eq!(hits(5), 1, "a multi-line `for` header is reached once");
    assert_eq!(
        hits(6),
        n,
        "a one-line body on its own line runs once per trip"
    );
    assert_eq!(hits(9), n + 1, "one-line `while`");
    assert_eq!(hits(11), n + 1, "one-line C-style `loop`");
    assert_eq!(hits(13), n + 1, "one-line `repeat ... while`");
    assert_eq!(hits(15), n + 1, "statement-modifier `for`");
    assert_eq!(
        hits(17),
        n + 1,
        "one-line `nqp::while` (a plain backward jump)"
    );
    assert_eq!(hits(18), 1);

    // ADR-0106 §8 gate 4 holds for the new arrivals too.
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
}

/// A return is not an arrival: a line that calls a routine is counted once per
/// time control reaches it, not once more each time the call comes back. The
/// interpreter (which polls every op) used to count the return and native code
/// (which hooks only line transitions) did not, so a JIT-compiled caller
/// reported fewer hits than an interpreted one (#8737).
#[test]
fn a_return_from_a_call_is_not_another_hit() {
    let path = fixture_path(
        "counts-call-return",
        "sub g() { 1 }\nmy $t = 0;\n$t += g() for ^20;\nfor ^20 {\n    $t += g();\n}\nsay $t;\n",
    );
    let off = profile(&path, &[("MUTSU_JIT", "off")]);
    let on = profile(&path, &[("MUTSU_JIT", "on"), ("MUTSU_JIT_THRESHOLD", "1")]);
    let _ = std::fs::remove_file(&path);

    assert_eq!(off.stdout, "40\n");
    assert_eq!(off.hits(3), Some(21), "statement-modifier loop calling `g`");
    assert_eq!(off.hits(5), Some(20), "a loop body line calling `g`");
    assert_eq!(off.hits(3), on.hits(3), "JIT parity on the calling line");
    assert_eq!(off.hits(5), on.hits(5), "JIT parity on the calling line");
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
