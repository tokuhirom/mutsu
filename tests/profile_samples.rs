//! ADR-0106 Slice 2 — the sampler, asserted the only way D5 permits.
//!
//! **No test here asserts a duration or a sample count**, because a test that
//! did would be a flaky test by construction: the sampler's numbers are a
//! function of how fast the machine ran, and under this repo's definition of
//! risk a flaky test is a worse outcome than a missing one. What is asserted
//! is *structure* — which locations a profile names, which routines and caller
//! edges it links, which threads it covers — all of which are a function of
//! the bytecode alone.
//!
//! The lever that makes that possible is `MUTSU_PROFILE_TICK=every-poll`: it
//! replaces the timer with "every poll is a tick", so the set of samples a run
//! takes is determined by the code it executes rather than by the clock. Its
//! *times* are not a ground truth (the mode's own per-poll overhead is several
//! times the work it measures) — only its structure is, and structure is all
//! these tests read.
//!
//! The one place a duration appears is
//! [`blocked_time_is_not_charged_to_a_raku_line`], and it is a one-sided bound
//! that cannot flake: a `sleep` is excluded from the weighting by
//! construction, so the gap it opens can only ever grow under load.

mod profile_doc;

use profile_doc::{fixture_path, profile};

const HOT_LOOP: &str = "\
sub work($n) {
    my $s = 0;
    my $i = 0;
    while $i < $n {
        $s = $s + $i;
        $i = $i + 1;
    }
    return $s;
}
my $t = work(4000);
say $t;
";

/// Every location a profile names is a location the program actually ran, and
/// every routine it names is a routine the program actually called. That is
/// the attribution claim the sampler can make without naming a duration: a
/// sampled row resolved through the static ip->line table, never through
/// `cur_source_line` (ADR-0106 D2/§6b, where reading the stale call-site line
/// would have produced exactly the plausible-looking wrong answer).
#[test]
fn a_sample_names_a_line_the_program_really_ran() {
    let path = fixture_path("structure", HOT_LOOP);
    let run = profile(
        &path,
        &[("MUTSU_PROFILE_TICK", "every-poll"), ("MUTSU_JIT", "off")],
    );
    let _ = std::fs::remove_file(&path);
    assert_eq!(run.stdout, "7998000\n");

    assert!(
        run.samples() > 0,
        "every poll is a tick in this mode, so a run that executes bytecode \
         samples; 0 means the sampler never ran"
    );
    // The sampled rows are a subset of the exactly-counted ones: a location
    // that no line-transition counter ever saw would be a fabricated one.
    for location in &run.self_lines() {
        assert!(
            run.counted_lines().contains(location),
            "{location:?} was sampled but never counted; counted: {:?}",
            run.counted_lines()
        );
    }
    // The loop body is what a profile of this fixture exists to find.
    let sampled = run.self_lines();
    let file = sampled
        .iter()
        .next()
        .expect("no sampled lines at all")
        .0
        .clone();
    for line in [4, 5, 6] {
        assert!(
            run.self_lines().contains(&(file.clone(), line)),
            "line {line} of the hot loop is missing from {:?}",
            run.self_lines()
        );
    }
    assert!(run.self_routines().contains("GLOBAL::work"));
    assert!(run.incl_routines().contains("GLOBAL::work"));
    assert!(
        run.incl_callsites()
            .iter()
            .any(|edge| edge.ends_with(":10 -> GLOBAL::work")),
        "the caller edge from line 10 is missing from {:?}",
        run.incl_callsites()
    );
    assert_eq!(run.truncated(), 0, "this stack is two frames deep");
    assert_eq!(run.threads(), 1);
    // The document says of itself that its times are sampled, so a pasted
    // profile cannot be mistaken for a bench-CI measurement (ADR-0106 §7), and
    // that a thread parked in a native call is absent rather than idle.
    assert_eq!(run.header("time_is_sampled"), &serde_json::json!(true));
    assert_eq!(
        run.header("blocked_threads_absent"),
        &serde_json::json!(true)
    );
    assert_eq!(run.schema_version(), 1);
}

/// ADR-0106 §8 gate 4, for the time half: the JIT's emitted per-line hooks and
/// the interpreter's per-opcode poll attribute to the same places.
///
/// Only the *sets* are compared. A native body and an interpreted one do not
/// take the same number of polls or spend the same time per line, and neither
/// of those is what gate 4 is about — it asks whether a profile silently loses
/// resolution the moment a loop gets hot, which is precisely when a profiler
/// is opened.
#[test]
fn jit_on_and_off_name_the_same_places() {
    let path = fixture_path("gate4", HOT_LOOP);
    let off = profile(
        &path,
        &[("MUTSU_PROFILE_TICK", "every-poll"), ("MUTSU_JIT", "off")],
    );
    let on = profile(
        &path,
        &[
            ("MUTSU_PROFILE_TICK", "every-poll"),
            ("MUTSU_JIT", "on"),
            ("MUTSU_JIT_THRESHOLD", "1"),
        ],
    );
    let _ = std::fs::remove_file(&path);

    assert_eq!(off.stdout, on.stdout, "the program's own output diverged");
    if cfg!(feature = "jit") {
        assert!(
            on.jit_entries() > 0,
            "the JIT never entered, so this proves nothing about JIT parity"
        );
    }
    assert_eq!(off.self_lines(), on.self_lines(), "sampled lines diverge");
    assert_eq!(off.self_routines(), on.self_routines());
    assert_eq!(off.incl_routines(), on.incl_routines());
    assert_eq!(off.incl_callsites(), on.incl_callsites());
}

/// A `start` block's work lands on its own thread's stack rather than going
/// missing — the threaded case a per-thread tick and per-thread buffers exist
/// for, and the one a retrofit would not have covered.
#[test]
fn a_start_block_is_profiled_on_its_own_thread() {
    let path = fixture_path(
        "threaded",
        "\
sub spun($n) {
    my $i = 0;
    while $i < $n { $i = $i + 1 }
    return $i;
}
my $p = start { spun(6000) };
my $main = spun(6000);
say $main + await $p;
",
    );
    let run = profile(
        &path,
        &[("MUTSU_PROFILE_TICK", "every-poll"), ("MUTSU_JIT", "off")],
    );
    let _ = std::fs::remove_file(&path);
    assert_eq!(run.stdout, "12000\n");

    assert!(
        run.threads() >= 2,
        "the worker never polled, so nothing was sampled on it"
    );
    assert!(
        run.incl_routines()
            .iter()
            .any(|routine| routine.contains("pointy-block")),
        "the `start` block itself is absent from {:?}",
        run.incl_routines()
    );
    assert!(run.self_routines().contains("GLOBAL::spun"));
    // `spun` is called once on each thread. A worker-thread table that the
    // report never drained would say 1 -- which is what it did say before the
    // tables were registered rather than folded only on thread exit.
    assert!(
        run.incl_routines().contains("GLOBAL::spun"),
        "{:?}",
        run.incl_routines()
    );
}

/// Time a thread spends blocked in a native call is not time a Raku line was
/// running, so the weighting does not charge it to one.
///
/// The assertion is one-sided and cannot flake: the fixture sleeps for a whole
/// second, that second is excluded by construction, and load can only make the
/// gap between wall time and sampled time wider — never narrower.
#[test]
fn blocked_time_is_not_charged_to_a_raku_line() {
    let path = fixture_path(
        "blocked",
        "my $i = 0;\nwhile $i < 2000 { $i = $i + 1 }\nsleep 1;\nsay $i;\n",
    );
    let run = profile(&path, &[("MUTSU_JIT", "off")]);
    let _ = std::fs::remove_file(&path);
    assert_eq!(run.stdout, "2000\n");

    assert!(
        run.wall_us() > run.sampled_us() + 500_000.0,
        "the second spent asleep was charged to Raku code: {}us wall vs {}us \
         sampled",
        run.wall_us(),
        run.sampled_us(),
    );
}

/// The shipped configuration — a timer thread, nobody touching
/// `MUTSU_PROFILE_TICK` — produces a well-formed report and the program's own
/// output is unchanged by profiling it.
#[test]
fn the_default_timer_tick_produces_a_well_formed_report() {
    let path = fixture_path("timer", HOT_LOOP);
    let plain = std::process::Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg(&path)
        .output()
        .expect("failed to spawn mutsu");
    let run = profile(&path, &[("MUTSU_PROFILE_RATE", "2000")]);
    let _ = std::fs::remove_file(&path);

    assert_eq!(
        run.stdout,
        String::from_utf8_lossy(&plain.stdout),
        "profiling changed what the program printed"
    );
    assert_eq!(
        run.rate_hz(),
        2000,
        "MUTSU_PROFILE_RATE did not reach the sampler"
    );
    assert_eq!(run.tick(), "timer");
    // Nothing is asserted about which lines got samples: at a fixed rate that
    // is a question about how fast this machine ran the fixture.
    for location in &run.self_lines() {
        assert!(
            run.counted_lines().contains(location),
            "{location:?} was sampled but never counted"
        );
    }
}
