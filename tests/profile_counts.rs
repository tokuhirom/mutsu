//! ADR-0106 §8 gates 3 and 4 for the exact counters (Slice 3).
//!
//! Gate 3 is attribution correctness: on a fixture whose trip count is known
//! by construction, the top self line is that line and its `hits` equals the
//! trip count. Gate 4 is JIT parity: the same fixture profiled with the JIT on
//! and off produces the same top line and the same `hits`.
//!
//! Both were unassertable until `profile::flush_at_exit` gained an observable
//! surface — the counters were folded into a static nothing read — so this is
//! the first end-to-end check that the interpreter's per-dispatch hook and the
//! JIT's emitted `profile_line` hook agree. They are also what pins the JIT
//! side at all: the only other coverage builds a `CompiledCode` by hand and
//! calls `record_line` directly, which no codegen change can break.

use std::io::Write;
use std::process::Command;

/// The `$i = $i + 1` and `$s = $s + $i` lines each run exactly `TRIPS` times;
/// the `while` condition runs once more (the test that ends the loop).
const TRIPS: u64 = 5000;

fn fixture() -> String {
    format!(
        "my $s = 0;\nmy $i = 0;\nwhile $i < {TRIPS} {{\n    $i = $i + 1;\n    $s = $s + $i;\n}}\nsay $s;\n"
    )
}

/// Write the fixture to a uniquely named file under the crate's `tmp/`, so the
/// reported locations are a real path rather than `-e`, and two test binaries
/// running at once cannot collide.
fn fixture_path(tag: &str) -> std::path::PathBuf {
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tmp");
    std::fs::create_dir_all(&dir).expect("cannot create tmp/");
    let path = dir.join(format!("profile-counts-{tag}-{}.raku", std::process::id()));
    let mut f = std::fs::File::create(&path).expect("cannot write the fixture");
    f.write_all(fixture().as_bytes())
        .expect("cannot write the fixture");
    path
}

struct Run {
    stdout: String,
    /// `(file, line, hits)` rows of the profile report, hottest first.
    lines: Vec<(String, u32, u64)>,
    routines: Vec<(String, u64)>,
    callsites: Vec<(String, u64)>,
    jit_entries: u64,
}

fn profile(path: &std::path::Path, envs: &[(&str, &str)]) -> Run {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg(path);
    for k in ["MUTSU_JIT", "MUTSU_JIT_THRESHOLD", "MUTSU_PROFILE"] {
        cmd.env_remove(k);
    }
    cmd.env("MUTSU_PROFILE", "1").env("MUTSU_VM_STATS", "1");
    for (k, v) in envs {
        cmd.env(k, v);
    }
    let out = cmd.output().expect("failed to spawn mutsu");
    assert!(
        out.status.success(),
        "profiled run failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    let mut lines = Vec::new();
    let mut routines = Vec::new();
    let mut callsites = Vec::new();
    for row in stderr.lines() {
        if let Some(rest) = row.strip_prefix("profile: line ") {
            // `<file>:<line> hits=<n>`
            let (location, hits) = rest.split_once(" hits=").expect("malformed line row");
            let (file, line) = location.rsplit_once(':').expect("malformed location");
            lines.push((
                file.to_string(),
                line.parse().expect("bad line number"),
                hits.parse().expect("bad hit count"),
            ));
        } else if let Some(rest) = row.strip_prefix("profile: routine ") {
            let (name, entries) = rest.split_once(" entries=").expect("malformed routine row");
            routines.push((name.to_string(), entries.parse().expect("bad entry count")));
        } else if let Some(rest) = row.strip_prefix("profile: callsite ") {
            let (name, calls) = rest.split_once(" calls=").expect("malformed callsite row");
            callsites.push((name.to_string(), calls.parse().expect("bad call count")));
        }
    }
    let jit_entries = stderr
        .lines()
        .find(|l| l.contains("jit: compiles="))
        .and_then(|l| {
            l.split_whitespace()
                .find_map(|w| w.strip_prefix("entries="))?
                .parse()
                .ok()
        })
        .unwrap_or(0);
    Run {
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        lines,
        routines,
        callsites,
        jit_entries,
    }
}

/// ADR-0106 §8 gate 3: the hot lines' hit counts are the trip counts, exactly.
#[test]
fn line_hits_are_the_trip_counts() {
    let path = fixture_path("gate3");
    let run = profile(&path, &[("MUTSU_JIT", "off")]);
    let _ = std::fs::remove_file(&path);
    assert_eq!(run.stdout, format!("{}\n", (TRIPS * (TRIPS + 1)) / 2));

    let hits = |line: u32| -> u64 {
        run.lines
            .iter()
            .find(|(_, l, _)| *l == line)
            .unwrap_or_else(|| panic!("line {line} is missing from {:?}", run.lines))
            .2
    };
    // The loop body's two lines run once per trip; the condition runs once
    // more, the time it is false. The body lines are therefore the answer to
    // "which line costs the most", which is what a profile is opened for.
    assert_eq!(hits(4), TRIPS, "$i = $i + 1");
    assert_eq!(hits(5), TRIPS, "$s = $s + $i");
    assert_eq!(hits(3), TRIPS + 1, "the while condition");
    // The top row is the hottest line, and everything outside the loop is 1.
    assert_eq!(run.lines[0].2, TRIPS + 1);
    assert_eq!(hits(1), 1);
    assert_eq!(hits(7), 1);
}

/// ADR-0106 §8 gate 4: the JIT-compiled body's emitted `profile_line` hooks
/// and the interpreter's per-dispatch hook produce the same counts, not merely
/// the same shape.
#[test]
fn jit_on_and_off_produce_the_same_counts() {
    let path = fixture_path("gate4");
    let off = profile(&path, &[("MUTSU_JIT", "off")]);
    let on = profile(&path, &[("MUTSU_JIT", "on"), ("MUTSU_JIT_THRESHOLD", "1")]);
    let _ = std::fs::remove_file(&path);

    assert_eq!(off.stdout, on.stdout, "the program's own output diverged");
    if cfg!(feature = "jit") {
        assert!(
            on.jit_entries > 0,
            "the JIT never entered, so this proves nothing about JIT parity"
        );
    }
    assert_eq!(
        off.lines, on.lines,
        "line counts diverge between JIT off and on"
    );
    assert_eq!(off.routines, on.routines);
    assert_eq!(off.callsites, on.callsites);
}

/// Routine entries and call sites are counted exactly too, and a routine that
/// is hot enough to be JIT-compiled is counted the same either way.
#[test]
fn routine_and_callsite_counts_are_exact() {
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tmp");
    std::fs::create_dir_all(&dir).expect("cannot create tmp/");
    let path = dir.join(format!("profile-counts-sub-{}.raku", std::process::id()));
    std::fs::write(
        &path,
        "sub add($a, $b) { return $a + $b }\nmy $s = 0;\nfor ^100 { $s = add($s, 1) }\nsay $s;\n",
    )
    .expect("cannot write the fixture");

    let off = profile(&path, &[("MUTSU_JIT", "off")]);
    let on = profile(&path, &[("MUTSU_JIT", "on"), ("MUTSU_JIT_THRESHOLD", "1")]);
    let _ = std::fs::remove_file(&path);

    assert_eq!(off.stdout, "100\n");
    assert_eq!(on.stdout, off.stdout);
    let entries = off
        .routines
        .iter()
        .find(|(name, _)| name.ends_with("::add"))
        .unwrap_or_else(|| panic!("`add` is missing from {:?}", off.routines))
        .1;
    assert_eq!(entries, 100, "`add` is called exactly 100 times");
    assert_eq!(off.routines, on.routines);
    assert_eq!(off.callsites, on.callsites);
}
