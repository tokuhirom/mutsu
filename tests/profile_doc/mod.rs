//! Shared reader for the profile document (`docs/profiler.md`), used by every
//! profiler test.
//!
//! The tests read the **documented JSON artifact**, not a debug print. Two
//! reasons, and they are the reason ADR-0106 Slice 5 replaced the scaffolding
//! report these tests used to parse: the document is what a consumer actually
//! reads, so asserting on it pins the schema rather than an internal spelling;
//! and there is exactly one place where the meaning of a field lives, so a test
//! cannot quietly assert something the published profile does not say.
//!
//! Every accessor here exposes **counts and structure**. Times are reachable
//! (the header's `wall_us`/`sampled_us`, needed for the one-sided
//! blocked-thread claim) but no test may assert one: ADR-0106 D5 makes a
//! duration assertion a flaky test by construction.

// Each of the three test files reads a different subset of the document.
#![allow(dead_code)]

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::process::Command;

pub struct Profiled {
    pub stdout: String,
    pub stderr: String,
    doc: serde_json::Value,
}

/// Write `source` to a uniquely named file under the crate's `tmp/`, so the
/// reported locations are a real path rather than `-e`, and two test binaries
/// running at once cannot collide.
pub fn fixture_path(tag: &str, source: &str) -> PathBuf {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tmp");
    std::fs::create_dir_all(&dir).expect("cannot create tmp/");
    let path = dir.join(format!("profile-{tag}-{}.raku", std::process::id()));
    std::fs::write(&path, source).expect("cannot write the fixture");
    path
}

/// Profile `path` and return its document.
///
/// `MUTSU_VM_STATS=1` rides along because two gates compare the JIT's hooks
/// against the interpreter's and need to know the JIT actually entered.
pub fn profile(path: &Path, envs: &[(&str, &str)]) -> Profiled {
    let out = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tmp")
        .join(format!(
            "profile-doc-{}-{}.json",
            std::process::id(),
            next_serial(),
        ));
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg(format!("--profile={}", out.display()))
        .arg("--profile-report=json");
    cmd.arg(path);
    // An inherited profiler or JIT setting would silently change what is being
    // measured, so the test owns every one of them.
    for key in [
        "MUTSU_JIT",
        "MUTSU_JIT_THRESHOLD",
        "MUTSU_PROFILE",
        "MUTSU_PROFILE_RATE",
        "MUTSU_PROFILE_TICK",
        "MUTSU_PROFILE_KIND",
        "MUTSU_PROFILE_REPORT",
        "MUTSU_PROFILE_OUT",
    ] {
        cmd.env_remove(key);
    }
    cmd.env("MUTSU_VM_STATS", "1");
    for (key, value) in envs {
        cmd.env(key, value);
    }
    let output = cmd.output().expect("failed to spawn mutsu");
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(output.status.success(), "profiled run failed: {stderr}");
    let json = std::fs::read_to_string(&out)
        .unwrap_or_else(|err| panic!("no profile at {}: {err}\nstderr: {stderr}", out.display()));
    let _ = std::fs::remove_file(&out);
    Profiled {
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr,
        doc: serde_json::from_str(&json).expect("the profile is not valid JSON"),
    }
}

fn next_serial() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static SERIAL: AtomicU64 = AtomicU64::new(0);
    SERIAL.fetch_add(1, Ordering::Relaxed)
}

impl Profiled {
    pub fn schema_version(&self) -> u64 {
        self.doc["mutsu_prof_version"]
            .as_u64()
            .expect("no schema version")
    }

    pub fn header(&self, field: &str) -> &serde_json::Value {
        &self.doc["header"][field]
    }

    fn sampling(&self, field: &str) -> &serde_json::Value {
        &self.doc["header"]["sampling"][field]
    }

    pub fn samples(&self) -> u64 {
        self.sampling("samples").as_u64().expect("no sample count")
    }

    pub fn truncated(&self) -> u64 {
        self.sampling("truncated_samples")
            .as_u64()
            .expect("no truncated count")
    }

    pub fn threads(&self) -> u64 {
        self.sampling("threads").as_u64().expect("no thread count")
    }

    pub fn rate_hz(&self) -> u64 {
        self.sampling("rate_hz").as_u64().expect("no rate")
    }

    pub fn tick(&self) -> String {
        self.sampling("tick")
            .as_str()
            .expect("no tick source")
            .to_string()
    }

    pub fn top_region(&self) -> String {
        self.sampling("top_region")
            .as_str()
            .unwrap_or("none")
            .to_string()
    }

    pub fn wall_us(&self) -> f64 {
        self.sampling("wall_us").as_f64().expect("no wall time")
    }

    pub fn sampled_us(&self) -> f64 {
        self.sampling("sampled_us")
            .as_f64()
            .expect("no sampled time")
    }

    fn lines(&self) -> impl Iterator<Item = (&str, &serde_json::Value)> {
        self.doc["files"]
            .as_array()
            .map(|files| files.as_slice())
            .unwrap_or_default()
            .iter()
            .flat_map(|file| {
                let path = file["path"].as_str().expect("a file row without a path");
                file["lines"]
                    .as_array()
                    .expect("a file row without lines")
                    .iter()
                    .map(move |line| (path, line))
            })
    }

    fn routines(&self) -> impl Iterator<Item = &serde_json::Value> {
        self.doc["routines"]
            .as_array()
            .map(|routines| routines.as_slice())
            .unwrap_or_default()
            .iter()
    }

    /// `(file, line, hits)` for every exactly-counted line, hottest first. The
    /// order is the one a report ranks by, so `[0]` is "the top line".
    pub fn line_hits(&self) -> Vec<(String, u32, u64)> {
        let mut rows: Vec<(String, u32, u64)> = self
            .lines()
            .filter_map(|(path, line)| {
                Some((path.to_string(), line_number(line), line["hits"].as_u64()?))
            })
            .collect();
        rows.sort_by(|a, b| b.2.cmp(&a.2).then(a.0.cmp(&b.0)).then(a.1.cmp(&b.1)));
        rows
    }

    /// Every line the counters saw, as a set.
    pub fn counted_lines(&self) -> BTreeSet<(String, u32)> {
        self.line_hits()
            .into_iter()
            .map(|(file, line, _)| (file, line))
            .collect()
    }

    /// The hit count of one line, by line number, in the only file the fixture
    /// has.
    pub fn hits(&self, line: u32) -> Option<u64> {
        self.lines()
            .find(|(_, row)| line_number(row) == line)
            .and_then(|(_, row)| row["hits"].as_u64())
    }

    /// Lines carrying sampled self time. A line with `hits` but no `self_us` is
    /// absent here rather than present with a zero — the document does not
    /// zero-fill what it did not measure.
    pub fn self_lines(&self) -> BTreeSet<(String, u32)> {
        self.lines()
            .filter(|(_, row)| row["self_us"].is_f64())
            .map(|(path, row)| (path.to_string(), line_number(row)))
            .collect()
    }

    /// `(file, line, region)` for every per-line subsystem split row.
    pub fn line_regions(&self) -> BTreeSet<(String, u32, String)> {
        let mut rows = BTreeSet::new();
        for (path, line) in self.lines() {
            for region in line["regions"].as_array().unwrap_or(&Vec::new()) {
                rows.insert((
                    path.to_string(),
                    line_number(line),
                    region["region"]
                        .as_str()
                        .expect("a region row without a tag")
                        .to_string(),
                ));
            }
        }
        rows
    }

    /// `Package::name -> entries` for every exactly-counted routine, hottest
    /// first.
    pub fn routine_entries(&self) -> Vec<(String, u64)> {
        let mut rows: Vec<(String, u64)> = self
            .routines()
            .filter_map(|routine| Some((qualified(routine), routine["entries"].as_u64()?)))
            .collect();
        rows.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        rows
    }

    pub fn self_routines(&self) -> BTreeSet<String> {
        self.routines()
            .filter(|routine| routine["self_us"].is_f64())
            .map(qualified)
            .collect()
    }

    pub fn incl_routines(&self) -> BTreeSet<String> {
        self.routines()
            .filter(|routine| routine["incl_us"].is_f64())
            .map(qualified)
            .collect()
    }

    /// `file:line -> Package::name` for every caller edge, with its exact call
    /// count, hottest first.
    pub fn callsite_calls(&self) -> Vec<(String, u64)> {
        let mut rows: Vec<(String, u64)> = self
            .routines()
            .flat_map(|routine| {
                let callee = qualified(routine);
                routine["callers"]
                    .as_array()
                    .cloned()
                    .unwrap_or_default()
                    .into_iter()
                    .filter_map(move |caller| {
                        Some((edge(&caller, &callee), caller["calls"].as_u64()?))
                    })
            })
            .collect();
        rows.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        rows
    }

    /// Caller edges carrying sampled inclusive time.
    pub fn incl_callsites(&self) -> BTreeSet<String> {
        let mut rows = BTreeSet::new();
        for routine in self.routines() {
            let callee = qualified(routine);
            for caller in routine["callers"].as_array().unwrap_or(&Vec::new()) {
                if caller["incl_us"].is_f64() {
                    rows.insert(edge(caller, &callee));
                }
            }
        }
        rows
    }

    /// `region -> samples`, the deterministic half of a region row.
    pub fn region_samples(&self) -> BTreeMap<String, u64> {
        self.doc["regions"]
            .as_array()
            .cloned()
            .unwrap_or_default()
            .iter()
            .map(|region| {
                (
                    region["region"].as_str().expect("no tag").to_string(),
                    region["samples"].as_u64().expect("no sample count"),
                )
            })
            .collect()
    }

    pub fn excluded_regions(&self) -> BTreeSet<String> {
        self.doc["excluded_regions"]
            .as_array()
            .cloned()
            .unwrap_or_default()
            .iter()
            .map(|region| region["region"].as_str().expect("no tag").to_string())
            .collect()
    }

    /// How many times the JIT entered a compiled body, from `MUTSU_VM_STATS`.
    /// Zero is a real answer (the JIT was off, or nothing got hot), which is
    /// why the gates that care assert on it explicitly.
    pub fn jit_entries(&self) -> u64 {
        self.stderr
            .lines()
            .find(|line| line.contains("jit: compiles="))
            .and_then(|line| {
                line.split_whitespace()
                    .find_map(|word| word.strip_prefix("entries="))?
                    .parse()
                    .ok()
            })
            .unwrap_or(0)
    }
}

fn line_number(row: &serde_json::Value) -> u32 {
    row["line"].as_u64().expect("a line row without a number") as u32
}

fn qualified(routine: &serde_json::Value) -> String {
    format!(
        "{}::{}",
        routine["package"].as_str().expect("no package"),
        routine["name"].as_str().expect("no name"),
    )
}

fn edge(caller: &serde_json::Value, callee: &str) -> String {
    format!(
        "{}:{} -> {callee}",
        caller["file"].as_str().expect("no caller file"),
        caller["line"].as_u64().expect("no caller line"),
    )
}
