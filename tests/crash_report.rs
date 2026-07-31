//! End-to-end coverage for the fatal-signal crash report (`src/crash_report.rs`).
//!
//! The point of the feature is that the *one* time a rare SIGSEGV fires on CI
//! it leaves evidence behind, so the pipeline has to be exercised for real: the
//! binary is made to fault via `MUTSU_CRASH_SELFTEST`, and these tests assert
//! both halves of the contract — a report file naming the signal, fault
//! address, pid and argv, and a wait status unchanged from what the crash would
//! have produced on its own.

#![cfg(unix)]

use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Run mutsu with the selftest crash requested, reporting into a private
/// directory. Returns (terminating signal, report directory).
fn run_crashing(selftest: &str, name: &str, extra_env: &[(&str, &str)]) -> (Option<i32>, PathBuf) {
    let dir =
        std::env::temp_dir().join(format!("mutsu-crash-test-{}-{}", name, std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    // The sentinel argument proves the report's argv line identifies *this*
    // process, which is the question a bare `Wstat: 11` cannot answer.
    cmd.args(["-e", "say 'sentinel-marker'"]);
    cmd.env("MUTSU_CRASH_SELFTEST", selftest);
    cmd.env("MUTSU_CRASH_DIR", &dir);
    for (k, v) in extra_env {
        cmd.env(k, v);
    }
    let status = cmd.output().expect("failed to spawn mutsu").status;
    (status.signal(), dir)
}

/// The single report the run left behind.
fn read_report(dir: &Path) -> String {
    let mut entries: Vec<PathBuf> = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("no crash report directory {}: {e}", dir.display()))
        .map(|e| e.expect("readdir").path())
        .collect();
    assert_eq!(entries.len(), 1, "expected one report, got {entries:?}");
    std::fs::read_to_string(entries.pop().unwrap()).expect("failed to read crash report")
}

fn field<'a>(report: &'a str, key: &str) -> &'a str {
    report
        .lines()
        .find_map(|l| l.strip_prefix(key))
        .unwrap_or_else(|| panic!("no {key:?} line in report:\n{report}"))
        .trim()
}

#[test]
fn segv_writes_a_report_and_keeps_the_wait_status() {
    let (signal, dir) = run_crashing("segv", "segv", &[]);
    assert_eq!(
        signal,
        Some(libc::SIGSEGV),
        "process must still die of SIGSEGV"
    );

    let report = read_report(&dir);
    assert_eq!(field(&report, "signal:"), "11 (SIGSEGV)");
    // The selftest dereferences null, so this distinguishes a null deref from
    // a garbage pointer exactly as it must for a real crash.
    assert_eq!(field(&report, "fault-addr:"), "0x0000000000000000");

    // pid must name the process that actually faulted, and the file is named
    // after it.
    let pid = field(&report, "pid:");
    assert!(
        dir.join(format!("{pid}.txt")).exists(),
        "report file should be named after the pid, got {pid}"
    );
    assert_ne!(field(&report, "ppid:"), pid);

    // argv is what answers "which process was this?" — the parent running the
    // .t file, or a subprocess it spawned.
    assert!(
        field(&report, "argv:").contains("sentinel-marker"),
        "argv line should carry this process's arguments:\n{report}"
    );
    assert!(!field(&report, "thread:").is_empty());
    assert!(field(&report, "version:").starts_with(char::is_numeric));
    assert!(report.contains("--- backtrace (raw) ---"));

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn abort_is_reported_too() {
    let (signal, dir) = run_crashing("abort", "abort", &[]);
    assert_eq!(signal, Some(libc::SIGABRT));
    let report = read_report(&dir);
    assert_eq!(field(&report, "signal:"), "6 (SIGABRT)");
    // `abort()` sends the signal, so there is no fault address to report and
    // the siginfo union holds the sender's pid instead — say so rather than
    // printing it as if it were an address.
    assert_eq!(
        field(&report, "fault-addr:"),
        "n/a (signal was sent, not a fault)"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn reporting_can_be_switched_off() {
    let (signal, dir) = run_crashing("segv", "off", &[("MUTSU_CRASH_REPORT", "0")]);
    // The crash itself must be completely unaffected by the opt-out.
    assert_eq!(signal, Some(libc::SIGSEGV));
    assert!(
        !dir.exists(),
        "MUTSU_CRASH_REPORT=0 must not even create the report directory"
    );
}

/// An ordinary, non-crashing run must leave no trace: the report directory is
/// created from inside the handler, never at startup.
#[test]
fn a_clean_run_creates_no_report_directory() {
    let dir = std::env::temp_dir().join(format!("mutsu-crash-test-clean-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    let out = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .args(["-e", "say 42"])
        .env("MUTSU_CRASH_DIR", &dir)
        .output()
        .expect("failed to spawn mutsu");
    assert!(out.status.success());
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "42");
    assert!(
        !dir.exists(),
        "a clean run must not create {}",
        dir.display()
    );
}
