//! Crash reports for fatal signals.
//!
//! When mutsu dies of SIGSEGV (or another fatal signal) all a CI log shows is
//! `Wstat: 11 (Signal: SEGV)`, which leaves the two basic questions unanswered:
//! *which* process faulted — the interpreter running the `.t` file, or a
//! subprocess it spawned — and *where*. A crash rare enough that it appears
//! once in several dozen CI runs has to yield its evidence the one time it
//! fires; there is no second chance to attach a debugger.
//!
//! [`install`] therefore registers a handler for the fatal signals that writes
//! a per-process report to `tmp/crash/<pid>.txt` and then lets the signal
//! through, so the wait status and any core-dump behaviour are exactly what
//! they are today.
//!
//! # Cost
//!
//! A handful of syscalls plus a small allocation at startup, and nothing at all
//! until a crash. In particular the report directory is **not** created at
//! startup — it is created from inside the handler — so an ordinary run in an
//! arbitrary working directory leaves no trace.
//!
//! # Limitations
//!
//! The handler is only partly async-signal-safe, deliberately. Everything down
//! to and including the raw backtrace uses nothing but `write`/`mkdir`/`open`
//! and a fixed stack buffer, so it lands even if the fault happened inside the
//! allocator. The *symbolized* backtrace at the end calls
//! [`std::backtrace::Backtrace::force_capture`], which allocates and takes
//! locks; it is guarded by an `alarm(10)` so a wedged handler still dies, and
//! it is written last so a deadlock there cannot cost us the parts that matter.
//! Symbolication quality is poor in the release build on purpose —
//! `[profile.release]` sets `debug = false` — but the raw frame addresses can
//! still be resolved offline with `addr2line -f -e target/release/mutsu`.
//!
//! The default `tmp/crash` is resolved against the process's **startup**
//! working directory, so a later `chdir` cannot move it — but a process that
//! *starts* elsewhere (a subprocess spawned with `:cwd`, or one inheriting a
//! parent's `chdir`) writes its report under that directory instead. A harness
//! that collects reports from one place must therefore export an absolute
//! `MUTSU_CRASH_DIR`, which every descendant inherits; CI does exactly that.
//!
//! This is instrumentation, not a fix: it buys nothing until the next crash,
//! and then it buys an answer instead of another dead end.

#[cfg(all(unix, feature = "native"))]
mod handler;
#[cfg(all(unix, feature = "native"))]
mod report;

/// Install the fatal-signal handler and an alternate signal stack for the
/// calling thread.
///
/// Idempotent for the process-wide part; the alternate stack is per-thread, so
/// calling this from a second thread gives that thread its own (leaking one
/// stack allocation per call — call it from long-lived threads only).
///
/// Disabled entirely by `MUTSU_CRASH_REPORT=0`. Reports are written to
/// `tmp/crash` relative to the startup working directory, or to
/// `$MUTSU_CRASH_DIR` when that is set.
pub fn install() {
    #[cfg(all(unix, feature = "native"))]
    handler::install();
}

/// Deliberately crash when `MUTSU_CRASH_SELFTEST` asks for it, so the report
/// pipeline itself can be tested end to end (`tests/crash_report.rs`).
///
/// `segv` dereferences a null pointer (a real fault, with a real `si_addr`),
/// `abort` calls `abort()`. Any other value is ignored.
pub fn selftest_if_requested() {
    #[cfg(all(unix, feature = "native"))]
    handler::selftest_if_requested();
}
