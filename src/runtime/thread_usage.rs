//! Process-wide thread counters behind `Thread.usage` and the scheduler's
//! `.usage` (what Rakudo's `Telemetry` samples, #9824).
//!
//! Rakudo keeps these as class-level atomics in `Thread` and reads the thread
//! pool's state for `ThreadPoolScheduler.usage`. mutsu runs Raku code on
//! threads it spawns through one chokepoint (`spawn_registered_thread`), so
//! the counters are kept there, in the same order `Thread.usage` reports
//! them: started, aborted, completed, joined, yields, highest thread id.

use std::sync::atomic::{AtomicI64, Ordering};

static STARTED: AtomicI64 = AtomicI64::new(0);
static ABORTED: AtomicI64 = AtomicI64::new(0);
static COMPLETED: AtomicI64 = AtomicI64::new(0);
static JOINED: AtomicI64 = AtomicI64::new(0);
static YIELDS: AtomicI64 = AtomicI64::new(0);
static HIGHEST_ID: AtomicI64 = AtomicI64::new(0);
/// Scheduler tasks that ran to the end (`$*SCHEDULER.usage`'s `gtc`).
static TASKS_COMPLETED: AtomicI64 = AtomicI64::new(0);

/// A thread running Raku code was started.
// Cost: O(1).
pub(crate) fn note_thread_started() {
    STARTED.fetch_add(1, Ordering::Relaxed);
}

/// A thread running Raku code ended: normally, or by unwinding.
// Cost: O(1).
pub(crate) fn note_thread_ended(completed: bool) {
    let counter = if completed { &COMPLETED } else { &ABORTED };
    counter.fetch_add(1, Ordering::Relaxed);
}

/// A thread was joined (`.finish` / `.join`).
// Cost: O(1).
pub(crate) fn note_thread_joined() {
    JOINED.fetch_add(1, Ordering::Relaxed);
}

/// `Thread.yield` was called.
// Cost: O(1).
pub(crate) fn note_thread_yield() {
    YIELDS.fetch_add(1, Ordering::Relaxed);
}

/// A thread id was handed out.
// Cost: O(1).
pub(crate) fn note_thread_id(id: u64) {
    HIGHEST_ID.fetch_max(i64::try_from(id).unwrap_or(i64::MAX), Ordering::Relaxed);
}

/// A scheduler task ran to the end.
// Cost: O(1).
pub(crate) fn note_task_completed() {
    TASKS_COMPLETED.fetch_add(1, Ordering::Relaxed);
}

/// `Thread.usage`: started, aborted, completed, joined, yields, highest id.
// Cost: O(1).
pub(crate) fn thread_usage() -> [i64; 6] {
    [
        STARTED.load(Ordering::Relaxed),
        ABORTED.load(Ordering::Relaxed),
        COMPLETED.load(Ordering::Relaxed),
        JOINED.load(Ordering::Relaxed),
        YIELDS.load(Ordering::Relaxed),
        HIGHEST_ID.load(Ordering::Relaxed),
    ]
}

/// Scheduler tasks completed so far.
// Cost: O(1).
pub(crate) fn tasks_completed() -> i64 {
    TASKS_COMPLETED.load(Ordering::Relaxed)
}

/// Counts the end of the thread that holds it: a normal return marks it
/// completed first, an unwind drops it unmarked and counts as aborted.
pub(crate) struct ThreadEndGuard {
    completed: bool,
}

impl ThreadEndGuard {
    // Cost: O(1).
    pub(crate) fn new() -> Self {
        Self { completed: false }
    }

    /// The thread's body returned normally.
    // Cost: O(1).
    pub(crate) fn complete(mut self) {
        self.completed = true;
    }
}

impl Drop for ThreadEndGuard {
    fn drop(&mut self) {
        note_thread_ended(self.completed);
    }
}
