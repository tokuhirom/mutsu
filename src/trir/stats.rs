//! `MUTSU_VM_STATS` counters for TRIR entries (#9072).
//!
//! A TRIR chunk that is *accepted* at compile time can still decline every
//! time it runs, and declining is silent by design: the untyped path takes
//! the call and the answer is the same. That is exactly how ADR-0110 Stage 2
//! sat inert on JSON::Fast — every entry into a module routine bailed at its
//! first `CallTr`, and re-ran untyped — with nothing reporting it. These
//! counters make the three outcomes of an entry from the ordinary VM visible:
//!
//! - `entries`: `run_trir_from_outside` was reached;
//! - `completed`: the chunk ran to its return;
//! - `bails`: it met something its proof did not cover and the untyped path
//!   re-ran the routine from the start;
//! - `bind-declines`: its arguments could not be bound, so it never started.
//!
//! In a program where TRIR is doing its job, `completed` is close to
//! `entries`. A `bails` count of the same order as `entries` means the typed
//! run is pure overhead.

use std::sync::atomic::{AtomicU64, Ordering};

static ENTRIES: AtomicU64 = AtomicU64::new(0);
static COMPLETED: AtomicU64 = AtomicU64::new(0);
static BAILS: AtomicU64 = AtomicU64::new(0);
static BIND_DECLINES: AtomicU64 = AtomicU64::new(0);

/// The outcome of one entry into a TRIR chunk from the ordinary VM.
#[derive(Clone, Copy)]
pub(crate) enum TrirEntry {
    Completed,
    Bailed,
    BindDeclined,
}

#[inline]
pub(crate) fn record(outcome: TrirEntry) {
    if !crate::vm::vm_stats::enabled() {
        return;
    }
    ENTRIES.fetch_add(1, Ordering::Relaxed);
    let counter = match outcome {
        TrirEntry::Completed => &COMPLETED,
        TrirEntry::Bailed => &BAILS,
        TrirEntry::BindDeclined => &BIND_DECLINES,
    };
    counter.fetch_add(1, Ordering::Relaxed);
}

/// The report line, printed with the rest of `MUTSU_VM_STATS`.
pub(crate) fn report() {
    eprintln!(
        "[mutsu vm-stats] trir: entries={} completed={} bails={} bind-declines={}",
        ENTRIES.load(Ordering::Relaxed),
        COMPLETED.load(Ordering::Relaxed),
        BAILS.load(Ordering::Relaxed),
        BIND_DECLINES.load(Ordering::Relaxed),
    );
}
