//! Per-thread exact profile counters (ADR-0106 Slice 3).
//!
//! The counters are deliberately independent of the report format.  A poll
//! records a line transition in the thread-local table, and the tables are
//! merged only when a caller asks for a snapshot.  That keeps the execution
//! path free of shared atomics and leaves Slice 5 to choose the JSON/text
//! representation.
//!
//! Tables are *registered*, not merely folded when their thread exits: a
//! worker-pool thread (a `start` block's, above all) is usually still alive
//! when the process reports, and a fold-on-drop alone silently lost everything
//! such a thread counted -- a routine called once on the mainline and once on a
//! worker reported `entries=1`.
//!
//! The *time* half of the profile lives in [`super::sampler`]: counts are
//! exact and sampled time is statistical, which is ADR-0106 D1.

use super::{CallsiteLocation, LineLocation, RoutineLocation};
use crate::opcode::CompiledCode;
use crate::runtime::RoutineFrame;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::sync::{Arc, Mutex, MutexGuard, OnceLock};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LastLine {
    chunk: usize,
    location: LineLocation,
}

#[derive(Default)]
struct Tables {
    line_hits: FxHashMap<LineLocation, u64>,
    routine_entries: FxHashMap<RoutineLocation, u64>,
    callsite_calls: FxHashMap<CallsiteLocation, u64>,
}

type TableHandle = Arc<Mutex<Tables>>;

/// A thread's tables, held by the thread and by the registry.
struct ThreadCounts(TableHandle);

impl ThreadCounts {
    fn new() -> Self {
        let handle: TableHandle = Arc::new(Mutex::new(Tables::default()));
        lock(registry()).push(Arc::clone(&handle));
        Self(handle)
    }

    fn tables(&self) -> MutexGuard<'_, Tables> {
        lock(&self.0)
    }
}

thread_local! {
    /// The line the previous poll stood on, per routine depth
    /// (`Interpreter::routine_stack`'s length). It is read on every poll and
    /// the tables behind it are only touched when the line actually changes,
    /// so the common "still on the same line" poll takes no lock at all.
    ///
    /// Per depth, because a *return* is not an arrival: a caller's line was
    /// already counted when control reached it, and a callee's polls at its
    /// own depth leave the caller's entry alone, so coming back from the call
    /// counts nothing. A single "last line" made the interpreter (which polls
    /// every op) count a line again after each call on it returned, while
    /// native code (which hooks only line transitions and jump targets) did
    /// not — the JIT-parity gap ADR-0106 §8 gate 4 forbids (#8737).
    static LAST_LINES: RefCell<Vec<Option<LastLine>>> = const { RefCell::new(Vec::new()) };
    static THREAD_COUNTS: RefCell<Option<ThreadCounts>> = const { RefCell::new(None) };
}

fn lock<T>(m: &Mutex<T>) -> MutexGuard<'_, T> {
    m.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn registry() -> &'static Mutex<Vec<TableHandle>> {
    static REGISTRY: OnceLock<Mutex<Vec<TableHandle>>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(Vec::new()))
}

/// Replace the last line recorded at routine depth `depth`, returning the old
/// one. A re-entrant borrow (impossible today) answers "unknown" rather than
/// panicking: a counter must never be the thing that panics inside the VM.
// Cost: O(1) amortized; the vector grows to the deepest routine depth polled.
fn swap_last_line(depth: usize, line: Option<LastLine>) -> Option<LastLine> {
    LAST_LINES
        .try_with(|cell| {
            let mut lines = cell.try_borrow_mut().ok()?;
            if lines.len() <= depth {
                lines.resize(depth + 1, None);
            }
            std::mem::replace(&mut lines[depth], line)
        })
        .ok()
        .flatten()
}

/// Run `f` against this thread's tables, creating and registering them on
/// first use.
fn with_tables<R>(f: impl FnOnce(&mut Tables) -> R) -> Option<R> {
    THREAD_COUNTS
        .try_with(|cell| {
            // A `try_borrow_mut` rather than `borrow_mut`: a counter must never
            // be the thing that panics inside the VM.
            let mut slot = cell.try_borrow_mut().ok()?;
            let counts = slot.get_or_insert_with(ThreadCounts::new);
            Some(f(&mut counts.tables()))
        })
        // `try_with` fails once a thread's locals are being destroyed, which is
        // exactly when there is nothing left worth counting.
        .unwrap_or(None)
}

/// Record a poll standing at `here` at routine depth `depth`, counting a hit
/// when it is a different line from the previous poll's at that depth.  The
/// chunk address distinguishes a routine re-entry at the same source line from
/// continuing the previous execution of that line.
///
/// `here` is resolved by the caller (`vm_poll::record_line`) because the
/// sampler needs the same answer, and resolving it twice per poll would be
/// paying for the armed gate twice over.
// Cost: O(1) amortized.
pub(crate) fn record_line_at(code: &CompiledCode, here: Option<LineLocation>, depth: usize) {
    let Some(location) = here else {
        swap_last_line(depth, None);
        return;
    };
    let last_line = LastLine {
        chunk: code as *const CompiledCode as usize,
        location,
    };
    // The hot case by a wide margin: another opcode on the line we are already
    // counting. It costs one thread-local access and a compare, no lock.
    if swap_last_line(depth, Some(last_line)) == Some(last_line) {
        return;
    }
    with_tables(|tables| {
        *tables.line_hits.entry(location).or_default() += 1;
    });
}

/// End the current line visit, so the next poll counts a hit even when it
/// stands on the same line.
///
/// A hit is an *arrival* at a line. Arriving from a different line is the
/// common case and needs nothing: [`record_line_at`] sees the line change.
/// The other way to arrive is to come back around a loop — a compound loop
/// entering its body range for the next trip, or a backward jump — and a loop
/// whose body sits on one line never changes line doing so. Without this, a
/// one-line `for 1..100 { $a++ }` counted its line once per loop *entry*
/// rather than once per trip (#8737). The poll network calls this on those
/// arrivals only, and only while the profiler is armed, at the routine depth
/// the arrival happens at.
// Cost: O(1) amortized.
pub(crate) fn end_line_visit(depth: usize) {
    swap_last_line(depth, None);
}

/// Reset the line-transition edge of the routine depth `frame` is about to be
/// pushed at (`callee_depth`), then count
/// the exact routine entry and its call site when that site has a source
/// location.
///
/// `frame.file` is the file the *call site* is in — a `RoutineFrame` push
/// resolves it as the caller's own lexical file
/// (`Interpreter::executing_source_file_sym`), not the dynamically-scoped
/// `?FILE`, so it already names the file a `use`d module's routine was
/// actually called from rather than the script `?FILE` reverted to once the
/// module finished loading ([#8743]).
///
/// [#8743]: https://github.com/tokuhirom/mutsu/issues/8743
pub(crate) fn record_routine_frame(frame: &RoutineFrame, callee_depth: usize) {
    swap_last_line(callee_depth, None);
    with_tables(|tables| {
        let routine = RoutineLocation {
            package: frame.package,
            name: frame.name,
            // `None` means "the same file as the caller" (see `RoutineFrame`),
            // so resolve it rather than let one routine become two rows.
            file: frame.def_file.or(frame.file),
        };
        *tables.routine_entries.entry(routine).or_default() += 1;
        if let (Some(caller_file), Some(caller_line)) = (frame.file, frame.line) {
            let callsite = CallsiteLocation {
                caller_file,
                caller_line,
                package: frame.package,
                name: frame.name,
            };
            *tables.callsite_calls.entry(callsite).or_default() += 1;
        }
    });
}

#[derive(Debug, Default)]
pub(crate) struct CountsSnapshot {
    pub(crate) line_hits: Vec<(LineLocation, u64)>,
    pub(crate) routine_entries: Vec<(RoutineLocation, u64)>,
    pub(crate) callsite_calls: Vec<(CallsiteLocation, u64)>,
}

/// Fold the calling thread and return all exact counters accumulated since the
/// previous snapshot.  Entries are sorted by interned ids so tests and the
/// future report builder receive deterministic input rather than hash order.
pub(crate) fn take_counts() -> CountsSnapshot {
    let mut folded = Tables::default();
    let handles: Vec<TableHandle> = lock(registry()).clone();
    for handle in &handles {
        let mut tables = lock(handle);
        for (location, count) in tables.line_hits.drain() {
            *folded.line_hits.entry(location).or_default() += count;
        }
        for (location, count) in tables.routine_entries.drain() {
            *folded.routine_entries.entry(location).or_default() += count;
        }
        for (location, count) in tables.callsite_calls.drain() {
            *folded.callsite_calls.entry(location).or_default() += count;
        }
    }
    // A handle only the registry still holds belongs to a thread that has
    // exited; its tables are now empty and can go.
    lock(registry()).retain(|handle| Arc::strong_count(handle) > 1);
    // No reconciliation pass, and so no second fold either: a source file has
    // one runtime identity now (#8719), so a callsite row already names the
    // same string as the line row above it and `folded` is already final.
    let mut line_hits: Vec<_> = folded.line_hits.drain().collect();
    let mut routine_entries: Vec<_> = folded.routine_entries.drain().collect();
    let mut callsite_calls: Vec<_> = folded.callsite_calls.drain().collect();
    line_hits.sort_by_key(|(location, _)| (location.file.id(), location.line));
    routine_entries.sort_by_key(|(location, _)| {
        (
            location.package.id(),
            location.name.id(),
            location.file.map_or(0, |file| file.id()),
        )
    });
    callsite_calls.sort_by_key(|(location, _)| {
        (
            location.caller_file.id(),
            location.caller_line,
            location.package.id(),
            location.name.id(),
        )
    });
    CountsSnapshot {
        line_hits,
        routine_entries,
        callsite_calls,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::Symbol;

    #[test]
    fn line_transition_counts_are_exact() {
        let _ = take_counts();
        let mut code = CompiledCode::new();
        code.source_file = Some(Symbol::intern("profile-fixture.raku"));
        code.op_lines = vec![10, 10, 11, 11, 10];
        for ip in [0, 1, 2, 3, 4, 0] {
            let here = code
                .location_at(ip)
                .map(|(file, line)| LineLocation { file, line });
            record_line_at(&code, here, 0);
        }
        let snapshot = take_counts();
        assert_eq!(
            snapshot
                .line_hits
                .iter()
                .map(|(location, count)| (location.line, *count))
                .collect::<Vec<_>>(),
            vec![(10, 2), (11, 1)]
        );
    }
}
