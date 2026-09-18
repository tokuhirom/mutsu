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
use crate::symbol::Symbol;
use rustc_hash::FxHashMap;
use std::cell::{Cell, RefCell};
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
    /// The line the previous poll stood on. A plain `Cell`, deliberately: it
    /// is read on every poll and the tables behind it are only touched when
    /// the line actually changes, so the common "still on the same line" poll
    /// takes no lock at all.
    static LAST_LINE: Cell<Option<LastLine>> = const { Cell::new(None) };
    static THREAD_COUNTS: RefCell<Option<ThreadCounts>> = const { RefCell::new(None) };
}

fn lock<T>(m: &Mutex<T>) -> MutexGuard<'_, T> {
    m.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn registry() -> &'static Mutex<Vec<TableHandle>> {
    static REGISTRY: OnceLock<Mutex<Vec<TableHandle>>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(Vec::new()))
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

/// Record a poll standing at `here`, counting a hit when it is a different
/// line from the previous poll's.  The chunk address distinguishes a routine
/// re-entry at the same source line from continuing the previous execution of
/// that line.
///
/// `here` is resolved by the caller (`vm_poll::record_line`) because the
/// sampler needs the same answer, and resolving it twice per poll would be
/// paying for the armed gate twice over.
pub(crate) fn record_line_at(code: &CompiledCode, here: Option<LineLocation>) {
    let Some(location) = here else {
        LAST_LINE.with(|cell| cell.set(None));
        return;
    };
    let last_line = LastLine {
        chunk: code as *const CompiledCode as usize,
        location,
    };
    // The hot case by a wide margin: another opcode on the line we are already
    // counting. It costs one thread-local load and a compare, no lock.
    if LAST_LINE.with(|cell| cell.replace(Some(last_line))) == Some(last_line) {
        return;
    }
    with_tables(|tables| {
        *tables.line_hits.entry(location).or_default() += 1;
    });
}

/// Reset the line-transition edge after a routine frame is pushed, then count
/// the exact routine entry and its call site when that site has a source
/// location.
///
/// `caller_file` is the file the *call site* is in, resolved by the caller from
/// the frame stack (`Interpreter::record_profile_routine_frame`): the frame's
/// own `file` is the dynamically-scoped `?FILE`, which still names the mainline
/// while a `use`d module's routine is running ([#8743]), so trusting it files
/// a module's callsites under the script's path.
///
/// [#8743]: https://github.com/tokuhirom/mutsu/issues/8743
pub(crate) fn record_routine_frame(frame: &RoutineFrame, caller_file: Option<Symbol>) {
    LAST_LINE.with(|cell| cell.set(None));
    with_tables(|tables| {
        let routine = RoutineLocation {
            package: frame.package,
            name: frame.name,
            // `None` means "the same file as the caller" (see `RoutineFrame`),
            // so resolve it rather than let one routine become two rows.
            file: frame.def_file.or(frame.file),
        };
        *tables.routine_entries.entry(routine).or_default() += 1;
        if let (Some(caller_file), Some(caller_line)) = (caller_file, frame.line) {
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
            record_line_at(&code, here);
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
