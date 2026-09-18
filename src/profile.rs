//! Per-thread exact profile counters (ADR-0106 Slice 3).
//!
//! The counters are deliberately independent of the report format.  A poll
//! records a line transition in the thread-local table, and the table is
//! folded only when a caller asks for a snapshot or when the thread exits.
//! That keeps the execution path free of shared atomics and leaves Slice 5 to
//! choose the JSON/text representation.

use crate::opcode::CompiledCode;
use crate::runtime::RoutineFrame;
use crate::symbol::Symbol;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::sync::{Mutex, MutexGuard, OnceLock};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct LineLocation {
    pub(crate) file: Symbol,
    pub(crate) line: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct RoutineLocation {
    pub(crate) package: Symbol,
    pub(crate) name: Symbol,
    pub(crate) file: Option<Symbol>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CallsiteLocation {
    pub(crate) caller_file: Symbol,
    pub(crate) caller_line: u32,
    pub(crate) package: Symbol,
    pub(crate) name: Symbol,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LastLine {
    chunk: usize,
    location: LineLocation,
}

#[derive(Default)]
struct ThreadCounts {
    last_line: Option<LastLine>,
    line_hits: FxHashMap<LineLocation, u64>,
    routine_entries: FxHashMap<RoutineLocation, u64>,
    callsite_calls: FxHashMap<CallsiteLocation, u64>,
}

#[derive(Default)]
struct FoldedCounts {
    line_hits: FxHashMap<LineLocation, u64>,
    routine_entries: FxHashMap<RoutineLocation, u64>,
    callsite_calls: FxHashMap<CallsiteLocation, u64>,
}

thread_local! {
    static THREAD_COUNTS: RefCell<ThreadCounts> = RefCell::new(ThreadCounts::default());
}

fn folded_counts() -> &'static Mutex<FoldedCounts> {
    static COUNTS: OnceLock<Mutex<FoldedCounts>> = OnceLock::new();
    COUNTS.get_or_init(|| Mutex::new(FoldedCounts::default()))
}

fn lock_folded_counts() -> MutexGuard<'static, FoldedCounts> {
    folded_counts()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn fold_thread_counts(thread: &mut ThreadCounts) {
    if thread.line_hits.is_empty()
        && thread.routine_entries.is_empty()
        && thread.callsite_calls.is_empty()
    {
        return;
    }
    let line_hits = std::mem::take(&mut thread.line_hits);
    let routine_entries = std::mem::take(&mut thread.routine_entries);
    let callsite_calls = std::mem::take(&mut thread.callsite_calls);
    let mut folded = lock_folded_counts();
    for (location, count) in line_hits {
        *folded.line_hits.entry(location).or_default() += count;
    }
    for (location, count) in routine_entries {
        *folded.routine_entries.entry(location).or_default() += count;
    }
    for (location, count) in callsite_calls {
        *folded.callsite_calls.entry(location).or_default() += count;
    }
}

impl Drop for ThreadCounts {
    fn drop(&mut self) {
        fold_thread_counts(self);
    }
}

/// Record the line at `ip` when execution enters a different line in a
/// chunk.  The chunk address distinguishes a routine re-entry at the same
/// source line from continuing the previous execution of that line.
pub(crate) fn record_line(code: &CompiledCode, ip: usize) {
    THREAD_COUNTS.with(|counts| {
        let mut counts = counts.borrow_mut();
        let Some((file, line)) = code.location_at(ip) else {
            counts.last_line = None;
            return;
        };
        let last_line = LastLine {
            chunk: code as *const CompiledCode as usize,
            location: LineLocation { file, line },
        };
        if counts.last_line == Some(last_line) {
            return;
        }
        *counts.line_hits.entry(last_line.location).or_default() += 1;
        counts.last_line = Some(last_line);
    });
}

/// Reset the line-transition edge after a routine frame is pushed, then count
/// the exact routine entry and its call site when that site has a source
/// location.
pub(crate) fn record_routine_frame(frame: &RoutineFrame) {
    THREAD_COUNTS.with(|counts| {
        let mut counts = counts.borrow_mut();
        counts.last_line = None;
        let routine = RoutineLocation {
            package: frame.package,
            name: frame.name,
            file: frame.def_file,
        };
        *counts.routine_entries.entry(routine).or_default() += 1;
        if let (Some(caller_file), Some(caller_line)) = (frame.file, frame.line) {
            let callsite = CallsiteLocation {
                caller_file,
                caller_line,
                package: frame.package,
                name: frame.name,
            };
            *counts.callsite_calls.entry(callsite).or_default() += 1;
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
    THREAD_COUNTS.with(|counts| fold_thread_counts(&mut counts.borrow_mut()));
    let mut folded = lock_folded_counts();
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

/// How many rows of each table the scaffolding report prints. A fixture small
/// enough to reason about fits well inside this; a real program does not, and
/// is Slice 5's problem.
const REPORT_ROWS: usize = 20;

/// Fold the calling thread at process shutdown and print what was counted.
///
/// **This is scaffolding, not the report.** Slice 5 ([#8705]) owns the profile
/// document — its format, its file, its CLI. What this prints is the minimum
/// that makes the counters *observable from outside the process*, because
/// until they are, ADR-0106 §8 gate 3 (the top self line and its exact hit
/// count) and gate 4 (the same line and hits with the JIT on and off) cannot
/// be asserted at all: the counts were collected into a static nothing read.
/// `tests/profile_counts.rs` is the consumer. Slice 5 replaces this body with
/// its document builder; the counters themselves do not change.
///
/// [#8705]: https://github.com/tokuhirom/mutsu/issues/8705
pub(crate) fn flush_at_exit() {
    if !crate::vm::vm_poll::profiler_armed() {
        return;
    }
    let snapshot = take_counts();
    // Hottest first, so "the top self line" is the first row; the location
    // breaks ties, so two lines with equal hits print in a stable order.
    let mut line_hits = snapshot.line_hits;
    line_hits.sort_by_key(|(location, hits)| {
        (
            std::cmp::Reverse(*hits),
            location.file.as_str(),
            location.line,
        )
    });
    for (location, hits) in line_hits.iter().take(REPORT_ROWS) {
        eprintln!(
            "profile: line {}:{} hits={hits}",
            location.file, location.line
        );
    }
    let mut routine_entries = snapshot.routine_entries;
    routine_entries.sort_by_key(|(location, entries)| {
        (
            std::cmp::Reverse(*entries),
            location.package.as_str(),
            location.name.as_str(),
        )
    });
    for (location, entries) in routine_entries.iter().take(REPORT_ROWS) {
        eprintln!(
            "profile: routine {}::{} entries={entries}",
            location.package, location.name
        );
    }
    let mut callsite_calls = snapshot.callsite_calls;
    callsite_calls.sort_by_key(|(location, calls)| {
        (
            std::cmp::Reverse(*calls),
            location.caller_file.as_str(),
            location.caller_line,
        )
    });
    for (location, calls) in callsite_calls.iter().take(REPORT_ROWS) {
        eprintln!(
            "profile: callsite {}:{} -> {}::{} calls={calls}",
            location.caller_file, location.caller_line, location.package, location.name
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_transition_counts_are_exact() {
        let _ = take_counts();
        let mut code = CompiledCode::new();
        code.source_file = Some(Symbol::intern("profile-fixture.raku"));
        code.op_lines = vec![10, 10, 11, 11, 10];
        for ip in [0, 1, 2, 3, 4, 0] {
            record_line(&code, ip);
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
