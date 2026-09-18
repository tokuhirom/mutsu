//! The sampled totals and the snapshot a report reads them from.
//!
//! Split out of [`super::aggregate`], which owns the per-thread buffers and
//! the fold that fills these tables: this half runs once, at report time, and
//! is where every file identity is reconciled and every row is put in a
//! deterministic order.

use super::paths;
use super::region::Region;
use super::{CallsiteLocation, LineLocation, LineRegion, RoutineLocation};
use rustc_hash::FxHashMap;
use std::sync::{Mutex, MutexGuard, OnceLock};

/// Sampled totals, keyed the way a report reads them.
#[derive(Default)]
pub(crate) struct SampledTotals {
    pub(crate) line_self_ns: FxHashMap<LineLocation, u64>,
    pub(crate) line_incl_ns: FxHashMap<LineLocation, u64>,
    pub(crate) line_region_ns: FxHashMap<LineRegion, u64>,
    pub(crate) routine_self_ns: FxHashMap<RoutineLocation, u64>,
    pub(crate) routine_incl_ns: FxHashMap<RoutineLocation, u64>,
    pub(crate) callsite_incl_ns: FxHashMap<CallsiteLocation, u64>,
    /// Sampled self time per subsystem, whole-run. A fixed-size array rather
    /// than a map: the tags are a closed enum (ADR-0106 D4) precisely so that
    /// neither the sample path nor the fold has to hash one.
    pub(crate) region_ns: [u64; Region::COUNT],
    pub(crate) region_samples: [u64; Region::COUNT],
    pub(crate) samples: u64,
    pub(crate) sampled_ns: u64,
    pub(crate) truncated: u64,
}

pub(crate) fn totals() -> &'static Mutex<SampledTotals> {
    static TOTALS: OnceLock<Mutex<SampledTotals>> = OnceLock::new();
    TOTALS.get_or_init(|| Mutex::new(SampledTotals::default()))
}

fn lock<T>(m: &Mutex<T>) -> MutexGuard<'_, T> {
    m.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}

/// Every sampled table, sorted by interned ids so a report and a test see a
/// deterministic order rather than hash order.
#[derive(Default)]
pub(crate) struct SampledSnapshot {
    pub(crate) line_self_ns: Vec<(LineLocation, u64)>,
    pub(crate) line_incl_ns: Vec<(LineLocation, u64)>,
    pub(crate) line_region_ns: Vec<(LineRegion, u64)>,
    pub(crate) routine_self_ns: Vec<(RoutineLocation, u64)>,
    pub(crate) routine_incl_ns: Vec<(RoutineLocation, u64)>,
    pub(crate) callsite_incl_ns: Vec<(CallsiteLocation, u64)>,
    pub(crate) region_ns: [u64; Region::COUNT],
    pub(crate) region_samples: [u64; Region::COUNT],
    /// Time **measured** out of the line table rather than sampled into it: a
    /// GC collect, a stop-the-world park. Named here so the report can say
    /// where it went instead of leaving a hole (ADR-0106 Slice 4).
    pub(crate) excluded_region_ns: Vec<(Region, u64)>,
    pub(crate) samples: u64,
    pub(crate) sampled_ns: u64,
    pub(crate) truncated: u64,
}

fn merge<K: std::hash::Hash + Eq>(rows: impl Iterator<Item = (K, u64)>) -> Vec<(K, u64)> {
    let mut merged: FxHashMap<K, u64> = FxHashMap::default();
    for (key, ns) in rows {
        *merged.entry(key).or_default() += ns;
    }
    merged.into_iter().collect()
}

fn merge_lines(table: &mut FxHashMap<LineLocation, u64>) -> Vec<(LineLocation, u64)> {
    merge(table.drain().map(|(mut location, ns)| {
        location.file = paths::canonical(location.file);
        (location, ns)
    }))
}

fn merge_routines(table: &mut FxHashMap<RoutineLocation, u64>) -> Vec<(RoutineLocation, u64)> {
    merge(table.drain().map(|(mut location, ns)| {
        location.file = paths::canonical_opt(location.file);
        (location, ns)
    }))
}

/// Fold every thread and take everything accumulated so far.
pub(crate) fn take_samples() -> SampledSnapshot {
    super::sampler::fold_this_thread();
    super::aggregate::fold_all_threads();
    let mut totals = lock(totals());
    // One identity per file (`super::paths`): the self table is keyed by the
    // chunk's own file and the inclusive/callsite tables by the frames' spelled
    // `$?FILE`, and a report that let those disagree would credit one file's
    // time to two names -- and would leave a caller row unmatchable against the
    // line row it belongs to.
    let mut snapshot = SampledSnapshot {
        line_self_ns: merge_lines(&mut totals.line_self_ns),
        line_incl_ns: merge_lines(&mut totals.line_incl_ns),
        line_region_ns: merge(totals.line_region_ns.drain().map(|(mut key, ns)| {
            key.location.file = paths::canonical(key.location.file);
            (key, ns)
        })),
        routine_self_ns: merge_routines(&mut totals.routine_self_ns),
        routine_incl_ns: merge_routines(&mut totals.routine_incl_ns),
        callsite_incl_ns: merge(totals.callsite_incl_ns.drain().map(|(mut location, ns)| {
            location.caller_file = paths::canonical(location.caller_file);
            (location, ns)
        })),
        region_ns: std::mem::take(&mut totals.region_ns),
        region_samples: std::mem::take(&mut totals.region_samples),
        excluded_region_ns: super::region::take_excluded_ns(),
        samples: std::mem::take(&mut totals.samples),
        sampled_ns: std::mem::take(&mut totals.sampled_ns),
        truncated: std::mem::take(&mut totals.truncated),
    };
    let by_line = |(location, _): &(LineLocation, u64)| (location.file.id(), location.line);
    snapshot.line_self_ns.sort_by_key(by_line);
    snapshot.line_incl_ns.sort_by_key(by_line);
    snapshot.line_region_ns.sort_by_key(|(key, _)| {
        (
            key.location.file.id(),
            key.location.line,
            key.region.index(),
        )
    });
    let by_routine = |(location, _): &(RoutineLocation, u64)| {
        (
            location.package.id(),
            location.name.id(),
            location.file.map_or(0, |file| file.id()),
        )
    };
    snapshot.routine_self_ns.sort_by_key(by_routine);
    snapshot.routine_incl_ns.sort_by_key(by_routine);
    snapshot.callsite_incl_ns.sort_by_key(|(location, _)| {
        (
            location.caller_file.id(),
            location.caller_line,
            location.package.id(),
            location.name.id(),
        )
    });
    snapshot
}

impl SampledSnapshot {
    /// The subsystem that took the most sampled self time, and its share of
    /// the sampled total.
    ///
    /// The one region figure worth asserting: *which* tag is on top is a
    /// function of what the fixture does, while the nanoseconds under it are a
    /// function of the machine (ADR-0106 D5).
    pub(crate) fn top_region(&self) -> Option<(Region, u64)> {
        self.region_ns
            .iter()
            .enumerate()
            .map(|(index, ns)| (Region::from_index(index), *ns))
            .filter(|(_, ns)| *ns > 0)
            .max_by_key(|(region, ns)| (*ns, std::cmp::Reverse(*region)))
    }
}
