//! Sample buffers and the fold that turns them into tables (ADR-0106 Slice 2).
//!
//! The split of work is the point: the **sample path** only copies fixed-size
//! records into space reserved at arm time, and every hash lookup, dedup and
//! sort happens here, when a buffer fills or at exit. That is what keeps the
//! profiler out of its own measurement.
//!
//! Buffers are registered globally rather than folded from `Drop` alone. A
//! worker-pool thread is typically still alive when the process reports, so a
//! `Drop`-only fold would silently lose every sample a `start` block took —
//! precisely the threaded case the sampler exists to cover.

use super::region::Region;
use super::snapshot::{SampledTotals, totals};
use super::{CallsiteLocation, LineLocation, LineRegion, RoutineLocation};
use crate::runtime::RoutineFrame;
use crate::symbol::Symbol;
use rustc_hash::FxHashSet;
use std::sync::{Arc, Mutex, MutexGuard, OnceLock};

/// One Raku-level frame as the sample path copies it: all `Copy`, no strings.
#[derive(Clone, Copy)]
struct SampleFrame {
    package: Symbol,
    name: Symbol,
    def_file: Option<Symbol>,
    call_file: Option<Symbol>,
    call_line: Option<u32>,
}

/// One sample: its weight, the line on top, and how many frames follow it in
/// the shared frame vector (innermost first).
#[derive(Clone, Copy)]
struct SampleHeader {
    elapsed_ns: u64,
    top: Option<LineLocation>,
    /// Which subsystem was running when the tick fired (ADR-0106 D4). One
    /// byte, copied like everything else on this path.
    region: Region,
    frames: u32,
}

/// A thread's pending samples. Both vectors are allocated at first sample with
/// a fixed capacity and never grow: the push path checks for room first and
/// the buffer is folded when it runs out.
#[derive(Default)]
struct Pending {
    headers: Vec<SampleHeader>,
    frames: Vec<SampleFrame>,
    /// Samples dropped because a single stack was deeper than the per-sample
    /// frame cap allowed — reported, never silently swallowed.
    truncated: u64,
}

/// The handle a thread keeps and the registry shares. The mutex is only ever
/// contended between the owning thread and the process-exit flush, so taking
/// it on the sample path costs one uncontended atomic.
type BufferHandle = Arc<Mutex<Pending>>;

/// A thread's sample buffer.
pub(crate) struct ThreadSamples {
    shared: BufferHandle,
    capacity: usize,
    max_frames: usize,
}

impl ThreadSamples {
    /// Allocate a thread's buffers and register them, so the process-exit
    /// flush can drain this thread even while it is still running.
    pub(crate) fn with_capacity(capacity: usize, max_frames: usize) -> Self {
        let pending = Pending {
            headers: Vec::with_capacity(capacity),
            frames: Vec::with_capacity(capacity.saturating_mul(4).max(max_frames)),
            truncated: 0,
        };
        let shared = Arc::new(Mutex::new(pending));
        lock(registry()).push(Arc::clone(&shared));
        Self {
            shared,
            capacity,
            max_frames,
        }
    }

    /// Append one sample: an interval, the line that was running when it
    /// ended, and the stack under it.
    ///
    /// `top` is the caller's business and the subtle part of the whole design
    /// — see [`super::sampler::sample_if_due`]. Copies at most `max_frames`
    /// frames, innermost first.
    ///
    /// One lock in the common case: the capacity check and the push share it,
    /// and it is contended only against the process-exit flush.
    pub(crate) fn record(
        &self,
        elapsed_ns: u64,
        top: Option<LineLocation>,
        region: Region,
        stack: &[RoutineFrame],
    ) {
        let mut pending = lock(&self.shared);
        if pending.headers.len() >= self.capacity
            || pending.frames.len() + self.max_frames > pending.frames.capacity()
        {
            drop(pending);
            fold_handle(&self.shared);
            pending = lock(&self.shared);
        }
        let kept = stack.len().min(self.max_frames);
        if stack.len() > kept {
            pending.truncated += 1;
        }
        for frame in stack.iter().rev().take(kept) {
            pending.frames.push(SampleFrame {
                package: frame.package,
                name: frame.name,
                // `def_file: None` means "the same file as the caller" (see
                // `RoutineFrame`), so resolving it here is what keeps ONE
                // routine from becoming two rows -- which it otherwise does as
                // soon as a `start` block calls it, because the frames pushed
                // on a worker do not always carry the declaring file.
                def_file: frame.def_file.or(frame.file),
                call_file: frame.file,
                call_line: frame.line,
            });
        }
        pending.headers.push(SampleHeader {
            elapsed_ns,
            top,
            region,
            frames: kept as u32,
        });
    }

    /// Drain this buffer into the shared tables.
    pub(crate) fn fold(&self) {
        fold_handle(&self.shared);
    }
}

impl Drop for ThreadSamples {
    fn drop(&mut self) {
        self.fold();
    }
}

fn lock<T>(m: &Mutex<T>) -> MutexGuard<'_, T> {
    m.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn registry() -> &'static Mutex<Vec<BufferHandle>> {
    static REGISTRY: OnceLock<Mutex<Vec<BufferHandle>>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(Vec::new()))
}

/// Fold every registered buffer, including those of threads still running.
pub(crate) fn fold_all_threads() {
    let handles: Vec<BufferHandle> = lock(registry()).clone();
    for handle in handles {
        fold_handle(&handle);
    }
    // A handle the registry alone still holds belongs to a thread that has
    // exited (its `ThreadSamples` folded on drop), so its reserved buffers can
    // go back. Without this a long run that churns worker threads would keep
    // every dead thread's reservation.
    lock(registry()).retain(|handle| Arc::strong_count(handle) > 1);
}

/// Scratch sets, so a fold's dedup does not allocate once per sample.
#[derive(Default)]
struct FoldScratch {
    lines: FxHashSet<LineLocation>,
    routines: FxHashSet<RoutineLocation>,
    callsites: FxHashSet<CallsiteLocation>,
}

fn fold_handle(handle: &BufferHandle) {
    let (headers, frames, truncated) = {
        let mut pending = lock(handle);
        if pending.headers.is_empty() && pending.truncated == 0 {
            return;
        }
        // `drain` keeps the reserved capacity, so the next sample path still
        // pushes into space that was allocated at arm time.
        let headers: Vec<SampleHeader> = pending.headers.drain(..).collect();
        let frames: Vec<SampleFrame> = pending.frames.drain(..).collect();
        (headers, frames, std::mem::take(&mut pending.truncated))
    };

    let mut scratch = FoldScratch::default();
    let mut totals = lock(totals());
    totals.truncated += truncated;
    let mut cursor = 0usize;
    for header in &headers {
        let frames = &frames[cursor..cursor + header.frames as usize];
        cursor += header.frames as usize;
        fold_one(&mut totals, &mut scratch, header, frames);
    }
}

fn fold_one(
    totals: &mut SampledTotals,
    scratch: &mut FoldScratch,
    header: &SampleHeader,
    frames: &[SampleFrame],
) {
    let ns = header.elapsed_ns;
    totals.samples += 1;
    totals.sampled_ns += ns;

    // The region split rides on *self* time: "line 412 is 38% self, of which
    // 71% call-resolve" is the sentence D4 exists to make printable, and
    // inclusive time would attribute a callee's subsystem to every caller
    // above it.
    totals.region_ns[header.region.index()] += ns;
    totals.region_samples[header.region.index()] += 1;
    if let Some(top) = header.top {
        *totals.line_self_ns.entry(top).or_default() += ns;
        *totals
            .line_region_ns
            .entry(LineRegion {
                location: top,
                region: header.region,
            })
            .or_default() += ns;
    }
    // The innermost frame is the routine the sample caught running; the rest
    // of the stack only earns inclusive credit.
    if let Some(innermost) = frames.first() {
        *totals
            .routine_self_ns
            .entry(routine_of(innermost))
            .or_default() += ns;
    }

    // Inclusive credit is per *distinct* key, so a recursive call does not
    // charge its own stack depth to itself.
    scratch.lines.clear();
    scratch.routines.clear();
    scratch.callsites.clear();
    if let Some(top) = header.top {
        scratch.lines.insert(top);
    }
    for frame in frames {
        if let (Some(file), Some(line)) = (frame.call_file, frame.call_line) {
            scratch.lines.insert(LineLocation { file, line });
            scratch.callsites.insert(CallsiteLocation {
                caller_file: file,
                caller_line: line,
                package: frame.package,
                name: frame.name,
            });
        }
        scratch.routines.insert(routine_of(frame));
    }
    for location in scratch.lines.iter() {
        *totals.line_incl_ns.entry(*location).or_default() += ns;
    }
    for location in scratch.routines.iter() {
        *totals.routine_incl_ns.entry(*location).or_default() += ns;
    }
    for location in scratch.callsites.iter() {
        *totals.callsite_incl_ns.entry(*location).or_default() += ns;
    }
}

fn routine_of(frame: &SampleFrame) -> RoutineLocation {
    RoutineLocation {
        package: frame.package,
        name: frame.name,
        file: frame.def_file,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::profile::paths;
    use crate::profile::snapshot::take_samples;

    /// The tables these tests drain are process-global by design (a profile
    /// spans every thread), so the tests that drain them take turns.
    fn test_lock() -> MutexGuard<'static, ()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(()))
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn file_line(file: Symbol, line: u32) -> LineLocation {
        LineLocation { file, line }
    }

    fn frame(package: &str, name: &str, call_line: Option<u32>) -> RoutineFrame {
        RoutineFrame {
            package: Symbol::intern(package),
            lexical_package: None,
            name: Symbol::intern(name),
            line: call_line,
            file: call_line.map(|_| Symbol::intern("fixture.raku")),
            is_method: false,
            is_submethod: false,
            is_block: false,
            def_file: Some(Symbol::intern("fixture.raku")),
            invocation_id: 1,
        }
    }

    fn at(file: &str, line: u32) -> Option<LineLocation> {
        Some(LineLocation {
            file: Symbol::intern(file),
            line,
        })
    }

    #[test]
    fn a_sample_credits_self_to_the_top_and_inclusive_to_the_stack() {
        let _lock = test_lock();
        let _ = take_samples();
        let buffer = ThreadSamples::with_capacity(8, 8);
        let stack = [
            frame("MAIN", "outer", Some(3)),
            frame("MAIN", "inner", Some(9)),
        ];
        buffer.record(1000, at("fixture.raku", 42), Region::Interp, &stack);
        buffer.fold();
        let snapshot = take_samples();

        let hot = LineLocation {
            file: paths::canonical(Symbol::intern("fixture.raku")),
            line: 42,
        };
        assert_eq!(snapshot.line_self_ns, vec![(hot, 1000)]);
        // Self time belongs to the INNERMOST frame, which is the last entry of
        // the routine stack, not the first.
        assert_eq!(
            snapshot
                .routine_self_ns
                .iter()
                .map(|(location, ns)| (location.name.as_str().to_string(), *ns))
                .collect::<Vec<_>>(),
            vec![("inner".to_string(), 1000)]
        );
        // Both frames plus the top line earn inclusive credit, once each.
        assert_eq!(snapshot.routine_incl_ns.len(), 2);
        assert!(snapshot.routine_incl_ns.iter().all(|(_, ns)| *ns == 1000));
        assert_eq!(snapshot.line_incl_ns.len(), 3);
        assert_eq!(snapshot.sampled_ns, 1000);
    }

    #[test]
    fn a_row_is_the_interval_the_sampler_handed_it() {
        let _lock = test_lock();
        let _ = take_samples();
        let buffer = ThreadSamples::with_capacity(8, 8);
        let file = Symbol::intern("fixture.raku");
        // The sampler hands `record` the line that was running when the tick
        // fired, not the poll site that noticed it, so a table row is exactly
        // the interval it was given.
        buffer.record(700, Some(file_line(file, 10)), Region::CallResolve, &[]);
        buffer.record(300, Some(file_line(file, 20)), Region::Regex, &[]);
        buffer.fold();
        let snapshot = take_samples();
        let ns_at = |line: u32| {
            snapshot
                .line_self_ns
                .iter()
                .find(|(location, _)| location.line == line)
                .map(|(_, ns)| *ns)
                .unwrap_or(0)
        };
        assert_eq!(ns_at(10), 700);
        assert_eq!(ns_at(20), 300);
        assert_eq!(snapshot.sampled_ns, 1000);
        // Each sample's self time also lands under the subsystem that claimed
        // its tick, so the whole-run split sums to the sampled total.
        assert_eq!(snapshot.region_ns[Region::CallResolve.index()], 700);
        assert_eq!(snapshot.region_ns[Region::Regex.index()], 300);
        assert_eq!(snapshot.region_samples[Region::CallResolve.index()], 1);
        assert_eq!(snapshot.top_region(), Some((Region::CallResolve, 700)));
        let split: Vec<(u32, &str, u64)> = snapshot
            .line_region_ns
            .iter()
            .map(|(key, ns)| (key.location.line, key.region.name(), *ns))
            .collect();
        assert_eq!(
            split,
            vec![(10, "call-resolve", 700), (20, "regex", 300)],
            "the per-line split names the subsystem each line's time went to"
        );
    }

    #[test]
    fn recursion_earns_inclusive_credit_once_per_distinct_routine() {
        let _lock = test_lock();
        let _ = take_samples();
        let buffer = ThreadSamples::with_capacity(8, 8);
        let stack = [
            frame("MAIN", "fib", Some(7)),
            frame("MAIN", "fib", Some(7)),
            frame("MAIN", "fib", Some(7)),
        ];
        buffer.record(500, at("fixture.raku", 7), Region::Interp, &stack);
        buffer.fold();
        let snapshot = take_samples();
        assert_eq!(snapshot.routine_incl_ns.len(), 1);
        assert_eq!(
            snapshot.routine_incl_ns[0].1, 500,
            "credited once, not thrice"
        );
        assert_eq!(snapshot.callsite_incl_ns.len(), 1);
        assert_eq!(snapshot.callsite_incl_ns[0].1, 500);
    }

    #[test]
    fn a_stack_deeper_than_the_cap_is_counted_as_truncated() {
        let _lock = test_lock();
        let _ = take_samples();
        let buffer = ThreadSamples::with_capacity(4, 2);
        let stack: Vec<RoutineFrame> = (0..5).map(|_| frame("MAIN", "deep", Some(1))).collect();
        buffer.record(10, None, Region::Interp, &stack);
        buffer.fold();
        let snapshot = take_samples();
        assert_eq!(snapshot.truncated, 1);
        assert_eq!(snapshot.samples, 1);
    }

    #[test]
    fn a_buffer_that_fills_folds_instead_of_dropping_samples() {
        let _lock = test_lock();
        let _ = take_samples();
        // Two samples per buffer, so this refills it three times over.
        let buffer = ThreadSamples::with_capacity(2, 2);
        let file = Symbol::intern("fixture.raku");
        for _ in 0..6 {
            buffer.record(100, Some(file_line(file, 1)), Region::Interp, &[]);
        }
        buffer.fold();
        let snapshot = take_samples();
        assert_eq!(snapshot.samples, 6);
        assert_eq!(snapshot.sampled_ns, 600);
    }
}
