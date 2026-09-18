//! Which interpreter subsystem a sample's time went to (ADR-0106 Slice 4, D4).
//!
//! A sampled line profile says "line 412 is 41% of the run". For mutsu that is
//! only half an answer: the time is nearly always spent in *mutsu's* code on
//! that line's behalf, and which part matters — call resolution, the regex
//! walk, GC, the parser, an `nqp::` op, a native builtin. Without the split,
//! every profile ends in the same next step: open callgrind and re-derive it by
//! hand, which is the manual translation ADR-0106 exists to remove.
//!
//! # Why a region is claimed at its *exit*, not read at the sample
//!
//! The obvious mechanism — a thread-local "current region" the sampler reads
//! when it takes a sample — cannot work here, and it is worth writing down why,
//! because it looks like it should. mutsu's sampler is poll-based: a tick is
//! noticed at the next VM poll, and the polls are in the bytecode dispatch
//! loops. The regions worth naming are precisely the long native stretches that
//! *do not* poll, so by the time a poll notices the tick the region has already
//! returned and the "current region" reads `Interp` every single time. A tag
//! read at the sample point would therefore report that mutsu spends ~100% of
//! its time interpreting bytecode, which is both useless and false.
//!
//! So the region claims the tick instead. Leaving a region costs one relaxed
//! load and a compare ([`super::sampler::tick_pending`]): if the epoch moved
//! while this region was running, the region latches itself into [`PENDING`],
//! and the next poll's sample — whose elapsed weight covers exactly the
//! stretch that region occupied — is tagged with it. Nothing is measured on
//! the region path: no clock read, no hash lookup, no allocation, no atomic
//! store. That matters beyond cost, because a region that paid for its own
//! instrumentation would inflate precisely the number it exists to report.
//!
//! # Why the first claim wins
//!
//! A claim only happens when the tick is *already* pending at that exit, which
//! means the tick fired before it — so the region claiming is one that was
//! genuinely running at the tick instant, and any region that runs afterwards
//! demonstrably was not. First-claim-wins therefore reads correctly in both
//! shapes it has to handle: nested (`method-dispatch { call-resolve }`, where
//! the inner guard drops first and the enclosing one finds the latch taken)
//! and sequential (a resolution walk, then a parse, with no poll between them,
//! where only the first can have been running when the tick fired).
//!
//! The one bias left is a tick that fires in bytecode and is claimed by a
//! region entered before the next poll. It is bounded by the gap between the
//! tick and the region's entry, which is small next to any region long enough
//! to span a tick in the first place — and the long regions are the ones this
//! table exists to name.
//!
//! # Coverage
//!
//! There is no `unknown` tag, by construction: a sample that no region claimed
//! was taken while the thread was running bytecode, and [`Region::Interp`] says
//! exactly that. Time that provably was not running Raku code — a GC collect, a
//! stop-the-world park — never reaches the sampled tables at all
//! ([`super::sampler::exclude_non_raku`] subtracts it); it is *measured* and
//! reported in its own table, so it is named rather than silently missing.

use std::cell::Cell;

/// The subsystem tags. A small fixed enum rather than a name: a string-keyed
/// tag on a dispatch path is the anti-pattern `docs/perf-callpath-scouting.md`
/// §1 catalogues, and these sites are some of the hottest code mutsu has.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub(crate) enum Region {
    /// Running bytecode: the default, and what a sample no region claimed
    /// means.
    Interp = 0,
    /// The full function-resolution walk (`resolve_function_with_types`) — the
    /// one already counted per name by `MUTSU_VM_STATS`.
    CallResolve,
    /// Method resolution and dispatch, up to entering the method's body.
    MethodDispatch,
    /// A native (pure-Rust) builtin function or method.
    NativeBuiltin,
    /// An `nqp::` op.
    Nqp,
    /// The regex engine's walk.
    Regex,
    /// The parser, including the re-entrant parses `EVAL`/`require` perform.
    Parse,
    /// A GC collect or a stop-the-world park. Excluded-time only: this never
    /// tags a sample, because GC time is subtracted from the sample weight
    /// rather than charged to the line that reached the safepoint.
    Gc,
}

impl Region {
    /// How many tags there are, for the fixed-size accumulators.
    pub(crate) const COUNT: usize = 8;

    /// The spelling the report and the tests use. Kebab-case, matching the
    /// names ADR-0106 D4 writes.
    pub(crate) fn name(self) -> &'static str {
        match self {
            Region::Interp => "interp",
            Region::CallResolve => "call-resolve",
            Region::MethodDispatch => "method-dispatch",
            Region::NativeBuiltin => "native-builtin",
            Region::Nqp => "nqp",
            Region::Regex => "regex",
            Region::Parse => "parse",
            Region::Gc => "gc",
        }
    }

    pub(crate) fn index(self) -> usize {
        self as usize
    }

    pub(crate) fn from_index(index: usize) -> Region {
        match index {
            1 => Region::CallResolve,
            2 => Region::MethodDispatch,
            3 => Region::NativeBuiltin,
            4 => Region::Nqp,
            5 => Region::Regex,
            6 => Region::Parse,
            7 => Region::Gc,
            _ => Region::Interp,
        }
    }
}

thread_local! {
    /// The region that claimed the pending tick, consumed by the next sample.
    /// A plain `Cell`: per-thread, no atomics, and touched only inside the
    /// armed gate.
    static PENDING: Cell<Region> = const { Cell::new(Region::Interp) };
}

/// Bracket a subsystem so a tick that fires inside it is attributed to it.
///
/// Held by value at the chokepoint (`let _region = profile::enter(...)`), so
/// the bracket follows the borrow checker's scope rather than a hand-placed
/// pair of calls that an early `return` or `?` could skip.
pub(crate) struct RegionGuard {
    region: Region,
    armed: bool,
}

/// Enter `region`. Disarmed this is one cached-bool load and a two-byte value;
/// armed it is that plus a relaxed load and a compare when the guard drops.
#[inline]
pub(crate) fn enter(region: Region) -> RegionGuard {
    RegionGuard {
        region,
        armed: crate::vm::vm_poll::profiler_armed(),
    }
}

impl Drop for RegionGuard {
    #[inline]
    fn drop(&mut self) {
        if self.armed {
            claim(self.region);
        }
    }
}

/// Latch `region` as the owner of the pending tick, if there is one and no
/// inner region has already claimed it.
fn claim(region: Region) {
    if !super::sampler::tick_pending() {
        return;
    }
    let _ = PENDING.try_with(|cell| {
        if cell.get() == Region::Interp {
            cell.set(region);
        }
    });
}

/// Take the region a sample should carry, resetting the latch. Called from the
/// sample path and nowhere else.
pub(crate) fn take_pending() -> Region {
    PENDING
        .try_with(|cell| cell.replace(Region::Interp))
        .unwrap_or(Region::Interp)
}

/// Measured (not sampled) nanoseconds per region, for the time
/// [`super::sampler::exclude_non_raku`] keeps out of the line table.
///
/// A global array of atomics rather than a per-thread table: the sites that
/// feed it are GC collects and stop-the-world parks, which are rare enough
/// that an uncontended `fetch_add` is free, and a single table means the report
/// does not have to reconcile a thread that exited early.
static EXCLUDED_NS: [std::sync::atomic::AtomicU64; Region::COUNT] =
    [const { std::sync::atomic::AtomicU64::new(0) }; Region::COUNT];

pub(crate) fn add_excluded_ns(region: Region, ns: u64) {
    EXCLUDED_NS[region.index()].fetch_add(ns, std::sync::atomic::Ordering::Relaxed);
}

/// Every region with measured excluded time, hottest first.
pub(crate) fn take_excluded_ns() -> Vec<(Region, u64)> {
    let mut rows: Vec<(Region, u64)> = EXCLUDED_NS
        .iter()
        .enumerate()
        .map(|(index, cell)| {
            (
                Region::from_index(index),
                cell.swap(0, std::sync::atomic::Ordering::Relaxed),
            )
        })
        .filter(|(_, ns)| *ns > 0)
        .collect();
    rows.sort_by_key(|(region, ns)| (std::cmp::Reverse(*ns), *region));
    rows
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_index_round_trips_to_its_own_tag() {
        for index in 0..Region::COUNT {
            assert_eq!(Region::from_index(index).index(), index);
        }
    }

    #[test]
    fn tags_are_named_uniquely() {
        let mut names: Vec<&str> = (0..Region::COUNT)
            .map(|index| Region::from_index(index).name())
            .collect();
        names.sort_unstable();
        let total = names.len();
        names.dedup();
        assert_eq!(names.len(), total, "two regions share a name");
    }

    #[test]
    fn the_first_region_to_claim_a_pending_tick_keeps_it() {
        // The claim order a nested `method-dispatch { call-resolve }` produces:
        // the inner guard drops first, and the outer -- which was also running
        // when the tick fired, but is the less specific answer -- must not
        // overwrite it. The same rule settles a sequential pair, where only the
        // first one to exit can have been running at the tick instant.
        PENDING.with(|cell| cell.set(Region::Interp));
        let claim_if_pending = |region: Region| {
            PENDING.with(|cell| {
                if cell.get() == Region::Interp {
                    cell.set(region);
                }
            })
        };
        claim_if_pending(Region::CallResolve);
        claim_if_pending(Region::MethodDispatch);
        assert_eq!(take_pending(), Region::CallResolve);
        // Taking it resets the latch, so the next interval starts clean.
        assert_eq!(take_pending(), Region::Interp);
    }

    #[test]
    fn excluded_time_is_reported_per_region_and_drained() {
        let _ = take_excluded_ns();
        add_excluded_ns(Region::Gc, 700);
        add_excluded_ns(Region::Gc, 300);
        assert_eq!(take_excluded_ns(), vec![(Region::Gc, 1000)]);
        assert!(take_excluded_ns().is_empty());
    }
}
