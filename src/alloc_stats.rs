//! Deterministic per-scope allocation accounting (`alloc-stats` feature).
//!
//! Why this exists: the ADR-0019 G3 investigation
//! (`todo/perf/adr0019-g3-diffuse-bless-allocation-cost.md`) stalled because
//! `bench-ctor`'s cost is *diffuse* — a flat `perf` profile shows ~7% in
//! `malloc`/`free` and ~6.5% in NaN-box GC/refcount ops with no single hot
//! function, and call-graph attribution was unusable in the dev container
//! (`--call-graph dwarf` died on a stale `/root/.debug` build-id store,
//! `--call-graph fp` produced garbage stacks through the optimized build). So
//! "which caller is doing all the allocating?" could not be answered with a
//! profiler at all.
//!
//! This module answers it without one. A counting `#[global_allocator]` plus a
//! scope stack turns the question into an exact, deterministic *count*:
//! `alloc_scope!("bless")` around a region reports how many allocations (and
//! bytes) happened inside it, both inclusive of nested scopes and exclusive of
//! them. Counts do not depend on machine load, thermals, or binary layout, so
//! a change either reduces the number of allocations per `bless` or it does
//! not — the same property that makes the `MUTSU_VM_STATS` counters useful for
//! the dual-store work (see the "Build profiles and benchmark numbers" section
//! of CLAUDE.md).
//!
//! # Usage
//!
//! ```text
//! cargo build --release --features alloc-stats
//! MUTSU_ALLOC_STATS=1 ./target/release/mutsu benchmarks/bench-ctor.raku
//! ```
//!
//! The report goes to stderr at the end of the run, via `dump_vm_stats()`.
//!
//! # Cost when the feature is off (the default)
//!
//! Zero. [`alloc_scope!`] expands to nothing and the custom allocator is not
//! installed, so a default `cargo build` is byte-identical to one from before
//! this module existed. The feature is a measurement tool, never shipped on.
//!
//! # Cost when the feature is on
//!
//! Every allocation does one const-initialized thread-local `Cell` bump. The
//! numbers are still exact — but wall-clock from an `alloc-stats` build is
//! meaningless; measure time with a normal release build.

/// Open an allocation-accounting scope for the rest of the enclosing block.
///
/// Expands to nothing unless the `alloc-stats` feature is on, so call sites can
/// be left in place permanently. The label must be a string literal.
///
/// ```ignore
/// fn dispatch_bless(&mut self, ..) -> Result<Value, RuntimeError> {
///     crate::alloc_scope!("bless");
///     ..
/// }
/// ```
#[macro_export]
macro_rules! alloc_scope {
    ($label:literal) => {
        #[cfg(feature = "alloc-stats")]
        let _mutsu_alloc_scope = $crate::alloc_stats::Scope::new($label);
    };
}

/// Like [`alloc_scope!`], but binds the guard to a named variable so the region
/// can be closed early with [`alloc_scope_end!`] instead of running to the end
/// of the block. Use it to split one function into sequential phases without
/// re-indenting it into nested blocks.
#[macro_export]
macro_rules! alloc_scope_named {
    ($ident:ident, $label:literal) => {
        #[cfg(feature = "alloc-stats")]
        let $ident = $crate::alloc_stats::Scope::new($label);
    };
}

/// Close a scope opened by [`alloc_scope_named!`].
#[macro_export]
macro_rules! alloc_scope_end {
    ($ident:ident) => {
        #[cfg(feature = "alloc-stats")]
        drop($ident);
    };
}

#[cfg(feature = "alloc-stats")]
pub use imp::{CountingAllocator, Scope, dump};

/// No-op stand-in used when the `alloc-stats` feature is off.
#[cfg(not(feature = "alloc-stats"))]
pub fn dump() {}

#[cfg(feature = "alloc-stats")]
mod imp {
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::cell::Cell;
    use std::collections::HashMap;
    use std::sync::{Mutex, OnceLock};

    thread_local! {
        /// Allocations made by this thread so far.
        static ALLOCS: Cell<u64> = const { Cell::new(0) };
        /// Bytes requested by this thread so far (allocations only; frees are
        /// not subtracted — this is turnover, not peak RSS).
        static BYTES: Cell<u64> = const { Cell::new(0) };
        /// Inclusive totals accumulated by the direct children of the scope
        /// currently on top of this thread's scope stack, so a scope can
        /// report exclusive counts by subtracting them.
        static CHILD_ALLOCS: Cell<u64> = const { Cell::new(0) };
        static CHILD_BYTES: Cell<u64> = const { Cell::new(0) };
        /// Set while a [`Scope`] is folding its result into the global report.
        /// That bookkeeping allocates (the report's `HashMap`), and counting
        /// those allocations would both pollute the numbers and, worse, mean
        /// the allocator re-enters the map it is already borrowing.
        static SUSPENDED: Cell<bool> = const { Cell::new(false) };
    }

    /// One scope's accumulated counts across every thread and every entry.
    #[derive(Default, Clone, Copy)]
    struct Stats {
        entries: u64,
        incl_allocs: u64,
        incl_bytes: u64,
        excl_allocs: u64,
        excl_bytes: u64,
    }

    fn report() -> &'static Mutex<HashMap<&'static str, Stats>> {
        static REPORT: OnceLock<Mutex<HashMap<&'static str, Stats>>> = OnceLock::new();
        REPORT.get_or_init(|| Mutex::new(HashMap::new()))
    }

    #[inline]
    fn record(size: usize) {
        // A thread whose TLS is being torn down would panic on `.with`; during
        // shutdown we simply stop counting.
        let _ = SUSPENDED.try_with(|s| {
            if s.get() {
                return;
            }
            let _ = ALLOCS.try_with(|a| a.set(a.get().wrapping_add(1)));
            let _ = BYTES.try_with(|b| b.set(b.get().wrapping_add(size as u64)));
        });
    }

    /// `System`, plus a counter bump on every allocating call.
    ///
    /// `dealloc` is deliberately not counted: the question this tool answers is
    /// "how many allocations does constructing one object cost", and a scope's
    /// frees are not necessarily made inside it (a value built during `bless`
    /// is dropped long after). `realloc` counts as one allocation of the new
    /// size, which is what it costs.
    pub struct CountingAllocator;

    unsafe impl GlobalAlloc for CountingAllocator {
        #[inline]
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            record(layout.size());
            unsafe { System.alloc(layout) }
        }

        #[inline]
        unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
            unsafe { System.dealloc(ptr, layout) }
        }

        #[inline]
        unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
            record(layout.size());
            unsafe { System.alloc_zeroed(layout) }
        }

        #[inline]
        unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
            record(new_size);
            unsafe { System.realloc(ptr, layout, new_size) }
        }
    }

    /// A live allocation-accounting region; see [`crate::alloc_scope!`].
    ///
    /// Nesting is handled by the same save/restore trick a profiler uses for
    /// self time: on entry the scope takes over the thread's child accumulator,
    /// on exit it reports `inclusive - children` as its exclusive cost and adds
    /// its own inclusive total to the parent's accumulator.
    pub struct Scope {
        label: &'static str,
        start_allocs: u64,
        start_bytes: u64,
        parent_child_allocs: u64,
        parent_child_bytes: u64,
    }

    impl Scope {
        pub fn new(label: &'static str) -> Self {
            Scope {
                label,
                start_allocs: ALLOCS.with(|a| a.get()),
                start_bytes: BYTES.with(|b| b.get()),
                parent_child_allocs: CHILD_ALLOCS.with(|a| a.replace(0)),
                parent_child_bytes: CHILD_BYTES.with(|b| b.replace(0)),
            }
        }
    }

    impl Drop for Scope {
        fn drop(&mut self) {
            let incl_allocs = ALLOCS.with(|a| a.get()).wrapping_sub(self.start_allocs);
            let incl_bytes = BYTES.with(|b| b.get()).wrapping_sub(self.start_bytes);
            let child_allocs = CHILD_ALLOCS.with(|a| a.replace(self.parent_child_allocs));
            let child_bytes = CHILD_BYTES.with(|b| b.replace(self.parent_child_bytes));
            CHILD_ALLOCS.with(|a| a.set(a.get().wrapping_add(incl_allocs)));
            CHILD_BYTES.with(|b| b.set(b.get().wrapping_add(incl_bytes)));

            // Fold into the shared report with counting suspended: the map's
            // own growth is not part of the program's allocation behavior.
            let was = SUSPENDED.with(|s| s.replace(true));
            if let Ok(mut map) = report().lock() {
                let e = map.entry(self.label).or_default();
                e.entries += 1;
                e.incl_allocs += incl_allocs;
                e.incl_bytes += incl_bytes;
                e.excl_allocs += incl_allocs.saturating_sub(child_allocs);
                e.excl_bytes += incl_bytes.saturating_sub(child_bytes);
            }
            SUSPENDED.with(|s| s.set(was));
        }
    }

    /// Print the per-scope allocation report to stderr.
    ///
    /// No-op unless `MUTSU_ALLOC_STATS` is set, so an `alloc-stats` build still
    /// behaves like a normal one for anything that reads stderr (the `t/` TAP
    /// suite, `is_run`-style tests) unless measurement was asked for.
    pub fn dump() {
        if std::env::var_os("MUTSU_ALLOC_STATS").is_none() {
            return;
        }
        let Ok(map) = report().lock() else { return };
        let mut rows: Vec<(&&str, &Stats)> = map.iter().collect();
        rows.sort_by_key(|(_, s)| std::cmp::Reverse(s.excl_allocs));
        eprintln!(
            "alloc-stats: {:<36} {:>10} {:>12} {:>12} {:>12} {:>12} {:>10}",
            "scope",
            "entries",
            "incl-allocs",
            "excl-allocs",
            "incl-bytes",
            "excl-bytes",
            "per-entry"
        );
        for (label, s) in rows {
            let per_entry = if s.entries == 0 {
                0.0
            } else {
                s.incl_allocs as f64 / s.entries as f64
            };
            eprintln!(
                "alloc-stats: {:<36} {:>10} {:>12} {:>12} {:>12} {:>12} {:>10.1}",
                label,
                s.entries,
                s.incl_allocs,
                s.excl_allocs,
                s.incl_bytes,
                s.excl_bytes,
                per_entry
            );
        }
        // The process-wide total is the main thread's raw counter; it bounds
        // every scope's inclusive number and shows how much of the run the
        // instrumented scopes actually cover.
        eprintln!(
            "alloc-stats: process total (main thread): {} allocations, {} bytes",
            ALLOCS.with(|a| a.get()),
            BYTES.with(|b| b.get())
        );
    }
}
