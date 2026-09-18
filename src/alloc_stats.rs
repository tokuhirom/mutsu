//! Deterministic per-scope allocation accounting (`alloc-stats` feature).
//!
//! Why this exists: the ADR-0019 G3 investigation
//! (adr0019-g3-diffuse-bless-allocation-cost, #7561) stalled because
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
//! Zero. [`alloc_scope!`](crate::alloc_scope) expands to nothing and the custom allocator is not
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

/// Like [`alloc_scope!`](crate::alloc_scope), but binds the guard to a named variable so the region
/// can be closed early with [`alloc_scope_end!`](crate::alloc_scope_end) instead of running to the end
/// of the block. Use it to split one function into sequential phases without
/// re-indenting it into nested blocks.
#[macro_export]
macro_rules! alloc_scope_named {
    ($ident:ident, $label:literal) => {
        #[cfg(feature = "alloc-stats")]
        let $ident = $crate::alloc_stats::Scope::new($label);
    };
}

/// Like [`alloc_scope!`](crate::alloc_scope), but takes the label as an
/// expression evaluating to a `&'static str` instead of a literal.
///
/// This exists for regions whose identity is only known at run time — above
/// all the bytecode dispatch loop, where the interesting question is "which
/// *opcode family* allocates", and the label has to come from
/// `alloc_stats::opcode_label` (an `alloc-stats`-only helper). The
/// expression is not evaluated at all unless the `alloc-stats` feature is on.
#[macro_export]
macro_rules! alloc_scope_dyn {
    ($label:expr) => {
        #[cfg(feature = "alloc-stats")]
        let _mutsu_alloc_scope = $crate::alloc_stats::Scope::new($label);
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
pub(crate) use imp::opcode_label;
#[cfg(feature = "alloc-stats")]
pub use imp::{CountingAllocator, Scope, dump};

/// Exact allocation totals attributed to one Raku source line.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct LineAllocation {
    pub(crate) count: u64,
    pub(crate) bytes: u64,
}

/// Whether this build is collecting source-line allocation totals.
#[cfg(feature = "alloc-stats")]
#[inline]
pub(crate) fn line_attribution_enabled() -> bool {
    imp::line_attribution_enabled()
}

#[cfg(feature = "alloc-stats")]
pub(crate) fn configure_line_attribution() {
    imp::configure_line_attribution();
}

#[cfg(not(feature = "alloc-stats"))]
#[inline]
pub(crate) fn line_attribution_enabled() -> bool {
    false
}

#[cfg(not(feature = "alloc-stats"))]
pub(crate) fn configure_line_attribution() {}

#[cfg(feature = "alloc-stats")]
#[inline]
pub(crate) fn enter_line(location: Option<crate::profile::LineLocation>) -> imp::LineGuard {
    imp::enter_line(location)
}

#[cfg(feature = "alloc-stats")]
#[inline]
pub(crate) fn set_current_line(location: Option<crate::profile::LineLocation>) {
    imp::set_current_line(location);
}

#[cfg(feature = "alloc-stats")]
#[inline]
pub(crate) fn clear_current_line() {
    imp::clear_current_line();
}

#[cfg(feature = "alloc-stats")]
#[inline]
pub(crate) fn take_line_stats() -> Vec<(crate::profile::LineLocation, LineAllocation)> {
    imp::take_line_stats()
}

#[cfg(not(feature = "alloc-stats"))]
#[inline]
pub(crate) fn take_line_stats() -> Vec<(crate::profile::LineLocation, LineAllocation)> {
    Vec::new()
}

/// No-op stand-in used when the `alloc-stats` feature is off.
#[cfg(not(feature = "alloc-stats"))]
pub fn dump() {}

#[cfg(feature = "alloc-stats")]
mod imp {
    use super::LineAllocation;
    use crate::profile::LineLocation;
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::cell::{Cell, RefCell};
    use std::collections::HashMap;
    use std::sync::{Arc, Mutex, MutexGuard, OnceLock};

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
        /// The source line whose VM operation is currently executing.
        static CURRENT_LINE: Cell<Option<LineLocation>> = const { Cell::new(None) };
        /// Allocation totals waiting to be folded when the current operation
        /// ends. The allocator only bumps these cells, so it never allocates
        /// or takes a lock on the allocation path.
        static CURRENT_LINE_ALLOCS: Cell<u64> = const { Cell::new(0) };
        static CURRENT_LINE_BYTES: Cell<u64> = const { Cell::new(0) };
        static LINE_TABLES: RefCell<Option<LineTables>> = const { RefCell::new(None) };
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

    #[derive(Default)]
    struct LineTable {
        allocations: HashMap<LineLocation, LineAllocation>,
    }

    type LineTableHandle = Arc<Mutex<LineTable>>;

    struct LineTables(LineTableHandle);

    impl LineTables {
        fn new() -> Self {
            let handle = Arc::new(Mutex::new(LineTable::default()));
            lock(line_registry()).push(Arc::clone(&handle));
            Self(handle)
        }

        fn table(&self) -> MutexGuard<'_, LineTable> {
            lock(&self.0)
        }
    }

    fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
        mutex
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn line_registry() -> &'static Mutex<Vec<LineTableHandle>> {
        static REGISTRY: OnceLock<Mutex<Vec<LineTableHandle>>> = OnceLock::new();
        REGISTRY.get_or_init(|| Mutex::new(Vec::new()))
    }

    static LINE_STATS_ENABLED: std::sync::atomic::AtomicBool =
        std::sync::atomic::AtomicBool::new(false);

    pub(crate) fn configure_line_attribution() {
        LINE_STATS_ENABLED.store(
            std::env::var_os("MUTSU_ALLOC_STATS").is_some(),
            std::sync::atomic::Ordering::Relaxed,
        );
    }

    pub(crate) fn line_attribution_enabled() -> bool {
        LINE_STATS_ENABLED.load(std::sync::atomic::Ordering::Relaxed)
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
            if line_attribution_enabled() {
                let _ = CURRENT_LINE.with(|line| {
                    if line.get().is_some() {
                        CURRENT_LINE_ALLOCS.with(|count| count.set(count.get().wrapping_add(1)));
                        CURRENT_LINE_BYTES
                            .with(|bytes| bytes.set(bytes.get().wrapping_add(size as u64)));
                    }
                });
            }
        });
    }

    fn fold_current_line() {
        let Some(location) = CURRENT_LINE.with(|line| line.get()) else {
            CURRENT_LINE_ALLOCS.with(|count| count.set(0));
            CURRENT_LINE_BYTES.with(|bytes| bytes.set(0));
            return;
        };
        let count = CURRENT_LINE_ALLOCS.with(|value| value.replace(0));
        let bytes = CURRENT_LINE_BYTES.with(|value| value.replace(0));
        if count == 0 && bytes == 0 {
            return;
        }

        let was = SUSPENDED.with(|suspended| suspended.replace(true));
        LINE_TABLES.with(|cell| {
            let Ok(mut slot) = cell.try_borrow_mut() else {
                return;
            };
            let tables = slot.get_or_insert_with(LineTables::new);
            let mut table = tables.table();
            let totals = table.allocations.entry(location).or_default();
            totals.count = totals.count.wrapping_add(count);
            totals.bytes = totals.bytes.wrapping_add(bytes);
        });
        SUSPENDED.with(|suspended| suspended.set(was));
    }

    /// Guard the source line for one interpreted VM operation.
    pub(crate) struct LineGuard {
        previous: Option<LineLocation>,
    }

    pub(crate) fn enter_line(location: Option<LineLocation>) -> LineGuard {
        if !line_attribution_enabled() {
            return LineGuard { previous: None };
        }
        fold_current_line();
        let previous = CURRENT_LINE.with(|line| line.replace(location));
        LineGuard { previous }
    }

    /// Set the line for native JIT code. The next hook replaces it, and the
    /// JIT entry clears it when native execution returns.
    pub(crate) fn set_current_line(location: Option<LineLocation>) {
        if !line_attribution_enabled() {
            return;
        }
        fold_current_line();
        CURRENT_LINE.with(|line| line.set(location));
    }

    pub(crate) fn clear_current_line() {
        if !line_attribution_enabled() {
            return;
        }
        fold_current_line();
        CURRENT_LINE.with(|line| line.set(None));
    }

    impl Drop for LineGuard {
        fn drop(&mut self) {
            if !line_attribution_enabled() {
                return;
            }
            fold_current_line();
            CURRENT_LINE.with(|line| line.set(self.previous));
        }
    }

    pub(crate) fn take_line_stats() -> Vec<(LineLocation, LineAllocation)> {
        if !line_attribution_enabled() {
            return Vec::new();
        }
        fold_current_line();
        let handles: Vec<LineTableHandle> = lock(line_registry()).clone();
        let mut folded: HashMap<LineLocation, LineAllocation> = HashMap::new();
        for handle in &handles {
            let mut table = lock(handle);
            for (location, totals) in table.allocations.drain() {
                let entry = folded.entry(location).or_default();
                entry.count = entry.count.wrapping_add(totals.count);
                entry.bytes = entry.bytes.wrapping_add(totals.bytes);
            }
        }
        drop(handles);
        lock(line_registry()).retain(|handle| Arc::strong_count(handle) > 1);
        let mut rows: Vec<_> = folded.into_iter().collect();
        rows.sort_by_key(|(location, _)| (location.file.id(), location.line));
        rows
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

    /// The bare variant name of an opcode, as a `&'static str` suitable for a
    /// [`Scope`] label.
    ///
    /// The dispatch loop needs a label per opcode *family* to attribute the
    /// `mfast:body` region, but `OpCode` has no name method and a `Scope`
    /// label must be `'static`. Derive it once per variant from the `Debug`
    /// output (`ForLoop(..)` / `WhileLoop { .. }` -> `ForLoop`, exactly as
    /// `vm_stats::record_opcode` does) and leak it; there are ~340 variants,
    /// so the leak is bounded and this only ever runs in a measurement build.
    pub(crate) fn opcode_label(op: &crate::opcode::OpCode) -> &'static str {
        fn cache()
        -> &'static Mutex<HashMap<std::mem::Discriminant<crate::opcode::OpCode>, &'static str>>
        {
            static CACHE: OnceLock<
                Mutex<HashMap<std::mem::Discriminant<crate::opcode::OpCode>, &'static str>>,
            > = OnceLock::new();
            CACHE.get_or_init(|| Mutex::new(HashMap::new()))
        }
        // The bookkeeping below allocates (the `format!`, the map growth); none
        // of it is the program's own allocation behavior, so it must not be
        // counted -- same reasoning as `Scope::drop`'s fold.
        let was = SUSPENDED.with(|s| s.replace(true));
        let d = std::mem::discriminant(op);
        let label = match cache().lock() {
            Ok(mut map) => match map.get(&d) {
                Some(name) => *name,
                None => {
                    let dbg = format!("{op:?}");
                    let name: String = dbg
                        .chars()
                        .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                        .collect();
                    *map.entry(d)
                        .or_insert(Box::leak(format!("op:{name}").into_boxed_str()))
                }
            },
            Err(_) => "op:<poisoned>",
        };
        SUSPENDED.with(|s| s.set(was));
        label
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
