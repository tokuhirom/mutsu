//! GC Level 1a synchronous cycle collector (ADR-0001 §3-8,
//! `docs/gc-level1-detailed-design.md` §5 / §9.5 / §11 step 8).
//!
//! Bacon-Rajan trial-deletion over the process-global candidate buffer. This is
//! the first slice that actually *reclaims* garbage: everything before it
//! (§11 steps 1-7) only built the `Gc` node graph and the candidate buffer.
//!
//! Scope of this cut:
//! - **Opt-in.** Reclaim runs from [`collect_cycles`], invoked either manually
//!   (`gc_debug_collect_now`) or automatically at VM safepoints when a
//!   `MUTSU_GC` trigger is set (see [`super::safepoint`]). With `MUTSU_GC` unset
//!   (default) the candidate buffer stays empty, so a call reclaims nothing and
//!   normal execution is unaffected.
//! - **Container cycles.** It walks whatever `Trace` exposes, so it already
//!   handles the migrated container types (`Array`/`Hash`/`Set`/`Bag`/`Mix`/
//!   `ContainerRef`). Async graphs (`Promise`/`Channel`/supply registries, §11
//!   steps 6-7) are covered once those migrate and gain `Trace`/`drop_gc_edges`.
//!
//! ## Algorithm (Bacon & Rajan, "Concurrent Cycle Collection in Reference
//! Counted Systems", synchronous variant)
//!
//! The candidate buffer holds nodes that survived a `Gc` drop (possible cycle
//! roots). A collect does trial-deletion over the subgraph reachable from them:
//! 1. `mark_gray` — tentatively remove each internal edge by decrementing
//!    children's GC strong count, coloring the subgraph gray.
//! 2. `scan` — any node left with `strong > 0` is kept alive by an *external*
//!    reference; `scan_black` restores it and its descendants. Nodes that reach
//!    `strong == 0` are pure cycle garbage → colored white.
//! 3. `collect_white` — gather the white nodes; [`reclaim`] breaks their edges
//!    so the backing `Arc`s free them.
//!
//! The GC strong count (`GcBox::gc_strong*`) is the scratch count trial-deletion
//! mutates; `scan_black` restores it for every survivor, so the heap's refcounts
//! are pristine afterward (only genuine garbage is disturbed).

// The collector has no production caller yet: it runs only via the manual
// `gc_debug_collect_now` hook / unit tests until safepoint wiring lands (§11
// step 8, second half). Module-wide dead-code allow until then, mirroring
// `gc_ptr`; removed when a safepoint / builtin invokes `collect_cycles`.
#![allow(dead_code)]

use std::collections::HashSet;
use std::time::Instant;

use super::gc_ptr::{CollectGuard, Color, ErasedGc, drain_candidates, erased_id, gc_drop_edges};
use crate::vm::vm_stats::record_gc_collection;

/// Outcome of one [`collect_cycles`] run.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct CollectStats {
    /// Candidate roots drained and scanned.
    pub roots_scanned: usize,
    /// Nodes proven to be cycle garbage and reclaimed.
    pub reclaimed_nodes: usize,
    /// Distinct white components reclaimed (a rough cycle count).
    pub reclaimed_cycles: usize,
}

/// Collect the direct `Gc` children of a node as owned handles.
///
/// Cloning an [`ErasedGc`] is a plain `Arc` bump (no `Gc::drop` bookkeeping), so
/// these temporaries are free to hold and drop during traversal.
fn children(node: &ErasedGc) -> Vec<ErasedGc> {
    let mut kids = Vec::new();
    node.gc_visit_children(&mut |c| kids.push(c.clone()));
    kids
}

fn mark_gray(node: &ErasedGc) {
    if node.gc_color() == Color::Gray {
        return;
    }
    node.gc_set_color(Color::Gray);
    for child in children(node) {
        child.gc_strong_dec();
        mark_gray(&child);
    }
}

fn scan(node: &ErasedGc) {
    if node.gc_color() != Color::Gray {
        return;
    }
    if node.gc_strong() > 0 {
        // Kept alive by an external reference — restore the subgraph.
        scan_black(node);
    } else {
        node.gc_set_color(Color::White);
        for child in children(node) {
            scan(&child);
        }
    }
}

fn scan_black(node: &ErasedGc) {
    node.gc_set_color(Color::Black);
    for child in children(node) {
        child.gc_strong_inc();
        if child.gc_color() != Color::Black {
            scan_black(&child);
        }
    }
}

/// Gather the white (garbage) nodes reachable from `node` into `out`, recoloring
/// them black so each is visited once. Returns whether it started a fresh white
/// component (used for the rough cycle count).
fn collect_white(node: &ErasedGc, out: &mut Vec<ErasedGc>, seen: &mut HashSet<usize>) -> bool {
    if node.gc_color() != Color::White || node.gc_buffered() {
        return false;
    }
    let started = seen.insert(erased_id(node));
    node.gc_set_color(Color::Black);
    out.push(node.clone());
    for child in children(node) {
        collect_white(&child, out, seen);
    }
    started
}

/// Break every white node's outgoing `Gc` edges so the backing `Arc`s free them.
///
/// Runs inside a [`CollectGuard`] so the `Gc::drop`s triggered by clearing a
/// node's collections are inert (they must not touch the mid-collect strong
/// counts / re-buffer — see `Gc`'s `Drop`). The `white` handles are dropped
/// while the guard is still held, so any value-drop cascade is inert too.
fn reclaim(white: Vec<ErasedGc>, roots: Vec<ErasedGc>) {
    let _guard = CollectGuard::new();
    for node in &white {
        gc_drop_edges(node);
    }
    // Drop the collector's owning handles *inside* the guard: once the internal
    // edges are gone, these were the last references, so the `Arc`s now free the
    // `GcBox`es — and any drop cascade stays inert under the guard.
    drop(white);
    drop(roots);
}

/// Run one synchronous cycle collection over the current candidate buffer.
///
/// Safe to call at any time: with no candidates buffered it is a no-op. Returns
/// what it reclaimed and records the same into `MUTSU_VM_STATS`.
pub(crate) fn collect_cycles() -> CollectStats {
    let start = Instant::now();
    let roots = drain_candidates();
    if roots.is_empty() {
        return CollectStats::default();
    }
    let roots_scanned = roots.len();

    for root in &roots {
        mark_gray(root);
    }
    for root in &roots {
        scan(root);
    }
    let mut white = Vec::new();
    let mut seen = HashSet::new();
    let mut cycles = 0;
    for root in &roots {
        if collect_white(root, &mut white, &mut seen) {
            cycles += 1;
        }
    }

    let reclaimed_nodes = white.len();
    reclaim(white, roots);

    let pause_ns = start.elapsed().as_nanos().min(u128::from(u64::MAX)) as u64;
    record_gc_collection(
        roots_scanned as u64,
        reclaimed_nodes as u64,
        cycles as u64,
        pause_ns,
    );
    CollectStats {
        roots_scanned,
        reclaimed_nodes,
        reclaimed_cycles: cycles,
    }
}

/// Debug hook (design doc §9.5): force a collect now, regardless of triggers.
/// The entry point a future `gc_debug_collect_now` builtin / REPL command and
/// the integration tests call.
pub(crate) fn gc_debug_collect_now() -> CollectStats {
    collect_cycles()
}

#[cfg(test)]
mod tests {
    use super::super::gc_ptr::{Gc, Trace};
    use super::*;
    use std::sync::Mutex;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// Serializes tests: they share the process-global candidate buffer, the
    /// `CollectGuard` flag, and the `DROPS` counter. Poison-tolerant.
    static TEST_LOCK: Mutex<()> = Mutex::new(());
    /// Counts `TestNode` frees, so a test can assert a cycle was reclaimed.
    static DROPS: AtomicUsize = AtomicUsize::new(0);

    fn lock() -> std::sync::MutexGuard<'static, ()> {
        TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner())
    }

    /// A GC node that holds mutable `Gc` children, so tests can wire cycles and
    /// the collector can trace / break them. Bumps `DROPS` when freed.
    struct TestNode {
        children: Mutex<Vec<Gc<TestNode>>>,
    }

    impl TestNode {
        fn new() -> Gc<TestNode> {
            Gc::new(TestNode {
                children: Mutex::new(Vec::new()),
            })
        }
        fn link(parent: &Gc<TestNode>, child: &Gc<TestNode>) {
            parent.children.lock().unwrap().push(child.clone());
        }
    }

    impl Trace for TestNode {
        fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
            for c in self.children.lock().unwrap().iter() {
                visit(&c.erased());
            }
        }
        fn drop_gc_edges(&mut self) {
            self.children.get_mut().unwrap().clear();
        }
    }

    impl Drop for TestNode {
        fn drop(&mut self) {
            DROPS.fetch_add(1, Ordering::Relaxed);
        }
    }

    #[test]
    fn empty_buffer_collects_nothing() {
        let _g = lock();
        drain_candidates();
        assert_eq!(collect_cycles(), CollectStats::default());
    }

    #[test]
    fn two_node_cycle_is_reclaimed() {
        let _g = lock();
        drain_candidates();
        let before = DROPS.load(Ordering::Relaxed);

        let a = TestNode::new();
        let b = TestNode::new();
        TestNode::link(&a, &b);
        TestNode::link(&b, &a); // a <-> b cycle
        // Register both as candidates while the external handles are still live,
        // then drop the external handles: the pair is now an isolated cycle.
        a.buffer_as_candidate();
        b.buffer_as_candidate();
        drop(a);
        drop(b);

        let stats = collect_cycles();
        assert_eq!(stats.reclaimed_nodes, 2, "both cycle nodes reclaimed");
        assert_eq!(stats.reclaimed_cycles, 1);
        assert_eq!(
            DROPS.load(Ordering::Relaxed) - before,
            2,
            "both TestNodes were actually freed"
        );
    }

    #[test]
    fn self_cycle_is_reclaimed() {
        let _g = lock();
        drain_candidates();
        let before = DROPS.load(Ordering::Relaxed);

        let a = TestNode::new();
        TestNode::link(&a, &a); // a -> a
        a.buffer_as_candidate();
        drop(a);

        let stats = collect_cycles();
        assert_eq!(stats.reclaimed_nodes, 1);
        assert_eq!(DROPS.load(Ordering::Relaxed) - before, 1);
    }

    #[test]
    fn cycle_with_external_ref_survives() {
        let _g = lock();
        drain_candidates();
        let before = DROPS.load(Ordering::Relaxed);

        let a = TestNode::new();
        let b = TestNode::new();
        TestNode::link(&a, &b);
        TestNode::link(&b, &a);
        a.buffer_as_candidate();
        b.buffer_as_candidate();
        // Keep `a` alive (external reference); only `b`'s external handle drops.
        drop(b);

        let stats = collect_cycles();
        assert_eq!(stats.reclaimed_nodes, 0, "live external ref => not garbage");
        assert_eq!(DROPS.load(Ordering::Relaxed) - before, 0);
        // `a` (and `b` through the cycle) are still usable.
        assert_eq!(a.children.lock().unwrap().len(), 1);
        drop(a);
    }

    #[test]
    fn acyclic_candidates_are_not_reclaimed() {
        let _g = lock();
        drain_candidates();
        let before = DROPS.load(Ordering::Relaxed);

        // a -> b, no back edge; keep both external handles alive.
        let a = TestNode::new();
        let b = TestNode::new();
        TestNode::link(&a, &b);
        a.buffer_as_candidate();
        b.buffer_as_candidate();

        let stats = collect_cycles();
        assert_eq!(stats.reclaimed_nodes, 0);
        assert_eq!(DROPS.load(Ordering::Relaxed) - before, 0);
        drop(a);
        drop(b);
    }
}
