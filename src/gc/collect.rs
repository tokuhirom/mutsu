//! GC Level 1a synchronous cycle collector (ADR-0001 §3-8,
//! `docs/gc-level1-detailed-design.md` §5 / §9.4 / §9.5 / §11 step 8).
//!
//! Bacon-Rajan trial-deletion over the process-global candidate buffer built by
//! the `Gc` node graph (`gc_ptr`, §11 steps 1-7).
//!
//! Debug surface: `MUTSU_GC_LOG=summary|detail|trace` emits per-collect log
//! lines (§9.4); `MUTSU_GC_VERIFY=1` checks the collector's soundness invariants
//! around every collect (§9.5). Both are opt-in and cost nothing when unset.
//!
//! Scope:
//! - **Default on.** Reclaim runs from [`collect_cycles_at`], invoked at VM
//!   safepoints under the ADR-0003 adaptive size threshold (see
//!   [`super::safepoint`]) and once at program end
//!   ([`collect_at_program_end`]). `MUTSU_GC=off` disarms safepoints and keeps
//!   the candidate buffer empty, so a call reclaims nothing.
//! - **Container cycles.** It walks whatever `Trace` exposes — all migrated
//!   container kinds (`Array`/`Hash`/`Set`/`Bag`/`Mix`/`ContainerRef`/`Sub`/
//!   `Instance`/`LazyList`) and the async graph (`Promise`/`Channel`, §11
//!   steps 6-7).
//! - **Cross-thread.** A collect first brings every other mutator to
//!   quiescence via the cooperative stop-the-world (`gc::stw`, §6.1).
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

use crate::runtime::thread_compat::Instant;
use std::collections::{HashMap, HashSet};
use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

use super::gc_ptr::{
    CollectGuard, Color, ErasedGc, drain_candidates, erased_id, gc_drop_edges, gc_finalize,
};
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

/// Per-collect phase counters, for the `MUTSU_GC_LOG` summary/detail lines.
#[derive(Default)]
struct Metrics {
    /// Nodes colored gray (the candidate subgraph size).
    traced: usize,
    /// Nodes `scan_black` restored (survivors kept alive by an external ref).
    revived: usize,
}

fn mark_gray(node: &ErasedGc, m: &mut Metrics) {
    if node.gc_color() == Color::Gray {
        return;
    }
    node.gc_set_color(Color::Gray);
    m.traced += 1;
    for child in children(node) {
        child.gc_strong_dec();
        mark_gray(&child, m);
    }
}

fn scan(node: &ErasedGc, m: &mut Metrics) {
    if node.gc_color() != Color::Gray {
        return;
    }
    if node.gc_strong() > 0 {
        // Kept alive by an external reference — restore the subgraph.
        scan_black(node, m);
    } else {
        node.gc_set_color(Color::White);
        for child in children(node) {
            scan(&child, m);
        }
    }
}

fn scan_black(node: &ErasedGc, m: &mut Metrics) {
    node.gc_set_color(Color::Black);
    m.revived += 1;
    for child in children(node) {
        child.gc_strong_inc();
        if child.gc_color() != Color::Black {
            scan_black(&child, m);
        }
    }
}

/// Gather the white (garbage) nodes reachable from `node` into `out`, recoloring
/// them black so each is visited once. Returns whether it started a fresh white
/// component (used for the rough cycle count).
///
/// A white node that is *currently buffered* is collected too: a mutator can
/// re-buffer a cycle member between the collector's drain and the world
/// actually stopping (its handle drop raced the STW rendezvous), and skipping
/// it would strand the node White with strong 0 — deferred a full collect and
/// flagged by `MUTSU_GC_VERIFY` as an inconsistent survivor. Reclaiming it
/// here is sound: whiteness was proven on the frozen post-STW counts, and the
/// re-buffered entry is only a `Weak` handle that fails to upgrade at the
/// next drain once reclaim frees the node.
fn collect_white(node: &ErasedGc, out: &mut Vec<ErasedGc>, seen: &mut HashSet<usize>) -> bool {
    if node.gc_color() != Color::White {
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
    // Value-level finalizers (Raku DESTROY queueing) run FIRST, while every
    // node's attributes/edges are still intact — `drop_gc_edges` below would
    // hand a DESTROY submethod a cleared object. Deliberately OUTSIDE the
    // CollectGuard: an Instance finalizer snapshots its attribute map, and the
    // `Value` clones in that snapshot take real strong references that keep the
    // DESTROY handler's arguments alive past this reclaim (a fellow cycle
    // member referenced from the snapshot survives — possibly with its own
    // edges cleared, which matches Raku's "destruction order within a cycle is
    // unspecified"). Runs after `verify_reclaimed_are_garbage` (the caller's
    // ordering), since these snapshot clones legitimately raise child strong
    // counts. Safepoint collects run on the interpreter thread, so the queued
    // DESTROYs drain at this thread's next drain point.
    for node in &white {
        gc_finalize(node);
    }
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

/// GC log verbosity (`MUTSU_GC_LOG`, design §9.4): `summary` = start/end lines
/// per non-empty collect; `detail` adds the cycle count; `trace` adds a line per
/// reclaimed node. Unset / `off` = silent. `1`/`on` alias `summary`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum LogMode {
    Off,
    Summary,
    Detail,
    Trace,
}

pub(crate) fn log_mode() -> LogMode {
    static M: OnceLock<LogMode> = OnceLock::new();
    *M.get_or_init(|| match std::env::var("MUTSU_GC_LOG").ok().as_deref() {
        None | Some("0") | Some("off") => LogMode::Off,
        Some("summary") | Some("1") | Some("on") => LogMode::Summary,
        Some("detail") => LogMode::Detail,
        Some("trace") => LogMode::Trace,
        Some(other) => {
            eprintln!("[mutsu gc] warning: unrecognized MUTSU_GC_LOG={other:?}, using summary");
            LogMode::Summary
        }
    })
}

/// `MUTSU_GC_VERIFY=1` (design §9.5): check the collector's invariants around
/// every collect. Opt-in — it costs an extra pristine-strong snapshot walk.
pub(crate) fn verify_enabled() -> bool {
    static V: OnceLock<bool> = OnceLock::new();
    *V.get_or_init(|| match std::env::var("MUTSU_GC_VERIFY").ok().as_deref() {
        Some("1") => true,
        None | Some("0") => false,
        Some(other) => {
            eprintln!("[mutsu gc] warning: unrecognized MUTSU_GC_VERIFY={other:?}, treating as 0");
            false
        }
    })
}

fn next_cycle_id() -> u64 {
    static N: AtomicU64 = AtomicU64::new(0);
    N.fetch_add(1, Ordering::Relaxed)
}

/// Snapshot every node reachable from `roots` with its *pristine* strong count
/// (called before `mark_gray` mutates it), for `MUTSU_GC_VERIFY`. Holding an
/// `ErasedGc` per node keeps it alive so survivors can be re-inspected after the
/// collect; the caller drops the garbage entries before `reclaim` so the white
/// nodes still free. Keyed by node id to dedup the graph.
fn snapshot_reachable(roots: &[ErasedGc]) -> HashMap<usize, (ErasedGc, usize)> {
    let mut snap: HashMap<usize, (ErasedGc, usize)> = HashMap::new();
    let mut stack: Vec<ErasedGc> = roots.to_vec();
    while let Some(node) = stack.pop() {
        let id = erased_id(&node);
        if snap.contains_key(&id) {
            continue;
        }
        let strong = node.gc_strong();
        for child in children(&node) {
            stack.push(child);
        }
        snap.insert(id, (node, strong));
    }
    snap
}

/// Verify the collector's SOUNDNESS invariant (design §9.5): no node with a live
/// external reference may be reclaimed. Every reclaimed (white) node must have
/// GC strong count 0 at reclaim time — trial-deletion having removed all its
/// internal cycle edges left nothing, i.e. it was genuine garbage. A white node
/// with `strong > 0` means a still-referenced node is about to be freed
/// (use-after-free). Runs BEFORE reclaim, while the nodes are still alive.
fn verify_reclaimed_are_garbage(cycle: u64, white: &[ErasedGc]) -> usize {
    let mut bad = 0;
    for node in white {
        let strong = node.gc_strong();
        if strong != 0 {
            bad += 1;
            eprintln!(
                "[mutsu gc] VERIFY FAIL cycle={cycle} id={} reclaimed with strong={strong} \
                 (a still-referenced node is being freed)",
                erased_id(node),
            );
        }
    }
    if bad > 0 {
        eprintln!("[mutsu gc] VERIFY cycle={cycle}: {bad} live node(s) wrongly reclaimed");
    }
    bad
}

/// Verify the post-collect heap sanity of the survivors (design §9.5): each must
/// end black (no node left stuck gray/white = incomplete scan) and must not have
/// GAINED references during the collect (`strong_after <= strong_before` — a
/// survivor only ever loses edges from reclaimed garbage; a larger count means an
/// over-restore / underflow-wrap in `scan_black`). Runs AFTER reclaim.
fn verify_survivors(cycle: u64, survivors: &[(ErasedGc, usize)]) -> usize {
    let mut failures = 0;
    for (node, strong_before) in survivors {
        let strong_after = node.gc_strong();
        let color = node.gc_color();
        if color != Color::Black || strong_after > *strong_before {
            failures += 1;
            eprintln!(
                "[mutsu gc] VERIFY FAIL cycle={cycle} id={} strong_before={strong_before} \
                 strong_after={strong_after} color={color:?} (survivor left inconsistent)",
                erased_id(node),
            );
        }
    }
    if failures > 0 {
        eprintln!("[mutsu gc] VERIFY cycle={cycle}: {failures} survivor invariant violation(s)");
    }
    failures
}

/// Run one synchronous cycle collection over the current candidate buffer,
/// attributing it to `reason` in the `MUTSU_GC_LOG` output (the safepoint kind,
/// `manual`, or `program-end`).
///
/// Safe to call at any time: with no candidates buffered it is a no-op. Returns
/// what it reclaimed and records the same into `MUTSU_VM_STATS`.
pub(crate) fn collect_cycles_at(reason: &str) -> CollectStats {
    let start = Instant::now();
    let drained = drain_candidates();
    if drained.is_empty() {
        return CollectStats::default();
    }
    // Partition off nodes whose GC-visible strong count already hit 0. With
    // the weak candidate buffer, a refcount death normally frees the node
    // inline at its last `Gc::drop` (its buffer entry then fails to upgrade
    // in `drain_candidates`), so this catches only the narrow race: a mutator
    // dropped its last handle between our upgrade and this check, leaving the
    // drained strong handle as the survivor. Such a node is not a cycle
    // suspect — trial deletion needs live handles to trial-decrement — so run
    // its value-level finalizer (idempotent; the mutator's drop path already
    // ran it) and drop it directly. The batch is bounded by what other
    // threads can drop in that window, so an inline release suffices (the
    // strong-buffer era's 16k-node sweeps with an off-thread reclamation
    // helper are gone — dead garbage no longer accumulates in the buffer).
    let (dead, suspects): (Vec<ErasedGc>, Vec<ErasedGc>) =
        drained.into_iter().partition(|n| n.gc_strong() == 0);
    let dead_count = dead.len();
    if log_mode() == LogMode::Trace {
        for node in &dead {
            eprintln!("[mutsu gc] reclaim dead id={}", erased_id(node));
        }
    }
    for node in &dead {
        gc_finalize(node);
    }
    drop(dead);

    // Nothing left to scan: return WITHOUT entering the scan/reclaim tail.
    // Falling through used to run `reclaim(vec![], vec![])`, whose
    // `CollectGuard` raised the process-global `collecting()` flag with NO
    // stop-the-world held (the STW attempt below is skipped for empty
    // suspects) — a window in which live worker threads' `Gc::drop`s were
    // silently skipped (missed decrements / candidate buffering) and the
    // reclaim-window debug asserts (`Gc::make_mut` during collect) fired
    // under gc-stress (t/lock.t test 10).
    if suspects.is_empty() {
        if dead_count > 0 {
            let pause_ns = start.elapsed().as_nanos().min(u128::from(u64::MAX)) as u64;
            record_gc_collection(0, dead_count as u64, 0, pause_ns);
            if log_mode() != LogMode::Off {
                eprintln!(
                    "[mutsu gc] dead-sweep reason={reason} dead={dead_count} (no cycle suspects)"
                );
            }
        }
        return CollectStats {
            roots_scanned: 0,
            reclaimed_nodes: dead_count,
            reclaimed_cycles: 0,
        };
    }

    // Trial deletion is not safe against concurrent mutation: it decrements and
    // restores strong counts, so a worker thread cloning/dropping a `Gc` (or
    // CAS-swapping a pointer) mid-collect corrupts the bookkeeping and can free
    // a still-live node. With worker threads live, stop the world first
    // (cooperative — design doc §6.1): every other mutator must be parked at a
    // safepoint or blocked in a quiescent safe region before the scan starts,
    // and stays stopped until it finishes (`_stw` guard). If quiescence is not
    // reached in time (an unwrapped blocking site), fall back to the previous
    // behavior: re-queue the suspects and defer the scan. Deferred, never
    // unsound.
    let _stw: Option<crate::gc::stw::StwGuard> = if crate::gc::stw::other_mutators_active() {
        let stw = if crate::gc::stw::stw_cooldown_active() {
            None
        } else {
            crate::gc::stw::try_stop_the_world(std::time::Duration::from_millis(50))
        };
        if stw.is_none() {
            // The requeued suspects went unscanned and stay in the buffer;
            // count them as survivors for the ADR-0003 adaptive threshold so
            // the size trigger backs off instead of re-arming a drain/requeue
            // round-trip on every push over the (still-exceeded) threshold.
            crate::gc::safepoint::note_collect_survivors(suspects.len());
            crate::gc::gc_ptr::requeue_candidates(suspects);
            if dead_count > 0 {
                let pause_ns = start.elapsed().as_nanos().min(u128::from(u64::MAX)) as u64;
                record_gc_collection(0, dead_count as u64, 0, pause_ns);
                if log_mode() != LogMode::Off {
                    eprintln!(
                        "[mutsu gc] dead-sweep reason={reason} dead={dead_count} (cycle scan deferred: workers active)"
                    );
                }
            }
            return CollectStats {
                roots_scanned: 0,
                reclaimed_nodes: dead_count,
                reclaimed_cycles: 0,
            };
        }
        stw
    } else {
        None
    };
    let roots = suspects;
    let roots_scanned = roots.len();
    let mode = log_mode();
    let verify = verify_enabled();
    let cycle = if mode != LogMode::Off || verify {
        next_cycle_id()
    } else {
        0
    };

    if mode != LogMode::Off {
        eprintln!(
            "[mutsu gc] start cycle={cycle} reason={reason} candidates={} dead={dead_count} suspects={roots_scanned}",
            roots_scanned + dead_count
        );
    }

    // Pristine snapshot for verify, before trial-deletion mutates strong counts.
    let snapshot = verify.then(|| snapshot_reachable(&roots));

    let mut m = Metrics::default();
    for root in &roots {
        mark_gray(root, &mut m);
    }
    for root in &roots {
        scan(root, &mut m);
    }
    let mut white = Vec::new();
    let mut seen = HashSet::new();
    let mut cycles = 0;
    for root in &roots {
        if collect_white(root, &mut white, &mut seen) {
            cycles += 1;
        }
    }
    // Reclaimed = cycle garbage plus the already-dead candidates dropped above
    // (both are memory the GC pass released; only the former counts as cycles).
    let reclaimed_nodes = white.len() + dead_count;

    // Keep only survivor snapshot handles across reclaim; dropping the garbage
    // handles here lets `reclaim` actually free the white nodes.
    let survivors = snapshot.map(|snap| {
        let white_ids: HashSet<usize> = white.iter().map(erased_id).collect();
        snap.into_iter()
            .filter(|(id, _)| !white_ids.contains(id))
            .map(|(_, entry)| entry)
            .collect::<Vec<_>>()
    });

    if mode == LogMode::Trace {
        for node in &white {
            eprintln!("[mutsu gc] reclaim cycle={cycle} id={}", erased_id(node));
        }
    }

    // Soundness check must run BEFORE reclaim frees the white nodes.
    if verify {
        verify_reclaimed_are_garbage(cycle, &white);
    }

    reclaim(white, roots);

    if let Some(survivors) = &survivors {
        verify_survivors(cycle, survivors);
    }

    let pause_ns = start.elapsed().as_nanos().min(u128::from(u64::MAX)) as u64;
    record_gc_collection(
        roots_scanned as u64,
        reclaimed_nodes as u64,
        cycles as u64,
        pause_ns,
    );
    // ADR-0003 adaptive threshold: the revived (live) portion of the scanned
    // subgraph is what the next scan would re-trace for nothing — back the
    // size trigger off proportionally (clamped to BASE when the scan was
    // productive).
    crate::gc::safepoint::note_collect_survivors(m.revived);

    if mode != LogMode::Off {
        let cycles_field = if mode >= LogMode::Detail {
            format!(" cycles={cycles}")
        } else {
            String::new()
        };
        eprintln!(
            "[mutsu gc] end cycle={cycle} traced={} revived={} reclaimed={reclaimed_nodes}{cycles_field} roots={roots_scanned} pause_ns={pause_ns}",
            m.traced, m.revived,
        );
    }

    CollectStats {
        roots_scanned,
        reclaimed_nodes,
        reclaimed_cycles: cycles,
    }
}

/// Run one collection attributed to `manual` (unit tests' entry point).
#[cfg(test)]
pub(crate) fn collect_cycles() -> CollectStats {
    collect_cycles_at("manual")
}

/// The program-end collect. Its only *observable* effect is delivering Raku
/// `DESTROY` for cycle members that survived to exit — process teardown frees
/// the memory either way. So when no user class/role defines a `DESTROY`
/// submethod (`has_destroy_methods`, the overwhelmingly common case) and no
/// observability surface is on (`MUTSU_GC_LOG` / `MUTSU_GC_VERIFY` /
/// `MUTSU_VM_STATS`, whose outputs tests and operators read), skip it: the
/// exit scan trial-deletes every live buffered suspect — re-enumerating big
/// captured envs — which measured 160-220ms on bench-class for zero
/// observable effect.
pub(crate) fn collect_at_program_end(has_destroy_methods: bool) {
    if !super::gc_ptr::gc_enabled() {
        return;
    }
    if !has_destroy_methods
        && log_mode() == LogMode::Off
        && !verify_enabled()
        && !crate::vm::vm_stats::enabled()
    {
        return;
    }
    collect_cycles_at("program-end");
}

#[cfg(test)]
mod tests {
    use super::super::gc_ptr::{Gc, Trace};
    use super::*;
    use std::sync::Mutex;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// Counts `TestNode` frees, so a test can assert a cycle was reclaimed.
    static DROPS: AtomicUsize = AtomicUsize::new(0);

    /// Serializes tests: they share the process-global candidate buffer, the
    /// `CollectGuard` flag, the STW/worker statics, and the `DROPS` counter.
    fn lock() -> std::sync::MutexGuard<'static, ()> {
        crate::gc::test_support::serial_lock()
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
        // Under `MUTSU_GC=on` (the CI gc-stress job) OTHER unit tests running
        // in parallel push real candidates into the shared buffer at any time,
        // so exact-count assertions on `stats` are impossible here; the
        // property is just that an (almost-)empty collect is a sound no-op.
        let _ = collect_cycles();
        let _ = collect_cycles();
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
        // `>=`: parallel tests may add stray candidates under `MUTSU_GC=on`
        // (see `empty_buffer_collects_nothing`); OUR nodes are asserted
        // precisely through the `DROPS` counter.
        assert!(stats.reclaimed_nodes >= 2, "both cycle nodes reclaimed");
        assert!(stats.reclaimed_cycles >= 1);
        assert_eq!(
            DROPS.load(Ordering::Relaxed) - before,
            2,
            "both TestNodes were actually freed"
        );
    }

    #[test]
    fn collect_is_deferred_while_a_mutator_worker_is_active() {
        use super::super::gc_ptr::{enter_mutator_worker, exit_mutator_worker};
        let _g = lock();
        drain_candidates();
        let before = DROPS.load(Ordering::Relaxed);

        // A reclaimable cycle sits in the candidate buffer.
        let a = TestNode::new();
        let b = TestNode::new();
        TestNode::link(&a, &b);
        TestNode::link(&b, &a);
        a.buffer_as_candidate();
        b.buffer_as_candidate();
        drop(a);
        drop(b);

        // While a (non-cooperating) worker thread may be mutating the graph,
        // the cycle scan must decline: the stop-the-world attempt times out
        // (this phantom worker never parks), the suspects are re-queued, and
        // crucially OUR cycle is neither freed nor drained-and-lost. (Stray
        // dead candidates from parallel tests may still be swept — the dead
        // sweep is concurrency-safe by design — so no assertion on `stats`.)
        enter_mutator_worker();
        let _ = collect_cycles();
        assert_eq!(
            DROPS.load(Ordering::Relaxed) - before,
            0,
            "nothing of ours freed while a worker is active"
        );

        // Once the worker is gone, the still-buffered cycle is reclaimed.
        exit_mutator_worker();
        super::super::stw::test_reset(); // clear the timeout's retry cooldown
        let stats = collect_cycles();
        assert!(
            stats.reclaimed_nodes >= 2,
            "candidates survived to be collected"
        );
        assert_eq!(DROPS.load(Ordering::Relaxed) - before, 2);
    }

    #[test]
    fn cycle_scan_runs_under_a_cooperatively_parking_worker() {
        use super::super::gc_ptr::{enter_mutator_worker, exit_mutator_worker};
        use std::sync::atomic::AtomicBool;
        let _g = lock();
        drain_candidates();
        let before = DROPS.load(Ordering::Relaxed);

        // A REAL worker thread that cooperates: it loops through the safepoint
        // park. The stop-the-world must land while it is alive, and the cycle
        // scan must reclaim OUR garbage cycle mid-run — the whole point of
        // gc::stw (a server-style worker never joins, its cycles must still be
        // collectable).
        enter_mutator_worker();
        let stop = std::sync::Arc::new(AtomicBool::new(false));
        let stop2 = stop.clone();
        let worker = std::thread::spawn(move || {
            super::super::stw::mark_thread_registered(true);
            while !stop2.load(Ordering::Relaxed) {
                super::super::stw::park_at_safepoint();
                std::thread::yield_now();
            }
            super::super::stw::mark_thread_registered(false);
        });

        let a = TestNode::new();
        let b = TestNode::new();
        TestNode::link(&a, &b);
        TestNode::link(&b, &a);
        a.buffer_as_candidate();
        b.buffer_as_candidate();
        drop(a);
        drop(b);

        let stats = collect_cycles();
        assert_eq!(
            DROPS.load(Ordering::Relaxed) - before,
            2,
            "the cycle is reclaimed WHILE the worker is still running"
        );
        assert!(stats.reclaimed_nodes >= 2);

        stop.store(true, Ordering::Relaxed);
        worker.join().unwrap();
        exit_mutator_worker();
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
        assert!(stats.reclaimed_nodes >= 1);
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

        let _ = collect_cycles();
        assert_eq!(
            DROPS.load(Ordering::Relaxed) - before,
            0,
            "live external ref => not garbage"
        );
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

        let _ = collect_cycles();
        assert_eq!(DROPS.load(Ordering::Relaxed) - before, 0);
        drop(a);
        drop(b);
    }
}
