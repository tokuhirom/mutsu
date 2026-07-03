//! GC Level 1a: `Gc<T>` smart pointer, node header, candidate buffer, and a
//! synchronous Bacon–Rajan cycle collector (ADR-0001 / ADR-0002,
//! `docs/gc-level1-detailed-design.md` §5 / §7 / §11 **step 4**).
//!
//! This is the *minimal machinery* only. Per the design's implementation order,
//! step 4 builds `Gc<T>` + the collector but wires **no** `Value` variant to it
//! yet — that is step 5 (`Array`/`Hash`/`ContainerRef` first wave). Nothing in
//! the interpreter allocates a `Gc<T>` at this point, so the collector is dead
//! code in production and is exercised only by this module's unit tests. It is
//! also **default-off** at runtime (design §9.1): `collect_cycles` runs only
//! when explicitly called, and the future safepoint caller will gate it behind
//! `MUTSU_GC` (added when step 8 wires collection in).
//!
//! ## Algorithm
//!
//! The classic synchronous Bacon–Rajan trial-deletion cycle collector (Bacon &
//! Rajan 2001, "Concurrent Cycle Collection in Reference Counted Systems").
//! Each node carries an external strong count plus a color and a "buffered"
//! flag. A strong-count decrement that does not reach zero records the node as
//! a *possible cycle root* (purple) in a candidate buffer. `collect_cycles`
//! then runs mark-gray / scan / collect-white over the buffered roots:
//!
//! - **mark_gray**: subtract internal edges — for every buffered root, walk its
//!   subgraph decrementing each child's count once per internal edge, coloring
//!   gray.
//! - **scan**: any gray node whose count is still > 0 has an *external* ref, so
//!   it (and everything reachable from it) is live — repaint black and restore
//!   the counts. Gray nodes that reach zero are garbage — paint white.
//! - **collect_white**: the white nodes form garbage cycles; free them.
//!
//! ## Memory safety
//!
//! `Gc<T>` owns a manually-allocated `GcBox<T>` (like `Arc`'s inner box but with
//! the extra GC header). Freeing a white cycle would re-enter `Gc::drop` for the
//! `Gc` fields inside each freed value and double-free. A thread-local
//! `GC_FREEING` guard makes `Gc::drop` inert during the free pass, so the
//! collector can `drop_in_place` every white value and then deallocate every
//! box without reentrant decrements (the same technique the `gc` crate uses).
//!
//! Cross-thread: the header uses atomics so `Gc<T>: Send + Sync` when `T` is,
//! matching the `Arc`-based model. `collect_cycles` is only ever run at a
//! safepoint with no other thread mutating the shared graph (design §6); the
//! candidate buffer is currently per-thread (thread-local), which is sufficient
//! for the single-threaded collect the design specifies for Level 1a.

// GC Level 1a step 4 is infrastructure-only: nothing wires a `Value` variant to
// `Gc<T>` until step 5, so every item here is unused in production and exercised
// only by this module's unit tests. Suppress dead-code warnings module-wide
// (CI builds with `-D warnings`) until the first-wave migration lands.
#![allow(dead_code)]

use std::cell::Cell;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU8, AtomicUsize, Ordering};

use crate::vm::vm_stats;

/// Bacon–Rajan node colors. `Black` = in use / live; `Gray` = provisional cycle
/// member (counts have had internal edges subtracted); `White` = confirmed
/// garbage-cycle member; `Purple` = possible cycle root, buffered as a
/// candidate. (`Bacon-Rajan`'s `Green`/`Red`/`Orange` are async-only and unused
/// in the synchronous collector.)
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
enum Color {
    Black = 0,
    Gray = 1,
    White = 2,
    Purple = 3,
}

impl Color {
    fn from_u8(v: u8) -> Color {
        match v {
            1 => Color::Gray,
            2 => Color::White,
            3 => Color::Purple,
            _ => Color::Black,
        }
    }
}

/// Per-node GC header stored inline ahead of the value in a [`GcBox`].
///
/// `strong` is the external reference count (mirrors `Arc`'s strong count). It
/// is atomic so clone/drop are thread-safe. `color`/`buffered` are only mutated
/// during a stop-the-world collect (or the buffering decrement), so plain
/// atomics with relaxed ordering suffice; they exist mainly to keep the header
/// `Sync`.
pub(crate) struct GcHeader {
    strong: AtomicUsize,
    color: AtomicU8,
    buffered: AtomicU8,
}

impl GcHeader {
    fn new() -> GcHeader {
        GcHeader {
            strong: AtomicUsize::new(1),
            color: AtomicU8::new(Color::Black as u8),
            buffered: AtomicU8::new(0),
        }
    }

    fn strong(&self) -> usize {
        self.strong.load(Ordering::Relaxed)
    }
    fn inc_strong(&self) {
        self.strong.fetch_add(1, Ordering::Relaxed);
    }
    /// Decrement and return the new value.
    fn dec_strong(&self) -> usize {
        self.strong.fetch_sub(1, Ordering::Relaxed) - 1
    }
    fn color(&self) -> Color {
        Color::from_u8(self.color.load(Ordering::Relaxed))
    }
    fn set_color(&self, c: Color) {
        self.color.store(c as u8, Ordering::Relaxed);
    }
    fn buffered(&self) -> bool {
        self.buffered.load(Ordering::Relaxed) != 0
    }
    fn set_buffered(&self, b: bool) {
        self.buffered.store(b as u8, Ordering::Relaxed);
    }
}

/// Enumerate the GC-managed children of a value. A `Value`-level implementation
/// (step 5) mirrors [`crate::value::Value::visit_gc_children`], but at the node
/// layer children are reported as type-erased [`GcNode`] pointers so the
/// collector can recurse without knowing each node's concrete type.
pub(crate) trait Trace {
    /// Call `f` once per directly-owned `Gc` child.
    fn trace(&self, f: &mut dyn FnMut(NonNull<dyn GcNode>));
}

/// Type-erased operations the collector needs on a node, independent of its
/// concrete `T`. Implemented by every [`GcBox<T>`].
pub(crate) trait GcNode {
    fn header(&self) -> &GcHeader;
    /// Trace the value's `Gc` children (no-op-safe: only called while the value
    /// is still alive, never during the free pass).
    fn trace_children(&self, f: &mut dyn FnMut(NonNull<dyn GcNode>));
}

/// Heap node: GC header followed by the value. Layout is deliberately a plain
/// struct (not `repr(C)`) — we only ever reach the value through the typed
/// `GcBox<T>` or the erased `dyn GcNode`, never by pointer arithmetic.
struct GcBox<T: Trace + ?Sized> {
    header: GcHeader,
    value: T,
}

impl<T: Trace> GcNode for GcBox<T> {
    fn header(&self) -> &GcHeader {
        &self.header
    }
    fn trace_children(&self, f: &mut dyn FnMut(NonNull<dyn GcNode>)) {
        self.value.trace(f);
    }
}

thread_local! {
    /// Set for the duration of the collector's free pass so that `Gc::drop`
    /// running from inside a freed value's destructor does not recurse into
    /// decrement/free (which would double-free the cycle).
    static GC_FREEING: Cell<bool> = const { Cell::new(false) };

    /// Per-thread candidate buffer of possible cycle roots (purple nodes).
    /// Level 1a keeps this thread-local: collection runs single-threaded at a
    /// safepoint (design §6 / §1.1), so a process-global buffer is unnecessary.
    static ROOTS: Cell<Vec<NonNull<dyn GcNode>>> = const { Cell::new(Vec::new()) };
}

fn with_roots<R>(f: impl FnOnce(&mut Vec<NonNull<dyn GcNode>>) -> R) -> R {
    ROOTS.with(|cell| {
        let mut roots = cell.take();
        let r = f(&mut roots);
        cell.set(roots);
        r
    })
}

/// A garbage-collected pointer with cycle-collection support (ADR-0001 level 1).
///
/// Behaves like `Arc<T>` for clone/deref/drop, but additionally participates in
/// the Bacon–Rajan cycle collector: a drop that leaves a nonzero strong count
/// buffers the node as a possible cycle root.
pub(crate) struct Gc<T: Trace + 'static> {
    ptr: NonNull<GcBox<T>>,
    _marker: PhantomData<T>,
}

// Safe for the same reasons `Arc<T>` is: the strong count is atomic, and the
// collector only mutates color/buffered/counts at a safepoint (design §6).
unsafe impl<T: Trace + Send + Sync> Send for Gc<T> {}
unsafe impl<T: Trace + Send + Sync> Sync for Gc<T> {}

impl<T: Trace + 'static> Gc<T> {
    /// Allocate a new GC node holding `value` with strong count 1.
    pub(crate) fn new(value: T) -> Gc<T> {
        let boxed = Box::new(GcBox {
            header: GcHeader::new(),
            value,
        });
        let ptr = NonNull::new(Box::into_raw(boxed)).expect("Box ptr is non-null");
        Gc {
            ptr,
            _marker: PhantomData,
        }
    }

    fn inner(&self) -> &GcBox<T> {
        // Safe: the box stays allocated while any `Gc` (this one) holds it.
        unsafe { self.ptr.as_ref() }
    }

    fn header(&self) -> &GcHeader {
        &self.inner().header
    }

    /// Type-erased node pointer for the candidate buffer / collector.
    fn as_node_ptr(&self) -> NonNull<dyn GcNode> {
        self.ptr as NonNull<GcBox<T>>
    }

    /// Current external strong count (test/introspection helper).
    #[cfg(test)]
    pub(crate) fn strong_count(&self) -> usize {
        self.header().strong()
    }
}

impl<T: Trace + 'static> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        self.header().inc_strong();
        // A freshly-referenced node is definitely in use.
        self.header().set_color(Color::Black);
        Gc {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T: Trace + 'static> std::ops::Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.inner().value
    }
}

impl<T: Trace + 'static> Drop for Gc<T> {
    fn drop(&mut self) {
        // During the collector's free pass, every `Gc` field inside a value
        // being torn down must be inert — the collector already accounts for
        // those edges and will free the boxes itself.
        if GC_FREEING.with(Cell::get) {
            return;
        }
        let node = self.as_node_ptr();
        let header = self.header();
        if header.dec_strong() == 0 {
            // No external refs left: release immediately (no cycle possible
            // through a zero-count node).
            release(node);
        } else {
            // Might be part of a cycle whose other members still reference it.
            possible_root(node);
        }
    }
}

/// `Release`: the node has hit strong count 0. Decrement its children and, if it
/// is not currently buffered as a candidate, free it. If it *is* buffered, leave
/// the box for the collector to reclaim (it will find color=black, count=0).
fn release(node: NonNull<dyn GcNode>) {
    // Decrement children first (they may themselves reach zero and release).
    // Gather child pointers before recursing so no trace lock is held across
    // the recursion (see `children_of`).
    for child in children_of(node) {
        let child_ref = unsafe { child.as_ref() };
        if child_ref.header().dec_strong() == 0 {
            release(child);
        } else {
            possible_root(child);
        }
    }
    let header = unsafe { node.as_ref() }.header();
    header.set_color(Color::Black);
    if !header.buffered() {
        unsafe { free_node(node) };
    }
}

/// `PossibleRoot`: buffer a node whose decrement left a nonzero count as a
/// candidate cycle root.
fn possible_root(node: NonNull<dyn GcNode>) {
    let header = unsafe { node.as_ref() }.header();
    if header.color() != Color::Purple {
        header.set_color(Color::Purple);
        if !header.buffered() {
            header.set_buffered(true);
            with_roots(|roots| roots.push(node));
            vm_stats::record_gc_candidate_push();
        }
    }
}

/// Drop the box's contents and deallocate it, running the value destructor with
/// the `GC_FREEING` guard set so reentrant `Gc::drop` is inert.
///
/// Works entirely through the raw box pointer: `drop_in_place` on the erased
/// `*mut dyn GcNode` runs `GcBox<T>`'s drop glue (dropping the header's atomics
/// and the value `T`) via the vtable, without ever forming a `&`/`&mut`
/// reference to write through — so it is Stacked-Borrows clean (a `*mut`-derived
/// drop, not a mutation through a shared `&self`).
///
/// # Safety
/// `node` must be a live box that is being reclaimed (strong count 0 and not
/// otherwise reachable, or a white cycle member during the collect free pass),
/// and must not be freed twice.
unsafe fn free_node(node: NonNull<dyn GcNode>) {
    let raw = node.as_ptr();
    // Read the layout from the fat pointer's vtable BEFORE dropping the value
    // (the `&` is confined to this statement and gone before the raw drop).
    let layout = std::alloc::Layout::for_value(unsafe { &*raw });
    GC_FREEING.with(|f| f.set(true));
    unsafe { std::ptr::drop_in_place(raw) };
    GC_FREEING.with(|f| f.set(false));
    unsafe { std::alloc::dealloc(raw as *mut u8, layout) };
}

/// Snapshot a node's GC-managed children as an owned `Vec`.
///
/// The collector must NOT recurse while a node's child-collection lock is held:
/// `trace` may lock a `Mutex`/`RwLock` (e.g. `Value::ContainerRef`, or a node
/// holding `Mutex<Vec<Gc<_>>>`) for the duration of the callback, and the
/// traversal can revisit a node whose lock is already held further up the stack,
/// deadlocking a non-reentrant lock. Gathering child pointers first (lock held
/// only for the copy) and recursing afterwards avoids that entirely.
fn children_of(node: NonNull<dyn GcNode>) -> Vec<NonNull<dyn GcNode>> {
    let mut children = Vec::new();
    unsafe { node.as_ref() }.trace_children(&mut |child| children.push(child));
    children
}

/// Run one synchronous cycle collection over the buffered candidate roots.
///
/// Returns `(reclaimed_nodes, reclaimed_cycles)`. Default-off in production:
/// only called explicitly (tests now; a `MUTSU_GC`-gated safepoint later).
pub(crate) fn collect_cycles() -> (usize, usize) {
    let start = std::time::Instant::now();
    let roots = with_roots(std::mem::take);
    let roots_scanned = roots.len() as u64;

    let roots = mark_roots(roots);
    for &node in &roots {
        scan(node);
    }
    let (reclaimed_nodes, reclaimed_cycles) = collect_roots(roots);

    let pause_ns = start.elapsed().as_nanos() as u64;
    vm_stats::record_gc_collection(
        roots_scanned,
        reclaimed_nodes as u64,
        reclaimed_cycles as u64,
        pause_ns,
    );
    (reclaimed_nodes, reclaimed_cycles)
}

/// `MarkRoots`: for each buffered root, mark_gray purple ones; drop non-purple
/// ones from the buffer (and free any that already reached count 0).
fn mark_roots(roots: Vec<NonNull<dyn GcNode>>) -> Vec<NonNull<dyn GcNode>> {
    let mut kept = Vec::with_capacity(roots.len());
    for node in roots {
        let header = unsafe { node.as_ref() }.header();
        if header.color() == Color::Purple {
            mark_gray(node);
            kept.push(node);
        } else {
            header.set_buffered(false);
            if header.color() == Color::Black && header.strong() == 0 {
                unsafe { free_node(node) };
            }
        }
    }
    kept
}

/// `MarkGray`: color the subgraph gray, subtracting one from each child's count
/// per internal edge.
fn mark_gray(node: NonNull<dyn GcNode>) {
    if unsafe { node.as_ref() }.header().color() != Color::Gray {
        unsafe { node.as_ref() }.header().set_color(Color::Gray);
        for child in children_of(node) {
            unsafe { child.as_ref() }.header().dec_strong();
            mark_gray(child);
        }
    }
}

/// `Scan`: a gray node with a surviving count has an external reference — it and
/// its subgraph are live (scan_black restores counts). Otherwise paint white and
/// recurse.
fn scan(node: NonNull<dyn GcNode>) {
    let header = unsafe { node.as_ref() }.header();
    if header.color() != Color::Gray {
        return;
    }
    if header.strong() > 0 {
        scan_black(node);
    } else {
        header.set_color(Color::White);
        for child in children_of(node) {
            scan(child);
        }
    }
}

/// `ScanBlack`: repaint a live subgraph black, restoring the counts mark_gray
/// subtracted.
fn scan_black(node: NonNull<dyn GcNode>) {
    unsafe { node.as_ref() }.header().set_color(Color::Black);
    for child in children_of(node) {
        let child_ref = unsafe { child.as_ref() };
        child_ref.header().inc_strong();
        if child_ref.header().color() != Color::Black {
            scan_black(child);
        }
    }
}

/// `CollectRoots` + `CollectWhite`: gather white cycle members and free them.
fn collect_roots(roots: Vec<NonNull<dyn GcNode>>) -> (usize, usize) {
    let mut white: Vec<NonNull<dyn GcNode>> = Vec::new();
    let mut cycles = 0usize;
    for node in roots {
        let header = unsafe { node.as_ref() }.header();
        header.set_buffered(false);
        if header.color() == Color::White {
            let before = white.len();
            collect_white(node, &mut white);
            if white.len() > before {
                cycles += 1;
            }
        }
    }
    let reclaimed = white.len();
    // Free pass: drop + deallocate each white box. `free_node` runs the value
    // dtor under the `GC_FREEING` guard, so a freed value's `Gc` fields (which
    // point at sibling white boxes) drop inertly instead of decrementing/freeing
    // them again — no double free even though we free the whole cycle.
    for node in white {
        unsafe { free_node(node) };
    }
    (reclaimed, cycles)
}

/// `CollectWhite`: collect a white node and its white children into `out`,
/// repainting black so each is collected exactly once.
fn collect_white(node: NonNull<dyn GcNode>, out: &mut Vec<NonNull<dyn GcNode>>) {
    let header = unsafe { node.as_ref() }.header();
    if header.color() == Color::White && !header.buffered() {
        header.set_color(Color::Black);
        for child in children_of(node) {
            collect_white(child, out);
        }
        out.push(node);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::sync::Mutex;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// Test node holding a shared drop counter so each test is independent —
    /// cargo runs tests in parallel threads, and the collector's candidate
    /// buffer (`ROOTS`) is thread-local, so per-test state must not touch any
    /// global. Each test creates its own `Arc<AtomicUsize>` counter.
    struct Node {
        children: Mutex<Vec<Gc<Node>>>,
        drops: Arc<AtomicUsize>,
    }

    impl Node {
        fn new(drops: &Arc<AtomicUsize>) -> Gc<Node> {
            Gc::new(Node {
                children: Mutex::new(Vec::new()),
                drops: drops.clone(),
            })
        }
        fn link(parent: &Gc<Node>, child: &Gc<Node>) {
            parent.children.lock().unwrap().push(child.clone());
        }
    }

    impl Trace for Node {
        fn trace(&self, f: &mut dyn FnMut(NonNull<dyn GcNode>)) {
            if let Ok(children) = self.children.lock() {
                for c in children.iter() {
                    f(c.as_node_ptr());
                }
            }
        }
    }

    impl Drop for Node {
        fn drop(&mut self) {
            self.drops.fetch_add(1, Ordering::Relaxed);
        }
    }

    fn counter() -> Arc<AtomicUsize> {
        Arc::new(AtomicUsize::new(0))
    }
    fn drops(c: &Arc<AtomicUsize>) -> usize {
        c.load(Ordering::Relaxed)
    }

    #[test]
    fn acyclic_graph_is_freed_no_leak() {
        let d = counter();
        {
            let a = Node::new(&d);
            let b = Node::new(&d);
            Node::link(&a, &b); // a -> b, no cycle
        } // `a` releases immediately; `b` was buffered (nonzero decrement) so it
        // is reclaimed by the next collect — Bacon-Rajan buffers any nonzero
        // decrement, cyclic or not.
        assert!(
            drops(&d) >= 1,
            "at least the root of the chain frees on drop"
        );
        collect_cycles();
        assert_eq!(
            drops(&d),
            2,
            "the whole acyclic graph is freed, nothing leaks"
        );
    }

    #[test]
    fn self_cycle_is_reclaimed_by_collect() {
        let d = counter();
        {
            let a = Node::new(&d);
            Node::link(&a, &a); // a -> a, self cycle, strong now 2
        } // drop leaves strong count 1 (the self-edge) -> buffered as root
        assert_eq!(drops(&d), 0, "self-cycle leaks without a collect");
        let (nodes, cycles) = collect_cycles();
        assert_eq!(nodes, 1, "one node reclaimed");
        assert_eq!(cycles, 1, "one cycle reclaimed");
        assert_eq!(drops(&d), 1, "self-cycle node dropped by collector");
    }

    #[test]
    fn two_node_cycle_is_reclaimed() {
        let d = counter();
        {
            let a = Node::new(&d);
            let b = Node::new(&d);
            Node::link(&a, &b);
            Node::link(&b, &a); // a <-> b cycle
        }
        assert_eq!(drops(&d), 0, "cycle leaks until collect");
        let (nodes, _cycles) = collect_cycles();
        assert_eq!(nodes, 2, "both cycle nodes reclaimed");
        assert_eq!(drops(&d), 2);
    }

    #[test]
    fn live_cycle_with_external_ref_is_kept() {
        let d = counter();
        let keep = Node::new(&d);
        {
            let a = Node::new(&d);
            let b = Node::new(&d);
            Node::link(&a, &b);
            Node::link(&b, &a);
            Node::link(&keep, &a); // external root keeps the cycle alive
        }
        let (nodes, _cycles) = collect_cycles();
        assert_eq!(
            nodes, 0,
            "externally-referenced cycle must NOT be reclaimed"
        );
        assert_eq!(drops(&d), 0);
        drop(keep); // now nothing external -> a,b,keep all releasable/cyclic
        let (nodes2, _) = collect_cycles();
        // keep released acyclically on drop; a<->b now unreferenced cycle.
        assert!(nodes2 >= 2, "cycle collectable once external ref is gone");
    }

    #[test]
    fn longer_cycle_chain_is_reclaimed() {
        let d = counter();
        {
            let nodes: Vec<Gc<Node>> = (0..5).map(|_| Node::new(&d)).collect();
            for i in 0..5 {
                Node::link(&nodes[i], &nodes[(i + 1) % 5]); // ring of 5
            }
        }
        let (reclaimed, cycles) = collect_cycles();
        assert_eq!(reclaimed, 5, "5-node ring reclaimed");
        assert_eq!(cycles, 1);
        assert_eq!(drops(&d), 5);
    }
}
