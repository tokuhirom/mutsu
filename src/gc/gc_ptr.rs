//! GC Level 1a managed-pointer primitive: [`Gc<T>`], its Bacon-Rajan node
//! header, the [`Trace`] trait, and the process-global cycle-candidate buffer
//! (ADR-0001 / ADR-0002, `docs/gc-level1-detailed-design.md` §5.1 / §9.1 /
//! §11 step 4).
//!
//! This lands the *type machinery* a Bacon-Rajan synchronous cycle collector
//! needs, "compiled in, default off" per the design doc §9.1:
//!
//! - [`Gc<T>`] — an `Arc`-backed managed pointer. `Arc` supplies allocation,
//!   `Send + Sync`, and the unsizing coercion to the type-erased [`ErasedGc`];
//!   the added [`GcHeader`] carries the Bacon-Rajan node state (a GC-visible
//!   strong count distinct from the `Arc`'s own count, a color, and a
//!   `buffered` flag).
//! - [`Trace`] — a node's ability to hand its direct `Gc` children to the
//!   collector. Object-safe so `GcBox<dyn Trace>` can be buffered type-erased.
//! - the process-global candidate buffer — where a drop that leaves the node
//!   with surviving handles records it as a possible cycle root (§4.2 / §5.2).
//!
//! State of the migration (§11 step 5):
//! - `Value::Hash` is the first variant migrated to `Gc<HashData>` (step 5b), so
//!   `Gc`'s `Clone`/`Drop`/`make_mut`/... run in production now. Candidate
//!   buffering still only happens under `MUTSU_GC=on` (default off), so ordinary
//!   runs pay just an atomic refcount op per hash clone/drop and push nothing.
//! - `Array`/`ContainerRef` (steps 5c/5d) remain `Arc`; the [`ContainerMakeMut`]
//!   bridge lets shared container macros work across the mixed state meanwhile.
//! - No trial-deletion reclaim runs. [`drain_candidates`] hands the buffered
//!   nodes to a future synchronous collector (§11 step 8); until then the
//!   `Color`/strong-count bookkeeping is maintained but never acted on.

// The collector-facing surface (Color scan states, drain_candidates,
// buffer_as_candidate, trace_children, ...) is not exercised until the
// synchronous collector lands (§11 step 8), so keep a module-wide dead-code
// allow rather than sprinkling per-item ones; drop it when step 8 wires them up.
#![allow(dead_code)]

use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use crate::vm::vm_stats::{record_gc_candidate_dedup_hit, record_gc_candidate_push};

/// Bacon-Rajan node color (design doc §5.1's "color / state").
///
/// Only `Black`/`Purple` are set outside a collection: a live node is `Black`,
/// and a node flagged as a possible cycle root when a `Gc` handle to it is
/// dropped is `Purple`. `Gray`/`White` are the transient trial-deletion states
/// the future collector (§11 step 8) will use while scanning a candidate
/// subgraph; they are defined now so the header layout is stable.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub(crate) enum Color {
    /// In use / live.
    Black = 0,
    /// Possible member of a cycle — buffered as a collection candidate.
    Purple = 1,
    /// Being scanned by trial-deletion.
    Gray = 2,
    /// Provisionally garbage during trial-deletion.
    White = 3,
}

impl Color {
    fn from_u8(v: u8) -> Color {
        match v {
            1 => Color::Purple,
            2 => Color::Gray,
            3 => Color::White,
            _ => Color::Black,
        }
    }
}

/// A GC-managed node's ability to expose its direct `Gc` children.
///
/// Object-safe (`&mut dyn FnMut` visitor, no generic method) so that
/// `GcBox<dyn Trace>` is a valid type — the candidate buffer stores nodes
/// type-erased as [`ErasedGc`]. A leaf node with no `Gc` children implements
/// this as an empty body. `Send + Sync` supertraits keep [`ErasedGc`]
/// (`Arc<GcBox<dyn Trace>>`) thread-shareable, matching the `Arc`-based
/// cross-thread `Value` model (ADR-0001 §3-4).
pub(crate) trait Trace: Send + Sync {
    /// Invoke `visit` once per direct `Gc` child of this node.
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc));
}

/// Bacon-Rajan node header stored alongside the value inside a [`GcBox`].
struct GcHeader {
    /// GC-visible strong count: the number of live `Gc<T>` handles. This is
    /// *distinct* from the backing `Arc`'s own reference count (which also
    /// counts the candidate buffer's retained handle). Trial-deletion mutates
    /// this count while scanning and restores it afterward, without touching
    /// the `Arc` — so scanning never frees live memory.
    strong: AtomicUsize,
    /// Current [`Color`], stored as its `u8` discriminant.
    color: AtomicU8,
    /// Whether this node is currently sitting in the candidate buffer. Prevents
    /// the same node being buffered twice (design doc §5.2 allows duplicates,
    /// but deduping keeps the buffer bounded and the dedup counter meaningful).
    buffered: AtomicBool,
}

impl GcHeader {
    /// Header for a brand-new single-handle live node (strong 1, `Black`,
    /// unbuffered). Shared by `Gc::new` and the copy-on-write clone in
    /// `Gc::make_mut`.
    fn fresh() -> GcHeader {
        GcHeader {
            strong: AtomicUsize::new(1),
            color: AtomicU8::new(Color::Black as u8),
            buffered: AtomicBool::new(false),
        }
    }
}

/// Heap allocation backing a [`Gc<T>`]: the node header plus the value. `T` is
/// `?Sized` so `GcBox<dyn Trace>` (the erased form) is nameable.
pub(crate) struct GcBox<T: ?Sized> {
    header: GcHeader,
    value: T,
}

/// Type-erased managed node, as stored in the candidate buffer.
pub(crate) type ErasedGc = Arc<GcBox<dyn Trace>>;

/// A garbage-collected managed pointer (Bacon-Rajan level-1, ADR-0001).
///
/// Cloneable and `Send + Sync` (both inherited from the backing `Arc`). Cloning
/// bumps the GC-visible strong count and marks the node live (`Black`); the
/// last-handle drop lets the `Arc` free the allocation, while a
/// drop-with-survivors flags the node as a possible cycle root.
pub(crate) struct Gc<T: Trace + 'static> {
    inner: Arc<GcBox<T>>,
}

impl<T: Trace + 'static> Gc<T> {
    /// Allocate a new managed node holding `value`, with one live handle.
    pub(crate) fn new(value: T) -> Gc<T> {
        Gc {
            inner: Arc::new(GcBox {
                header: GcHeader::fresh(),
                value,
            }),
        }
    }

    /// Two handles point at the same node (`Arc::ptr_eq`). The GC container
    /// migration (§11 step 5) needs this to preserve mutsu's container identity
    /// checks that currently use `Arc::ptr_eq` on the backing `Arc`.
    pub(crate) fn ptr_eq(a: &Gc<T>, b: &Gc<T>) -> bool {
        Arc::ptr_eq(&a.inner, &b.inner)
    }

    /// Mutable access to the pointee IFF this is the only live handle to the node
    /// (no other `Gc` handle and — when GC is enabled — no retained candidate
    /// buffer clone). Returns `None` when the node is shared. Mirrors
    /// `Arc::get_mut`, the non-cloning half of the `Arc::make_mut` sites the
    /// container migration replaces.
    pub(crate) fn get_mut(&mut self) -> Option<&mut T> {
        Arc::get_mut(&mut self.inner).map(|b| &mut b.value)
    }

    /// The GC-visible strong count (live `Gc` handles to this node).
    pub(crate) fn strong_count(&self) -> usize {
        self.inner.header.strong.load(Ordering::Relaxed)
    }

    /// This node's current [`Color`].
    pub(crate) fn color(&self) -> Color {
        Color::from_u8(self.inner.header.color.load(Ordering::Relaxed))
    }

    /// Hand each direct `Gc` child of the pointee to `visit` (delegates to the
    /// value's [`Trace`] impl). Named to avoid clashing with `Trace::trace`.
    pub(crate) fn trace_children(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        self.inner.value.trace(visit);
    }

    /// A type-erased [`ErasedGc`] handle to this node (a fresh `Arc` clone
    /// coerced to `dyn Trace`). This is how a parent node exposes a `Gc` child
    /// to the collector's tracer, and how a candidate is retained in the
    /// buffer. Bumps the backing `Arc`'s count but NOT the GC-visible
    /// `header.strong`, so an erased handle held by the collector does not read
    /// as a live program reference.
    pub(crate) fn erased(&self) -> ErasedGc {
        self.inner.clone()
    }

    // ---- `Arc<T>` drop-in API ----------------------------------------------
    //
    // These mirror the `Arc::` associated functions the pre-GC container code
    // used on `Arc<ArrayData>` / `Arc<HashData>`, so migrating a `Value`
    // variant from `Arc<T>` to `Gc<T>` (§11 step 5+) is a mechanical
    // call-site rewrite (`Arc::foo(x)` -> `Gc::foo(x)`) rather than a
    // behavioral change. Uniqueness is judged by `header.strong` (live GC
    // handles, excluding the candidate buffer's retained `Arc`), which equals
    // what `Arc::strong_count` reported before the buffer existed — so COW
    // behavior is preserved whether or not `MUTSU_GC` is on.

    /// `Arc::ptr_eq` analog: whether two handles designate the same node.
    pub(crate) fn ptr_eq(a: &Gc<T>, b: &Gc<T>) -> bool {
        Arc::ptr_eq(&a.inner, &b.inner)
    }

    /// `Arc::strong_count` analog in associated-fn form (the method
    /// [`Gc::strong_count`] returns the same value). Live GC handles only.
    pub(crate) fn strong_count_of(this: &Gc<T>) -> usize {
        this.inner.header.strong.load(Ordering::Relaxed)
    }

    /// `Arc::as_ptr` analog: a raw pointer to the *value* (past the node
    /// header), matching what `Arc::as_ptr` yields for a plain `Arc<T>`.
    pub(crate) fn as_ptr(this: &Gc<T>) -> *const T {
        &this.inner.value as *const T
    }

    /// `Arc::get_mut` analog. Returns `&mut T` only when this is the sole live
    /// GC handle (`header.strong == 1`), ignoring the backing `Arc`'s weak/
    /// buffer references. Sound because a buffered node's value is read only at
    /// a collect safepoint, never concurrently with a live-handle mutation
    /// (same contract as [`gc_contents_mut`]).
    pub(crate) fn get_mut(this: &mut Gc<T>) -> Option<&mut T> {
        if this.inner.header.strong.load(Ordering::Relaxed) == 1 {
            // SAFETY: sole live handle => no other live-handle borrow into this
            // value exists. See `gc_contents_mut`'s contract.
            Some(unsafe { &mut *(Gc::as_ptr(this) as *mut T) })
        } else {
            None
        }
    }

    /// `Arc::make_mut` analog (copy-on-write). If more than one live GC handle
    /// exists, clone the value into a fresh node and rebind `this`; then return
    /// `&mut` to the now-unique value. Matches the COW semantics the pre-GC
    /// `Arc::make_mut(&mut Arc<ArrayData>)` sites relied on.
    pub(crate) fn make_mut(this: &mut Gc<T>) -> &mut T
    where
        T: Clone,
    {
        if this.inner.header.strong.load(Ordering::Relaxed) != 1 {
            let cloned = this.inner.value.clone();
            *this = Gc::new(cloned);
        }
        // SAFETY: exactly one live handle after the check/clone above.
        unsafe { &mut *(Gc::as_ptr(this) as *mut T) }
    }

    /// Offer this node to the candidate buffer as a possible cycle root,
    /// bypassing the `MUTSU_GC` gate. Used by tests and (later) by an explicit
    /// `gc_debug_collect_now`-style hook.
    #[cfg(test)]
    pub(crate) fn buffer_as_candidate(&self) {
        buffer_candidate(self.inner.clone());
    }
}

impl<T: Trace + Clone + 'static> Gc<T> {
    /// Copy-on-write mutable access, mirroring `Arc::make_mut` (which the
    /// container migration, §11 step 5, replaces at every `Arc::make_mut`
    /// site on `ArrayData`/`HashData`/...).
    ///
    /// If this is the only live handle, returns `&mut` to the existing value.
    /// Otherwise the value is cloned into a *fresh single-handle node* and this
    /// handle is retargeted to it — so the write is private to this handle and
    /// the previously-shared node is untouched.
    ///
    /// GC-strong accounting: the fresh node starts at 1, and the old node loses
    /// this handle's contribution (it stays alive for the other handles). This
    /// keeps the GC-visible strong count consistent with the actual handle
    /// distribution even though the retarget goes through the backing `Arc`
    /// directly rather than `Gc::clone`/`Gc::drop`.
    pub(crate) fn make_mut(&mut self) -> &mut T {
        if Arc::get_mut(&mut self.inner).is_none() {
            // Shared: this handle is moving off the old node onto a fresh copy.
            self.inner.header.strong.fetch_sub(1, Ordering::Relaxed);
            let cloned = self.inner.value.clone();
            self.inner = Arc::new(GcBox {
                header: GcHeader::fresh(),
                value: cloned,
            });
        }
        &mut Arc::get_mut(&mut self.inner)
            .expect("unique after copy-on-write")
            .value
    }
}

impl<T: Trace + 'static> std::ops::Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.inner.value
    }
}

impl<T: Trace + 'static> Clone for Gc<T> {
    fn clone(&self) -> Self {
        // A fresh handle means the node is live: bump the GC-visible count and
        // (re)mark it Black. Bacon-Rajan clears any stale Purple candidacy on a
        // new reference; the collector re-buffers on the next survivor-drop.
        self.inner.header.strong.fetch_add(1, Ordering::Relaxed);
        self.inner
            .header
            .color
            .store(Color::Black as u8, Ordering::Relaxed);
        Gc {
            inner: self.inner.clone(),
        }
    }
}

impl<T: Trace + 'static> Drop for Gc<T> {
    fn drop(&mut self) {
        let prev = self.inner.header.strong.fetch_sub(1, Ordering::Relaxed);
        // `prev > 1`: after this drop the node still has live handles, so it may
        // be reachable only through a cycle that outlives every stack root —
        // record it as a candidate. `prev == 1`: this was the last handle; the
        // `Arc` frees the allocation once its own count reaches zero.
        if prev > 1 && gc_enabled() {
            buffer_candidate(self.inner.clone());
        }
    }
}

impl<T: Trace + 'static> From<T> for Gc<T> {
    fn from(value: T) -> Gc<T> {
        Gc::new(value)
    }
}

impl<T: Trace + 'static> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        &self.inner.value
    }
}

impl<T: Trace + PartialEq + 'static> PartialEq for Gc<T> {
    /// Value equality, matching `Arc<T>`'s `PartialEq` (compares pointees, with
    /// a same-node fast path) — NOT pointer identity. Use [`Gc::ptr_eq`] for
    /// identity.
    fn eq(&self, other: &Self) -> bool {
        Gc::ptr_eq(self, other) || self.inner.value == other.inner.value
    }
}

impl<T: Trace + std::fmt::Debug + 'static> std::fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Forward to the pointee so `Value`'s derived `Debug` prints a migrated
        // `Gc<ArrayData>` exactly like the old `Arc<ArrayData>` did.
        std::fmt::Debug::fmt(&self.inner.value, f)
    }
}

/// Copy-on-write `make_mut` bridged over both `Arc<T>` and `Gc<T>`, so a shared
/// macro/generic that mutates a container works while only *some* container
/// variants have migrated from `Arc` to `Gc` (§11 step 5 lands one type at a
/// time). Once every container variant is `Gc`, call sites can use `Gc::make_mut`
/// directly and this bridge can be retired.
pub(crate) trait ContainerMakeMut {
    type Inner;
    fn container_make_mut(&mut self) -> &mut Self::Inner;
}

impl<T: Clone> ContainerMakeMut for Arc<T> {
    type Inner = T;
    fn container_make_mut(&mut self) -> &mut T {
        Arc::make_mut(self)
    }
}

impl<T: Trace + Clone + 'static> ContainerMakeMut for Gc<T> {
    type Inner = T;
    fn container_make_mut(&mut self) -> &mut T {
        Gc::make_mut(self)
    }
}

/// `Gc<T>` analog of [`crate::value::aliased_mut::arc_contents_mut`]: a `&mut T`
/// aliasing a shared node's value for a deliberate aliased in-place mutation of
/// a container (Raku container identity — a push through one alias must be
/// visible through every holder of the same node).
///
/// # Safety
///
/// Same contract as `arc_contents_mut`: the caller must ensure no other borrow
/// into this value is live for the duration of the returned `&mut`, and no
/// concurrent access from another thread. The candidate buffer's retained `Arc`
/// adds no new hazard — a buffered node's value is read only at a collect
/// safepoint, never concurrently with a live mutation.
#[allow(clippy::mut_from_ref)]
pub(crate) unsafe fn gc_contents_mut<T: Trace + 'static>(gc: &Gc<T>) -> &mut T {
    // SAFETY: delegated to the caller per the contract above.
    unsafe { &mut *(Gc::as_ptr(gc) as *mut T) }
}

/// Process-global cycle-candidate buffer (design doc §5.2: "buffer は
/// `Vec<GcId>` でよい"). Holds an `Arc` clone of each candidate so the future
/// collector can trace it; `Mutex` because candidates are pushed from any
/// thread (`start`/`Promise`/`hyper`/`race`).
fn candidate_buffer() -> &'static Mutex<Vec<ErasedGc>> {
    static BUF: OnceLock<Mutex<Vec<ErasedGc>>> = OnceLock::new();
    BUF.get_or_init(|| Mutex::new(Vec::new()))
}

/// Record `node` as a possible cycle root: mark it `Purple` and push it, unless
/// it is already buffered (deduped). Wires the design doc §8 candidate counters.
fn buffer_candidate(node: ErasedGc) {
    if node.header.buffered.swap(true, Ordering::Relaxed) {
        record_gc_candidate_dedup_hit();
        return;
    }
    node.header
        .color
        .store(Color::Purple as u8, Ordering::Relaxed);
    if let Ok(mut buf) = candidate_buffer().lock() {
        buf.push(node);
    }
    record_gc_candidate_push();
}

/// Drain the candidate buffer, clearing each node's `buffered` flag, and return
/// the nodes. The synchronous collector (§11 step 8) will consume this to run
/// trial-deletion; exposed now as the seam between candidate registration and
/// collection so the two land in separate slices.
pub(crate) fn drain_candidates() -> Vec<ErasedGc> {
    let drained = match candidate_buffer().lock() {
        Ok(mut buf) => std::mem::take(&mut *buf),
        Err(_) => Vec::new(),
    };
    for node in &drained {
        node.header.buffered.store(false, Ordering::Relaxed);
    }
    drained
}

/// Whether GC candidate registration is active. `off`/unset (the default) makes
/// [`Gc`]'s drop skip candidate buffering entirely, so the `Gc` machinery is
/// inert until explicitly enabled (design doc §9.1: "compiled in, default off").
///
/// Resolved once from `MUTSU_GC` (`on`/`1` = on; `off`/`0`/unset = off; an
/// unrecognized value warns once and falls back to off, per §9.1a).
pub(crate) fn gc_enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| match std::env::var("MUTSU_GC").ok().as_deref() {
        Some("on") | Some("1") => true,
        None | Some("off") | Some("0") => false,
        Some(other) => {
            eprintln!("[mutsu gc] warning: unrecognized MUTSU_GC={other:?}, defaulting to off");
            false
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Serializes tests that touch the process-global candidate buffer, since
    /// the test harness runs them on multiple threads and a concurrent push
    /// would leak into another test's `drain_candidates`. Poison-tolerant so a
    /// panicking test does not cascade into the rest.
    static BUFFER_TEST_LOCK: Mutex<()> = Mutex::new(());

    fn lock_buffer_tests() -> std::sync::MutexGuard<'static, ()> {
        BUFFER_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner())
    }

    /// Leaf test node: no `Gc` children.
    struct Leaf;
    impl Trace for Leaf {
        fn trace(&self, _visit: &mut dyn FnMut(&ErasedGc)) {}
    }

    /// Node holding one optional `Gc<Leaf>` child, so `trace` has an edge to
    /// hand to a visitor.
    struct Node {
        child: Mutex<Option<Gc<Leaf>>>,
    }
    impl Trace for Node {
        fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
            if let Ok(guard) = self.child.lock()
                && let Some(child) = guard.as_ref()
            {
                visit(&child.erased());
            }
        }
    }

    /// Clonable leaf node carrying a payload, for COW / mutation tests.
    #[derive(Clone, Debug, PartialEq)]
    struct Cell(i64);
    impl Trace for Cell {
        fn trace(&self, _visit: &mut dyn FnMut(&ErasedGc)) {}
    }

    #[test]
    fn new_node_is_black_with_one_handle() {
        let gc = Gc::new(Leaf);
        assert_eq!(gc.strong_count(), 1);
        assert_eq!(gc.color(), Color::Black);
    }

    #[test]
    fn clone_bumps_strong_count_and_drop_lowers_it() {
        let gc = Gc::new(Leaf);
        let clone = gc.clone();
        assert_eq!(gc.strong_count(), 2);
        drop(clone);
        assert_eq!(gc.strong_count(), 1);
    }

    #[test]
    fn trace_visits_the_single_child() {
        let child = Gc::new(Leaf);
        let node = Gc::new(Node {
            child: Mutex::new(Some(child)),
        });
        let mut seen = 0;
        node.trace_children(&mut |_erased| seen += 1);
        assert_eq!(seen, 1);
    }

    #[test]
    fn trace_of_a_leaf_visits_nothing() {
        let gc = Gc::new(Leaf);
        let mut seen = 0;
        gc.trace_children(&mut |_erased| seen += 1);
        assert_eq!(seen, 0);
    }

    #[test]
    fn ptr_eq_distinguishes_shared_from_independent() {
        let a = Gc::new(Cell(1));
        let shared = a.clone();
        let b = Gc::new(Cell(1));
        assert!(Gc::ptr_eq(&a, &shared), "clones share the node");
        assert!(!Gc::ptr_eq(&a, &b), "independent nodes differ");
    }

    #[test]
    fn make_mut_shares_when_unique_and_cows_when_aliased() {
        // Unique: make_mut writes in place, both this handle and any later
        // clone observe it.
        let mut a = Gc::new(Cell(1));
        *Gc::make_mut(&mut a) = Cell(2);
        assert_eq!(*a, Cell(2));

        // Aliased: make_mut clones, so the write is NOT visible through the
        // pre-existing alias (Arc::make_mut COW semantics).
        let alias = a.clone();
        assert_eq!(Gc::strong_count_of(&a), 2);
        *Gc::make_mut(&mut a) = Cell(9);
        assert_eq!(*a, Cell(9));
        assert_eq!(
            *alias,
            Cell(2),
            "the alias must not see the post-clone write"
        );
        assert_eq!(Gc::strong_count_of(&a), 1, "a is a fresh unique node");
    }

    #[test]
    fn get_mut_is_some_only_when_unique() {
        let mut a = Gc::new(Cell(1));
        assert!(Gc::get_mut(&mut a).is_some());
        let alias = a.clone();
        assert!(Gc::get_mut(&mut a).is_none(), "aliased => no unique &mut");
        drop(alias);
        assert!(
            Gc::get_mut(&mut a).is_some(),
            "unique again after alias drops"
        );
    }

    #[test]
    fn gc_contents_mut_writes_through_a_shared_node() {
        // Unlike make_mut, gc_contents_mut mutates the shared node in place, so
        // an alias DOES observe the write (Raku shared-container identity).
        let a = Gc::new(Cell(1));
        let alias = a.clone();
        // SAFETY: no other borrow into the value is live across this write.
        unsafe { *gc_contents_mut(&a) = Cell(7) };
        assert_eq!(*a, Cell(7));
        assert_eq!(
            *alias,
            Cell(7),
            "the shared write is visible through the alias"
        );
    }

    #[test]
    fn from_and_debug_forward_to_the_value() {
        let gc: Gc<Cell> = Cell(5).into();
        assert_eq!(*gc, Cell(5));
        assert_eq!(format!("{gc:?}"), format!("{:?}", Cell(5)));
    }

    #[test]
    fn buffering_marks_purple_and_dedupes() {
        let _serial = lock_buffer_tests();
        drain_candidates(); // start from an empty buffer
        let gc = Gc::new(Leaf);
        gc.buffer_as_candidate();
        assert_eq!(gc.color(), Color::Purple);

        // Second offer of the same node must dedup (buffered flag already set).
        gc.buffer_as_candidate();

        let drained = drain_candidates();
        assert_eq!(drained.len(), 1, "the same node must be buffered only once");
        // After draining, the buffered flag is cleared so it can be re-buffered.
        gc.buffer_as_candidate();
        assert_eq!(drain_candidates().len(), 1);
    }

    #[test]
    fn drop_does_not_buffer_when_gc_disabled() {
        // Default (MUTSU_GC unset) => gc_enabled() is false, so a
        // drop-with-survivors must NOT push a candidate.
        let _serial = lock_buffer_tests();
        assert!(!gc_enabled(), "test assumes MUTSU_GC is unset");
        drain_candidates();
        let gc = Gc::new(Leaf);
        let clone = gc.clone();
        drop(clone); // survivor drop; would buffer if GC were enabled
        assert_eq!(drain_candidates().len(), 0);
        drop(gc);
    }

    #[test]
    fn gc_is_send_and_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Gc<Leaf>>();
        assert_send_sync::<ErasedGc>();
    }

    /// A `Clone` leaf carrying an integer payload, for the mutation-API tests.
    #[derive(Clone)]
    struct IntLeaf(i64);
    impl Trace for IntLeaf {
        fn trace(&self, _visit: &mut dyn FnMut(&ErasedGc)) {}
    }

    #[test]
    fn get_mut_is_some_when_unique_none_when_shared() {
        let mut gc = Gc::new(IntLeaf(1));
        assert!(gc.get_mut().is_some(), "unique handle is mutable");
        gc.get_mut().unwrap().0 = 42;
        assert_eq!(gc.0, 42);

        let shared = gc.clone();
        assert!(gc.get_mut().is_none(), "shared handle is not mutable");
        drop(shared);
        assert!(gc.get_mut().is_some(), "unique again after the clone drops");
    }

    #[test]
    fn make_mut_copies_on_write_and_leaves_sharers_untouched() {
        let mut a = Gc::new(IntLeaf(10));
        let b = a.clone(); // shared: a,b same node
        assert!(Gc::ptr_eq(&a, &b));
        *a.make_mut() = IntLeaf(20); // COW
        assert!(!Gc::ptr_eq(&a, &b), "writer got a private copy");
        assert_eq!(a.0, 20);
        assert_eq!(b.0, 10, "the other sharer is unchanged");
        // The fresh node is a single-handle live node.
        assert_eq!(a.strong_count(), 1);
        assert_eq!(a.color(), Color::Black);
        // `b` retained its handle to the old node.
        assert_eq!(b.strong_count(), 1);
    }

    #[test]
    fn make_mut_is_in_place_for_a_truly_unique_handle() {
        let mut a = Gc::new(IntLeaf(5));
        let ptr_before = a.inner.as_ref() as *const GcBox<IntLeaf>;
        *a.make_mut() = IntLeaf(6);
        let ptr_after = a.inner.as_ref() as *const GcBox<IntLeaf>;
        assert_eq!(
            ptr_before, ptr_after,
            "unique handle mutates in place, no COW"
        );
        assert_eq!(a.0, 6);
    }
}
