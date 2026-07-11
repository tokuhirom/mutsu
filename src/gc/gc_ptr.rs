//! GC Level 1a managed-pointer primitive: [`Gc<T>`], its Bacon-Rajan node
//! header, the [`Trace`] trait, and the process-global cycle-candidate buffer
//! (ADR-0001 / ADR-0002, `docs/gc-level1-detailed-design.md` §5.1 / §9.1 /
//! §11 step 4).
//!
//! The *type machinery* the Bacon-Rajan synchronous cycle collector
//! (`gc::collect`) runs on:
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
//! State of the migration (§11 step 5 — COMPLETE, layer 3a shipped):
//! - Every container-kind variant is `Gc<_>`: `Hash`/`Array`/`ContainerRef`
//!   (first wave), `Set`/`Bag`/`Mix` (#4117), `Sub`/`Instance` attributes
//!   (#4123), `Promise`/`Channel` (#4127), `LazyList` (#4125). Scalar variants
//!   stay GC-free per the ADR-0001 type filter. The [`ContainerMakeMut`] bridge
//!   remains for shared macros generic over `Arc` (immutable wrappers) and `Gc`.
//! - The synchronous trial-deletion collector (`gc::collect`) runs at VM
//!   safepoints (`gc::safepoint`) under the ADR-0003 adaptive threshold.
//!   **`MUTSU_GC` defaults to on** (ADR-0003 §5, 2026-07-05); `MUTSU_GC=off`
//!   disarms safepoints and leaves the candidate buffer empty, so a run pays
//!   just an atomic refcount op per container clone/drop.

use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

use crate::vm::vm_stats::{record_gc_candidate_dedup_hit, record_gc_candidate_push};

/// Bacon-Rajan node color (design doc §5.1's "color / state").
///
/// Only `Black`/`Purple` are set outside a collection: a live node is `Black`,
/// and a node flagged as a possible cycle root when a `Gc` handle to it is
/// dropped is `Purple`. `Gray`/`White` are the transient trial-deletion states
/// the collector (`gc::collect`) uses while scanning a candidate subgraph.
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

    /// Drop this node's outgoing `Gc` edges, breaking any cycle it is part of.
    ///
    /// Called by the collector (`gc::collect`) ONLY on a node it has proven to
    /// be unreachable garbage, and ONLY inside a `CollectGuard` window (so the
    /// resulting `Gc::drop`s are inert — see `Gc`'s `Drop`). The `&mut self` is
    /// obtained by the collector through the backing `Arc` (see
    /// [`gc_drop_edges`]). The default clears nothing (scalar leaves have no
    /// `Gc` edges); container nodes override it to clear their `Value`-holding
    /// collections.
    fn drop_gc_edges(&mut self) {}

    /// Run this node's *value-level* finalizer (Raku `DESTROY` queueing), once
    /// per node death. Decouples language-level finalization from Rust `Drop`:
    /// with GC on, a node's memory drop can be arbitrarily deferred by the
    /// candidate buffer's retained handle, so waiting for `Drop` would delay
    /// (or, for a short program, entirely skip) `DESTROY`.
    ///
    /// Called from exactly two places, both on an interpreter thread:
    /// - `Gc::drop`, when the *last live handle* goes away (GC on; the memory
    ///   may live on in the candidate buffer) — the prompt refcount-death path.
    /// - the collector's reclaim pass, on proven cycle garbage, BEFORE
    ///   `drop_gc_edges` clears the node (so a `DESTROY` submethod still sees
    ///   the attributes).
    ///
    /// Implementations must be idempotent (guard with an internal once-flag):
    /// the eventual Rust `Drop` of the value typically funnels into the same
    /// logic for the GC-off path. The default does nothing (only `Instance`
    /// attributes carry a Raku destructor).
    fn finalize(&self) {}
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

/// A non-owning handle to a `Gc` node — the `Gc` analogue of `std::sync::Weak`,
/// backing `WeakSub`. Does not keep the node alive; `upgrade` returns a
/// live [`Gc`] handle (a new strong reference) only while the node still exists.
pub(crate) struct WeakGc<T: Trace + 'static> {
    inner: std::sync::Weak<GcBox<T>>,
}

impl<T: Trace + 'static> WeakGc<T> {
    /// A permanently-dangling weak handle (`Weak::new()`) whose [`upgrade`]
    /// always returns `None`. For initializing a weak slot that starts empty —
    /// e.g. a not-yet-set back-reference in a cycle-breaking design, where the
    /// forward link is a strong [`Gc`] and the back link is a `WeakGc` so the
    /// cycle never keeps itself alive.
    ///
    /// [`upgrade`]: WeakGc::upgrade
    #[cfg(test)]
    pub(crate) fn new() -> WeakGc<T> {
        WeakGc {
            inner: std::sync::Weak::new(),
        }
    }

    /// Whether two weak handles point at the same node (`Weak::ptr_eq`), for
    /// `WeakSub` identity comparison.
    pub(crate) fn ptr_eq(a: &WeakGc<T>, b: &WeakGc<T>) -> bool {
        std::sync::Weak::ptr_eq(&a.inner, &b.inner)
    }

    /// Backing-`Arc` strong count of the node (0 if it has been freed). Used by
    /// the consumed-LazyList registry to prune dead weak entries.
    pub(crate) fn strong_count(&self) -> usize {
        self.inner.strong_count()
    }

    /// Raw `*const T` to the pointee for identity comparison, or null if the node
    /// is gone. (`Weak::as_ptr` yields the `GcBox`; project the value.)
    pub(crate) fn as_ptr(&self) -> *const T {
        let box_ptr = self.inner.as_ptr();
        if box_ptr.is_null() {
            std::ptr::null()
        } else {
            unsafe { std::ptr::addr_of!((*box_ptr).value) }
        }
    }

    /// Reconstitute a strong [`Gc`] handle if the node is still alive. Bumps the
    /// GC-visible strong count and marks the node live (`Black`) — upgrading a
    /// weak reference *is* taking a new temporary strong reference.
    pub(crate) fn upgrade(&self) -> Option<Gc<T>> {
        self.inner.upgrade().map(|arc| {
            arc.header.strong.fetch_add(1, Ordering::Relaxed);
            arc.header
                .color
                .store(Color::Black as u8, Ordering::Relaxed);
            Gc { inner: arc }
        })
    }
}

impl<T: Trace + 'static> Clone for WeakGc<T> {
    fn clone(&self) -> Self {
        WeakGc {
            inner: self.inner.clone(),
        }
    }
}

impl<T: Trace + std::fmt::Debug + 'static> std::fmt::Debug for WeakGc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WeakGc(..)")
    }
}

// A weak handle carries no `T` by value, so it is `Send + Sync` on the backing
// `Weak`'s own guarantees (mirroring `WeakSub`'s old `Weak<SubData>`).
unsafe impl<T: Trace + 'static> Send for WeakGc<T> {}
unsafe impl<T: Trace + 'static> Sync for WeakGc<T> {}

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

    /// A non-owning [`WeakGc`] handle to this node (the `Gc` analogue of
    /// `Arc::downgrade`), for `WeakSub`. A weak handle does not count
    /// toward the node staying alive, so a reference made weak (rather than a
    /// [`Gc`] clone) breaks any cycle it would otherwise close.
    pub(crate) fn downgrade(this: &Gc<T>) -> WeakGc<T> {
        WeakGc {
            inner: std::sync::Arc::downgrade(&this.inner),
        }
    }

    /// Number of live [`WeakGc`] handles observing this node. A node with weak
    /// observers but no strong handle keeps only its *allocation* alive (its
    /// value is dropped when the strong count hits 0), so an outstanding weak
    /// reference never resurrects reclaimed data — `upgrade` returns `None`.
    #[cfg(test)]
    pub(crate) fn weak_count(this: &Gc<T>) -> usize {
        Arc::weak_count(&this.inner)
    }

    /// Two handles point at the same node (`Arc::ptr_eq`). The GC container
    /// migration (§11 step 5) needs this to preserve mutsu's container identity
    /// checks that currently use `Arc::ptr_eq` on the backing `Arc`.
    pub(crate) fn ptr_eq(a: &Gc<T>, b: &Gc<T>) -> bool {
        Arc::ptr_eq(&a.inner, &b.inner)
    }

    /// Mutable access to the pointee IFF this is the only *live* handle to the
    /// node. Uniqueness is judged by the GC-visible strong count (live `Gc`
    /// handles), NOT the backing `Arc`'s count: with `MUTSU_GC` on, the candidate
    /// buffer retains an extra `Arc` clone of a possibly-cyclic node, which would
    /// make an `Arc::get_mut` check return `None` spuriously and, via
    /// [`Gc::make_mut`], sever container identity (Raku aliasing). Returns `None`
    /// when genuinely shared.
    ///
    /// Production container mutation goes through [`gc_contents_mut`] /
    /// [`Gc::make_mut`]; this `Arc::get_mut` mirror is exercised by unit tests
    /// only (it documents the uniqueness contract `make_mut` shares).
    #[cfg(test)]
    pub(crate) fn get_mut(&mut self) -> Option<&mut T> {
        // Mutators never take `&mut` inside a collect's reclaim window: the
        // collector runs at a safepoint (no interpreter frame is mid-mutation)
        // and only its own `drop_gc_edges` runs under `CollectGuard`.
        debug_assert!(!collecting(), "Gc::get_mut during a collect reclaim window");
        if self.inner.header.strong.load(Ordering::Relaxed) == 1 {
            // SAFETY: sole live handle, so no other live-handle borrow into the
            // value exists. A buffered node's retained `Arc` never dereferences
            // the value except at a collect safepoint (same contract as
            // `gc_contents_mut`), so this aliased `&mut` is sound. The pointer
            // comes from `Arc::as_ptr` (not a `&`-to-`&mut` cast), dodging the
            // `invalid_reference_casting` lint.
            let boxed = Arc::as_ptr(&self.inner) as *mut GcBox<T>;
            Some(unsafe { &mut (*boxed).value })
        } else {
            None
        }
    }

    /// The GC-visible strong count (live `Gc` handles to this node).
    pub(crate) fn strong_count(&self) -> usize {
        self.inner.header.strong.load(Ordering::Relaxed)
    }

    /// Raw `*const T` to the pointee, mirroring `Arc::as_ptr`. Used by
    /// `gc_contents_mut` (the `Gc` analogue of `arc_contents_mut`) for the
    /// deliberate aliased in-place container mutation the migration preserves.
    pub(crate) fn as_ptr(this: &Gc<T>) -> *const T {
        // Reach the value field through the backing Arc's stable address. Uses a
        // raw offset (no intermediate reference) so it composes with the
        // `*mut`-write in `gc_contents_mut` without a Stacked-Borrows conflict.
        let box_ptr = Arc::as_ptr(&this.inner);
        unsafe { std::ptr::addr_of!((*box_ptr).value) }
    }

    /// This node's current [`Color`]. Production code reads colors through the
    /// type-erased [`GcBox<dyn Trace>::gc_color`]; this typed mirror is for tests.
    #[cfg(test)]
    pub(crate) fn color(&self) -> Color {
        Color::from_u8(self.inner.header.color.load(Ordering::Relaxed))
    }

    /// Hand each direct `Gc` child of the pointee to `visit` (delegates to the
    /// value's [`Trace`] impl). Named to avoid clashing with `Trace::trace`.
    /// The collector traverses via the type-erased
    /// [`GcBox<dyn Trace>::gc_visit_children`]; this typed mirror is for tests.
    #[cfg(test)]
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

    // ---- `Arc<T>` drop-in API (supplements the `ptr_eq`/`get_mut`/`make_mut`
    // added in #4112) --------------------------------------------------------
    //
    // The container migration (§11 step 5) rewrites the pre-GC `Arc::` calls on
    // `ArrayData`/`HashData` to their `Gc` equivalents. #4112 landed
    // `ptr_eq`/`get_mut`/`make_mut`; these two add the remaining `Arc::`
    // associated fns the migration's call sites use.

    /// `Arc::strong_count` analog in associated-fn form (the method
    /// [`Gc::strong_count`] returns the same value). Live GC handles only.
    pub(crate) fn strong_count_of(this: &Gc<T>) -> usize {
        this.inner.header.strong.load(Ordering::Relaxed)
    }

    // (`as_ptr` is defined once above, in raw-`addr_of` form — the earlier
    // reference-forming variant from the parallel #4113 branch was dropped in
    // the #4113/#4114 merge-dedup so `gc_contents_mut`'s `*mut` write stays
    // Stacked-Borrows clean.)

    /// Offer this node to the candidate buffer as a possible cycle root,
    /// bypassing the `MUTSU_GC` gate, so tests can seed a collect directly.
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
        // See `Gc::get_mut`: no `&mut` inside a collect's reclaim window.
        debug_assert!(
            !collecting(),
            "Gc::make_mut during a collect reclaim window"
        );
        // Uniqueness by GC-visible strong count, NOT the `Arc` count: the
        // candidate buffer (MUTSU_GC on) holds an extra `Arc` clone of a
        // possibly-cyclic node, so an `Arc::get_mut` check would COW spuriously
        // and sever container identity (Raku aliasing broke on e.g. slice-
        // assignment lvalues). `header.strong` excludes the buffer's ref.
        //
        // Ordering: `Relaxed` everywhere on `header.strong` is sound because no
        // decision ever *reads another thread's count concurrently*. The `== 1`
        // uniqueness test only acts when this thread holds the sole live handle
        // (`&mut self`), so no other thread can be cloning/dropping THIS node
        // through a strong handle; and the collector — the only cross-thread
        // reader of `header.strong` — runs under the cooperative STW whose
        // SeqCst handshake (`gc::stw`) orders every mutator's prior relaxed
        // writes before the scan. The one theoretical gap is a cross-thread
        // `WeakGc::upgrade` racing the `== 1` fast path (a check-then-act window
        // `Arc::get_mut` closes with its weak-lock protocol); mutsu's only
        // `WeakGc` user is `WeakSub`, whose upgrades happen on the thread that
        // owns the sub's env, so the race is not reachable today. If `WeakGc`
        // grows a cross-thread consumer, port the weak-lock (see ADR-0001 §3-8).
        if self.inner.header.strong.load(Ordering::Relaxed) != 1 {
            // Shared: this handle is moving off the old node onto a fresh copy.
            let prev = self.inner.header.strong.fetch_sub(1, Ordering::Relaxed);
            debug_assert!(prev > 1, "Gc::make_mut strong-count underflow");
            let cloned = self.inner.value.clone();
            self.inner = Arc::new(GcBox {
                header: GcHeader::fresh(),
                value: cloned,
            });
        }
        // Sole live handle. SAFETY / lint: see `Gc::get_mut`.
        let boxed = Arc::as_ptr(&self.inner) as *mut GcBox<T>;
        unsafe { &mut (*boxed).value }
    }
}

impl<T: Trace + 'static> std::ops::Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.inner.value
    }
}

// `Eq`/`Hash` delegate to the pointee, mirroring `Arc<T>`'s blanket impls, so
// `Gc<T>` is a drop-in for `Arc<T>` inside a `#[derive(...)]`d `Value`. (`Debug`
// and `PartialEq` are defined once below — the #4113/#4114 merge kept both
// copies; the duplicate delegating versions were removed here.)
impl<T: Trace + Eq + 'static> Eq for Gc<T> {}

impl<T: Trace + std::hash::Hash + 'static> std::hash::Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.value.hash(state);
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
        // During a collect's reclaim phase, the collector is deliberately
        // dropping a garbage node's internal `Gc` edges. Those drops must NOT
        // touch `header.strong` or re-buffer (the collector owns that
        // bookkeeping and the counts are mid-scan scratch) — just let the
        // backing `Arc` drop. See `gc::collect`.
        if collecting() {
            return;
        }
        let prev = self.inner.header.strong.fetch_sub(1, Ordering::Relaxed);
        // Underflow here (a drop with no live handle on the books) is the
        // corruption signature MUTSU_GC_VERIFY reports as a survivor-invariant
        // violation; catch it at the source in debug builds.
        debug_assert!(prev >= 1, "Gc::drop strong-count underflow");
        // `prev > 1`: after this drop the node still has live handles, so it may
        // be reachable only through a cycle that outlives every stack root —
        // record it as a candidate. `prev == 1`: this was the last handle; the
        // `Arc` frees the allocation once its own count reaches zero.
        if gc_enabled() {
            if prev > 1 {
                // Already-buffered fast path (Bacon-Rajan's Decrement does this
                // same test): a hot loop cloning and dropping the same handle —
                // a recursive sub's `Sub` node, a reused instance — hits this on
                // every call, so it must cost one relaxed load, not the Arc
                // round-trip (clone + unsize + swap + drop) that
                // `buffer_candidate`'s authoritative dedup would spend. The swap
                // inside `buffer_candidate` still decides races (two threads
                // both loading `false` here is fine); a load that reads `true`
                // just as a concurrent drain clears the flag skips one
                // re-buffering, which the next survivor-drop (or the program-end
                // collect) re-offers — deferral, never unsoundness. This load
                // was the bulk of the measured GC-on overhead on bench-fib
                // (+30%) / bench-class (+61%): 0.4M-16.5M dedup hits each
                // paying the round-trip.
                if self.inner.header.buffered.load(Ordering::Relaxed) {
                    record_gc_candidate_dedup_hit();
                } else {
                    buffer_candidate(self.inner.clone());
                }
            } else {
                // Last live handle. The allocation may outlive this drop in the
                // candidate buffer, deferring the value's Rust `Drop` (and with
                // it any `Drop`-coupled Raku `DESTROY`) until some future
                // collect — or forever, in a program too short to trigger one.
                // Run the value-level finalizer NOW, on the dropping
                // (interpreter) thread, exactly when refcount death occurs.
                self.inner.value.finalize();
            }
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
        // `Gc<ArrayData>` exactly like the old `crate::gc::Gc<ArrayData>` did.
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
/// `Vec<GcId>` でよい"). Holds an `Arc` clone of each candidate so the
/// collector can trace it. SHARDED by node address: candidates are pushed
/// from every thread (`start`/`Promise`/`hyper`/`race` *and* the collector's
/// own dead-sweep drop cascade), and one global `Mutex` measured ~250µs per
/// push under thread churn (S17-lowlevel/thread.t: 16k-node dead sweeps went
/// from 40ms to 3.5-4.7s purely on lock ping-pong). Dedup correctness does
/// not live here — the per-node `buffered` bit is the authority — so a drain
/// simply concatenates the shards.
const CANDIDATE_SHARDS: usize = 64;

/// Approximate total buffered count, maintained alongside the shards so the
/// ADR-0003 size-threshold check needs no locks. Push/drain/requeue keep it
/// paired; transient over/undershoot only skews the *trigger point* by a few
/// entries, never correctness.
static APPROX_BUFFERED: AtomicUsize = AtomicUsize::new(0);

fn candidate_shards() -> &'static Vec<Mutex<Vec<ErasedGc>>> {
    static BUF: OnceLock<Vec<Mutex<Vec<ErasedGc>>>> = OnceLock::new();
    BUF.get_or_init(|| {
        (0..CANDIDATE_SHARDS)
            .map(|_| Mutex::new(Vec::new()))
            .collect()
    })
}

fn candidate_shard_for(node: &ErasedGc) -> &'static Mutex<Vec<ErasedGc>> {
    // Node addresses are allocator-aligned; drop the low bits before sharding.
    let id = erased_id(node) >> 6;
    &candidate_shards()[id & (CANDIDATE_SHARDS - 1)]
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
    // Count BEFORE pushing: a concurrent drain that takes the shard between
    // the push and a later count would subtract a node it drained but we had
    // not yet added, wrapping APPROX_BUFFERED below zero — after which the
    // debug-build `+ 1` on the wrapped usize::MAX panicked with "attempt to
    // add with overflow" (intermittent under threaded churn: gc_stress
    // spawn_churn / threaded_cycle_churn). Counting first only ever leaves a
    // transient OVERSHOOT (a counted node the drain hasn't seen), which the
    // saturating drain-side update absorbs.
    let len = APPROX_BUFFERED
        .fetch_add(1, Ordering::Relaxed)
        .saturating_add(1);
    if let Ok(mut buf) = candidate_shard_for(&node).lock() {
        buf.push(node);
    }
    record_gc_candidate_push();
    // May arm a pending collect (the MUTSU_GC_EVERY_CANDIDATE stress period,
    // or the ADR-0003 production size threshold); never collects inline here —
    // this runs from `Gc::drop`, which can hold a borrow (design §1.2).
    super::safepoint::note_candidate_push();
    super::safepoint::note_buffer_len(len);
}

/// Number of user worker threads (`start`/`Promise`/`hyper`/`race`/Supply
/// callbacks) that may be concurrently mutating the `Gc` graph.
///
/// The level-1a collector's trial deletion (`mark_gray` decrements each node's
/// strong count, `scan_black` restores it) is **not** safe against concurrent
/// mutation, so a collect first brings every other mutator to quiescence via
/// the cooperative stop-the-world (`gc::stw`, design doc §6.1). This count is
/// the STW's quiescence target: `stw` waits until this many threads (minus the
/// collecting one, if it is itself a worker) are parked at a safepoint or
/// inside a registered blocking wait.
static MUTATOR_WORKER_THREADS: AtomicUsize = AtomicUsize::new(0);

/// Register that a user worker thread is about to start (or is running). Called
/// on the *parent* thread before the worker is spawned so the count is raised
/// the instant `spawn_user_thread` returns, closing the window in which the
/// parent could reach a safepoint and collect while the worker comes up.
pub(crate) fn enter_mutator_worker() {
    MUTATOR_WORKER_THREADS.fetch_add(1, Ordering::AcqRel);
}

/// Register that a user worker thread has finished. Balanced against
/// `enter_mutator_worker`; run from the worker via an RAII guard so a panic in
/// user code still decrements. Wakes any collector waiting for quiescence —
/// an exit *lowers* the quiescence target, which is just as satisfying as a
/// park (see `stw::notify_worker_exit`).
pub(crate) fn exit_mutator_worker() {
    MUTATOR_WORKER_THREADS.fetch_sub(1, Ordering::AcqRel);
    super::stw::notify_worker_exit();
}

/// Current number of registered user worker threads. With exactly one
/// additional mutator (the main thread), this is also the number of *other*
/// mutators a stopping collector must see quiescent, whichever thread it runs
/// on (see `gc::stw`).
pub(crate) fn mutator_worker_count() -> usize {
    MUTATOR_WORKER_THREADS.load(Ordering::Acquire)
}

/// Drain the candidate buffer, clearing each node's `buffered` flag, and return
/// the nodes. The synchronous collector (§11 step 8) will consume this to run
/// trial-deletion; exposed now as the seam between candidate registration and
/// collection so the two land in separate slices.
pub(crate) fn drain_candidates() -> Vec<ErasedGc> {
    let mut drained = Vec::new();
    for shard in candidate_shards() {
        if let Ok(mut buf) = shard.lock() {
            drained.append(&mut buf);
        }
    }
    // Saturating: with count-before-push on the producer side the counter can
    // only overshoot, never undershoot — but keep the subtraction saturating
    // so no interleaving can ever wrap it (the counter is an approximation
    // that only steers the ADR-0003 trigger point).
    let removed = drained.len();
    let _ = APPROX_BUFFERED.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |v| {
        Some(v.saturating_sub(removed))
    });
    for node in &drained {
        node.header.buffered.store(false, Ordering::Relaxed);
    }
    drained
}

/// Put still-suspect candidates back into the buffer (the collector drained
/// them but declined trial deletion — e.g. worker threads were active). Skips
/// any node a concurrent `Gc::drop` re-buffered since the drain (its `buffered`
/// flag is already set), so the buffer never holds duplicates.
pub(crate) fn requeue_candidates(nodes: Vec<ErasedGc>) {
    for node in nodes {
        if node.header.buffered.swap(true, Ordering::Relaxed) {
            continue;
        }
        // Count-before-push, mirroring `buffer_candidate` (see the comment
        // there for the underflow race this ordering prevents).
        APPROX_BUFFERED.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut buf) = candidate_shard_for(&node).lock() {
            buf.push(node);
        }
    }
}

// ---- Collector-facing internals (used by `gc::collect`) --------------------

/// Set while the collector is dropping a garbage node's edges, so `Gc::drop`
/// stays inert (see `Gc`'s `Drop`). A plain relaxed flag: the level-1a
/// collector is single-threaded / cooperative-STW, so no ordering is needed.
static COLLECTING: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Whether a collect's reclaim phase is currently in progress on this process.
pub(crate) fn collecting() -> bool {
    COLLECTING.load(Ordering::Relaxed)
}

/// RAII guard that marks the reclaim window: `Gc::drop`s inside it skip their
/// refcount/candidate bookkeeping (the collector owns it). Held only around the
/// edge-clearing in `gc::collect`, never across a re-entry into the VM.
pub(crate) struct CollectGuard;

impl CollectGuard {
    pub(crate) fn new() -> CollectGuard {
        let was = COLLECTING.swap(true, Ordering::Relaxed);
        // Nested reclaim windows would let an inner guard's drop re-enable
        // `Gc::drop` bookkeeping while the outer reclaim is still clearing
        // edges — the counts are mid-scan scratch at that point.
        debug_assert!(!was, "nested CollectGuard (re-entrant reclaim window)");
        CollectGuard
    }
}

impl Drop for CollectGuard {
    fn drop(&mut self) {
        COLLECTING.store(false, Ordering::Relaxed);
    }
}

/// A stable per-node identity (the backing allocation's address), for the
/// collector's visited-set / dedup. Two `ErasedGc`s designate the same node iff
/// their ids are equal.
pub(crate) fn erased_id(node: &ErasedGc) -> usize {
    Arc::as_ptr(node) as *const () as usize
}

/// Header/edge accessors the collector needs on a type-erased node. Kept here
/// (rather than in `gc::collect`) because `GcHeader`'s fields are private to
/// this module.
impl GcBox<dyn Trace> {
    pub(crate) fn gc_color(&self) -> Color {
        Color::from_u8(self.header.color.load(Ordering::Relaxed))
    }
    pub(crate) fn gc_set_color(&self, c: Color) {
        self.header.color.store(c as u8, Ordering::Relaxed);
    }
    pub(crate) fn gc_strong(&self) -> usize {
        self.header.strong.load(Ordering::Relaxed)
    }
    pub(crate) fn gc_strong_dec(&self) {
        let prev = self.header.strong.fetch_sub(1, Ordering::Relaxed);
        // Trial deletion removes each internal edge once, so the scratch count
        // can reach 0 but never go below it. Underflow means the tracer
        // reported a phantom edge (e.g. the shared-Arc-wrapper over-trace
        // fixed in #4135) — the earliest observable point of that bug class.
        debug_assert!(prev >= 1, "trial-deletion strong-count underflow");
    }
    pub(crate) fn gc_strong_inc(&self) {
        self.header.strong.fetch_add(1, Ordering::Relaxed);
    }
    /// Visit each direct `Gc` child (delegates to the node's [`Trace`] impl).
    pub(crate) fn gc_visit_children(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        self.value.trace(visit);
    }
}

/// Drop `node`'s outgoing `Gc` edges (collector reclaim; delegates to
/// [`Trace::drop_gc_edges`]).
///
/// # Safety
///
/// Collector-only. The `&mut` to the node's value is taken through the backing
/// `Arc`'s pointer (not a `&`-to-`&mut` cast), so the caller must guarantee — as
/// the collector does inside a [`CollectGuard`] on a proven-garbage node — that
/// no other reference into the value is live for the duration.
pub(crate) fn gc_drop_edges(node: &ErasedGc) {
    // SAFETY: as above. `Arc::as_ptr` yields the box's genuine heap pointer.
    let boxed = Arc::as_ptr(node) as *mut GcBox<dyn Trace>;
    unsafe { (*boxed).value.drop_gc_edges() };
}

/// Run a type-erased node's value-level finalizer (see [`Trace::finalize`]).
/// Shared-reference access — no aliasing concerns.
pub(crate) fn gc_finalize(node: &ErasedGc) {
    node.value.finalize();
}

/// Whether GC candidate registration is active. **Default ON** since the
/// ADR-0003 acceptance gates passed (2026-07-05): a Raku interpreter that
/// leaks reference cycles is defective (ADR-0001 "GC is table stakes"), so
/// cycle collection is part of normal execution — `MUTSU_GC=off` is the
/// explicit opt-out (perf comparisons, debugging the collector itself).
///
/// Resolved once from `MUTSU_GC` (`off`/`0` = off; `on`/`1` = on; an
/// unrecognized value warns once and keeps the default). Unset = the default:
/// ON in production builds, OFF in the crate's own unit-test build
/// (`cfg!(test)`) — `cargo test` runs tests on parallel threads that share
/// the process-global collector state, so in-process safepoint collects
/// cross-talk between tests (see `gc::test_support`); the CI gc-stress job
/// still exercises GC-on unit tests by setting `MUTSU_GC=on` explicitly and
/// running single-threaded. Subprocess-based integration tests
/// (`tests/gc_stress.rs`) spawn the real (default-on) binary.
///
/// Cached in a tri-state atomic rather than the `OnceLock` alone: this sits on
/// `Gc::drop`'s survivor path (once per handle drop, millions of times on
/// class-heavy programs), where the `OnceLock` fast path's state-check +
/// cell-read double load is measurable; the cache makes it one relaxed load.
pub(crate) fn gc_enabled() -> bool {
    // 0 = uninitialized, 1 = off, 2 = on.
    static CACHE: std::sync::atomic::AtomicU8 = std::sync::atomic::AtomicU8::new(0);
    match CACHE.load(Ordering::Relaxed) {
        2 => true,
        1 => false,
        _ => {
            let on = gc_enabled_slow();
            CACHE.store(if on { 2 } else { 1 }, Ordering::Relaxed);
            on
        }
    }
}

#[cold]
fn gc_enabled_slow() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| match std::env::var("MUTSU_GC").ok().as_deref() {
        Some("off") | Some("0") => false,
        Some("on") | Some("1") => true,
        None => !cfg!(test),
        Some(other) => {
            let default = !cfg!(test);
            eprintln!(
                "[mutsu gc] warning: unrecognized MUTSU_GC={other:?}, defaulting to {}",
                if default { "on" } else { "off" }
            );
            default
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
    fn lock_buffer_tests() -> std::sync::MutexGuard<'static, ()> {
        crate::gc::test_support::serial_lock()
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
        // drop-with-survivors must NOT push a candidate. Under the GC=on CI
        // stress run the opposite is the point (it DOES buffer), so skip.
        if gc_enabled() {
            return;
        }
        let _serial = lock_buffer_tests();
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

    // --- Arc-drop-in trait impls (needed for the container-variant flip, §11
    //     step 5c). A `#[derive(Debug/PartialEq/Eq/Hash)]`d `Value` requires its
    //     `Gc<_>` field to provide these, so they delegate to the pointee.

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    struct KeyLeaf(i64);
    impl Trace for KeyLeaf {
        fn trace(&self, _visit: &mut dyn FnMut(&ErasedGc)) {}
    }

    #[test]
    fn debug_delegates_to_the_pointee() {
        let gc = Gc::new(KeyLeaf(7));
        assert_eq!(format!("{gc:?}"), format!("{:?}", KeyLeaf(7)));
    }

    #[test]
    fn eq_compares_values_not_pointers() {
        let a = Gc::new(KeyLeaf(1));
        let b = Gc::new(KeyLeaf(1)); // distinct node, same value
        let c = Gc::new(KeyLeaf(2));
        assert!(!Gc::ptr_eq(&a, &b), "distinct nodes");
        assert_eq!(a, b, "value equality, like Arc<T>");
        assert_ne!(a, c);
    }

    #[test]
    // `Gc`'s header carries atomics (interior mutability), so clippy flags it as
    // a "mutable key type"; that is a false positive here because `Hash`/`Eq`
    // delegate to the immutable pointee value, not the header. (The eventual
    // container flip will need the same allow wherever a `Gc`-backed `Value` is
    // used as a hash key — an object hash, e.g. — since `Value` already is.)
    #[allow(clippy::mutable_key_type)]
    fn hash_uses_the_pointee_so_equal_values_collide_in_a_set() {
        use std::collections::HashSet;
        let mut set: HashSet<Gc<KeyLeaf>> = HashSet::new();
        set.insert(Gc::new(KeyLeaf(1)));
        set.insert(Gc::new(KeyLeaf(1))); // equal value -> dedup
        set.insert(Gc::new(KeyLeaf(2)));
        assert_eq!(set.len(), 2);
    }

    #[test]
    fn as_ptr_and_erased_reference_the_same_node() {
        let gc = Gc::new(IntLeaf(9));
        // as_ptr points at the value inside the node.
        let p = Gc::as_ptr(&gc);
        assert_eq!(unsafe { (*p).0 }, 9);
        // erased shares the node without bumping the GC-visible count.
        let before = gc.strong_count();
        let _erased = gc.erased();
        assert_eq!(
            gc.strong_count(),
            before,
            "erased view is not a user handle"
        );
    }

    // ---- Weak references (cycle breaking) ----------------------------------

    static WEAK_DROPS: AtomicUsize = AtomicUsize::new(0);

    /// A node with a strong forward link and a weak back link, so a two-node
    /// cycle can be closed with the back edge made weak. `trace` yields ONLY the
    /// strong child — a `WeakGc` is not a GC edge, which is exactly what lets it
    /// break the cycle. Bumps `WEAK_DROPS` when freed.
    struct WNode {
        strong: Mutex<Option<Gc<WNode>>>,
        weak: Mutex<Option<WeakGc<WNode>>>,
    }
    impl WNode {
        fn new() -> Gc<WNode> {
            Gc::new(WNode {
                strong: Mutex::new(None),
                weak: Mutex::new(None),
            })
        }
    }
    impl Trace for WNode {
        fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
            if let Ok(g) = self.strong.lock()
                && let Some(child) = g.as_ref()
            {
                visit(&child.erased());
            }
            // `self.weak` is deliberately NOT traced.
        }
        fn drop_gc_edges(&mut self) {
            *self.strong.get_mut().unwrap() = None;
            *self.weak.get_mut().unwrap() = None;
        }
    }
    impl Drop for WNode {
        fn drop(&mut self) {
            WEAK_DROPS.fetch_add(1, Ordering::Relaxed);
        }
    }

    #[test]
    fn dangling_weak_upgrades_to_none() {
        let w: WeakGc<Cell> = WeakGc::new();
        assert!(w.upgrade().is_none());
    }

    #[test]
    fn upgrade_is_some_while_alive_none_after_drop() {
        // Serialize + start clean: under `MUTSU_GC=on` the transient strong
        // handle that `upgrade().is_some()` creates is a survivor-drop
        // (prev-strong == 2), so `Gc::drop` conservatively buffers the node as a
        // cycle candidate — the buffer then holds an `Arc` clone that keeps the
        // allocation alive. Draining that buffer (what a safepoint collect does)
        // restores the plain refcount semantics this test asserts.
        let _serial = lock_buffer_tests();
        drain_candidates();

        let a = Gc::new(Cell(7));
        let w = Gc::downgrade(&a);
        assert_eq!(Gc::weak_count(&a), 1);
        assert!(w.upgrade().is_some(), "alive => upgradeable");
        drop(a);
        drain_candidates(); // release any conservatively-buffered `Arc` clone
        assert!(w.upgrade().is_none(), "dropped => not upgradeable");
    }

    #[test]
    fn weak_back_edge_breaks_a_cycle_without_the_collector() {
        let _serial = lock_buffer_tests();
        drain_candidates();
        let before = WEAK_DROPS.load(Ordering::Relaxed);

        // A --strong--> B --weak--> A : the back edge is weak, so the cycle
        // never keeps itself alive; plain refcounting frees both.
        let a = WNode::new();
        let b = WNode::new();
        *a.strong.lock().unwrap() = Some(b.clone());
        *b.weak.lock().unwrap() = Some(Gc::downgrade(&a));
        assert_eq!(a.strong_count(), 1, "the weak back-edge does not count");

        drop(a);
        drop(b);

        // Under `MUTSU_GC=on`, dropping `a`'s strong edge to `b` while `b` still
        // has the local handle is a survivor-drop, so `b` is conservatively
        // buffered as a cycle candidate — an `Arc` clone in the buffer keeps its
        // allocation alive. Draining the buffer (NOT running the collector)
        // releases that clone; the weak back-edge means plain refcounting then
        // frees both nodes. With `MUTSU_GC=off` the buffer is empty and this is a
        // no-op. Either way, no *collector* pass is needed — the point of the test.
        drop(drain_candidates());

        assert_eq!(
            WEAK_DROPS.load(Ordering::Relaxed) - before,
            2,
            "both nodes freed by refcounting alone — the weak edge broke the cycle"
        );
        // Nothing remains buffered as a cycle candidate.
        assert!(drain_candidates().is_empty());
    }

    #[test]
    fn weak_observer_of_a_collected_strong_cycle_sees_none() {
        let _serial = lock_buffer_tests();
        drain_candidates();

        // A <--strong--> B strong cycle, plus a weak observer of A.
        let a = WNode::new();
        let b = WNode::new();
        *a.strong.lock().unwrap() = Some(b.clone());
        *b.strong.lock().unwrap() = Some(a.clone());
        let observer = Gc::downgrade(&a);
        assert!(observer.upgrade().is_some());

        // Make them collectable garbage: buffer, then drop the external handles.
        a.buffer_as_candidate();
        b.buffer_as_candidate();
        drop(a);
        drop(b);

        let stats = super::super::collect::collect_cycles();
        assert_eq!(stats.reclaimed_nodes, 2, "the strong cycle is reclaimed");
        assert!(
            observer.upgrade().is_none(),
            "a weak observer never resurrects a reclaimed node"
        );
    }

    /// Regression (gc_stress `spawn_churn_does_not_starve_stop_the_world` /
    /// `threaded_cycle_churn_is_collected_soundly`, intermittent): when
    /// `buffer_candidate` incremented `APPROX_BUFFERED` *after* pushing into
    /// the shard, a concurrent `drain_candidates` could drain the node and
    /// subtract it before the producer's add — wrapping the counter to
    /// `usize::MAX` and panicking on the debug-build `+ 1` ("attempt to add
    /// with overflow"). Hammer push against a concurrent drainer: no thread
    /// may panic, and once quiescent the count must return exactly to zero.
    #[test]
    fn concurrent_buffer_and_drain_never_wraps_the_approx_count() {
        let _serial = lock_buffer_tests();
        // Clear any leftover candidates so the final assertion is exact.
        drop(drain_candidates());
        assert_eq!(APPROX_BUFFERED.load(Ordering::Relaxed), 0);

        let stop = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let drainer = {
            let stop = Arc::clone(&stop);
            std::thread::spawn(move || {
                while !stop.load(Ordering::Relaxed) {
                    drop(drain_candidates());
                }
            })
        };
        let producers: Vec<_> = (0..4)
            .map(|_| {
                std::thread::spawn(|| {
                    for _ in 0..50_000 {
                        Gc::new(Leaf).buffer_as_candidate();
                    }
                })
            })
            .collect();
        for p in producers {
            p.join()
                .expect("producer must not panic (APPROX_BUFFERED underflow wrap)");
        }
        stop.store(true, Ordering::Relaxed);
        drainer.join().expect("drainer must not panic");

        drop(drain_candidates());
        assert_eq!(
            APPROX_BUFFERED.load(Ordering::Relaxed),
            0,
            "count-before-push accounting must settle back to exactly zero"
        );
    }
}
