//! `GcCell<T>` — interior mutability for a GC-managed container payload
//! (ADR-0013, PLAN §2.1 Step 3 phase 1).
//!
//! # Why this exists
//!
//! Raku container semantics require *aliased, identity-preserving* in-place
//! mutation: a write through one alias of an `@a` / `%h` must be visible through
//! **every** holder of the same node (`:=` binds, captures, the env mirror).
//! `Gc::get_mut` returns `None` the moment the node is aliased, and
//! `Gc::make_mut` clones (severing the alias) — so an in-place write through the
//! *shared* node is fundamental, not an optimization.
//!
//! Before ADR-0013 that write was `gc::gc_contents_mut`:
//! `&mut *(Gc::as_ptr(gc) as *mut T)`. Deriving a `&mut` from a `*const`
//! obtained from a shared `&` is a **provenance violation** under Stacked/Tree
//! Borrows — Rust UB even single-threaded. `GcCell` fixes exactly that: the
//! `&mut` now comes from [`UnsafeCell::get`], which is *designed* to hand out a
//! `&mut` while shared `&` borrows into the same cell are live. That is the whole
//! point of `UnsafeCell`, and it is the one thing the raw-pointer cast could not
//! give.
//!
//! # What this does and does not fix
//!
//! - ✅ **Provenance UB (broad, every run, Miri-detectable)** — fixed. Reads stay
//!   `&T` via [`Deref`] (zero hot-path cost); only the mutation sites change.
//! - ⏳ **Cross-thread data race (narrow)** — *not* fixed here, and deliberately
//!   so (ADR-0013 §1.3 / §5). mutsu already routes cross-thread structural
//!   container mutation through the synchronized `__mutsu_atomic_arr::` /
//!   `shared_vars` lanes, so the `GcCell` sites are overwhelmingly same-thread
//!   aliased writes. The residual cross-thread concern is deferred to ADR-0001
//!   layer 3c (biased refcounting / owning-thread discipline). The
//!   `unsafe impl Sync` below carries the same justification today's
//!   `Gc<ArrayData>: Sync` relies on — minus the provenance UB.

use std::cell::UnsafeCell;
use std::ops::Deref;

use crate::gc::{ErasedGc, Trace};

/// Interior mutability for a GC container payload. See the module docs.
pub(crate) struct GcCell<T> {
    value: UnsafeCell<T>,
    /// Debug-only re-entrancy detector: set while an aliased `&mut` is live via
    /// [`GcCell::with_aliased_mut`]. A nested aliased mutation of the *same* node
    /// on the *same* thread (the ADR-0001 §3-6 landmine) sets it twice and logs a
    /// non-fatal diagnostic. Compiled out in release; never panics (mirrors the
    /// collector's `VERIFY` stderr convention, so a false positive stays
    /// advisory).
    #[cfg(debug_assertions)]
    writing: std::sync::atomic::AtomicBool,
}

// `UnsafeCell<T>` is `!Sync`, which would strip `GcCell` of the `Sync` its
// `Trace` supertrait demands. SAFETY: cross-thread structural mutation of the
// wrapped container is routed through the synchronized shared-store lanes
// (ADR-0013 §1.3-2); this restores the exact `Sync` posture the pre-GcCell
// `Gc<ArrayData>` already relied on, with the provenance UB removed. The narrow
// residual race is deferred to ADR-0001 layer 3c.
unsafe impl<T: Send + Sync> Sync for GcCell<T> {}

impl<T> GcCell<T> {
    /// Wrap a value for GC-managed interior mutation.
    pub(crate) fn new(value: T) -> GcCell<T> {
        GcCell {
            value: UnsafeCell::new(value),
            #[cfg(debug_assertions)]
            writing: std::sync::atomic::AtomicBool::new(false),
        }
    }

    /// Consume the cell and return the wrapped value.
    #[allow(dead_code)]
    pub(crate) fn into_inner(self) -> T {
        self.value.into_inner()
    }

    /// Shared read access. Also reachable through [`Deref`], so the thousands of
    /// existing `.iter()`/`.len()`/index read sites are unchanged after a
    /// container migrates to `Gc<GcCell<Data>>`.
    #[inline]
    pub(crate) fn get(&self) -> &T {
        // SAFETY: a shared `&` to the pointee. No `&mut` from `with_aliased_mut`
        // / `aliased_mut` may be live across this on the same thread (the caller
        // contract); the collector, the only cross-thread reader, reads at a
        // safepoint with all mutators parked.
        unsafe { &*self.value.get() }
    }

    /// Unique mutable access when the cell is not aliased (`&mut self` proves it).
    /// Safe — no aliasing is possible through a `&mut GcCell`.
    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_mut(&mut self) -> &mut T {
        self.value.get_mut()
    }

    /// Aliased, identity-preserving in-place mutable access — **the sound
    /// replacement for `gc::gc_contents_mut`**. The returned `&mut` is derived
    /// from [`UnsafeCell::get`], so it has valid provenance even while shared `&`
    /// borrows into the same node exist (Raku container identity).
    ///
    /// # Safety
    ///
    /// The caller must ensure that, on this thread, no other `&`/`&mut` into this
    /// cell is *dereferenced* for the lifetime of the returned borrow (the
    /// aliasing is logical — visible through the container's other `Gc` holders —
    /// not two live Rust borrows at once). Cross-thread structural mutation must
    /// remain routed through the synchronized shared-store lanes until ADR-0001
    /// layer 3c formalizes the owning-thread discipline. Prefer
    /// [`GcCell::with_aliased_mut`] where the borrow scope is a single
    /// expression — it adds the debug re-entrancy detector.
    #[allow(clippy::mut_from_ref)]
    #[allow(dead_code)] // the 51 gc_contents_mut sites migrate to this in ADR-0013 §4 phase 3
    #[inline]
    pub(crate) unsafe fn aliased_mut(&self) -> &mut T {
        // SAFETY: delegated to the caller per the contract above.
        unsafe { &mut *self.value.get() }
    }

    /// Scoped aliased mutation with a debug re-entrancy detector. Equivalent to
    /// `f(&mut *self.aliased_mut())`, but brackets the debug `writing` flag so a
    /// nested aliased mutation of the same node on the same thread is caught.
    ///
    /// # Safety
    ///
    /// Same contract as [`GcCell::aliased_mut`] (the closure receives an aliased
    /// `&mut`).
    #[allow(dead_code)] // consumed by the phase-3 site migration (ADR-0013 §4)
    #[inline]
    pub(crate) unsafe fn with_aliased_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        #[cfg(debug_assertions)]
        let reentrant = self
            .writing
            .swap(true, std::sync::atomic::Ordering::Relaxed);
        #[cfg(debug_assertions)]
        if reentrant {
            eprintln!(
                "[mutsu gc] VERIFY: GcCell re-entrant aliased mutation on the same node \
                 (nested &mut on one thread — the ADR-0001 §3-6 re-entry landmine)"
            );
        }
        // SAFETY: delegated to the caller (aliased-write contract above).
        let out = f(unsafe { &mut *self.value.get() });
        #[cfg(debug_assertions)]
        if !reentrant {
            self.writing
                .store(false, std::sync::atomic::Ordering::Relaxed);
        }
        out
    }
}

impl<T> Deref for GcCell<T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &T {
        self.get()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for GcCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.get().fmt(f)
    }
}

impl<T: Default> Default for GcCell<T> {
    fn default() -> GcCell<T> {
        GcCell::new(T::default())
    }
}

impl<T: Clone> Clone for GcCell<T> {
    fn clone(&self) -> GcCell<T> {
        GcCell::new(self.get().clone())
    }
}

// Forward the GC contract to the wrapped payload so `Gc<GcCell<Data>>` traces,
// breaks edges, and finalizes exactly as `Gc<Data>` did.
impl<T: Trace> Trace for GcCell<T> {
    fn trace(&self, visit: &mut dyn FnMut(&ErasedGc)) {
        // The collector traces at a safepoint with every mutator parked, so a
        // shared read of the pointee is sound even though a program mutator uses
        // `aliased_mut` elsewhere.
        self.get().trace(visit);
    }

    fn drop_gc_edges(&mut self) {
        // `&mut self` is supplied by the collector via the backing `Arc`
        // (`gc::gc_drop_edges`); unique access, no aliasing.
        self.value.get_mut().drop_gc_edges();
    }

    fn finalize(&self) {
        self.get().finalize();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::Gc;

    #[derive(Debug, Clone, PartialEq)]
    struct Leaf(Vec<i64>);

    impl Trace for Leaf {
        fn trace(&self, _visit: &mut dyn FnMut(&ErasedGc)) {}
    }

    #[test]
    fn read_through_deref_and_get() {
        let c = GcCell::new(Leaf(vec![1, 2, 3]));
        assert_eq!(c.get().0, vec![1, 2, 3]);
        assert_eq!(c.0.len(), 3, "Deref reaches the pointee's fields");
    }

    #[test]
    fn aliased_write_is_visible_through_a_gc_clone() {
        // The whole point: a write through one Gc handle is visible through
        // every clone of the same node (Raku container identity), because the
        // in-place mutation goes through the SAME allocation — not a COW copy.
        let a = Gc::new(GcCell::new(Leaf(vec![10])));
        let b = a.clone();
        // SAFETY: no other borrow into the cell is live across this write.
        unsafe { a.aliased_mut().0.push(20) };
        assert_eq!(a.get().0, vec![10, 20]);
        assert_eq!(
            b.get().0,
            vec![10, 20],
            "the alias observes the shared write"
        );
    }

    #[test]
    fn with_aliased_mut_returns_the_closure_value() {
        let c = GcCell::new(Leaf(vec![1]));
        let len = unsafe {
            c.with_aliased_mut(|d| {
                d.0.push(2);
                d.0.len()
            })
        };
        assert_eq!(len, 2);
        assert_eq!(c.get().0, vec![1, 2]);
    }

    #[test]
    fn get_mut_gives_unique_access() {
        let mut c = GcCell::new(Leaf(vec![1]));
        c.get_mut().0.push(2);
        assert_eq!(c.into_inner().0, vec![1, 2]);
    }

    #[test]
    fn trace_forwards_to_the_payload() {
        // A GcCell wrapping a leaf (no Gc children) visits nothing; the forward
        // is exercised (no panic, correct child count).
        let c = GcCell::new(Leaf(vec![1, 2]));
        let mut seen = 0;
        c.trace(&mut |_erased| seen += 1);
        assert_eq!(seen, 0);
    }

    #[test]
    fn clone_deep_copies_the_payload() {
        // GcCell::clone copies the value (a fresh, independent cell) — unlike a
        // Gc handle clone, which shares the node. Used when a container is
        // value-cloned rather than alias-shared.
        let a = GcCell::new(Leaf(vec![1]));
        let b = a.clone();
        unsafe { a.aliased_mut().0.push(2) };
        assert_eq!(a.get().0, vec![1, 2]);
        assert_eq!(b.get().0, vec![1], "a value-clone is independent");
    }
}
