//! The single audited choke point for *aliased, in-place* mutation of a
//! shared `Arc<ArrayData>` / `Arc<HashData>` container.
//!
//! # Why this exists
//!
//! mutsu represents `Value::Array`/`Value::Hash` as `Arc<ArrayData>` /
//! `Arc<HashData>` copy-on-write containers that nonetheless carry a *shared
//! identity*: when a container is bound (`:=`), pushed to through an alias, or
//! grown through a `ContainerRef`, the mutation must be visible through **every**
//! holder of the same `Arc` (Raku container semantics). `Arc::get_mut` /
//! `Arc::make_mut` cannot express that — `get_mut` returns `None` the moment the
//! `Arc` is aliased (which is exactly when we need the shared write), and
//! `make_mut` clones, severing the alias. So the in-place write through the
//! shared `Arc`'s contents is fundamental, not an optimization.
//!
//! Before this module those writes were ~35 scattered, hand-rolled
//! `unsafe { &mut *(Arc::as_ptr(arc) as *mut _) }` blocks, several carrying the
//! *incorrect* `// SAFETY: mutsu is single-threaded` justification (ANALYSIS
//! §2.3: `clone_for_thread` + `thread::spawn` can move a clone of the same `Arc`
//! onto another OS thread, so "single-threaded" is false). Concentrating them
//! here gives one audited primitive, one accurate safety contract, and — most
//! importantly — **one site to change** when arrays/hashes gain interior
//! mutability (first-class container cells / PLAN.md Track B), which is what
//! actually removes the underlying unsoundness.
//!
//! # ⚠️ Known unsoundness (tracked, not removed here)
//!
//! Writing through a pointer derived from `Arc::as_ptr` (a `*const T`) is a
//! provenance violation under Stacked/Tree Borrows even single-threaded, and a
//! genuine data race when the same `Arc` is shared across threads. This module
//! **concentrates and honestly documents** that pre-existing unsoundness; it does
//! not fix it. The real fix is wrapping the mutated collections in interior
//! mutability (`UnsafeCell` / a lock) once arrays/hashes become first-class
//! `ContainerRef` cells. Until then, this is the deliberate, single, auditable
//! choke point — do not reintroduce ad-hoc `Arc::as_ptr as *mut` casts elsewhere.

use std::sync::Arc;

/// Returns a `&mut T` aliasing the contents of a shared `Arc<T>`, for a
/// deliberate aliased in-place mutation of an `ArrayData`/`HashData` container.
///
/// The returned borrow is tied to the lifetime of the `&Arc<T>` argument, so the
/// `&mut` cannot outlive the handle it came from (a small improvement over the
/// raw `Arc::as_ptr as *mut` casts this replaces, which produced an unbounded
/// pointer).
///
/// # Safety
///
/// The caller must guarantee that for the entire lifetime of the returned `&mut`:
///
/// * **No aliasing borrow is live.** No other reference (`&T` or `&mut T`) into
///   the same `Arc`'s contents may exist while this `&mut` is held. In practice
///   this means: read what you need out first, then take this borrow, then write,
///   and do not re-enter the VM (which could observe the container) while the
///   borrow is held.
/// * **No concurrent access from another thread.** See the module-level "Known
///   unsoundness" note — this obligation is *currently violable* for containers
///   captured into a spawned thread, and that gap is intentionally left for the
///   first-class-container (Track B) work to close.
///
/// This function does not, and cannot, check these obligations; it is the
/// single place where the project's container-aliasing invariant is trusted.
#[allow(clippy::mut_from_ref)]
pub(crate) unsafe fn arc_contents_mut<T>(arc: &Arc<T>) -> &mut T {
    // SAFETY: delegated to the caller per the contract above. This is the only
    // `Arc::as_ptr as *mut` cast in the codebase.
    unsafe { &mut *(Arc::as_ptr(arc) as *mut T) }
}
