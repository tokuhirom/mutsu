//! Shared-aware mutable access to a `Gc`-managed container's backing data.
//!
//! # Why this exists
//!
//! mutsu represents `Value::Array` / `Value::Hash` as `crate::gc::Gc<ArrayData>` /
//! `Gc<HashData>` copy-on-write containers that nonetheless carry a *shared
//! identity*: when a container is bound (`:=`), pushed to through an alias, or
//! grown through a `ContainerRef`, the mutation must be visible through **every**
//! holder of the same node (Raku container semantics). `Gc::get_mut` /
//! `Gc::make_mut` cannot express that — `get_mut` returns `None` the moment the
//! node is aliased (which is exactly when we need the shared write), and
//! `make_mut` clones, severing the alias. So the in-place write through the
//! shared node's contents is fundamental, not an optimization.
//!
//! [`gc_data_mut`] is the routing decision on top of that: aliased ⇒ write
//! through the shared node, unique ⇒ plain `make_mut`. The unsafe primitive it
//! routes to lives in the GC ([`crate::gc::gc_contents_mut`]) — there is exactly
//! one such primitive in the codebase, and this module does not duplicate it.
//!
//! # Soundness posture (ADR-0013)
//!
//! The old `unsafe { &mut *(Arc::as_ptr(arc) as *mut _) }` shape was a provenance
//! violation under Stacked/Tree Borrows even single-threaded. That is **fixed**:
//! since [ADR-0013](../../../docs/adr/0013-container-interior-mutability-cellvalue.md)
//! a `Gc` payload lives in the `GcBox`'s `UnsafeCell`, so `Gc::as_ptr` hands back
//! an interior-mutable pointer and the aliased `&mut` has valid provenance. The
//! fix landed at the primitive, so every call site became sound at once — there
//! is no per-container migration and, contrary to what this header used to say,
//! Track B is not the fix (ADR-0001 §7).
//!
//! What remains deferred, by decision rather than omission, is the **narrow
//! cross-thread race** on a genuinely shared node (ADR-0013 §1.3-2 → ADR-0001
//! layer 3c): concurrent structural mutation must stay routed through the
//! synchronized shared-store lanes, and nothing mechanically checks that.
//!
//! # ⚠️ One container is still `Arc`-backed, so its aliased write is still UB
//!
//! ADR-0013 fixed the provenance for `Gc`-managed containers. [`arc_contents_mut`]
//! below is the `Arc` counterpart and it has **one live call site**: the
//! `ValueView::Mixin` overrides map (`ValueRepr::Mixin(Arc<Value>,
//! Arc<HashMap<String, Value>>)`), written in place by `$type.^set_name(...)`.
//! An `Arc` payload has no `UnsafeCell`, so deriving `&mut` from `Arc::as_ptr`
//! there is the same provenance violation ADR-0013 removed everywhere else.
//! Tracked in `todo/tickets/mixin-overrides-aliased-write-is-still-arc.md`.
//!
//! Do not add new `as_ptr as *mut` casts: for a `Gc` container route the write
//! through the GC primitive, and do not give a new container the `Arc` shape
//! that forces this one.

/// Returns a `&mut T` aliasing the contents of a shared `Arc<T>`, for a
/// deliberate aliased in-place mutation of a still-`Arc`-backed container.
///
/// The returned borrow is tied to the lifetime of the `&Arc<T>` argument, so the
/// `&mut` cannot outlive the handle it came from (a small improvement over a raw
/// `Arc::as_ptr as *mut` cast, which produces an unbounded pointer).
///
/// # Safety
///
/// The caller must guarantee that for the entire lifetime of the returned `&mut`:
///
/// * **No aliasing borrow is live.** No other reference (`&T` or `&mut T`) into
///   the same `Arc`'s contents may exist while this `&mut` is held. In practice:
///   read what you need out first, then take this borrow, then write, and do not
///   re-enter the VM (which could observe the container) while it is held.
/// * **No concurrent access from another thread.**
///
/// Beyond those, note that this cast is **itself** a provenance violation — see
/// the module header's "One container is still `Arc`-backed" section. It has a
/// single live call site (the `Mixin` overrides map); every other aliased
/// container write goes through [`crate::gc::gc_contents_mut`], which is sound.
/// Do not add a second call site: give the container the `Gc` shape instead.
#[allow(clippy::mut_from_ref)]
pub(crate) unsafe fn arc_contents_mut<T>(arc: &std::sync::Arc<T>) -> &mut T {
    // SAFETY: delegated to the caller per the contract above. This is the only
    // `Arc::as_ptr as *mut` cast in the codebase.
    unsafe { &mut *(std::sync::Arc::as_ptr(arc) as *mut T) }
}

/// Shared-aware mutable access to a container's backing data for a mutation
/// of the *variable's own container* (container identity, §3): when the node
/// is aliased (`strong_count > 1` — e.g. the array was captured by value into
/// a list `(0, @a)`, stored in an element, or bound), write THROUGH the
/// shared node so every holder observes the mutation; when exclusively owned,
/// plain `Gc::make_mut` access (which does not clone at `strong_count == 1`).
///
/// This is the mutation-side counterpart of `detach_shared_container`: Raku
/// `=` copy semantics are enforced at copy time (detach), so a mutation must
/// never COW-detach the container from its aliases.
///
/// # Safety (inherited)
///
/// The aliased branch is [`crate::gc::gc_contents_mut`] — the caller must uphold
/// the same contract: no other borrow into this node is dereferenced for the
/// lifetime of the returned `&mut`, and concurrent structural mutation from
/// another thread stays routed through the synchronized shared-store lanes.
pub(crate) fn gc_data_mut<T: crate::gc::Trace + Clone + 'static>(
    gc: &mut crate::gc::Gc<T>,
) -> &mut T {
    if crate::gc::Gc::strong_count(gc) > 1 {
        // SAFETY: audited aliased in-place container write per the module
        // contract; callers keep no competing borrow live across the returned
        // `&mut` (single-threaded VM mutation paths).
        unsafe { crate::gc::gc_contents_mut(gc) }
    } else {
        crate::gc::Gc::make_mut(gc)
    }
}
