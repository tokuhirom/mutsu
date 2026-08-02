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
//! # No container is `Arc`-backed for an aliased write any more
//!
//! There used to be an `arc_contents_mut` here — the `Arc` counterpart of the
//! GC primitive — kept alive by one call site: the `Mixin` overrides map,
//! written in place by `$type.^set_name(...)`. An `Arc` payload has no
//! `UnsafeCell`, so that cast was the same provenance violation ADR-0013
//! removed everywhere else. `ValueRepr::Mixin`'s overrides map is now a
//! `Gc<MixinOverrides>` node, the write routes through
//! [`crate::gc::gc_contents_mut`], and the `Arc` primitive is gone.
//!
//! Do not reintroduce an `as_ptr as *mut` cast: for a `Gc` container route the
//! write through the GC primitive, and do not give a new container the `Arc`
//! shape that would force one.

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
