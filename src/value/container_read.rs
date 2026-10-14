//! The reader side of ADR-0068: how a read of an `Array`/`Hash` that another
//! thread may be restructuring avoids walking a torn node (§16, §20).
//!
//! `ArrayData` dereferences to a borrowed `&[Value]`, so no lock can cover what
//! a caller does with the borrow. A reader of a node another thread can mutate
//! in place therefore takes one of two shapes, chosen by what it runs:
//!
//! - a **leaf** read (no user code) holds the node's stripe while it walks
//!   ([`super::container_lock::ContainerStructGuard`]);
//! - anything else runs on a [`shared_snapshot`], a shallow copy taken under
//!   the stripe, so user code never runs with a stripe held.
//!
//! Which methods are which is decided here, once, for every method funnel
//! (`method_table::dispatch::call`, `call_method_with_values`, the compiled
//! mut-call tail). Since §20 the snapshot is the DEFAULT: a method runs on the
//! live receiver only when [`reads_live_receiver`] names it -- a mutator, a
//! leaf read, an identity answer or an element-producer view. A reader added
//! later is therefore safe without being listed; only a method that writes
//! through its receiver must be.
//!
//! Every function here is a no-op (one relaxed load) until a second VM mutator
//! thread exists.

use super::container_lock::{ContainerStructGuard, multi_mutator_threads_live};
use crate::value::{ArrayData, HashData, Value, ValueView};

/// Whether `method` structurally mutates its container receiver, i.e. whether
/// two threads running it on one container race on the backing
/// `Vec`/`HashMap`. Its callers hold the receiver's stripe across the call, so
/// the list holds the mutators that run no user code.
// Cost: O(1).
pub(crate) fn is_container_mutator(method: &str) -> bool {
    matches!(
        method,
        "push"
            | "append"
            | "prepend"
            | "unshift"
            | "pop"
            | "shift"
            | "splice"
            | "STORE"
            | "ASSIGN-POS"
            | "ASSIGN-KEY"
            | "DELETE-POS"
            | "DELETE-KEY"
    )
}

/// Whether `method` is a READ of a container's structure that runs no user
/// code: it walks the backing `Vec`/`HashMap`, so it must exclude a concurrent
/// structural writer (a rehash under an iterating `.keys` is a use after free,
/// #11701). The allowlist is the leaf reads only -- a method that takes a
/// callback (`map`, `grep`, `first`, ...) would hold the lock across user code,
/// which the module docs forbid.
///
/// `.keys`/`.values`/`.kv`/`.pairs`/`.antipairs` on an Array answer a lazy
/// view over the live node; each pull of it takes the stripe again
/// (`ListGen::pull_one`, §20), so the view can keep handing out the
/// receiver's own element containers.
// Cost: O(1).
pub(crate) fn is_leaf_structure_read(method: &str) -> bool {
    matches!(
        method,
        "keys"
            | "values"
            | "kv"
            | "pairs"
            | "antipairs"
            | "elems"
            | "end"
            | "list"
            | "Bool"
            | "so"
            | "AT-KEY"
            | "AT-POS"
            | "EXISTS-KEY"
            | "EXISTS-POS"
            | "Numeric"
            | "Int"
    )
}

/// Whether `method` on a shared `Array`/`Hash` runs on the LIVE receiver
/// rather than on a [`shared_snapshot`] of it (ADR-0068 §20). Everything not
/// named here is a read that runs on the snapshot.
// Cost: O(1).
pub(crate) fn reads_live_receiver(method: &str) -> bool {
    is_container_mutator(method)
        || is_leaf_structure_read(method)
        || matches!(
            method,
            // Answer the receiver itself, so a snapshot would break identity
            // (raku: `@a.list === @a`, `%h.Hash === %h`). `.Map` on a Hash is
            // a copy in raku, but mutsu answers the receiver too, and its walk
            // holds the stripe (§19).
            "self"
                | "item"
                | "cache"
                | "eager"
                | "VAR"
                | "Hash"
                | "Map"
                // Read no element at all: a copy would be pure cost.
                | "WHAT"
                | "WHICH"
                | "WHERE"
                | "WHO"
                | "HOW"
                | "WHY"
                | "defined"
                | "DEFINITE"
                | "isa"
                | "does"
                | "can"
                | "sink"
                | "of"
                | "default"
                | "dynamic"
                | "is-lazy"
                // Write through the receiver while running user code, so they
                // can neither hold the stripe nor run on a copy.
                | "BIND-POS"
                | "BIND-KEY"
                | "classify-list"
                | "categorize-list"
        )
}

/// The receiver a `method` call should run on: a [`shared_snapshot`] of
/// `container`, or `None` to run on `container` itself (ADR-0068 §16, §20).
///
/// `.map`/`.grep` on a non-shaped Array are exempt (#12484): their callback
/// rw-binds `$_` to the source element, so they must write back into the LIVE
/// container, which a snapshot cannot (ADR-0068 §16.1). They never walk the
/// live node with nothing held instead: `MapGrepItems::Live` reads it under the
/// stripe, and the pull's element promotion (`grep`) and write-back (`map`)
/// take the stripe around the in-place store, with no user code inside.
// Cost: O(1) when no snapshot is taken; `shared_snapshot`'s otherwise.
pub(crate) fn snapshot_read_receiver(method: &str, container: &Value) -> Option<Value> {
    if !multi_mutator_threads_live() || reads_live_receiver(method) {
        return None;
    }
    if matches!(method, "map" | "grep")
        && matches!(container.view(), ValueView::Array(..))
        && !crate::value::shaped_array::is_shaped_array(container)
    {
        return None;
    }
    shared_snapshot(container)
}

/// A shallow copy of a shared `Hash`/`Array`, taken while holding the stripe,
/// for a method that must then run user code over it with nothing held
/// (ADR-0068 §16). `None` -- run on the original -- when no second mutator
/// thread exists, when this thread already holds a stripe, or when `container`
/// is no `Hash`/`Array`. The elements are cloned `Value`s, so an element that is
/// itself a shared container cell stays shared with the live container.
// Cost: O(n), n = elements, the shallow clone of the backing node.
pub(crate) fn shared_snapshot(container: &Value) -> Option<Value> {
    if !multi_mutator_threads_live() {
        return None;
    }
    match container.view() {
        ValueView::Hash(map) => {
            let _guard = ContainerStructGuard::acquire_for(None, container)?;
            Some(Value::hash_with_data_itemized(
                crate::gc::Gc::new(HashData::clone(&map)),
                container.hash_is_itemized(),
            ))
        }
        ValueView::Array(items, kind) => {
            let _guard = ContainerStructGuard::acquire_for(None, container)?;
            Some(Value::array_with_kind(
                crate::gc::Gc::new(ArrayData::clone(&items)),
                kind,
            ))
        }
        _ => None,
    }
}

/// An operand of a VM op that only READS a container and is already linear in
/// its size (`eqv`, the set operators, `Z`/`X`, a hyper or reduction
/// operand), made safe to walk with nothing held: a [`shared_snapshot`] of a
/// shared `Array`/`Hash` (also one held in a `Scalar`), else the operand
/// itself. The op may run user code over the elements (an element's own
/// `eqv`, a hyper's operator), so it cannot hold the stripe instead.
// Cost: O(1) until a second mutator thread exists; then `shared_snapshot`'s
// for an Array/Hash operand, which the op's own walk already matches.
pub(crate) fn read_operand(operand: Value) -> Value {
    if !multi_mutator_threads_live() {
        return operand;
    }
    if let ValueView::Scalar(inner) = operand.view() {
        return match shared_snapshot(inner) {
            Some(copy) => Value::scalar(copy),
            None => operand,
        };
    }
    shared_snapshot(&operand).unwrap_or(operand)
}
