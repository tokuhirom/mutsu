//! Bounded-depth release of nested values (#11217).
//!
//! Dropping the last reference to a container drops its elements, and an
//! element that is itself the last reference to a container drops *its*
//! elements, all through ordinary Rust `Drop` -- one native frame chain per
//! nesting level. A structure built at run time, `$a = [$a]` a million times,
//! walked the thread off its stack (SIGSEGV) when it died. The ADR-0100 guard
//! cannot help: the whole recursion sits inside a single Raku statement,
//! between two call-boundary checks.
//!
//! So [`release_nested`] counts the releases in progress on this thread, and
//! once [`MAX_NESTED_RELEASES`] are active it parks the word on a thread-local
//! work list instead of releasing it. The outermost release drains the list
//! after its own payload is gone, each parked word starting a fresh, again
//! bounded chain. The native depth is then O(`MAX_NESTED_RELEASES`) whatever
//! the data's depth; the work list holds at most one word per level of the
//! structure.
//!
//! Only the order in which already-dead payloads are freed changes, and only
//! past the threshold: every reference count is decremented exactly as
//! before, and a deferred word is released before the outermost drop returns.

use std::cell::{Cell, RefCell};

use super::{Kind, NanBox, PayloadOp, payload_op};

/// Releases allowed in progress on one thread before further ones are
/// deferred. Each level of a dying `Array` costs a few hundred bytes of frames
/// (`NanBox::drop` -> `Gc::drop` -> `Arc::drop_slow` -> `Vec` drop), so this
/// keeps a release chain well under a megabyte even in a debug build.
const MAX_NESTED_RELEASES: u32 = 256;

thread_local! {
    /// Releases of nesting kinds currently on this thread's stack.
    static DEPTH: Cell<u32> = const { Cell::new(0) };
    /// Words whose release was deferred because [`DEPTH`] hit the limit.
    static DEFERRED: RefCell<Vec<NanBox>> = const { RefCell::new(Vec::new()) };
}

/// Whether releasing a word of this kind can drop further `Value`s, i.e. can
/// recurse. Leaf payloads (numbers, strings, buffers) release in O(1) native
/// depth and skip the bookkeeping.
#[inline(always)]
pub(super) const fn kind_may_nest(kind: Kind) -> bool {
    !matches!(
        kind,
        Kind::Str
            | Kind::Regex
            | Kind::BigInt
            | Kind::IntBoxed
            | Kind::Range
            | Kind::RangeExcl
            | Kind::RangeExclStart
            | Kind::RangeExclBoth
            | Kind::Rat
            | Kind::FatRat
            | Kind::BigRat
            | Kind::Complex
            | Kind::Version
            | Kind::Uni
            | Kind::BufStorage
            | Kind::WeakSub
    )
}

/// Release the payload owned by `bits`, deferring it when too many releases
/// are already nested on this thread.
///
/// # Safety
/// `bits` must be a live word of `kind` whose ownership is being consumed
/// (exactly once), as for `payload_op(.., PayloadOp::Release)`.
// Cost: O(1) amortized per released word; native depth O(MAX_NESTED_RELEASES).
#[inline(never)]
pub(super) unsafe fn release_nested(kind: Kind, bits: u64) {
    // During thread teardown the thread-locals may already be gone; release
    // directly then (the recursion is what it was before this guard).
    let Ok(depth) = DEPTH.try_with(Cell::get) else {
        // SAFETY: forwarded from the caller.
        unsafe { payload_op(kind, bits, PayloadOp::Release) };
        return;
    };
    if depth >= MAX_NESTED_RELEASES {
        // SAFETY: the caller hands over this word's ownership; the parked
        // `NanBox` now owns it and releases it when the work list drops it.
        let word = NanBox(unsafe { std::num::NonZeroU64::new_unchecked(bits) });
        // If the work list is already torn down the closure is dropped
        // uncalled, which drops (releases) `word` there: never a leak or a
        // double release.
        let _ = DEFERRED.try_with(|q| q.borrow_mut().push(word));
        return;
    }
    DEPTH.set(depth + 1);
    // SAFETY: forwarded from the caller.
    unsafe { payload_op(kind, bits, PayloadOp::Release) };
    if depth == 0 {
        drain_deferred();
    }
    DEPTH.set(depth);
}

/// Release every parked word, each from depth 1 so the drain itself never
/// recurses into another drain. Runs only from the outermost release, with
/// [`DEPTH`] at 1.
fn drain_deferred() {
    while let Some(word) = DEFERRED.with(|q| q.borrow_mut().pop()) {
        drop(word);
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;

    /// Builds `[[[...]]]` `depth` levels deep and drops it.
    fn nested_array(depth: usize) -> Value {
        let mut v = Value::array(Vec::new());
        for _ in 0..depth {
            v = Value::array(vec![v]);
        }
        v
    }

    #[test]
    fn dropping_a_deep_array_does_not_overflow() {
        // Far past what the default 2 MiB test-thread stack survives with
        // recursive drops.
        drop(nested_array(200_000));
        super::DEFERRED.with(|q| assert!(q.borrow().is_empty()));
        super::DEPTH.with(|d| assert_eq!(d.get(), 0));
    }

    #[test]
    fn shared_inner_value_survives_outer_drop() {
        let inner = nested_array(1_000);
        let outer = Value::array(vec![inner.clone(), nested_array(1_000)]);
        drop(outer);
        // `inner` is still a live 1000-deep structure.
        drop(inner);
        super::DEFERRED.with(|q| assert!(q.borrow().is_empty()));
    }
}
