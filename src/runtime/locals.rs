//! The frame slot stack ([`Locals`]) — ADR-0077.
//!
//! Every live frame's slots live in **one contiguous `Vec<Value>`**, and the
//! executing frame is the window `[base ..]` at the top of it. A call extends
//! the vector (amortized O(1), no allocation once warm); a return truncates it.
//! There is no per-frame vector, no free list, and no `mem::take` of the slot
//! array — the caller's slots are simply the region below the callee's base.
//!
//! Slice 0 introduced this type as a `#[repr(transparent)]` newtype over the
//! pooled `Vec<Value>` so the ~330 `self.locals[i]` sites and the ~100 that
//! read through `Deref` would not have to change when the inside did. This is
//! that change, and those sites indeed did not move.
//!
//! Three invariants the rest of the VM depends on:
//!
//! - **The executing frame is always the top region.** [`Self::push_frame`]
//!   sets `base` to the current length, so `[base ..]` is exactly this frame
//!   and nothing else. `Index` and `Deref` are defined in those terms.
//! - **A frame handle is an index, never a pointer.** Pushing a frame can
//!   reallocate the vector, so nothing — Rust or JIT-emitted — may cache the
//!   data pointer across a push. The JIT reloads it per access already
//!   (`vm_jit_tier_b`'s GetLocal fast path), which is what makes this cheap.
//! - **An enclosing frame is reached only through its base**, with
//!   [`Self::frame_slots`], which stops at the executing frame's base. Reading
//!   past a lower frame's length must not silently reach into the frame above
//!   it.
//!
//! GC roots must visit [`Self::all_slots`], not the `Deref` window: the window
//! is one frame, and every frame below it holds live values too.

use crate::value::Value;

/// A handle to the frame that was executing when a frame was opened.
///
/// Deliberately **not `Copy`**: [`Locals::pop_frame`] consumes it, so the
/// compiler enforces one close per open exactly as the moved `Vec<Value>` did
/// before frames shared a stack. That protection matters, because closing a
/// frame twice is silently destructive — the second close truncates the
/// *caller's* slots — and several call paths close on more than one exclusive
/// exit branch, where only the move proves the branches really are exclusive.
#[must_use = "an opened frame must be closed with Locals::pop_frame"]
#[derive(Debug)]
pub(crate) struct CallerFrame(usize);

impl CallerFrame {
    /// The caller's base, for the cross-frame sites that only need to *read*
    /// where a saved frame starts. Reading is unrestricted; closing is not.
    #[inline]
    pub(crate) fn base(&self) -> usize {
        self.0
    }
}

/// All live frames' slots, plus the executing frame's base.
#[derive(Debug, Default)]
pub(crate) struct Locals {
    /// Frame slots, oldest frame first. The executing frame occupies
    /// `slots[base ..]`; everything below belongs to suspended callers.
    slots: Vec<Value>,
    /// Index of the executing frame's slot 0.
    base: usize,
}

impl Clone for Locals {
    #[inline]
    fn clone(&self) -> Self {
        Locals {
            slots: self.slots.clone(),
            base: self.base,
        }
    }

    /// Hand-written so it keeps `Vec`'s buffer reuse; `#[derive(Clone)]` would
    /// inherit the allocating default (`*self = source.clone()`).
    #[inline]
    fn clone_from(&mut self, source: &Self) {
        self.slots.clone_from(&source.slots);
        self.base = source.base;
    }
}

impl Locals {
    /// Byte offset of the slot vector inside `Locals`, for the Tier B JIT
    /// emitter, which loads the `Vec` header words itself. Kept here because
    /// the fields are private (see `vm_jit_layout`). Gated like its only
    /// consumer: with `jit` off, `vm_jit_layout` is not compiled and these
    /// would be dead code — a warning only the `--no-default-features
    /// --features native` lint configuration can see.
    #[cfg(feature = "jit")]
    pub(crate) const SLOTS_BYTE_OFFSET: usize = std::mem::offset_of!(Locals, slots);
    /// Byte offset of the executing frame's base, for the same reason.
    #[cfg(feature = "jit")]
    pub(crate) const BASE_BYTE_OFFSET: usize = std::mem::offset_of!(Locals, base);

    /// An empty stack with no frame.
    #[inline]
    pub(crate) fn new() -> Self {
        Locals {
            slots: Vec::new(),
            base: 0,
        }
    }

    /// Open a frame of `num_locals` `Nil` slots on top of the stack and return
    /// the **caller's base**, to be handed back to [`Self::pop_frame`]. This
    /// replaces both `mem::take(&mut self.locals)` (`push_frame(0)`) and the
    /// old locals pool (`push_frame(n)`).
    #[inline]
    pub(crate) fn push_frame(&mut self, num_locals: usize) -> CallerFrame {
        let caller_base = self.base;
        self.base = self.slots.len();
        self.slots.resize(self.base + num_locals, Value::NIL);
        CallerFrame(caller_base)
    }

    /// Open a frame holding a copy of `slots` — restoring an owned snapshot
    /// (a suspended `gather` coroutine, a hyper/race worker's seed frame).
    #[inline]
    pub(crate) fn push_frame_from(&mut self, slots: &[Value]) -> CallerFrame {
        let caller_base = self.base;
        self.base = self.slots.len();
        self.slots.extend_from_slice(slots);
        CallerFrame(caller_base)
    }

    /// Seed the *root* frame of an interpreter whose frame stack starts empty —
    /// a worker VM that is discarded whole when its task ends, so this frame is
    /// never closed and there is no handle to return.
    #[inline]
    pub(crate) fn install_root_frame(&mut self, slots: &[Value]) {
        debug_assert!(
            self.slots.is_empty() && self.base == 0,
            "install_root_frame is for a fresh stack; use push_frame_from otherwise"
        );
        self.slots.extend_from_slice(slots);
    }

    /// Drop the executing frame's slots and make the caller's frame current
    /// again. Consumes the handle, because doing this twice would truncate the
    /// caller's own slots.
    #[inline]
    pub(crate) fn pop_frame(&mut self, caller: CallerFrame) {
        self.slots.truncate(self.base);
        self.base = caller.0;
    }

    /// Replace the executing frame with `num_locals` fresh `Nil` slots, keeping
    /// its base. This is what "install a new slot array in the current frame"
    /// (`self.locals = vec![Nil; n]`) means once frames share a stack.
    #[inline]
    pub(crate) fn refill_slots(&mut self, num_locals: usize) {
        self.slots.truncate(self.base);
        self.slots.resize(self.base + num_locals, Value::NIL);
    }

    /// Replace the executing frame's slots with a copy of `slots`, keeping its
    /// base.
    #[inline]
    pub(crate) fn refill_from(&mut self, slots: &[Value]) {
        self.slots.truncate(self.base);
        self.slots.extend_from_slice(slots);
    }

    /// Resize the executing frame to `num_locals` slots, *keeping* the values
    /// of the slots that survive — `Vec::resize` semantics, unlike
    /// [`Self::refill_slots`]. The top-level `run` entry needs this: it sizes
    /// the program frame and then seeds slots from `env`, and a re-entrant run
    /// must not lose the slots already there.
    #[inline]
    pub(crate) fn resize_slots(&mut self, num_locals: usize) {
        self.slots.resize(self.base + num_locals, Value::NIL);
    }

    /// Copy the executing frame's slots out, for a snapshot that outlives it.
    #[inline]
    pub(crate) fn to_vec(&self) -> Vec<Value> {
        self.slots[self.base..].to_vec()
    }

    /// The executing frame's base, i.e. the handle [`Self::push_frame`] would
    /// hand back. Needed to bound the topmost *saved* frame's region.
    #[inline]
    pub(crate) fn base(&self) -> usize {
        self.base
    }

    /// The slots of the frame based at `base`, which must be the frame directly
    /// below the executing one: the region stops at the executing frame's base,
    /// so an out-of-range slot cannot reach into the frame above it.
    #[inline]
    pub(crate) fn frame_slots(&self, caller: &CallerFrame) -> &[Value] {
        &self.slots[caller.0..self.base]
    }

    /// One slot by *absolute* stack index, for the cross-frame propagation sites
    /// (a shared container has to reach the same lexical in every suspended
    /// frame that owns it). The caller derives the index from a frame's region,
    /// which is why this cannot bound-check the frame for you — see
    /// `propagate_shared_container_to_frames`.
    #[inline]
    pub(crate) fn absolute_slot_mut(&mut self, index: usize) -> &mut Value {
        &mut self.slots[index]
    }

    /// Every live slot in every frame. **This, not the `Deref` window, is what
    /// a GC root scan must visit** — the window covers one frame, and the
    /// frames below it hold live values.
    #[inline]
    pub(crate) fn all_slots(&self) -> &[Value] {
        &self.slots
    }
}

impl std::ops::Index<usize> for Locals {
    type Output = Value;

    #[inline]
    fn index(&self, slot: usize) -> &Value {
        &self.slots[self.base + slot]
    }
}

impl std::ops::IndexMut<usize> for Locals {
    #[inline]
    fn index_mut(&mut self, slot: usize) -> &mut Value {
        &mut self.slots[self.base + slot]
    }
}

impl std::ops::Deref for Locals {
    type Target = [Value];

    #[inline]
    fn deref(&self) -> &[Value] {
        &self.slots[self.base..]
    }
}

impl std::ops::DerefMut for Locals {
    #[inline]
    fn deref_mut(&mut self) -> &mut [Value] {
        &mut self.slots[self.base..]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ints(l: &Locals) -> Vec<Option<i64>> {
        l.iter().map(|v| v.as_int()).collect()
    }

    #[test]
    fn a_frame_is_the_top_window() {
        let mut l = Locals::new();
        let outer = l.push_frame(2);
        assert_eq!(outer.base(), 0);
        l[0] = Value::int(1);
        l[1] = Value::int(2);
        assert_eq!(ints(&l), vec![Some(1), Some(2)]);

        // A callee's frame hides the caller's, and indexing is frame-relative.
        let caller = l.push_frame(1);
        assert_eq!(l.len(), 1);
        l[0] = Value::int(9);
        assert_eq!(ints(&l), vec![Some(9)]);

        // The caller's slots are still there, reachable only through its base,
        // and the region stops at the callee's base — the callee's own slot 0
        // must not be reachable as the caller's slot 2.
        let outer_slots: Vec<_> = l.frame_slots(&caller).iter().map(|v| v.as_int()).collect();
        assert_eq!(outer_slots, vec![Some(1), Some(2)]);

        l.pop_frame(caller);
        assert_eq!(ints(&l), vec![Some(1), Some(2)]);
        l.pop_frame(outer);
        assert_eq!(l.len(), 0);
    }

    #[test]
    fn push_frame_zero_is_an_empty_window_over_a_live_caller() {
        let mut l = Locals::new();
        let outer = l.push_frame(1);
        l[0] = Value::int(7);
        // What `mem::take(&mut self.locals)` used to do: the frame reads empty.
        let caller = l.push_frame(0);
        assert!(l.is_empty());
        // The callee then sizes its own frame in place.
        l.refill_slots(2);
        assert_eq!(ints(&l), vec![None, None]);
        l.pop_frame(caller);
        assert_eq!(ints(&l), vec![Some(7)]);
        l.pop_frame(outer);
    }

    #[test]
    fn refill_replaces_and_resize_keeps() {
        let mut l = Locals::new();
        let _root = l.push_frame(2);
        l[0] = Value::int(1);
        l[1] = Value::int(2);

        l.resize_slots(3);
        assert_eq!(
            ints(&l),
            vec![Some(1), Some(2), None],
            "resize keeps values"
        );

        l.refill_slots(2);
        assert_eq!(ints(&l), vec![None, None], "refill drops them");

        l.refill_from(&[Value::int(5)]);
        assert_eq!(ints(&l), vec![Some(5)]);
    }

    /// The GC hazard this slice introduces: `Deref` is one frame, so a root
    /// scan that reads through it would miss every suspended caller's slots.
    #[test]
    fn all_slots_spans_every_frame_not_just_the_window() {
        let mut l = Locals::new();
        let _root = l.push_frame(2);
        l[0] = Value::int(1);
        l[1] = Value::int(2);
        let caller = l.push_frame(1);
        l[0] = Value::int(3);

        assert_eq!(l.len(), 1, "the window is the executing frame");
        assert_eq!(l.all_slots().len(), 3, "roots span all frames");
        let all: Vec<_> = l.all_slots().iter().map(|v| v.as_int()).collect();
        assert_eq!(all, vec![Some(1), Some(2), Some(3)]);
        l.pop_frame(caller);
    }

    #[test]
    fn snapshots_round_trip_through_an_owned_vec() {
        let mut l = Locals::new();
        let _root = l.push_frame(2);
        l[0] = Value::int(1);
        l[1] = Value::int(2);
        let snap = l.to_vec();
        assert_eq!(
            snap.len(),
            2,
            "a snapshot is the frame, not the whole stack"
        );

        let caller = l.push_frame_from(&snap);
        assert_eq!(ints(&l), vec![Some(1), Some(2)]);
        l.pop_frame(caller);
        assert_eq!(ints(&l), vec![Some(1), Some(2)]);
    }

    /// Closing a frame restores the caller's slots exactly, at any depth. The
    /// handle is consumed, so closing twice is a compile error rather than the
    /// silent truncation of the caller's own slots it would otherwise be —
    /// several call paths close on more than one exclusive exit branch, and that
    /// move is what proves the branches are exclusive.
    #[test]
    fn closing_a_frame_restores_the_caller_at_any_depth() {
        let mut l = Locals::new();
        let f0 = l.push_frame(1);
        l[0] = Value::int(4);
        let f1 = l.push_frame(2);
        l[0] = Value::int(5);
        let f2 = l.push_frame(0);
        l.refill_slots(1);
        l[0] = Value::int(6);
        assert_eq!(l.all_slots().len(), 4);

        l.pop_frame(f2);
        assert_eq!(ints(&l), vec![Some(5), None]);
        l.pop_frame(f1);
        assert_eq!(ints(&l), vec![Some(4)]);
        l.pop_frame(f0);
        assert_eq!(l.len(), 0);
        assert_eq!(l.all_slots().len(), 0, "the stack drains with the frames");
    }
}
