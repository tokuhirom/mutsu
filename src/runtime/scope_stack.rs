//! Per-call scope stacks ([`ScopeStack`]) — ADR-0078.
//!
//! The VM keeps several stacks of *scope frames* — the names a `for`/`while`
//! body declared, the names a `BlockScope` declared, the parameter names of the
//! loops currently iterating, the env values a loop-local declaration shadowed.
//! Each of them is pushed and popped by block/loop entry and exit, and each of
//! them must be **invisible to a callee**: a routine's own `my $x` must not
//! register in the caller's active block scope and be reverted at the caller's
//! block exit.
//!
//! Every call path used to buy that isolation with `std::mem::take`: swap an
//! empty `Vec` in, keep the caller's in a local, assign it back on return. That
//! costs a 24-byte header move each way, and the assignment back *drops* the
//! callee's vector — freeing a buffer the next call immediately re-allocates.
//! On `fib(22)` those drops ran three times per call.
//!
//! This is [`Locals`](super::locals::Locals)' answer applied to the same
//! problem: one contiguous `Vec`, and a per-call `base` marking where the
//! executing call's frames begin. Isolation is then a pair of integers —
//! [`ScopeStack::push_frame`] moves the base up, [`ScopeStack::pop_frame`]
//! truncates back to it — and the buffer is never handed over, dropped, or
//! re-allocated.
//!
//! Two invariants, mirroring `Locals`:
//!
//! - **The executing call sees exactly `[base ..]`.** `Deref` yields that
//!   window, so `.iter()`, `.last_mut()`, `.is_empty()` and `.len()` all speak
//!   about this call's frames and never reach a suspended caller's.
//!   [`ScopeStack::pop`] is bounded by the base for the same reason: a callee
//!   that pops one frame too many must not eat its caller's.
//! - **A frame handle is not `Copy`,** and `pop_frame` consumes it. Closing
//!   twice would truncate the *caller's* frames, and several call paths close on
//!   more than one exclusive exit branch — the move is what proves the branches
//!   really are exclusive. (This is exactly the bug `Locals` found the hard way;
//!   see ADR-0077.)
//!
//! A GC root scan must visit [`ScopeStack::all_frames`], not the `Deref`
//! window — the window is one call, and suspended callers below it hold live
//! values too. `loop_local_saved_env` is the stack where that matters.

/// A handle to the scope frames that belonged to the call which was executing
/// when a new call opened its own.
///
/// Deliberately **not `Copy`**: [`ScopeStack::pop_frame`] consumes it, so the
/// compiler enforces one close per open.
#[must_use = "an opened scope frame must be closed with ScopeStack::pop_frame"]
#[derive(Debug)]
pub(crate) struct ScopeFrame(usize);

/// A stack of scope frames shared by every live call, plus the executing
/// call's base.
#[derive(Debug)]
pub(crate) struct ScopeStack<T> {
    /// Scope frames, oldest call first. The executing call owns
    /// `frames[base ..]`; everything below belongs to a suspended caller.
    frames: Vec<T>,
    /// Index of the executing call's first frame.
    base: usize,
}

impl<T> Default for ScopeStack<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T> ScopeStack<T> {
    /// An empty stack owned by a call with no frames yet.
    #[inline]
    pub(crate) fn new() -> Self {
        ScopeStack {
            frames: Vec::new(),
            base: 0,
        }
    }

    /// Hide the executing call's frames from a callee and return the handle
    /// that restores them. This is what `mem::take(&mut self.field)` meant
    /// before the calls shared a stack — but it moves no buffer, so the callee
    /// inherits the caller's spare capacity instead of allocating its own.
    #[inline]
    pub(crate) fn push_frame(&mut self) -> ScopeFrame {
        let caller_base = self.base;
        self.base = self.frames.len();
        ScopeFrame(caller_base)
    }

    /// Discard whatever frames the executing call still has open and make the
    /// caller's visible again. Consumes the handle: doing this twice would
    /// truncate the caller's own frames.
    #[inline]
    pub(crate) fn pop_frame(&mut self, caller: ScopeFrame) {
        // `Vec::truncate` is out-of-line (it drops a range of `T`s), and the
        // overwhelmingly common case is a callee that opened no scope frame at
        // all -- a leaf routine with no block and no loop. Testing the length
        // first keeps that case to a compare, and was worth 1.6% of `fib(22)`'s
        // profile; the same trick `Locals::push_frame` plays with `resize`.
        if self.frames.len() > self.base {
            self.frames.truncate(self.base);
        }
        self.base = caller.0;
    }

    /// Open a scope frame in the executing call.
    #[inline]
    pub(crate) fn push(&mut self, frame: T) {
        self.frames.push(frame);
    }

    /// Close the innermost scope frame *of the executing call*, or `None` when
    /// it has none open. The base is the floor: a callee that pops one frame
    /// too many must not close a suspended caller's scope.
    #[inline]
    pub(crate) fn pop(&mut self) -> Option<T> {
        if self.frames.len() > self.base {
            self.frames.pop()
        } else {
            None
        }
    }

    /// Every frame of every live call. **This, not the `Deref` window, is what
    /// a GC root scan must visit** — the window covers one call, and suspended
    /// callers below it hold live values.
    #[inline]
    pub(crate) fn all_frames(&self) -> &[T] {
        &self.frames
    }
}

impl<T> std::ops::Deref for ScopeStack<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &[T] {
        &self.frames[self.base..]
    }
}

impl<T> std::ops::DerefMut for ScopeStack<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut [T] {
        &mut self.frames[self.base..]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_call_sees_only_its_own_frames() {
        let mut s: ScopeStack<i32> = ScopeStack::new();
        s.push(1);
        s.push(2);
        assert_eq!(&*s, &[1, 2]);

        let caller = s.push_frame();
        assert!(s.is_empty(), "a callee starts with no scope frames");
        assert_eq!(s.last(), None);
        s.push(9);
        assert_eq!(&*s, &[9]);

        s.pop_frame(caller);
        assert_eq!(&*s, &[1, 2], "the caller's frames come back untouched");
    }

    /// The floor that `mem::take` used to provide for free: a callee holding an
    /// empty window cannot pop into its caller's frames.
    #[test]
    fn pop_stops_at_the_base() {
        let mut s: ScopeStack<i32> = ScopeStack::new();
        s.push(1);
        let caller = s.push_frame();
        assert_eq!(s.pop(), None);
        assert_eq!(s.pop(), None);
        s.pop_frame(caller);
        assert_eq!(&*s, &[1], "the caller's frame survived the callee's pops");
    }

    /// Unbalanced frames left open by an early exit are discarded by the close,
    /// exactly as dropping the callee's vector used to discard them.
    #[test]
    fn closing_discards_frames_the_callee_left_open() {
        let mut s: ScopeStack<i32> = ScopeStack::new();
        s.push(1);
        let caller = s.push_frame();
        s.push(7);
        s.push(8);
        s.pop_frame(caller);
        assert_eq!(&*s, &[1]);
        assert_eq!(s.all_frames(), &[1]);
    }

    /// The GC hazard: `Deref` is one call's window, so a root scan reading
    /// through it would miss every suspended caller's frames.
    #[test]
    fn all_frames_spans_every_call_not_just_the_window() {
        let mut s: ScopeStack<i32> = ScopeStack::new();
        s.push(1);
        let outer = s.push_frame();
        s.push(2);
        let inner = s.push_frame();
        s.push(3);

        assert_eq!(&*s, &[3], "the window is the executing call");
        assert_eq!(s.all_frames(), &[1, 2, 3], "roots span every call");

        s.pop_frame(inner);
        assert_eq!(&*s, &[2]);
        s.pop_frame(outer);
        assert_eq!(&*s, &[1]);
        assert_eq!(s.all_frames(), &[1], "the stack drains with the frames");
    }

    #[test]
    fn the_window_is_mutable_through_deref_mut() {
        let mut s: ScopeStack<i32> = ScopeStack::new();
        s.push(1);
        let caller = s.push_frame();
        s.push(5);
        if let Some(top) = s.last_mut() {
            *top = 6;
        }
        assert_eq!(&*s, &[6]);
        s.pop_frame(caller);
        assert_eq!(
            &*s,
            &[1],
            "writing through the window cannot reach a caller"
        );
    }
}
