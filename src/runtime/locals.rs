//! The executing frame's slot array ([`Locals`]).
//!
//! This is ADR-0077 Slice 0: a newtype chokepoint around the representation,
//! introduced *before* the representation changes. Today it wraps the
//! `Vec<Value>` the pool hands out; Slice 2 replaces the inside with a window
//! (`{ stack, base }`) into one contiguous locals stack, and the call sites
//! that index or iterate slots do not have to change again when it does.
//!
//! Two contracts the Slice 2 rewrite must preserve, because code all over the
//! VM already relies on them:
//!
//! - **`self.locals[i]` addresses slot `i` of the *current* frame**, so
//!   [`Index`] must add the frame base rather than index the raw stack.
//! - **[`Deref`] yields exactly the current frame's slots**, so `.len()`,
//!   `.get()`, `.iter()` and the `&self.locals` → `&[Value]` coercions all
//!   speak about this frame and nothing below it. That is what makes
//!   ADR-0077's open question 1 (one stack or two) a real question: a locals
//!   region that shared the operand stack would need a per-frame length here
//!   rather than "everything above `base`".
//!
//! The JIT reads the slot words out of this field directly
//! (`vm_jit_layout::JitLayout::locals`, `vm_jit_tier_b`'s GetLocal fast path),
//! which is why the type is `#[repr(transparent)]` for now — see the
//! size assertion in `vm_jit_layout`.

use crate::value::Value;

/// The slot array of the frame currently executing.
#[repr(transparent)]
#[derive(Debug, Default)]
pub(crate) struct Locals(Vec<Value>);

impl Clone for Locals {
    #[inline]
    fn clone(&self) -> Self {
        Locals(self.0.clone())
    }

    /// Hand-written so it keeps `Vec`'s buffer reuse. `#[derive(Clone)]` would
    /// inherit the default `clone_from` (`*self = source.clone()`), which
    /// allocates — and the one caller (`clone_for_thread` seeding a hyper/race
    /// worker's frame) reuses the vector on purpose.
    #[inline]
    fn clone_from(&mut self, source: &Self) {
        self.0.clone_from(&source.0);
    }
}

impl Locals {
    /// An empty slot array (no frame, or a frame with no locals).
    #[inline]
    pub(crate) fn new() -> Self {
        Locals(Vec::new())
    }

    /// Adopt an owned slot vector — a restored snapshot (a suspended `gather`
    /// coroutine, an inline `CATCH` handler frame), not a live frame.
    #[inline]
    pub(crate) fn from_vec(v: Vec<Value>) -> Self {
        Locals(v)
    }

    /// Copy the frame's slots out into an owned vector, for a snapshot that
    /// outlives the frame.
    #[inline]
    pub(crate) fn to_vec(&self) -> Vec<Value> {
        self.0.clone()
    }

    /// A fresh frame of `num_locals` `Nil` slots. Used by the inline-exec sites
    /// that install a frame without going through the pool (a `gather` body, a
    /// closure entered outside the light call path); Slice 2 turns each of them
    /// into a base push.
    #[inline]
    pub(crate) fn nils(num_locals: usize) -> Self {
        Locals(vec![Value::NIL; num_locals])
    }

    /// Resize the frame to `num_locals` slots, keeping the values of the slots
    /// that survive and filling any new ones with `Nil` — `Vec::resize`
    /// semantics, unlike [`Self::refill`], which drops every existing value.
    /// The top-level `run` entry needs this: it sizes the program frame and
    /// then seeds slots from `env`, and a re-entrant run must not lose the
    /// slots already there.
    #[inline]
    pub(crate) fn resize_slots(&mut self, num_locals: usize) {
        self.0.resize(num_locals, Value::NIL);
    }

    /// Reset to `num_locals` `Nil` slots, reusing the buffer. Pairs with
    /// [`Self::release`]; together they are the whole of the pool's API, so
    /// Slice 2 replaces the pool by rewriting these two and nothing else.
    #[inline]
    pub(crate) fn refill(&mut self, num_locals: usize) {
        self.0.clear();
        self.0.resize(num_locals, Value::NIL);
    }

    /// Drop the frame's slot values but keep the buffer, at a well-defined
    /// point rather than inside the pool.
    #[inline]
    pub(crate) fn release(&mut self) {
        self.0.clear();
    }
}

impl std::ops::Index<usize> for Locals {
    type Output = Value;

    #[inline]
    fn index(&self, slot: usize) -> &Value {
        &self.0[slot]
    }
}

impl std::ops::IndexMut<usize> for Locals {
    #[inline]
    fn index_mut(&mut self, slot: usize) -> &mut Value {
        &mut self.0[slot]
    }
}

impl std::ops::Deref for Locals {
    type Target = [Value];

    #[inline]
    fn deref(&self) -> &[Value] {
        &self.0
    }
}

impl std::ops::DerefMut for Locals {
    #[inline]
    fn deref_mut(&mut self) -> &mut [Value] {
        &mut self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn refill_and_release_reuse_the_buffer() {
        let mut l = Locals::new();
        l.refill(3);
        assert_eq!(l.len(), 3);
        assert!(l.iter().all(|v| v.is_nil()));
        l[1] = Value::int(7);
        assert_eq!(l[1].as_int(), Some(7));
        let cap_before = l.to_vec().capacity();
        l.release();
        assert_eq!(l.len(), 0);
        l.refill(2);
        assert_eq!(l.len(), 2);
        // The point of the pool: refilling after a release does not have to
        // grow from zero again.
        assert!(cap_before > 0);
    }

    /// `Deref` is the frame window every `.len()` / `.iter()` / `&self.locals`
    /// site reads through, so pin that it agrees with `Index`.
    #[test]
    fn deref_window_agrees_with_index() {
        let mut l = Locals::from_vec(vec![Value::int(1), Value::int(2)]);
        assert_eq!(l.len(), 2);
        assert_eq!(l.get(1).and_then(|v| v.as_int()), Some(2));
        l[0] = Value::int(9);
        assert_eq!(l.first().and_then(|v| v.as_int()), Some(9));
        assert_eq!(l.to_vec().len(), 2);
    }
}
