//! The generation store a [`super::SeqBody`] reifies into, and the claim that
//! serializes its non-consuming pulls across threads (#11242).
//!
//! # Generations
//!
//! Every alias of a `Value::Seq` shares one body and reads its elements
//! through a plain `&[Value]` (`Deref`), so a pull can never move or
//! overwrite what a reader may still be holding. The elements live in a
//! [`Generation`]: a buffer whose first `len` slots are initialized and never
//! written again. A prefix pull that fits the buffer's spare capacity writes
//! the new elements past `len` and then publishes the longer `len`; a reader
//! that loaded the old `len` keeps reading its shorter, untouched prefix. A
//! pull that does not fit starts a new generation with twice the capacity,
//! so a Seq read one element at a time copies O(n) elements in total, not
//! O(n^2) (#13023). A wholesale replacement ([`Generations::publish`]) also
//! starts a new generation. Superseded generations stay in the append-only
//! graveyard until the body itself is dropped, and `live` is swung to the
//! newest. A reader loads `live` alone and never touches the graveyard's own
//! `Vec`, whose buffer a later push may reallocate; that is what makes a read
//! on one thread sound while another thread publishes.
//!
//! # Pull claims
//!
//! A non-consuming pull moves the source out of the state lock and runs it
//! (possibly VM code) without the lock. Another thread arriving meanwhile
//! would read the moved-out source as `Taken` — "consumed" — although the
//! Seq was `.cache`d and the pull will store everything it produces. So the
//! pulling thread records itself as the body's `puller`, and every other
//! thread that takes the state lock waits until the pull is over
//! ([`super::SeqBody::lock_state`]). The puller itself never waits: a pull
//! that re-enters the same Seq keeps seeing `Taken`, exactly as before.

use super::{SeqCore, SeqState};
use crate::value::Value;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::sync::{Mutex, MutexGuard, PoisonError};
use std::thread::ThreadId;

/// One element buffer; see the module docs. The allocation is a `Vec<Value>`
/// taken apart, so that slots past `len` can be written while readers borrow
/// the slots before it. Its fields are atomics, so it is `Send`/`Sync`
/// without an `unsafe impl`; that is sound only because `Value` itself is,
/// which the assertion below pins.
struct Generation {
    ptr: AtomicPtr<Value>,
    cap: usize,
    /// Initialized slots. Only grows, and only under the graveyard lock.
    len: AtomicUsize,
}

const _: () = {
    const fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<Value>();
};

impl Generation {
    // Cost: O(1).
    fn from_vec(items: Vec<Value>) -> Self {
        let mut items = std::mem::ManuallyDrop::new(items);
        Generation {
            ptr: AtomicPtr::new(items.as_mut_ptr()),
            cap: items.capacity(),
            len: AtomicUsize::new(items.len()),
        }
    }

    /// The initialized prefix as of this call.
    // Cost: O(1).
    fn items(&self) -> &[Value] {
        let len = self.len.load(Ordering::Acquire);
        // SAFETY: `ptr` is the buffer of a `Vec<Value>` with capacity `cap`
        // that this generation owns until it is dropped, so it outlives the
        // `&self` borrow the slice is tied to. Its first `len` slots were
        // initialized before the `Release` store of `len` this `Acquire` load
        // pairs with, and a slot is never written again once it is counted,
        // so no `&mut` to them can exist while this slice does.
        unsafe { std::slice::from_raw_parts(self.ptr.load(Ordering::Relaxed), len) }
    }

    /// Append `items` in place when they fit the spare capacity. Must be
    /// called with the graveyard lock held (the only writer). Returns them
    /// back when they do not fit.
    // Cost: O(k), k = items appended.
    fn try_append(&self, items: Vec<Value>) -> Result<(), Vec<Value>> {
        let len = self.len.load(Ordering::Relaxed);
        if self.cap - len < items.len() {
            return Err(items);
        }
        let base = self.ptr.load(Ordering::Relaxed);
        let added = items.len();
        for (i, item) in items.into_iter().enumerate() {
            // SAFETY: `len + i < cap`, so the slot is inside the allocation,
            // and it is past `len`, so no reader has a reference to it (they
            // only ever borrow `[0, len)`). The graveyard lock makes this the
            // only writer. `write` does not drop the uninitialized slot.
            unsafe { base.add(len + i).write(item) };
        }
        self.len.store(len + added, Ordering::Release);
        Ok(())
    }
}

impl Drop for Generation {
    fn drop(&mut self) {
        // SAFETY: the parts are those of the `Vec` `from_vec` took apart,
        // with exactly `len` slots initialized; `&mut self` means no reader
        // is left.
        drop(unsafe { Vec::from_raw_parts(*self.ptr.get_mut(), *self.len.get_mut(), self.cap) });
    }
}

/// Append-only store of a Seq body's generations; see the module docs.
pub(super) struct Generations {
    /// The newest generation, pointing into a `Box` the graveyard owns, or
    /// null while nothing was published yet (an unpulled deferred body).
    live: AtomicPtr<Generation>,
    graveyard: Mutex<Vec<Box<Generation>>>,
}

impl Generations {
    /// A store with no generation yet.
    // Cost: O(1).
    pub(super) fn empty() -> Self {
        Generations {
            live: AtomicPtr::new(std::ptr::null_mut()),
            graveyard: Mutex::new(Vec::new()),
        }
    }

    /// A store whose first generation is `items`.
    // Cost: O(1).
    pub(super) fn with(items: Vec<Value>) -> Self {
        let generations = Self::empty();
        generations.publish(items);
        generations
    }

    /// The newest generation's elements.
    // Cost: O(1).
    pub(super) fn live(&self) -> &[Value] {
        let ptr = self.live.load(Ordering::Acquire);
        if ptr.is_null() {
            return &[];
        }
        // SAFETY: `ptr` came from a `Box` the graveyard keeps until `self` is
        // dropped, so it outlives the `&self` borrow this reference is tied
        // to, and the `Acquire` load pairs with the `Release` store in
        // `install`, so the generation is fully constructed.
        unsafe { &*ptr }.items()
    }

    // Cost: O(1) amortized.
    fn graveyard(&self) -> MutexGuard<'_, Vec<Box<Generation>>> {
        self.graveyard
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
    }

    /// Make `generation` the newest one, under the graveyard lock, so two
    /// publishers' stores land in the order of their pushes: `live` is always
    /// the last entry.
    // Cost: O(1) amortized.
    fn install(&self, graveyard: &mut Vec<Box<Generation>>, generation: Generation) {
        let generation = Box::new(generation);
        let ptr = std::ptr::from_ref::<Generation>(&generation).cast_mut();
        graveyard.push(generation);
        self.live.store(ptr, Ordering::Release);
    }

    /// Make `items` the newest generation. Earlier generations stay alive
    /// (and unchanged) for every reference still borrowing them.
    // Cost: O(1) amortized.
    pub(super) fn publish(&self, items: Vec<Value>) {
        let mut graveyard = self.graveyard();
        self.install(&mut graveyard, Generation::from_vec(items));
    }

    /// Append `items` to the newest generation: in place when its buffer has
    /// room, else into a new generation of at least twice the capacity, so
    /// repeated small appends copy each element O(1) times amortized.
    // Cost: O(k) amortized, k = items appended.
    pub(super) fn append(&self, items: Vec<Value>) {
        if items.is_empty() {
            return;
        }
        let mut graveyard = self.graveyard();
        let items = match graveyard.last() {
            Some(newest) => match newest.try_append(items) {
                Ok(()) => return,
                Err(items) => items,
            },
            None => items,
        };
        let current = graveyard.last().map_or(&[][..], |g| g.items());
        let cap = (current.len() + items.len()).max(current.len() * 2).max(8);
        let mut grown = Vec::with_capacity(cap);
        grown.extend_from_slice(current);
        grown.extend(items);
        self.install(&mut graveyard, Generation::from_vec(grown));
    }

    /// Visit every element of every generation (GC tracing).
    // Cost: O(g), g = elements over all generations.
    pub(super) fn for_each_value(&self, visit: &mut dyn FnMut(&Value)) {
        let graveyard = self.graveyard();
        for generation in graveyard.iter() {
            generation.items().iter().for_each(&mut *visit);
        }
    }
}

/// Held for the duration of one non-consuming pull; dropping it (normally,
/// on an error return, or while unwinding) ends the pull and wakes the
/// threads waiting on it.
pub(super) struct PullClaim<'a> {
    core: &'a SeqCore,
}

impl Drop for PullClaim<'_> {
    fn drop(&mut self) {
        self.core
            .state
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .puller = None;
        self.core.pulled.notify_all();
    }
}

/// Record the calling thread as `state`'s puller. `None` when it already is
/// (a re-entrant pull on the same thread): the outer claim ends the pull.
// Cost: O(1).
pub(super) fn claim_pull<'a>(core: &'a SeqCore, state: &mut SeqState) -> Option<PullClaim<'a>> {
    if state.puller.is_some() {
        return None;
    }
    state.puller = Some(std::thread::current().id());
    Some(PullClaim { core })
}

/// Whether a thread other than the caller is pulling a body in `state`.
// Cost: O(1).
fn pulled_elsewhere(puller: Option<ThreadId>) -> bool {
    puller.is_some_and(|id| id != std::thread::current().id())
}

/// Lock `core`'s state, first waiting out a pull another thread is running.
// Cost: O(1), plus the wait.
pub(super) fn lock_state(core: &SeqCore) -> MutexGuard<'_, SeqState> {
    let state = core.state.lock().unwrap_or_else(PoisonError::into_inner);
    if !pulled_elsewhere(state.puller) {
        return state;
    }
    drop(state);
    // STW-aware: the waiting thread counts as quiescent for a collect, and
    // the pool may grow if it is a worker (the puller may need one).
    crate::gc::wait_until(&core.state, &core.pulled, |s| !pulled_elsewhere(s.puller))
        // wasm32 has one thread, so another thread can never be pulling.
        .unwrap_or_else(|| core.state.lock().unwrap_or_else(PoisonError::into_inner))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn appends_grow_in_place_and_keep_earlier_borrows_intact() {
        let generations = Generations::empty();
        generations.append(vec![Value::int(0)]);
        let first = generations.live();
        for i in 1..1000 {
            generations.append(vec![Value::int(i)]);
        }
        assert_eq!(
            first,
            &[Value::int(0)],
            "an earlier borrow keeps its prefix"
        );
        let live = generations.live();
        assert_eq!(live.len(), 1000);
        assert!(
            live.iter()
                .enumerate()
                .all(|(i, v)| *v == Value::int(i as i64))
        );
        // Doubling: 1000 one-element appends need about log2(1000) buffers,
        // not one per append (#13023).
        assert!(generations.graveyard().len() <= 10);
    }

    #[test]
    fn publish_replaces_and_a_later_append_extends_the_replacement() {
        let generations = Generations::with(vec![Value::int(1), Value::int(2)]);
        generations.publish(vec![Value::int(7)]);
        generations.append(vec![Value::int(8)]);
        assert_eq!(generations.live(), &[Value::int(7), Value::int(8)]);
    }
}
