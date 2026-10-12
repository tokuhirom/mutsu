//! The generation store a [`super::SeqBody`] reifies into, and the claim that
//! serializes its non-consuming pulls across threads (#11242).
//!
//! # Generations
//!
//! Every alias of a `Value::Seq` shares one body and reads its elements
//! through a plain `&Vec<Value>` (`Deref`), so a pull can never overwrite
//! what a reader may still be holding. Each pull instead *publishes* a new
//! generation: the elements go into an `Arc<Vec<Value>>` that the append-only
//! graveyard keeps alive until the body itself is dropped, and `live` is
//! swung to point at it. A reader loads `live` alone and never touches the
//! graveyard's own `Vec`, whose buffer a later push may reallocate; that is
//! what makes a read on one thread sound while another thread publishes.
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
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, PoisonError};
use std::thread::ThreadId;

/// Append-only store of a Seq body's generations; see the module docs.
pub(super) struct Generations {
    /// The newest generation, pointing into an `Arc` the graveyard owns, or
    /// null while nothing was published yet (an unpulled deferred body).
    live: AtomicPtr<Vec<Value>>,
    graveyard: Mutex<Vec<Arc<Vec<Value>>>>,
}

static EMPTY: Vec<Value> = Vec::new();

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
    pub(super) fn live(&self) -> &Vec<Value> {
        let ptr = self.live.load(Ordering::Acquire);
        if ptr.is_null() {
            return &EMPTY;
        }
        // SAFETY: `ptr` came from `Arc::as_ptr` of an `Arc` the graveyard
        // keeps until `self` is dropped, so it outlives the `&self` borrow
        // this reference is tied to. A published generation is never written
        // again (only superseded), and the `Acquire` load pairs with the
        // `Release` store in `publish`, so its contents are fully visible.
        unsafe { &*ptr }
    }

    /// Make `items` the newest generation. Earlier generations stay alive
    /// (and unchanged) for every reference still borrowing them.
    // Cost: O(1) amortized.
    pub(super) fn publish(&self, items: Vec<Value>) {
        let generation = Arc::new(items);
        let ptr = Arc::as_ptr(&generation).cast_mut();
        let mut graveyard = self
            .graveyard
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        graveyard.push(generation);
        // Under the graveyard lock, so two publishers' stores land in the
        // order of their pushes: `live` is always the last entry.
        self.live.store(ptr, Ordering::Release);
    }

    /// Visit every element of every generation (GC tracing).
    // Cost: O(g), g = elements over all generations.
    pub(super) fn for_each_value(&self, visit: &mut dyn FnMut(&Value)) {
        let graveyard = self
            .graveyard
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        for generation in graveyard.iter() {
            generation.iter().for_each(&mut *visit);
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
