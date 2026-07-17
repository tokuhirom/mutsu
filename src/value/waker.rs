//! `ReactWaker`: a per-consumer event queue + condvar used to deliver supply
//! events (emit/done/quit) from producer threads to a consuming drive loop
//! (`react`, `await $supply`, throttle control waits, ...) without polling.
//!
//! Producers push events (or bare wake-ups) under their own registry lock;
//! the consumer drains the queue and blocks on the condvar when idle. This
//! replaces the old snapshot-polling scheme, which both busy-spun the
//! consumer thread and could *lose* events when a producer reset its state
//! (e.g. `Supplier.done` clearing the emitted buffer) before the consumer's
//! next poll observed it. Pushed events are owned by the queue, so a
//! producer-side reset can no longer un-publish them.
//!
//! Lock ordering: a producer may take a registry lock (e.g. the supplier
//! state map) and then this queue's mutex. The consumer takes only this
//! queue's mutex while draining/waiting, and never calls back into producer
//! registries while holding it.
use crate::value::Value;
use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex};
use std::time::Duration;

/// One event delivered to a consumer. `key` (stored alongside in the queue)
/// identifies which subscription of the consumer the event belongs to.
#[derive(Debug, Clone)]
pub(crate) enum SinkEvent {
    Emit(Value),
    Done,
    Quit(Value),
}

#[derive(Debug, Default)]
struct WakerState {
    events: VecDeque<(usize, SinkEvent)>,
    /// Set by `notify()` (a bare wake-up with no event payload, e.g. a
    /// promise resolving or a channel send). Cleared by the next wait.
    poked: bool,
}

/// Cloneable handle; all clones share the same queue.
#[derive(Debug, Clone, Default)]
pub(crate) struct ReactWaker {
    inner: Arc<(Mutex<WakerState>, Condvar)>,
}

impl ReactWaker {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Identity of the shared queue, for registry deduplication/removal.
    pub(crate) fn id(&self) -> usize {
        Arc::as_ptr(&self.inner) as usize
    }

    /// Queue an event for subscription `key` and wake the consumer.
    pub(crate) fn push(&self, key: usize, event: SinkEvent) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.events.push_back((key, event));
        cvar.notify_all();
    }

    /// Bare wake-up: no event payload, just make the current/next
    /// `wait_activity` return promptly so the consumer re-polls its
    /// non-queue sources.
    pub(crate) fn notify(&self) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.poked = true;
        cvar.notify_all();
    }

    /// Take all queued events (non-blocking).
    pub(crate) fn drain(&self) -> Vec<(usize, SinkEvent)> {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.events.drain(..).collect()
    }

    /// Block until an event is queued, a `notify()` lands, or `timeout`
    /// elapses. Counts the thread GC-quiescent for the duration (the wait
    /// touches no `Gc` state). Consumes a pending poke.
    pub(crate) fn wait_activity(&self, timeout: Duration) {
        crate::gc::block_quiescent(|| {
            let (lock, cvar) = &*self.inner;
            let mut state = lock.lock().unwrap();
            if state.events.is_empty() && !state.poked {
                let (guard, _) = cvar.wait_timeout(state, timeout).unwrap();
                state = guard;
            }
            state.poked = false;
        });
    }

    /// Visit every `Value` held in queued events, for GC root enumeration
    /// (this queue is a root container like the supplier registries: the
    /// collector never frees it, but must see the `Value`s it keeps alive).
    pub(crate) fn visit_roots(&self, visitor: &mut dyn crate::gc::RootVisitor) {
        let (lock, _) = &*self.inner;
        if let Ok(state) = lock.lock() {
            for (_, ev) in &state.events {
                match ev {
                    SinkEvent::Emit(v) | SinkEvent::Quit(v) => visitor.visit_value(v),
                    SinkEvent::Done => {}
                }
            }
        }
    }
}
