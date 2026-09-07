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
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::Duration;

/// Global monotonic sequence stamped on every queued supply event **and** read
/// by every `Tap.close`, so the two are totally ordered against each other.
///
/// Delivery inside a `react` is deferred to the drive loop's pump, so by the
/// time a close is processed the queue may already hold values that were
/// emitted *before* it. Rakudo delivers exactly those and drops only what was
/// emitted afterwards; without a shared order a consumer can only see "this
/// subscription is closed" and has to drop the whole batch. One counter serves
/// both because the supplier registry already stamped its buffered values from
/// it (`native_methods::state::next_emit_seq`), so a replayed backlog keeps the
/// sequence it was really emitted at.
pub(crate) fn next_event_seq() -> u64 {
    static EVENT_SEQ: AtomicU64 = AtomicU64::new(1);
    EVENT_SEQ.fetch_add(1, Ordering::Relaxed)
}

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
    /// `(subscription key, event, sequence)` — see [`next_event_seq`].
    events: VecDeque<(usize, SinkEvent, u64)>,
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

    /// Queue an event for subscription `key` and wake the consumer, stamping it
    /// with a fresh sequence.
    pub(crate) fn push(&self, key: usize, event: SinkEvent) {
        self.push_at(key, event, next_event_seq());
    }

    /// [`Self::push`] with an already-allocated sequence — used when the event
    /// was ordered earlier than the push (a producer that stamped it under its
    /// own registry lock, or a backlog replayed to a late-registered sink).
    pub(crate) fn push_at(&self, key: usize, event: SinkEvent, seq: u64) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.events.push_back((key, event, seq));
        cvar.notify_all();
    }

    /// Is an event queued for `key` that was sequenced at or before `seq`?
    /// Answers "this closed subscription still owes deliveries" without
    /// consuming the queue.
    pub(crate) fn has_event_upto(&self, key: usize, seq: u64) -> bool {
        let (lock, _) = &*self.inner;
        let state = lock.lock().unwrap();
        state.events.iter().any(|(k, _, s)| *k == key && *s <= seq)
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

    /// Take all queued events (non-blocking), each with its sequence.
    pub(crate) fn drain(&self) -> Vec<(usize, SinkEvent, u64)> {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.events.drain(..).collect()
    }

    /// Block until an event is queued, a `notify()` lands, or `timeout`
    /// elapses. Counts the thread GC-quiescent for the duration (the wait
    /// touches no `Gc` state). Consumes a pending poke.
    pub(crate) fn wait_activity(&self, timeout: Duration) {
        #[cfg(target_arch = "wasm32")]
        {
            // In a browser the producers are not other threads — they are the
            // queued tasks and timers of `runtime::wasm_sched`, and a condvar
            // wait would both park the only thread that could ever run them and
            // panic (wasm32 std has no condvar). So run them instead, until an
            // event shows up or the requested timeout elapses on the virtual
            // clock. Letting the timeout elapse matters: it is what lets the
            // caller's own deadline (the react drive loop's) expire rather than
            // spin, when nothing is left that could produce.
            use crate::runtime::thread_compat::mono_now;
            let (lock, _) = &*self.inner;
            let deadline = mono_now() + timeout.as_secs_f64();
            loop {
                {
                    let state = lock.lock().unwrap();
                    if !state.events.is_empty() || state.poked {
                        break;
                    }
                }
                if mono_now() >= deadline {
                    break;
                }
                if !crate::runtime::wasm_sched::pump() {
                    crate::runtime::wasm_sched::advance_clock_to(deadline);
                    break;
                }
            }
            lock.lock().unwrap().poked = false;
        }
        #[cfg(not(target_arch = "wasm32"))]
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
            for (_, ev, _) in &state.events {
                match ev {
                    SinkEvent::Emit(v) | SinkEvent::Quit(v) => visitor.visit_value(v),
                    SinkEvent::Done => {}
                }
            }
        }
    }
}
