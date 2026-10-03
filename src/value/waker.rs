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
    /// Synchronous delivery (see [`ReactWaker::begin_synchronous_delivery`]):
    /// the thread running the consuming drive loop, while it runs. `None` for
    /// a consumer that only collects (`await $supply`, `.list`) or a drive
    /// loop that has not started or has ended — producers never wait on it.
    consumer: Option<std::thread::ThreadId>,
    /// The consumer is still running its `react` body: its `whenever`s have
    /// tapped live suppliers (each holds this waker as a setup hold), but the
    /// drive loop has not registered their sinks yet. A producer emitting now
    /// waits for the registration, which replays its event into this queue.
    setup: bool,
    /// Sequences the consumer drained and is still handling.
    in_flight: Vec<u64>,
    /// How many blocking waits the consumer is inside *while* handling events
    /// (an `await`, a `Lock`, a nested `react`'s idle wait, its own `emit`
    /// into another react). While non-zero it cannot make progress on this
    /// queue, so producers stop waiting for it (see [`park_dispatching`]).
    consumer_parked: usize,
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

    /// Queue an event and report whether its producer has to wait for it with
    /// [`Self::await_delivery`]: true when a synchronous consumer on another
    /// thread is running.
    pub(crate) fn push_sync(&self, key: usize, event: SinkEvent, seq: u64) -> bool {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.events.push_back((key, event, seq));
        cvar.notify_all();
        state
            .consumer
            .is_some_and(|c| c != std::thread::current().id())
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

    /// Take all queued events (non-blocking), each with its sequence. Under
    /// synchronous delivery they stay *in flight* — their producers keep
    /// waiting — until [`Self::finish_in_flight`].
    pub(crate) fn drain(&self) -> Vec<(usize, SinkEvent, u64)> {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        let events: Vec<_> = state.events.drain(..).collect();
        if state.consumer.is_some() {
            state
                .in_flight
                .extend(events.iter().map(|(_, _, seq)| *seq));
        }
        events
    }

    /// The consumer finished handling everything it drained (delivered or
    /// dropped): release the producers waiting on those events.
    pub(crate) fn finish_in_flight(&self) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if !state.in_flight.is_empty() {
            state.in_flight.clear();
            cvar.notify_all();
        }
    }

    /// Make every event pushed from another thread *synchronous* from here
    /// until [`Self::end_synchronous_delivery`]: its producer (`emit`, `done`,
    /// `quit` on a live `Supplier`) does not return until this thread has run
    /// the event's handler. That is Rakudo's model — a live supply runs each
    /// tap's callback on the emitting thread before `emit` returns, serialized
    /// with the `react` by its lock — reached from the other side: the handler
    /// stays on the react's own interpreter, and the emitter waits for it
    /// (issue #11268). Called by the drive loop on the thread that consumes,
    /// through [`SynchronousDelivery`].
    fn begin_synchronous_delivery(&self) {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let (lock, cvar) = &*self.inner;
            let mut state = lock.lock().unwrap();
            state.consumer = Some(std::thread::current().id());
            state.setup = false;
            cvar.notify_all();
        }
    }

    /// The consumer's `react` body is about to tap a live supplier with this
    /// waker as its setup hold (see `WakerState::setup`): producers wait from
    /// now until [`Self::begin_synchronous_delivery`] or
    /// [`Self::end_synchronous_delivery`].
    pub(crate) fn begin_setup(&self) {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let (lock, _) = &*self.inner;
            let mut state = lock.lock().unwrap();
            state.consumer = Some(std::thread::current().id());
            state.setup = true;
        }
    }

    /// Whether a producer on this thread has to wait for this consumer.
    pub(crate) fn holds_producer(&self) -> bool {
        let (lock, _) = &*self.inner;
        lock.lock()
            .unwrap()
            .consumer
            .is_some_and(|c| c != std::thread::current().id())
    }

    /// The drive loop is over: nothing will handle the queue any more, so
    /// release every producer still waiting on it.
    pub(crate) fn end_synchronous_delivery(&self) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        state.consumer = None;
        state.setup = false;
        state.in_flight.clear();
        cvar.notify_all();
    }

    /// Producer side of synchronous delivery: block until the consumer has
    /// handled the event pushed (or, during setup, replayed) at `seq` — or has
    /// ended, or is itself parked in
    /// a blocking wait while handling events. The last condition is what keeps
    /// this deadlock-free: a consumer that `await`s, takes a `Lock` or emits
    /// into a react that is waiting on *this* thread cannot get to the event,
    /// and Rakudo would have run the handler regardless (its react lock is
    /// released across an `await`, and re-entry on one thread is queued, not
    /// blocked). The producer then returns with the event still queued — the
    /// asynchronous delivery this replaces. A no-op on the consumer's own
    /// thread, whose drive loop re-drains what its handlers emit.
    // Cost: O(q) per wake-up, q = events queued or in flight on this waker.
    pub(crate) fn await_delivery(&self, seq: u64) {
        let (lock, cvar) = &*self.inner;
        let me = std::thread::current().id();
        DELIVERED_ELSEWHERE.with(|d| d.set(true));
        let _ = crate::gc::wait_until(lock, cvar, |state: &WakerState| {
            state.consumer.is_none_or(|c| c == me)
                || state.consumer_parked > 0
                || !(state.setup
                    || state.in_flight.contains(&seq)
                    || state.events.iter().any(|(_, _, s)| *s == seq))
        });
    }

    #[cfg_attr(target_arch = "wasm32", allow(dead_code))] // only native threads park
    fn set_parked(&self, parked: bool) {
        let (lock, cvar) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if parked {
            state.consumer_parked += 1;
        } else {
            state.consumer_parked = state.consumer_parked.saturating_sub(1);
        }
        cvar.notify_all();
    }

    /// Block until an event is queued, a `notify()` lands, or `timeout`
    /// elapses. Counts the thread GC-quiescent for the duration (the wait
    /// touches no `Gc` state). Consumes a pending poke.
    pub(crate) fn wait_activity(&self, timeout: Duration) {
        #[cfg(target_arch = "wasm32")]
        {
            // In a browser the producers are not other threads — they are the
            // queued tasks and timers of `wasm_sched`, and a condvar
            // wait would both park the only thread that could ever run them and
            // panic (wasm32 std has no condvar). So run them instead, until an
            // event shows up or the requested timeout elapses on the virtual
            // clock. Letting the timeout elapse matters: it is what lets the
            // caller's own deadline (the react drive loop's) expire rather than
            // spin, when nothing is left that could produce.
            use crate::thread_compat::mono_now;
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
                if !crate::wasm_sched::pump() {
                    crate::wasm_sched::advance_clock_to(deadline);
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

/// Synchronous delivery on a drive loop's waker for as long as it lives (see
/// [`ReactWaker::begin_synchronous_delivery`]); ending it on drop means a
/// drive loop that unwinds still releases its waiting producers.
pub(crate) struct SynchronousDelivery {
    waker: ReactWaker,
}

impl SynchronousDelivery {
    pub(crate) fn begin(waker: &ReactWaker) -> Self {
        waker.begin_synchronous_delivery();
        Self {
            waker: waker.clone(),
        }
    }
}

impl Drop for SynchronousDelivery {
    fn drop(&mut self) {
        self.waker.end_synchronous_delivery();
    }
}

thread_local! {
    /// Set when this thread waited for another thread's react to handle an
    /// event it produced ([`ReactWaker::await_delivery`]).
    static DELIVERED_ELSEWHERE: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Take (and clear) the [`ReactWaker::await_delivery`] mark: the caller then
/// makes the other thread's writes visible here.
pub(crate) fn take_delivered_elsewhere() -> bool {
    DELIVERED_ELSEWHERE.with(|d| d.replace(false))
}

thread_local! {
    /// The wakers whose events this thread is handling right now, innermost
    /// last (a `whenever` body can run a nested `react`).
    static DISPATCHING: std::cell::RefCell<Vec<ReactWaker>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Marks `waker`'s drained events as being handled on this thread; dropping it
/// releases their producers ([`ReactWaker::finish_in_flight`]).
#[derive(Debug)]
pub(crate) struct DispatchGuard {
    waker: ReactWaker,
}

impl DispatchGuard {
    pub(crate) fn enter(waker: &ReactWaker) -> Self {
        DISPATCHING.with(|d| d.borrow_mut().push(waker.clone()));
        Self {
            waker: waker.clone(),
        }
    }
}

impl Drop for DispatchGuard {
    fn drop(&mut self) {
        DISPATCHING.with(|d| {
            let mut d = d.borrow_mut();
            if let Some(pos) = d.iter().rposition(|w| w.id() == self.waker.id()) {
                d.remove(pos);
            }
        });
        self.waker.finish_in_flight();
    }
}

/// Undoes [`park_dispatching`] on drop.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))] // only native threads park
pub(crate) struct ParkGuard {
    parked: Vec<ReactWaker>,
}

impl Drop for ParkGuard {
    fn drop(&mut self) {
        for waker in &self.parked {
            waker.set_parked(false);
        }
    }
}

/// This thread is about to block: every react whose events it is handling
/// cannot progress until it wakes, so their producers must not wait for it
/// (see [`ReactWaker::await_delivery`]). Called at every blocking point
/// (`worker_pool::enter_blocking`).
#[cfg_attr(target_arch = "wasm32", allow(dead_code))] // only native threads park
// Cost: O(d), d = reacts this thread is handling events of (nesting depth).
pub(crate) fn park_dispatching() -> ParkGuard {
    let parked = DISPATCHING.with(|d| d.borrow().clone());
    for waker in &parked {
        waker.set_parked(true);
    }
    ParkGuard { parked }
}
