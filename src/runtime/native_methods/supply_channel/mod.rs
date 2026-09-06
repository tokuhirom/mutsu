//! Waker-aware `SupplyEvent` broadcast: an mpsc pair whose sender pokes every
//! [`ReactWaker`] registered on the receiving end after each send (and on
//! sender drop), so a react/await drive loop blocked on its waker wakes
//! immediately instead of waiting out the poll-round idle cap (ADR-0008
//! follow-up — these mpsc receiver sources were the last supply inputs that
//! could only be observed by polling).
//!
//! The receiving end is a **broadcast point**, not a single consumer
//! (ADR-0074): a channel-backed Supply can be tapped any number of times, and
//! each tap gets its own subscriber queue fed with a clone of every event
//! distributed after it subscribed. Subscribers start empty — nothing emitted
//! before a tap existed is replayed to it — which is what Raku's *live* Supply
//! semantics require.
//!
//! Distribution is pull-driven and needs no broker thread: whichever
//! subscriber polls first drains the upstream mpsc and clones each event into
//! every registered queue, then serves itself. Queues are held by `Weak`, so a
//! dropped subscriber is pruned and its backlog released.
//!
//! The waker set is shared between all sender clones and every subscriber;
//! registration is idempotent per waker identity, and the drive loop
//! unregisters on every exit path.
mod broadcast;

use super::SupplyEvent;
use crate::value::waker::ReactWaker;
use broadcast::{Broadcast, Signal, SubQueue, WakerSet, notify_all};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;
use std::sync::{Arc, Mutex, Weak};

/// Sending half. Cloneable like `mpsc::Sender`; every clone shares the
/// receiving end's waker set. Dropping the last live clone also pokes the
/// wakers so a drive loop notices the disconnect promptly.
#[derive(Debug)]
pub(crate) struct SupplySender {
    tx: mpsc::Sender<SupplyEvent>,
    wakers: WakerSet,
    closed: Arc<AtomicBool>,
    broadcast: Weak<Broadcast>,
}

impl Clone for SupplySender {
    fn clone(&self) -> Self {
        Self {
            tx: self.tx.clone(),
            wakers: Arc::clone(&self.wakers),
            closed: Arc::clone(&self.closed),
            broadcast: Weak::clone(&self.broadcast),
        }
    }
}

impl SupplySender {
    pub(crate) fn send(&self, event: SupplyEvent) -> Result<(), mpsc::SendError<SupplyEvent>> {
        // A closed channel refuses further sends so producers holding a
        // sender clone (e.g. the interval-timer heap entry) observe the
        // teardown as "receiver gone" and retire themselves. The flag is set
        // only by `Tap.close`/`.cancel` via the act-loop close registry — for
        // every other channel user it stays false forever.
        if self.closed.load(Ordering::Acquire) {
            return Err(mpsc::SendError(event));
        }
        // Report "receiver gone" once every tap has been dropped, so producers
        // that retire on a send failure still retire even though the registry
        // template keeps the mpsc itself alive. See `Broadcast::all_taps_gone`.
        let broadcast = self.broadcast.upgrade();
        if broadcast.as_ref().is_some_and(|b| b.all_taps_gone()) {
            return Err(mpsc::SendError(event));
        }
        self.tx.send(event)?;
        if let Some(broadcast) = broadcast {
            notify_all(&self.wakers, &broadcast.signal);
        }
        Ok(())
    }
}

impl Drop for SupplySender {
    fn drop(&mut self) {
        // A disconnected channel is an event too (the drive loop retires the
        // subscription on `TryRecvError::Disconnected`): wake the consumers so
        // they observe the hangup without waiting out their idle cap. Bare
        // pokes from non-final clones are harmless.
        if let Some(broadcast) = self.broadcast.upgrade() {
            notify_all(&self.wakers, &broadcast.signal);
        }
    }
}

/// Receiving handle on a broadcast point (ADR-0074).
///
/// Each handle has its own queue and sees every event distributed after that
/// queue was registered — never anything from before, which is what Raku's
/// *live* Supply semantics require.
///
/// The queue is registered **lazily, on first use**. That matters because the
/// handle returned by [`supply_event_channel`] is used two different ways: most
/// producers park it in the supply-channel registry, where it acts as a
/// template that [`SupplyReceiver::subscribe`] hands out taps from and which
/// must not itself accumulate events while unclaimed; but several sites (a
/// `whenever <Promise>`'s one-shot channel, the signal watcher, socket
/// listeners) keep and poll it directly, and those must behave exactly like an
/// ordinary consumer. Deferring registration to the first poll serves both: an
/// unpolled template registers nothing, and a directly-polled handle registers
/// itself before it pumps, so the upstream backlog still reaches it.
///
/// Non-blocking `try_recv` is the drive loops' entry point; the blocking
/// variants exist for dedicated consumer threads.
#[derive(Debug)]
pub(crate) struct SupplyReceiver {
    broadcast: Arc<Broadcast>,
    queue: std::sync::OnceLock<Arc<SubQueue>>,
}

impl SupplyReceiver {
    /// Register an independent subscriber on the same broadcast point. The new
    /// subscriber starts empty: it sees every event distributed from now on,
    /// and nothing that came before.
    pub(crate) fn subscribe(&self) -> SupplyReceiver {
        let queue = std::sync::OnceLock::new();
        let _ = queue.set(self.broadcast.subscribe());
        SupplyReceiver {
            broadcast: Arc::clone(&self.broadcast),
            queue,
        }
    }

    /// Mark this stream's consumers as *competing* rather than broadcast.
    ///
    /// Not every channel-backed source is a Raku broadcast point. An
    /// `IO::Socket::Async` connection's read Supply is the measured
    /// counter-example: rakudo hands each incoming chunk to exactly one of its
    /// taps (which one varies between runs), it does not give every tap a copy.
    /// A source that marks itself exclusive keeps the old behaviour —
    /// `take_supply_channel` removes it from the registry, so the first
    /// consumer owns the stream — while everything else fans out.
    pub(crate) fn mark_exclusive(&self) {
        self.broadcast.exclusive.store(true, Ordering::Release);
    }

    pub(crate) fn is_exclusive(&self) -> bool {
        self.broadcast.exclusive.load(Ordering::Acquire)
    }

    /// This handle's queue, registering it on the broadcast if this is the
    /// first use. Must be called before `pump`, so the backlog is distributed
    /// to a queue that already exists.
    fn own_queue(&self) -> &Arc<SubQueue> {
        self.queue.get_or_init(|| self.broadcast.subscribe())
    }

    fn pop(&self) -> Option<SupplyEvent> {
        self.own_queue().events.lock().ok()?.pop_front()
    }

    pub(crate) fn try_recv(&self) -> Result<SupplyEvent, mpsc::TryRecvError> {
        self.own_queue();
        self.broadcast.pump();
        if let Some(event) = self.pop() {
            return Ok(event);
        }
        if self.broadcast.upstream_done.load(Ordering::Acquire) {
            // Re-check after observing the flag: a concurrent pump may have
            // queued the final events between our pop and this load.
            if let Some(event) = self.pop() {
                return Ok(event);
            }
            return Err(mpsc::TryRecvError::Disconnected);
        }
        Err(mpsc::TryRecvError::Empty)
    }

    /// Bounded slice of a blocking wait. Keeping each nap short means a
    /// notification lost between our pump and our wait costs milliseconds of
    /// latency instead of deadlocking a consumer.
    const NAP: std::time::Duration = std::time::Duration::from_millis(5);

    /// Blocking receive, for dedicated consumer threads (e.g. the
    /// Proc::Async stdin feeder). Drive loops must use `try_recv` +
    /// waker-blocking instead.
    pub(crate) fn recv(&self) -> Result<SupplyEvent, mpsc::RecvError> {
        loop {
            let seen = self.broadcast.signal.generation();
            match self.try_recv() {
                Ok(event) => return Ok(event),
                Err(mpsc::TryRecvError::Disconnected) => return Err(mpsc::RecvError),
                Err(mpsc::TryRecvError::Empty) => {}
            }
            crate::gc::block_quiescent(|| self.broadcast.signal.wait(seen, Self::NAP));
        }
    }

    /// Bounded blocking receive, for consumer threads that must observe an
    /// external close flag (the act-loop teardown re-checks its flag between
    /// waits — see `run_supply_act_loop`).
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn recv_timeout(
        &self,
        timeout: std::time::Duration,
    ) -> Result<SupplyEvent, mpsc::RecvTimeoutError> {
        let deadline = std::time::Instant::now() + timeout;
        loop {
            let seen = self.broadcast.signal.generation();
            match self.try_recv() {
                Ok(event) => return Ok(event),
                Err(mpsc::TryRecvError::Disconnected) => {
                    return Err(mpsc::RecvTimeoutError::Disconnected);
                }
                Err(mpsc::TryRecvError::Empty) => {}
            }
            let now = std::time::Instant::now();
            if now >= deadline {
                return Err(mpsc::RecvTimeoutError::Timeout);
            }
            let nap = Self::NAP.min(deadline - now);
            crate::gc::block_quiescent(|| self.broadcast.signal.wait(seen, nap));
        }
    }

    /// Handle on the shared close flag, kept by the tap site after the
    /// receiver moves into its worker. Setting it makes `send` fail and lets
    /// the worker's bounded wait notice the close.
    pub(crate) fn close_flag(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.broadcast.closed)
    }

    /// Register a drive-loop waker to poke on future sends (no-op if this
    /// exact waker is already registered).
    pub(crate) fn register_waker(&self, waker: &ReactWaker) {
        if let Ok(mut ws) = self.broadcast.wakers.lock()
            && !ws.iter().any(|w| w.id() == waker.id())
        {
            ws.push(waker.clone());
        }
    }

    pub(crate) fn unregister_waker(&self, waker_id: usize) {
        if let Ok(mut ws) = self.broadcast.wakers.lock() {
            ws.retain(|w| w.id() != waker_id);
        }
    }
}

/// Create a waker-aware `SupplyEvent` broadcast pair. The returned receiver is
/// the queue-less template described on [`SupplyReceiver`]; call
/// [`SupplyReceiver::subscribe`] (or go through `take_supply_channel`) to get a
/// handle that actually receives events.
pub(crate) fn supply_event_channel() -> (SupplySender, SupplyReceiver) {
    let (tx, rx) = mpsc::channel();
    let wakers: WakerSet = Arc::new(Mutex::new(Vec::new()));
    let closed = Arc::new(AtomicBool::new(false));
    let broadcast = Arc::new(Broadcast {
        upstream: Mutex::new(rx),
        subscribers: Mutex::new(Vec::new()),
        upstream_done: AtomicBool::new(false),
        ever_subscribed: AtomicBool::new(false),
        exclusive: AtomicBool::new(false),
        wakers: Arc::clone(&wakers),
        closed: Arc::clone(&closed),
        signal: Signal::default(),
    });
    (
        SupplySender {
            tx,
            wakers,
            closed,
            broadcast: Arc::downgrade(&broadcast),
        },
        SupplyReceiver {
            broadcast,
            queue: std::sync::OnceLock::new(),
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    fn emit(tx: &SupplySender, n: i64) {
        tx.send(SupplyEvent::Emit(Value::int(n))).unwrap();
    }

    fn drain(rx: &SupplyReceiver) -> Vec<i64> {
        let mut got = Vec::new();
        while let Ok(SupplyEvent::Emit(v)) = rx.try_recv() {
            if let crate::value::ValueView::Int(i) = v.view() {
                got.push(i);
            }
        }
        got
    }

    #[test]
    fn two_subscribers_each_see_every_event() {
        let (tx, template) = supply_event_channel();
        let a = template.subscribe();
        let b = template.subscribe();
        emit(&tx, 1);
        emit(&tx, 2);
        assert_eq!(drain(&a), vec![1, 2]);
        assert_eq!(drain(&b), vec![1, 2]);
    }

    #[test]
    fn a_late_subscriber_sees_only_later_events() {
        // Live semantics (ADR-0074): no replay of what came before.
        let (tx, template) = supply_event_channel();
        let early = template.subscribe();
        emit(&tx, 1);
        assert_eq!(drain(&early), vec![1]);
        let late = template.subscribe();
        emit(&tx, 2);
        assert_eq!(drain(&early), vec![2]);
        assert_eq!(drain(&late), vec![2]);
    }

    #[test]
    fn an_unpolled_template_registers_no_queue() {
        // The registry parks the handle from `supply_event_channel` without
        // ever polling it, so it must not accumulate a backlog of its own.
        let (tx, template) = supply_event_channel();
        let sub = template.subscribe();
        emit(&tx, 1);
        assert_eq!(drain(&sub), vec![1]);
        assert_eq!(template.broadcast.subscribers.lock().unwrap().len(), 1);
    }

    #[test]
    fn the_handle_from_supply_event_channel_works_when_polled_directly() {
        // A `whenever <Promise>`'s one-shot channel, the signal watcher and the
        // socket listeners keep this handle and poll it themselves instead of
        // going through the registry. Its queue registers on first use, so the
        // events already sent upstream still reach it.
        let (tx, direct) = supply_event_channel();
        emit(&tx, 1);
        emit(&tx, 2);
        assert_eq!(drain(&direct), vec![1, 2]);
    }

    #[test]
    fn disconnect_is_reported_after_the_queue_drains() {
        let (tx, template) = supply_event_channel();
        let sub = template.subscribe();
        emit(&tx, 7);
        drop(tx);
        assert!(matches!(sub.try_recv(), Ok(SupplyEvent::Emit(_))));
        assert!(matches!(
            sub.try_recv(),
            Err(mpsc::TryRecvError::Disconnected)
        ));
    }

    #[test]
    fn the_first_subscriber_drains_the_backlog_then_sees_the_hangup() {
        // Nothing had subscribed when the producer ran and finished, so the
        // events are still queued upstream and belong to the first subscriber
        // (the `Proc::Async` reader-finishes-before-react-subscribes case).
        let (tx, template) = supply_event_channel();
        emit(&tx, 1);
        drop(tx);
        let first = template.subscribe();
        assert_eq!(drain(&first), vec![1]);
        assert!(matches!(
            first.try_recv(),
            Err(mpsc::TryRecvError::Disconnected)
        ));
    }

    #[test]
    fn a_subscriber_registering_after_the_stream_drained_retires_immediately() {
        // Once a tap has consumed the stream there is nothing left to hand a
        // later tap: live semantics, no replay.
        let (tx, template) = supply_event_channel();
        let first = template.subscribe();
        emit(&tx, 1);
        drop(tx);
        assert_eq!(drain(&first), vec![1]);
        let late = template.subscribe();
        assert!(matches!(
            late.try_recv(),
            Err(mpsc::TryRecvError::Disconnected)
        ));
    }

    #[test]
    fn sends_succeed_before_anything_has_subscribed() {
        // A Proc::Async reader thread starts before the react loop subscribes;
        // its first chunk must be waiting in the upstream queue, not refused.
        let (tx, template) = supply_event_channel();
        emit(&tx, 1);
        let late = template.subscribe();
        assert_eq!(drain(&late), vec![1]);
    }

    #[test]
    fn sends_fail_once_every_tap_has_gone() {
        // How an infinite producer (Supply.interval) still retires even though
        // the registry template keeps the mpsc alive.
        let (tx, template) = supply_event_channel();
        {
            let sub = template.subscribe();
            emit(&tx, 1);
            assert_eq!(drain(&sub), vec![1]);
        }
        assert!(tx.send(SupplyEvent::Emit(Value::int(2))).is_err());
    }

    #[test]
    fn a_dropped_subscriber_stops_being_fed() {
        let (tx, template) = supply_event_channel();
        let keep = template.subscribe();
        {
            let _gone = template.subscribe();
        }
        emit(&tx, 1);
        assert_eq!(drain(&keep), vec![1]);
        assert_eq!(template.broadcast.subscribers.lock().unwrap().len(), 1);
    }
}
