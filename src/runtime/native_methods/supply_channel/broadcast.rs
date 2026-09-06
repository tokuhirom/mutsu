//! The broadcast point behind a channel-backed Supply (ADR-0074).
//!
//! Holds the upstream `mpsc` receiver plus the set of per-tap queues, and
//! distributes each upstream event into every registered queue. Distribution is
//! pull-driven: whichever subscriber polls first does the draining, so there is
//! no broker thread. Queues are held by `Weak`, so a dropped tap is pruned and
//! its backlog released.
use super::SupplyEvent;
use crate::value::waker::ReactWaker;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;
use std::sync::{Arc, Condvar, Mutex, Weak};

pub(super) type WakerSet = Arc<Mutex<Vec<ReactWaker>>>;

/// Condvar-backed "something happened" signal, bumped by every send. Blocking
/// subscribers wait on it instead of spinning; the wait is bounded anyway, so
/// a lost notification costs latency, never a hang.
#[derive(Debug, Default)]
pub(super) struct Signal {
    generation: Mutex<u64>,
    cv: Condvar,
}

impl Signal {
    fn bump(&self) {
        if let Ok(mut g) = self.generation.lock() {
            *g = g.wrapping_add(1);
        }
        self.cv.notify_all();
    }

    /// Wait until the generation moves off `seen`, or the timeout elapses.
    pub(super) fn wait(&self, seen: u64, timeout: std::time::Duration) {
        if let Ok(g) = self.generation.lock() {
            let _ = self.cv.wait_timeout_while(g, timeout, |g| *g == seen);
        }
    }

    pub(super) fn generation(&self) -> u64 {
        self.generation.lock().map(|g| *g).unwrap_or(0)
    }
}

pub(super) fn notify_all(wakers: &WakerSet, signal: &Signal) {
    if let Ok(ws) = wakers.lock() {
        for w in ws.iter() {
            w.notify();
        }
    }
    signal.bump();
}

/// One tap's private queue.
#[derive(Debug, Default)]
pub(super) struct SubQueue {
    pub(super) events: Mutex<VecDeque<SupplyEvent>>,
}

/// State shared by the sender clones, the registry template, and every
/// subscriber handle.
#[derive(Debug)]
pub(super) struct Broadcast {
    pub(super) upstream: Mutex<mpsc::Receiver<SupplyEvent>>,
    pub(super) subscribers: Mutex<Vec<Weak<SubQueue>>>,
    /// Set once the upstream mpsc has hung up. A subscriber reports
    /// `Disconnected` only after this is set *and* its own queue is drained.
    pub(super) upstream_done: AtomicBool,
    /// Whether anything has ever subscribed. Until it has, a producer keeps
    /// sending into the upstream queue so the values are there for the first
    /// subscriber (a `Proc::Async` reader thread starts before the react loop
    /// subscribes, and must not lose its first chunk).
    pub(super) ever_subscribed: AtomicBool,
    /// Marks a stream whose consumers *compete* rather than each seeing every
    /// value, so it must keep the old exclusive-transfer behaviour instead of
    /// broadcasting. See `SupplyReceiver::mark_exclusive`.
    pub(super) exclusive: AtomicBool,
    pub(super) wakers: WakerSet,
    pub(super) closed: Arc<AtomicBool>,
    pub(super) signal: Signal,
}

impl Broadcast {
    /// Drain everything pending upstream into every live subscriber queue.
    ///
    /// Returns without doing anything if another thread is already pumping —
    /// that thread is doing this thread's work too, and the caller re-checks
    /// its own queue afterwards either way.
    pub(super) fn pump(&self) {
        let Ok(upstream) = self.upstream.try_lock() else {
            return;
        };
        loop {
            match upstream.try_recv() {
                Ok(event) => self.distribute(event),
                Err(mpsc::TryRecvError::Empty) => return,
                Err(mpsc::TryRecvError::Disconnected) => {
                    self.upstream_done.store(true, Ordering::Release);
                    return;
                }
            }
        }
    }

    fn distribute(&self, event: SupplyEvent) {
        let Ok(mut subs) = self.subscribers.lock() else {
            return;
        };
        // Prune dropped taps while we are here, so an abandoned subscriber
        // stops costing a clone per event.
        subs.retain(|weak| weak.strong_count() > 0);
        for weak in subs.iter() {
            if let Some(queue) = weak.upgrade()
                && let Ok(mut events) = queue.events.lock()
            {
                events.push_back(event.clone());
            }
        }
    }

    pub(super) fn subscribe(&self) -> Arc<SubQueue> {
        let queue = Arc::new(SubQueue::default());
        if let Ok(mut subs) = self.subscribers.lock() {
            subs.retain(|weak| weak.strong_count() > 0);
            subs.push(Arc::downgrade(&queue));
            self.ever_subscribed.store(true, Ordering::Release);
        }
        queue
    }

    /// Every tap this broadcast ever had has gone away.
    ///
    /// This is how a producer that retires on "receiver gone" still retires
    /// (ADR-0074). Dropping the last consumer used to disconnect the mpsc,
    /// because the consumer *was* the receiver; now the registry keeps a
    /// template alive to serve future taps, so the mpsc never hangs up on its
    /// own and an infinite source like `Supply.interval` would tick forever.
    /// Losing the last tap is exactly when Raku stops a live Supply's
    /// producer, so that is the condition reported to the sender.
    pub(super) fn all_taps_gone(&self) -> bool {
        if !self.ever_subscribed.load(Ordering::Acquire) {
            return false;
        }
        self.subscribers
            .lock()
            .map(|subs| subs.iter().all(|weak| weak.strong_count() == 0))
            .unwrap_or(false)
    }
}
