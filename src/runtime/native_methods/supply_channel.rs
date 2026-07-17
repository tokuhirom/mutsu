//! Waker-aware `SupplyEvent` channel: an mpsc pair whose sender pokes every
//! [`ReactWaker`] registered on the receiving end after each send (and on
//! sender drop), so a react/await drive loop blocked on its waker wakes
//! immediately instead of waiting out the poll-round idle cap (ADR-0008
//! follow-up — these mpsc receiver sources were the last supply inputs that
//! could only be observed by polling).
//!
//! The waker set is shared between all sender clones and the receiver;
//! registration is idempotent per waker identity, and the drive loop
//! unregisters on every exit path.
use super::SupplyEvent;
use crate::value::waker::ReactWaker;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};

type WakerSet = Arc<Mutex<Vec<ReactWaker>>>;

fn notify_all(wakers: &WakerSet) {
    if let Ok(ws) = wakers.lock() {
        for w in ws.iter() {
            w.notify();
        }
    }
}

/// Sending half. Cloneable like `mpsc::Sender`; every clone shares the
/// receiver's waker set. Dropping the last live clone also pokes the wakers
/// so a drive loop notices the disconnect promptly.
#[derive(Debug)]
pub(crate) struct SupplySender {
    tx: mpsc::Sender<SupplyEvent>,
    wakers: WakerSet,
}

impl Clone for SupplySender {
    fn clone(&self) -> Self {
        Self {
            tx: self.tx.clone(),
            wakers: Arc::clone(&self.wakers),
        }
    }
}

impl SupplySender {
    pub(crate) fn send(&self, event: SupplyEvent) -> Result<(), mpsc::SendError<SupplyEvent>> {
        self.tx.send(event)?;
        notify_all(&self.wakers);
        Ok(())
    }
}

impl Drop for SupplySender {
    fn drop(&mut self) {
        // A disconnected channel is an event too (the drive loop retires the
        // subscription on `TryRecvError::Disconnected`): wake the consumer so
        // it observes the hangup without waiting out its idle cap. Bare pokes
        // from non-final clones are harmless.
        notify_all(&self.wakers);
    }
}

/// Receiving half. Non-blocking `try_recv` only — the drive loop's pacing
/// comes from blocking on its waker, which senders poke.
#[derive(Debug)]
pub(crate) struct SupplyReceiver {
    rx: mpsc::Receiver<SupplyEvent>,
    wakers: WakerSet,
}

impl SupplyReceiver {
    pub(crate) fn try_recv(&self) -> Result<SupplyEvent, mpsc::TryRecvError> {
        self.rx.try_recv()
    }

    /// Blocking receive, for dedicated consumer threads (e.g. the
    /// Proc::Async stdin feeder). Drive loops must use `try_recv` +
    /// waker-blocking instead.
    pub(crate) fn recv(&self) -> Result<SupplyEvent, mpsc::RecvError> {
        self.rx.recv()
    }

    /// Register a drive-loop waker to poke on future sends (no-op if this
    /// exact waker is already registered).
    pub(crate) fn register_waker(&self, waker: &ReactWaker) {
        if let Ok(mut ws) = self.wakers.lock()
            && !ws.iter().any(|w| w.id() == waker.id())
        {
            ws.push(waker.clone());
        }
    }

    pub(crate) fn unregister_waker(&self, waker_id: usize) {
        if let Ok(mut ws) = self.wakers.lock() {
            ws.retain(|w| w.id() != waker_id);
        }
    }
}

/// Create a waker-aware `SupplyEvent` channel pair.
pub(crate) fn supply_event_channel() -> (SupplySender, SupplyReceiver) {
    let (tx, rx) = mpsc::channel();
    let wakers: WakerSet = Arc::new(Mutex::new(Vec::new()));
    (
        SupplySender {
            tx,
            wakers: Arc::clone(&wakers),
        },
        SupplyReceiver { rx, wakers },
    )
}
