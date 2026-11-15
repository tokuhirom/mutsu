//! The setup phase of a `react` block's synchronous delivery (#11268).
//!
//! Rakudo taps a `whenever`'s live source the moment the `whenever` runs, so an
//! `emit` from another thread after that point runs the handler (serialized
//! with the still-running react body by the react's lock) before it returns.
//! mutsu registers the drive loop's supplier sinks only once the body has
//! finished; until then an emission is buffered and replayed. [`ReactSetup`]
//! closes that window: the first `whenever` on a live supplier creates the
//! react's [`ReactWaker`] and records it as a *setup hold* on each tapped
//! supplier, and a producer emitting meanwhile waits until the drive loop has
//! registered the sinks — replaying its event into that same waker — and then
//! until the handler has run, exactly as for a later emission.
//!
//! The hold counts as a dispatching react of the body's thread, so a body that
//! blocks (an `await`, a `Lock`) releases the waiting producers instead of
//! deadlocking with them; their events are then replayed asynchronously, the
//! behavior this replaces.
use crate::value::waker::{DispatchGuard, ReactWaker};
use crate::value::{Value, ValueView};

#[derive(Debug)]
pub(crate) struct ReactSetup {
    waker: ReactWaker,
    /// Suppliers this setup holds, released on drop.
    suppliers: Vec<u64>,
    /// Present while the react body runs (see the module docs).
    body: Option<DispatchGuard>,
}

impl ReactSetup {
    fn new() -> Self {
        let waker = ReactWaker::new();
        waker.begin_setup();
        let body = Some(DispatchGuard::enter(&waker));
        Self {
            waker,
            suppliers: Vec::new(),
            body,
        }
    }

    /// Hold `supplier_id`'s producers for this react, creating the setup on
    /// its first live `whenever`.
    // Cost: O(s), s = suppliers this react body has tapped so far.
    pub(crate) fn hold(setup: &mut Option<Self>, supplier_id: u64, preserving: bool) {
        let setup = setup.get_or_insert_with(Self::new);
        if !setup.suppliers.contains(&supplier_id) {
            crate::runtime::native_methods::supplier_add_setup_hold(
                supplier_id,
                &setup.waker,
                preserving,
            );
            setup.suppliers.push(supplier_id);
        }
    }

    /// The react body has finished: hand the waker to the drive loop, which
    /// registers its sinks on it and starts synchronous delivery. The holds
    /// stay until then, and are dropped with `self` afterwards.
    pub(crate) fn finish_body(&mut self) -> ReactWaker {
        self.body = None;
        self.waker.clone()
    }
}

impl Drop for ReactSetup {
    fn drop(&mut self) {
        self.body = None;
        for &sid in &self.suppliers {
            crate::runtime::native_methods::supplier_remove_setup_hold(sid, self.waker.id());
        }
        // A react that ended without a drive loop (a `done` in its body, an
        // error) must not leave a producer waiting on a setup that never ends.
        // After a drive loop this is a no-op: its own guard already ended it.
        self.waker.end_synchronous_delivery();
    }
}

/// A `whenever` registered inside another handler has no setup frame. Keep
/// emissions made before the drive loop adopts it, including those queued
/// before its tap closes. The sink registration releases this hold.
pub(crate) fn hold_pending_subscription(source: &Value, waker: Option<&ReactWaker>) {
    if let Some(waker) = waker
        && let ValueView::Instance { attributes, .. } = source.view()
        && !attributes.as_map().contains_key("preserving")
        && let Some(ValueView::Int(sid)) = attributes.as_map().get("supplier_id").map(Value::view)
        && sid > 0
    {
        crate::runtime::native_methods::supplier_add_setup_hold(sid as u64, waker, false);
    }
}

/// Release holds whose nested subscriptions never reached sink registration,
/// for example when a react exits on an error.
pub(crate) fn release_pending_subscriptions(markers: &[Value], waker: &ReactWaker) {
    for marker in markers {
        if let ValueView::Array(items, ..) = marker.view()
            && let Some(source) = items.first()
            && let ValueView::Instance { attributes, .. } = source.view()
            && let Some(ValueView::Int(sid)) =
                attributes.as_map().get("supplier_id").map(Value::view)
            && sid > 0
        {
            crate::runtime::native_methods::supplier_remove_setup_hold(sid as u64, waker.id());
        }
    }
}

impl crate::runtime::Interpreter {
    /// A `react` on another thread just handled an event this thread produced
    /// (`emit`/`done`/`quit`, delivered synchronously): pull its writes to the
    /// variables this thread shares with it, as returning from an `await` does,
    /// so code right after the `emit` observes the handler's effect.
    // Cost: O(1) when nothing was delivered elsewhere; otherwise one
    // `sync_shared_vars_to_env`, O(d), d = dirty shared variables.
    pub(crate) fn sync_after_synchronous_delivery(&mut self) {
        if crate::value::waker::take_delivered_elsewhere() {
            self.sync_shared_vars_to_env();
        }
    }
}
