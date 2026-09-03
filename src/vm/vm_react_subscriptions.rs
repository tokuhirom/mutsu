//! Shared react/supply subscription drive loop (`drive_react_subscriptions_nested`),
//! split from `vm_react_loop` (§7-8 file split).
//!
//! Supplier-backed subscriptions are driven **push-style**: the drive loop
//! registers a [`ReactWaker`] sink on each subscribed supplier, and every
//! `emit`/`done`/`quit` is pushed into the waker's queue by the producer (under
//! the supplier registry lock, so cross-supplier push order matches emit
//! order). The loop drains that queue and blocks on the waker when idle. This
//! replaces the old snapshot-polling scheme, which busy-spun a core per idle
//! react and lost events when `Supplier.done` reset the registry state before
//! the loop's next poll (roast S17: `react whenever $s { }` hung forever when
//! `$s.done` raced the poll).
use super::*;
use crate::runtime::native_methods::{
    PromiseCombinator, SupplyEvent, supplier_sink_unregister, supplier_sinks_register_batch,
    take_promise_combinator_sources,
};
use crate::runtime::subtest::{ReactSubscription, SupplyDrivePolicy};
use crate::value::waker::{ReactWaker, SinkEvent};
use std::sync::mpsc;
use std::time::Duration;

/// Idle-wait cap for one drive-loop round. Every source now wakes the loop
/// (supplier sinks, promise/on-demand-done/tap-close `on_resolve` hooks,
/// channel and mpsc-receiver `SupplySender` pokes), so this is a safety net
/// against a missed wake-up, not a delivery-latency bound.
const REACT_IDLE_WAIT: Duration = Duration::from_millis(250);

impl Interpreter {
    /// Deliver every event queued on the drive loop's waker to its
    /// subscription's consumer, in push order (== the order producers
    /// emitted, across suppliers: pushes happen under the one supplier
    /// registry lock). Re-drains until the queue is quiet, since a consumer
    /// may synchronously emit more. Sets `*progressed` when at least one
    /// event was dispatched. Returns `Ok(true)` if a consumer raised react
    /// `done`; propagates `Err` for an unhandled supplier `quit`.
    fn dispatch_waker_events(
        &mut self,
        waker: &ReactWaker,
        react_subs: &mut [ReactSubscription],
        progressed: &mut bool,
        policy: &SupplyDrivePolicy,
    ) -> Result<bool, RuntimeError> {
        loop {
            let events = waker.drain();
            if events.is_empty() {
                return Ok(false);
            }
            *progressed = true;
            for (key, event) in events {
                if key >= react_subs.len() || react_subs[key].done {
                    continue;
                }
                if react_subs[key]
                    .whenever_id
                    .is_some_and(crate::runtime::native_methods::is_whenever_closed)
                {
                    react_subs[key].done = true;
                    continue;
                }
                match event {
                    SinkEvent::Emit(value) => {
                        if let Some(limit) = react_subs[key].head_limit
                            && react_subs[key].emit_count >= limit
                        {
                            continue;
                        }
                        if react_subs[key].is_lines {
                            let chunk = value.to_string_value();
                            react_subs[key].line_buffer.push_str(&chunk);
                            while let Some(pos) = react_subs[key].line_buffer.find('\n') {
                                let line = react_subs[key].line_buffer[..pos].to_string();
                                react_subs[key].line_buffer =
                                    react_subs[key].line_buffer[pos + 1..].to_string();
                                if self
                                    .run_react_consumer(&mut react_subs[key], Value::str(line))?
                                {
                                    return Ok(true);
                                }
                                if react_subs[key].done {
                                    break;
                                }
                                react_subs[key].emit_count += 1;
                                if self.head_limit_reached(&mut react_subs[key])? {
                                    break;
                                }
                            }
                        } else {
                            if self.run_react_consumer(&mut react_subs[key], value)? {
                                return Ok(true);
                            }
                            if !react_subs[key].done {
                                react_subs[key].emit_count += 1;
                                self.head_limit_reached(&mut react_subs[key])?;
                            }
                        }
                    }
                    SinkEvent::Done => {
                        if react_subs[key].is_lines && !react_subs[key].line_buffer.is_empty() {
                            let remaining = std::mem::take(&mut react_subs[key].line_buffer);
                            let cb = react_subs[key].callback.clone();
                            match self.call_react_callback(&cb, vec![Value::str(remaining)]) {
                                Err(e) if e.is_react_done() => return Ok(true),
                                other => {
                                    other?;
                                }
                            }
                        }
                        for cb in react_subs[key].last_callbacks.clone() {
                            if let Err(err) = self.call_react_callback(&cb, Vec::new()) {
                                // A LAST-phaser die on a subscription flattened
                                // out of an on-demand `supply { ... }` body is
                                // that supply's own completion, not a raw crash
                                // of this drive loop: `supplier_quit` its
                                // emitter so the owning `on_demand_done`-tracked
                                // subscription's QUIT phasers (see
                                // `vm_react_subscriptions.rs`'s Phase 2 poll)
                                // get a chance to handle it, matching `raku`.
                                if let Some(sid) = react_subs[key].emitter_supplier_id {
                                    let cause = err
                                        .exception
                                        .as_deref()
                                        .cloned()
                                        .unwrap_or_else(|| Value::str(err.message.to_string()));
                                    crate::runtime::native_methods::supplier_quit(sid, cause);
                                    react_subs[key].done = true;
                                    break;
                                }
                                return Err(err);
                            }
                        }
                        react_subs[key].done = true;
                        // Symmetric with the die path above: this subscription
                        // completing without error is the owning on-demand
                        // supply's own completion too (the minimal case of one
                        // nested live-source `whenever` — a body with several
                        // would need each to finish before the emitter is
                        // truly done, which this single-emitter signal does not
                        // yet track). `supplier_done` is a no-op if the emitter
                        // was already quit/done.
                        if let Some(sid) = react_subs[key].emitter_supplier_id {
                            crate::runtime::native_methods::supplier_done(sid);
                        }
                        // Supply.on-demand(..., closing => { ... }): fire this
                        // subscription's own `closing` callbacks promptly, right
                        // when its source actually signals done, instead of
                        // deferring them to react-loop teardown (`take` drains
                        // the list so the final catch-all `run_react_close_callbacks`
                        // does not refire them).
                        for cb in std::mem::take(&mut react_subs[key].close_callbacks) {
                            let _ = self.call_react_callback(&cb, Vec::new());
                        }
                    }
                    SinkEvent::Quit(error) => {
                        let mut handled = false;
                        for quit_cb in react_subs[key].quit_callbacks.clone() {
                            self.call_supply_quit_handler(quit_cb, error.clone())?;
                            handled = true;
                        }
                        if handled {
                            react_subs[key].done = true;
                            continue;
                        }
                        // Under `SupplyDrivePolicy::Promise` this loop runs
                        // detached on its own thread whose `Result` the caller
                        // already discards (see `supply_promise_on_demand`'s
                        // `spawn_user_thread`) — there is no `react {}` block
                        // whose die this could become, and returning `Err`
                        // here left the promise `Planned` forever (e.g. a live
                        // Supplier-backed `whenever` with no `quit =>` handler
                        // of its own inside `Promise(supply {...})`, the shape
                        // `Cro::MessageWithBody.body-blob` uses over a
                        // `preserve()`d nested source). Break the promise
                        // directly instead, matching what an explicit
                        // `quit_cb` above would have driven towards anyway.
                        // Returning `Ok(true)` (not `Err`) lets the caller's
                        // normal `break 'react_loop` path run the close
                        // callbacks exactly once, instead of duplicating that
                        // call here.
                        if let SupplyDrivePolicy::Promise { promise, .. } = policy {
                            promise.break_with(error, String::new(), String::new());
                            return Ok(true);
                        }
                        Self::run_react_close_callbacks(self, react_subs);
                        let quit_err =
                            crate::runtime::Interpreter::runtime_error_from_supply_reason(error);
                        return Err(crate::runtime::Interpreter::wrap_react_died(quit_err));
                    }
                }
            }
        }
    }

    /// If `sub` has reached its `head`/`.head(N)` limit, fire its LAST callbacks
    /// and mark it done. Returns whether the limit was reached.
    fn head_limit_reached(&mut self, sub: &mut ReactSubscription) -> Result<bool, RuntimeError> {
        if let Some(limit) = sub.head_limit
            && sub.emit_count >= limit
        {
            for cb in sub.last_callbacks.clone() {
                self.call_react_callback(&cb, Vec::new())?;
            }
            sub.done = true;
            return Ok(true);
        }
        Ok(false)
    }

    /// Shared subscription drive loop backing both `react { ... }` and the
    /// `await $supply` / `$supply.Promise` paths. `react`-built and
    /// promise-built subscriptions are driven through here; `policy` selects
    /// how each emitted value is dispatched and when the loop completes (see
    /// [`SupplyDrivePolicy`]).
    pub(crate) fn drive_react_subscriptions_nested(
        &mut self,
        react_subs: Vec<ReactSubscription>,
        policy: SupplyDrivePolicy,
    ) -> Result<(), RuntimeError> {
        self.drive_react_subscriptions_nested_prewired(react_subs, policy, None)
    }

    /// [`Self::drive_react_subscriptions_nested`] with optionally pre-registered
    /// supplier sinks. A caller that defers the drive loop to a spawned thread
    /// (`supply_promise_on_demand`) must register the sinks on the *calling*
    /// thread before it returns — a producer that runs `emit`/`done` between the
    /// coercion returning and the spawned thread starting would otherwise have
    /// its terminal `done` dispatch `supplier_reset` the buffered state away
    /// before the late sink registration could replay it, leaving the promise
    /// `Planned` forever (t/promise-supply-coercion-async-drive.t test 3 under
    /// CPU oversubscription). Registered sinks survive `supplier_reset`, and the
    /// waker queue buffers every pushed event until the loop drains it.
    pub(crate) fn drive_react_subscriptions_nested_prewired(
        &mut self,
        react_subs: Vec<ReactSubscription>,
        policy: SupplyDrivePolicy,
        prewired: Option<(ReactWaker, Vec<(u64, u64)>)>,
    ) -> Result<(), RuntimeError> {
        // Mark the drive loop active so a `whenever` that taps an on-demand
        // supply from inside a running react routes the supply's
        // `closing => { ... }` callbacks to this (main) thread via
        // `pending_tap_closes`, rather than firing them on an async body's
        // worker thread (see `native_supply_mut_methods` tap on-demand path).
        self.react_active += 1;
        let result = self.drive_react_subscriptions_inner(react_subs, policy, prewired);
        self.react_active -= 1;
        // Fire any close callbacks whose emitter completed but was not drained
        // in-loop (e.g. the final tap's emitter finishing as the react ended).
        let _ = self.fire_ready_tap_closes();
        result
    }

    /// Fire the `closing => { ... }` callbacks of any nested-`whenever` on-demand
    /// tap whose emitter has signalled `done`/`quit`, on the current (main react)
    /// thread. Draining removes each serviced entry so a callback runs once per
    /// tap. Runs both each drive-loop poll and once when the loop exits.
    /// Returns whether any callback fired.
    fn fire_ready_tap_closes(&mut self) -> Result<bool, RuntimeError> {
        if self.pending_tap_closes.is_empty() {
            return Ok(false);
        }
        let mut fired = false;
        let mut i = 0;
        while i < self.pending_tap_closes.len() {
            if self.pending_tap_closes[i].0.is_resolved() {
                let (_, cbs) = self.pending_tap_closes.remove(i);
                for cb in cbs {
                    let _ = self.call_react_callback(&cb, Vec::new());
                }
                fired = true;
            } else {
                i += 1;
            }
        }
        Ok(fired)
    }

    /// Waker-registration wrapper: registers push sinks / wake hooks on every
    /// subscription source, runs the drive loop, and unregisters on all exit
    /// paths (normal completion, react `done`, propagated errors).
    fn drive_react_subscriptions_inner(
        &mut self,
        mut react_subs: Vec<ReactSubscription>,
        policy: SupplyDrivePolicy,
        prewired: Option<(ReactWaker, Vec<(u64, u64)>)>,
    ) -> Result<(), RuntimeError> {
        if react_subs.is_empty() {
            // Defensive: a prewired caller never passes an empty subscription
            // list, but if one did, its sinks must not leak.
            if let Some((_, sink_regs)) = prewired {
                for (sid, sink_id) in sink_regs {
                    supplier_sink_unregister(sid, sink_id);
                }
            }
            if let SupplyDrivePolicy::Promise {
                promise,
                last_value,
                ..
            } = &policy
                && !promise.is_resolved()
            {
                promise.keep(last_value.clone(), String::new(), String::new());
            }
            return Ok(());
        }

        // Supplier-backed subscriptions: register push sinks in one batch so
        // any already-buffered values across sibling derived supplies (e.g. two
        // `whenever $s.grep(...)`) replay merged in true emit order, not one
        // supplier's whole buffer at a time (PLAN.md 8.19). Registering them
        // one at a time would interleave-lose the order when a producer thread
        // races ahead of this registration and buffers values first.
        //
        // A prewired caller already did this on the thread that built the
        // subscriptions (see `drive_react_subscriptions_nested_prewired`).
        let (waker, mut sink_regs): (ReactWaker, Vec<(u64, u64)>) = match prewired {
            Some((waker, sink_regs)) => (waker, sink_regs),
            None => {
                let waker = ReactWaker::new();
                let regs: Vec<(u64, usize)> = react_subs
                    .iter()
                    .enumerate()
                    .filter_map(|(i, sub)| sub.supplier_id.map(|sid| (sid, i)))
                    .collect();
                let sink_regs = supplier_sinks_register_batch(&regs, &waker);
                (waker, sink_regs)
            }
        };
        // Promise / channel / mpsc-receiver sources still deliver their
        // payloads through the existing receiver / poll paths, but wake the
        // loop instantly instead of waiting out the idle cap.
        for sub in &react_subs {
            if let Some(p) = &sub.promise {
                let w = waker.clone();
                let _ = p.on_resolve(Box::new(move |_, _, _, _| w.notify()));
            }
            if let Some(p) = &sub.on_demand_done {
                let w = waker.clone();
                let _ = p.on_resolve(Box::new(move |_, _, _, _| w.notify()));
            }
            if let Some(ch) = &sub.channel {
                ch.register_waker(&waker);
            }
            if let Some(rx) = &sub.receiver {
                rx.register_waker(&waker);
            }
        }
        // Publish this loop's waker so sources wired up mid-loop (a nested
        // `whenever` tapping an async on-demand supply) can wake it too.
        let prev_waker = self.current_react_waker.replace(waker.clone());
        // Arm any `whenever <Promise>` stand-in suppliers only now, AFTER their
        // sinks are registered. An already-resolved promise fires its arm
        // closure synchronously; before this ordering the closure's emit+done
        // landed on the stand-in before any sink existed, and the `done`
        // handler's `supplier_reset` wiped the buffered value, so the later
        // sink replay found nothing and the react hung forever
        // (t/react-whenever-kept-promise-nested-supply.t).
        self.arm_pending_promise_whenevers();
        let result =
            self.drive_react_subscriptions_loop(&mut react_subs, policy, &waker, &mut sink_regs);
        self.current_react_waker = prev_waker;
        for (sid, sink_id) in sink_regs {
            supplier_sink_unregister(sid, sink_id);
        }
        for sub in &react_subs {
            if let Some(ch) = &sub.channel {
                ch.unregister_waker(waker.id());
            }
            if let Some(rx) = &sub.receiver {
                rx.unregister_waker(waker.id());
            }
        }
        result
    }

    /// Is this value one of the 5-element `[source, body, [LAST…], [QUIT…], id]`
    /// arrays `whenever` registers, rather than a value a supply body emitted?
    fn is_whenever_subscription_marker(value: &Value) -> bool {
        let ValueView::Array(items, ..) = value.view() else {
            return false;
        };
        items.len() == 5
            && matches!(
                items[0].view(),
                ValueView::Promise(_) | ValueView::Channel(_) | ValueView::Instance { .. }
            )
            && matches!(items[1].view(), ValueView::Sub(_))
    }

    fn whenever_marker_is_closed(marker: &Value) -> bool {
        let ValueView::Array(items, ..) = marker.view() else {
            return false;
        };
        matches!(items.get(4).map(Value::view), Some(ValueView::Int(id)) if id >= 0
            && crate::runtime::native_methods::is_whenever_closed(id as u64))
    }

    /// Adopt any `whenever` subscription registered while the drive loop was
    /// running (a `whenever` inside another `whenever`'s body) and wire its
    /// source into this loop's waker. Returns `Ok(true)` when building the
    /// subscription already ended the react.
    fn adopt_newly_registered_subscriptions(
        &mut self,
        react_subs: &mut Vec<ReactSubscription>,
        waker: &ReactWaker,
        sink_regs: &mut Vec<(u64, u64)>,
    ) -> Result<bool, RuntimeError> {
        if self.pending_react_subscriptions.is_empty() {
            return Ok(false);
        }
        let pending: Vec<Value> = std::mem::take(&mut self.pending_react_subscriptions)
            .into_iter()
            .filter(|marker| !Self::whenever_marker_is_closed(marker))
            .collect();
        for marker in &pending {
            if let ValueView::Array(items, ..) = marker.view()
                && items.len() >= 2
                && let ValueView::Sub(data) = items[1].view()
            {
                self.nested_react_callbacks.insert(data.id);
            }
        }
        let first_new = react_subs.len();
        let mut stream_base = None;
        let finished = self.build_react_subscriptions(&pending, react_subs, &mut stream_base)?;
        let new_supplier_regs: Vec<(u64, usize)> = react_subs[first_new..]
            .iter()
            .enumerate()
            .filter_map(|(i, sub)| sub.supplier_id.map(|sid| (sid, first_new + i)))
            .collect();
        sink_regs.extend(supplier_sinks_register_batch(&new_supplier_regs, waker));
        for sub in &react_subs[first_new..] {
            if let Some(p) = &sub.promise {
                let w = waker.clone();
                let _ = p.on_resolve(Box::new(move |_, _, _, _| w.notify()));
            }
            if let Some(p) = &sub.on_demand_done {
                let w = waker.clone();
                let _ = p.on_resolve(Box::new(move |_, _, _, _| w.notify()));
            }
            if let Some(ch) = &sub.channel {
                ch.register_waker(waker);
            }
            if let Some(rx) = &sub.receiver {
                rx.register_waker(waker);
            }
        }
        // Arm any `whenever <Promise>` markers this batch's nested `supply { }`
        // bodies registered — only now, AFTER the new subscriptions' sinks are
        // wired to this loop's waker. An already-resolved promise fires its arm
        // closure synchronously, and arming before the sink registration let
        // the closure's emit+done hit the sink-less stand-in supplier, whose
        // `done` handler resets the buffered value away (see the matching call
        // in `drive_react_subscriptions_inner`).
        self.arm_pending_promise_whenevers();
        Ok(finished)
    }

    fn drive_react_subscriptions_loop(
        &mut self,
        react_subs: &mut Vec<ReactSubscription>,
        mut policy: SupplyDrivePolicy,
        waker: &ReactWaker,
        sink_regs: &mut Vec<(u64, u64)>,
    ) -> Result<(), RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        'react_loop: loop {
            // A `whenever` nested inside another `whenever`'s body only
            // registers when that body runs, which is inside this loop. Adopt
            // whatever the last round registered, so the react keeps running
            // until the nested subscription is done too (without this, the
            // outer subscription completing ended the react and the inner one
            // never fired at all).
            if self.adopt_newly_registered_subscriptions(react_subs, waker, sink_regs)? {
                break 'react_loop;
            }
            for sub in react_subs.iter_mut() {
                if sub
                    .whenever_id
                    .is_some_and(crate::runtime::native_methods::is_whenever_closed)
                {
                    sub.done = true;
                }
            }
            // GC park point: an idle react loop blocks on the waker without
            // dispatching bytecode, so it would never reach the backedge
            // safepoint — park here so a stop-the-world can proceed while the
            // loop waits for events. (Unconditional: `gc_safepoint` below only
            // parks when a trigger is armed.)
            crate::gc::gc_park_point();
            // GC safepoint (§9.2a `react_poll`): one drive-loop poll unit.
            crate::gc::gc_safepoint(crate::gc::SafepointKind::ReactPoll);
            let mut progressed = false;
            if let SupplyDrivePolicy::Promise {
                promise, deadline, ..
            } = &policy
            {
                // The supply body's inner `done` keeps this promise through the
                // supplier registry; once that happens the await is satisfied.
                if promise.is_resolved() {
                    return Ok(());
                }
                // Bound the wait so a stalled source cannot hang the await.
                if crate::runtime::thread_compat::Instant::now() >= *deadline {
                    promise.keep(Value::NIL, String::new(), String::new());
                    return Ok(());
                }
            }
            // A `done` raised by a whenever body that was fed through a
            // StreamConsumer (an `emit` inside a `supply { }` source re-routed to
            // the outer whenever's callback) is recorded as `StreamConsumer::done`
            // rather than propagated — `try_stream_emit` has to swallow it so the
            // emitting body can unwind. Honour it here: `done` ends the react.
            if self.supply_stream_consumers.iter().any(|c| c.done) {
                break 'react_loop;
            }
            // Phase 1: deliver all queued supplier events in push (= emit)
            // order, honouring per-supplier done/quit.
            if self.dispatch_waker_events(waker, react_subs, &mut progressed, &policy)? {
                break 'react_loop;
            }
            // Service any nested-`whenever` on-demand taps whose emitter finished,
            // firing their `closing => { ... }` callbacks on this thread.
            if self.fire_ready_tap_closes()? {
                progressed = true;
            }
            // Phase 2: poll the non-supplier subscriptions (on-demand / channel /
            // receiver sources).
            let mut all_done = true;
            for si in 0..react_subs.len() {
                let sub = &mut react_subs[si];
                if sub.done {
                    continue;
                }
                // Supplier-backed subs are fully serviced by the waker queue.
                if sub.supplier_id.is_some() {
                    all_done = false;
                    continue;
                }
                all_done = false;
                // On-demand supply completion: a `done` inside the `supply { ... }`
                // body was rewritten to `$emitter.done()`, which resolves this
                // subscription's done-signal promise rather than raising the
                // react-done signal. Once that resolves the flattened subscription
                // is complete, so fire its LAST callbacks and stop polling it
                // (otherwise an infinite source like `Supply.interval` would spin
                // forever — see S17-supply/syntax.t).
                if let Some(done_promise) = sub.on_demand_done.clone()
                    && done_promise.is_resolved()
                {
                    // A `supplier_quit`'d emitter (see the `SinkEvent::Done`
                    // LAST-phaser-die handling above) breaks this promise
                    // instead of keeping it: that is the on-demand supply's
                    // own QUIT, not a normal completion — dispatch to this
                    // subscription's QUIT phasers rather than its LAST ones.
                    if done_promise.status() == "Broken" {
                        let reason = done_promise.result_blocking();
                        // Supply.on-demand(..., closing => { ... }): `closing`
                        // fires when the tap closes, including when it closes
                        // via an unhandled `quit`/die in the on-demand body's
                        // producer — fire it promptly here, not deferred to
                        // react-loop teardown.
                        for close_cb in std::mem::take(&mut sub.close_callbacks) {
                            let _ = self.call_react_callback(&close_cb, Vec::new());
                        }
                        let mut handled = false;
                        for quit_cb in sub.quit_callbacks.clone() {
                            self.call_supply_quit_handler(quit_cb, reason.clone())?;
                            handled = true;
                        }
                        sub.done = true;
                        progressed = true;
                        if !handled {
                            // Under `SupplyDrivePolicy::Promise` this loop runs
                            // detached on its own thread (see
                            // `supply_promise_on_demand`'s `spawn_user_thread`,
                            // whose caller already returned and discards this
                            // function's `Result`) — there is no `react {}`
                            // block whose die this could reasonably become, and
                            // returning `Err` here left the promise `Planned`
                            // forever (e.g. `Cro::MessageWithBody.body-blob`'s
                            // `Promise(supply { whenever self.body-byte-stream
                            // {...} })` never resolving when the nested raw
                            // body parser's own unhandled `LAST`-phaser die
                            // quit it). Break the promise directly instead —
                            // exactly what an explicit `quit_cb` above would
                            // have driven towards anyway.
                            if let SupplyDrivePolicy::Promise { promise, .. } = &policy {
                                promise.break_with(reason, String::new(), String::new());
                                return Ok(());
                            }
                            let quit_err =
                                crate::runtime::Interpreter::runtime_error_from_supply_reason(
                                    reason,
                                );
                            return Err(crate::runtime::Interpreter::wrap_react_died(quit_err));
                        }
                        continue;
                    }
                    for callback in &sub.last_callbacks {
                        match self.call_react_callback(&callback.clone(), Vec::new()) {
                            Err(e) if e.is_react_done() => break 'react_loop,
                            other => {
                                other?;
                            }
                        }
                    }
                    // Supply.on-demand(..., closing => { ... }): fire this
                    // tap's `closing` callback promptly, right when its
                    // producer actually signals done, instead of batching it
                    // with every other pending `closing` callback at
                    // react-loop teardown (the bug this fixes: see
                    // news/2026-08/supply-on-demand-closing-callback-prompt.md).
                    for close_cb in std::mem::take(&mut sub.close_callbacks) {
                        match self.call_react_callback(&close_cb, Vec::new()) {
                            Err(e) if e.is_react_done() => break 'react_loop,
                            other => {
                                other?;
                            }
                        }
                    }
                    sub.done = true;
                    progressed = true;
                    continue;
                }
                // Handle Channel sources: poll values directly
                if let Some(ref ch) = sub.channel {
                    match ch.poll_result() {
                        Ok(Some(value)) => {
                            if self.run_react_consumer(sub, value)? {
                                break 'react_loop;
                            }
                            sub.emit_count += 1;
                            progressed = true;
                        }
                        Ok(None) => {
                            // No value available yet. Only mark done once the
                            // channel is closed *and* fully drained. Gating on
                            // `!can_send()` here would race: a value `send`+`close`d
                            // between this `poll_result()` (seen empty) and the
                            // close check is still queued, but `can_send()` already
                            // reports closed, so the value would be dropped
                            // (roast S17-supply/syntax.t test 57 lost the final
                            // channel value under load). `is_drained_closed()` flips
                            // only when the queue empties on a closed channel, so we
                            // keep polling until that late value is delivered.
                            if ch.is_drained_closed() {
                                for callback in &sub.last_callbacks {
                                    self.call_react_callback(&callback.clone(), Vec::new())?;
                                }
                                sub.done = true;
                                progressed = true;
                            }
                        }
                        Err(_err) => {
                            sub.done = true;
                            progressed = true;
                        }
                    }
                    continue;
                }
                if react_subs[si].receiver.is_none() {
                    // A source-less subscription normally exists only to carry
                    // close callbacks, fired promptly by the `on_demand_done`
                    // check just above once its promise actually resolves (with
                    // `run_react_close_callbacks` at loop exit as a catch-all
                    // for one whose promise never settles before the react
                    // ends). One that also carries `quit_callbacks` (the
                    // `emitter_supplier_id`-owning subscription's own QUIT
                    // phasers, see the `SinkEvent::Done`/die handling above)
                    // must instead wait for its `on_demand_done` promise to
                    // actually resolve — marking it done here would let the
                    // react conclude before that promise's Kept/Broken status
                    // (checked just above) is ever observed. The same is true
                    // of one carrying `last_callbacks` with no source of its
                    // own (`register_nested_on_demand_source`'s shadow
                    // subscription for a nested `whenever <derived-supply> {
                    // ...; LAST {...} }`), or one carrying `close_callbacks`
                    // (a `Supply.on-demand(..., closing => { ... })` tap whose
                    // async body has not completed yet, see
                    // `build_react_subscriptions`'s on-demand branch): marking
                    // it done here — before `on_demand_done` resolves — would
                    // skip straight past this subscription on every later
                    // iteration (the `sub.done` guard at the top of this loop),
                    // so its LAST phasers / `closing` callback would only ever
                    // fire via the loop-exit catch-all instead of promptly when
                    // the promise settles.
                    let awaiting_on_demand_done = react_subs[si].on_demand_done.is_some()
                        && (!react_subs[si].last_callbacks.is_empty()
                            || !react_subs[si].close_callbacks.is_empty());
                    if react_subs[si].quit_callbacks.is_empty() && !awaiting_on_demand_done {
                        react_subs[si].done = true;
                    }
                    continue;
                }
                // A `whenever Promise.allof(...)` settles only once every source
                // has, so waiting on the sources is what drives it. `anyof` also
                // registers its sources (for deferred `Proc::Async` tap replay),
                // but blocking on all of them would defeat its whole point — let
                // it fall through to the ordinary receiver poll.
                if let Some(promise) = react_subs[si].promise.clone()
                    && let Some((kind, sources)) = take_promise_combinator_sources(&promise)
                    && kind == PromiseCombinator::Allof
                {
                    for source in sources {
                        source.result_blocking();
                    }
                    continue;
                }
                // Poll the receiver without blocking: the idle wait at the end
                // of the round provides the pacing. The `Result` is owned, so
                // the borrow of the receiver ends on this line — freeing
                // `react_subs` for the pre-drain below.
                let poll = react_subs[si].receiver.as_ref().map(|r| r.try_recv());
                // Raku ordering guarantee: values `emit`ted into a supplier
                // *before* the event this receiver just delivered are causally
                // earlier and must reach their `whenever`s first — even when that
                // event's callback ends the react (e.g. `whenever start { emit … }`
                // finishing while a sibling `whenever` calls `done`, so the sibling
                // supplier's already-emitted values would otherwise be lost). Drain
                // the waker queue before running this receiver's consumer, so
                // their pending values are delivered in source order.
                if matches!(poll, Some(Ok(SupplyEvent::Emit(_))))
                    && matches!(policy, SupplyDrivePolicy::React)
                    && self.dispatch_waker_events(waker, react_subs, &mut progressed, &policy)?
                {
                    break 'react_loop;
                }
                let sub = &mut react_subs[si];
                match poll {
                    Some(Ok(SupplyEvent::Emit(value))) => {
                        progressed = true;
                        match &mut policy {
                            SupplyDrivePolicy::Promise {
                                promise,
                                last_value,
                                ..
                            } => {
                                // Capture values the whenever block `emit`s so a
                                // later `done` resolves the promise with the last one.
                                self.supply_emit_buffer.push(Vec::new());
                                let cb_result =
                                    self.call_react_callback(&sub.callback.clone(), vec![value]);
                                let emitted = self.supply_emit_buffer.pop().unwrap_or_default();
                                for item in emitted {
                                    // A `whenever` nested in this body registered
                                    // its subscription marker into the same
                                    // frame. It is not a value the supply
                                    // emitted: hand it to the adoption queue
                                    // instead of letting it become the promise's
                                    // result.
                                    if Self::is_whenever_subscription_marker(&item) {
                                        self.pending_react_subscriptions.push(item);
                                    } else {
                                        *last_value = item;
                                    }
                                }
                                if promise.is_resolved() {
                                    return Ok(());
                                }
                                if let Err(err) = cb_result {
                                    // `done`/`last` inside the whenever complete the
                                    // supply: keep the promise with the last emitted
                                    // value immediately rather than spinning to the
                                    // deadline.
                                    if err.is_react_done()
                                        || err.is_last()
                                        || err.is_supply_body_done()
                                    {
                                        promise.keep(
                                            last_value.clone(),
                                            String::new(),
                                            String::new(),
                                        );
                                        return Ok(());
                                    }
                                    // `next`/`redo` are loop control, not completion.
                                    if !err.is_next() && !err.is_redo() {
                                        // A `die` quits the supply: break with the cause.
                                        let cause =
                                            err.exception.as_deref().cloned().unwrap_or_else(
                                                || Value::str(err.message.to_string()),
                                            );
                                        promise.break_with(cause, String::new(), String::new());
                                        return Ok(());
                                    }
                                }
                            }
                            SupplyDrivePolicy::React => {
                                if sub.is_lines {
                                    let chunk = value.to_string_value();
                                    sub.line_buffer.push_str(&chunk);
                                    while let Some(pos) = sub.line_buffer.find('\n') {
                                        let line = sub.line_buffer[..pos].to_string();
                                        sub.line_buffer = sub.line_buffer[pos + 1..].to_string();
                                        if self.run_react_consumer(sub, Value::str(line))? {
                                            break 'react_loop;
                                        }
                                        if sub.done {
                                            break;
                                        }
                                    }
                                } else if self.run_react_consumer(sub, value)? {
                                    break 'react_loop;
                                }
                            }
                        }
                    }
                    Some(Ok(SupplyEvent::Done)) => {
                        progressed = true;
                        if matches!(policy, SupplyDrivePolicy::Promise { .. }) {
                            // Inner supply done: the promise resolves through the
                            // supplier registry, not the channel close — just
                            // retire this receiver.
                            sub.done = true;
                        } else {
                            if sub.is_lines && !sub.line_buffer.is_empty() {
                                let remaining = std::mem::take(&mut sub.line_buffer);
                                match self.call_react_callback(
                                    &sub.callback.clone(),
                                    vec![Value::str(remaining)],
                                ) {
                                    Err(e) if e.is_react_done() => break 'react_loop,
                                    other => {
                                        other?;
                                    }
                                }
                            }
                            for callback in &sub.last_callbacks {
                                self.call_react_callback(&callback.clone(), Vec::new())?;
                            }
                            sub.done = true;
                        }
                    }
                    Some(Ok(SupplyEvent::Quit(error))) => {
                        progressed = true;
                        if matches!(policy, SupplyDrivePolicy::Promise { .. }) {
                            // On the await path an inner quit just retires the
                            // receiver; the promise is resolved/broken elsewhere.
                            sub.done = true;
                        } else {
                            let mut handled = false;
                            for quit_cb in &sub.quit_callbacks {
                                self.call_supply_quit_handler(quit_cb.clone(), error.clone())?;
                                handled = true;
                            }
                            sub.done = true;
                            if !handled {
                                let ch_quit_err =
                                    crate::runtime::Interpreter::runtime_error_from_supply_reason(
                                        error,
                                    );
                                return Err(crate::runtime::Interpreter::wrap_react_died(
                                    ch_quit_err,
                                ));
                            }
                        }
                    }
                    Some(Err(mpsc::TryRecvError::Empty)) | None => {}
                    Some(Err(mpsc::TryRecvError::Disconnected)) => {
                        sub.done = true;
                        progressed = true;
                    }
                }
            }
            // A whenever body dispatched just above (Phase 1's `dispatch_waker_events`
            // or Phase 2's `run_react_consumer`) may itself have registered a nested
            // `whenever` — e.g. `whenever <Promise> -> $x { whenever <Supply> -> $y {
            // emit $y } }`, where the Promise-backed subscription's callback runs the
            // inner `whenever` and marks itself done in the very same drain (its
            // `emit`+`done` are pushed back-to-back by the resolving thread and land
            // in one `waker.drain()` batch). That registration sits in
            // `pending_react_subscriptions` until the *next* iteration's top-of-loop
            // `adopt_newly_registered_subscriptions` call — so "every known
            // subscription is done" must not end the react while a fresh one is still
            // waiting to be adopted, or the nested whenever's source is silently
            // dropped (Cro::TCP::Connector.establish never received its response).
            let has_pending_adoptions = !self.pending_react_subscriptions.is_empty();
            match &policy {
                SupplyDrivePolicy::React => {
                    if !has_pending_adoptions && (all_done || react_subs.iter().all(|s| s.done)) {
                        break;
                    }
                }
                SupplyDrivePolicy::Promise {
                    promise,
                    last_value,
                    emitter_supplier_id,
                    ..
                } => {
                    if promise.is_resolved() {
                        return Ok(());
                    }
                    // Every `whenever` finished, so the `supply { ... }` block
                    // is done -- Raku keeps its promise with the final value it
                    // emitted. `done`ing the emitter supplier resolves the
                    // promise through the registry with exactly that value;
                    // keeping `Nil` here (as this used to) reported a supply
                    // that had emitted as though it never had, so
                    // `await (supply { whenever $p { emit … } })` answered Nil
                    // after waiting out the whole 30s deadline.
                    if !has_pending_adoptions && (all_done || react_subs.iter().all(|s| s.done)) {
                        if let Some(sid) = emitter_supplier_id {
                            crate::runtime::native_methods::supplier_done(*sid);
                        }
                        if !promise.is_resolved() {
                            promise.keep(last_value.clone(), String::new(), String::new());
                        }
                        return Ok(());
                    }
                }
            }
            // Nothing moved this round: block until a producer wakes us (or
            // the cap elapses, for the sources that still poll). Bounded by
            // the Promise-policy deadline so a stalled source cannot oversleep
            // the await's completion check.
            if !progressed {
                let mut cap = REACT_IDLE_WAIT;
                if let SupplyDrivePolicy::Promise { deadline, .. } = &policy {
                    let now = crate::runtime::thread_compat::Instant::now();
                    cap = if *deadline > now {
                        cap.min(*deadline - now)
                    } else {
                        Duration::ZERO
                    };
                }
                if !cap.is_zero() {
                    waker.wait_activity(cap);
                }
            }
        }

        Self::run_react_close_callbacks(self, react_subs);
        Ok(())
    }
}
