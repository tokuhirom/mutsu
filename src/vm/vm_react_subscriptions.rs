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
    SupplyEvent, supplier_sink_register, supplier_sink_unregister, take_promise_combinator_sources,
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
                            self.call_react_callback(&cb, Vec::new())?;
                        }
                        react_subs[key].done = true;
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
        // Mark the drive loop active so a `whenever` that taps an on-demand
        // supply from inside a running react routes the supply's
        // `closing => { ... }` callbacks to this (main) thread via
        // `pending_tap_closes`, rather than firing them on an async body's
        // worker thread (see `native_supply_mut_methods` tap on-demand path).
        self.react_active += 1;
        let result = self.drive_react_subscriptions_inner(react_subs, policy);
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
    ) -> Result<(), RuntimeError> {
        if react_subs.is_empty() {
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

        let waker = ReactWaker::new();
        // Supplier-backed subscriptions: register push sinks (this also
        // replays any already-buffered values into the queue).
        let mut sink_regs: Vec<(u64, u64)> = Vec::new();
        for (i, sub) in react_subs.iter().enumerate() {
            if let Some(sid) = sub.supplier_id {
                sink_regs.push((sid, supplier_sink_register(sid, i, &waker)));
            }
        }
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
        let result = self.drive_react_subscriptions_loop(&mut react_subs, policy, &waker);
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

    fn drive_react_subscriptions_loop(
        &mut self,
        react_subs: &mut [ReactSubscription],
        mut policy: SupplyDrivePolicy,
        waker: &ReactWaker,
    ) -> Result<(), RuntimeError> {
        'react_loop: loop {
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
            // Phase 1: deliver all queued supplier events in push (= emit)
            // order, honouring per-supplier done/quit.
            if self.dispatch_waker_events(waker, react_subs, &mut progressed)? {
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
                    for callback in &sub.last_callbacks {
                        match self.call_react_callback(&callback.clone(), Vec::new()) {
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
                    react_subs[si].done = true;
                    continue;
                }
                if let Some(promise) = react_subs[si].promise.clone()
                    && let Some(sources) = take_promise_combinator_sources(&promise)
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
                    && self.dispatch_waker_events(waker, react_subs, &mut progressed)?
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
                                if let Some(last) = emitted.last() {
                                    *last_value = last.clone();
                                }
                                if promise.is_resolved() {
                                    return Ok(());
                                }
                                if let Err(err) = cb_result {
                                    // `done`/`last` inside the whenever complete the
                                    // supply: keep the promise with the last emitted
                                    // value immediately rather than spinning to the
                                    // deadline.
                                    if err.is_react_done() || err.is_last() {
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
                                        let cause = err
                                            .exception
                                            .as_deref()
                                            .cloned()
                                            .unwrap_or_else(|| Value::str(err.message.clone()));
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
            match &policy {
                SupplyDrivePolicy::React => {
                    if all_done || react_subs.iter().all(|s| s.done) {
                        break;
                    }
                }
                SupplyDrivePolicy::Promise { promise, .. } => {
                    if promise.is_resolved() {
                        return Ok(());
                    }
                    // All channels closed without the promise resolving:
                    // complete the await with Nil (matching the legacy
                    // supply_promise_on_demand loop).
                    if all_done || react_subs.iter().all(|s| s.done) {
                        promise.keep(Value::NIL, String::new(), String::new());
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
