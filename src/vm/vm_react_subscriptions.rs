//! Shared react/supply subscription drive loop (`drive_react_subscriptions_nested`),
//! split from `vm_react_loop` (§7-8 file split).
use super::*;
use crate::runtime::native_methods::{
    SupplyEvent, supplier_snapshot_seqs, take_promise_combinator_sources,
};
use crate::runtime::subtest::{ReactSubscription, SupplyDrivePolicy};
use std::sync::mpsc;
use std::time::Duration;

impl Interpreter {
    /// Deliver every value currently buffered across all supplier-backed
    /// subscriptions to their `whenever` consumers, merged into a single global
    /// emit order (by each value's emit sequence number), then honour any
    /// per-supplier `done`/`quit` signals. Returns `Ok(true)` if a consumer
    /// raised react `done`; propagates `Err` for an unhandled supplier `quit`.
    ///
    /// Global-order merge (rather than draining one supplier fully before the
    /// next) is what makes sibling `whenever $s.grep(...)` derived supplies
    /// interleave the way Raku delivers them — each value reaches its consumer
    /// in the order it was `emit`ted into the source, across suppliers. It also
    /// honours Raku's ordering guarantee that values emitted before a later
    /// terminating event are still delivered, since the polling loop calls this
    /// both at the top of each iteration and just before running a receiver
    /// (Promise / `start`) source's consumer.
    fn drain_supplier_subs_ordered(
        &mut self,
        react_subs: &mut [ReactSubscription],
    ) -> Result<bool, RuntimeError> {
        // Deliver the globally-earliest pending value, one at a time; re-scan
        // each round because a consumer may emit more or mark subs done.
        loop {
            let mut best: Option<(u64, usize, Value)> = None;
            for (i, sub) in react_subs.iter().enumerate() {
                if sub.done {
                    continue;
                }
                let Some(sid) = sub.supplier_id else {
                    continue;
                };
                if let Some(limit) = sub.head_limit
                    && sub.emit_count >= limit
                {
                    continue;
                }
                let (values, seqs, _done, _quit) = supplier_snapshot_seqs(sid);
                let idx = sub.supplier_next_index;
                if idx < values.len() {
                    let seq = seqs.get(idx).copied().unwrap_or(0);
                    if best.as_ref().is_none_or(|(bseq, _, _)| seq < *bseq) {
                        best = Some((seq, i, values[idx].clone()));
                    }
                }
            }
            let Some((_seq, i, value)) = best else {
                break;
            };
            react_subs[i].supplier_next_index += 1;
            if react_subs[i].is_lines {
                let chunk = value.to_string_value();
                react_subs[i].line_buffer.push_str(&chunk);
                while let Some(pos) = react_subs[i].line_buffer.find('\n') {
                    let line = react_subs[i].line_buffer[..pos].to_string();
                    react_subs[i].line_buffer = react_subs[i].line_buffer[pos + 1..].to_string();
                    if self.run_react_consumer(&mut react_subs[i], Value::str(line))? {
                        return Ok(true);
                    }
                    if react_subs[i].done {
                        break;
                    }
                    react_subs[i].emit_count += 1;
                    if self.head_limit_reached(&mut react_subs[i])? {
                        break;
                    }
                }
            } else {
                if self.run_react_consumer(&mut react_subs[i], value)? {
                    return Ok(true);
                }
                if !react_subs[i].done {
                    react_subs[i].emit_count += 1;
                    self.head_limit_reached(&mut react_subs[i])?;
                }
            }
        }
        // All pending values delivered — now honour per-supplier done/quit.
        for i in 0..react_subs.len() {
            if react_subs[i].done {
                continue;
            }
            let Some(sid) = react_subs[i].supplier_id else {
                continue;
            };
            let (values, _seqs, done, quit) = supplier_snapshot_seqs(sid);
            if react_subs[i].supplier_next_index < values.len() {
                continue;
            }
            if let Some(error) = quit {
                let mut handled = false;
                for quit_cb in react_subs[i].quit_callbacks.clone() {
                    self.call_supply_quit_handler(quit_cb, error.clone())?;
                    handled = true;
                }
                if handled {
                    react_subs[i].done = true;
                    continue;
                }
                Self::run_react_close_callbacks(self, react_subs);
                let quit_err = crate::runtime::Interpreter::runtime_error_from_supply_reason(error);
                return Err(crate::runtime::Interpreter::wrap_react_died(quit_err));
            }
            if done {
                if react_subs[i].is_lines && !react_subs[i].line_buffer.is_empty() {
                    let remaining = std::mem::take(&mut react_subs[i].line_buffer);
                    let cb = react_subs[i].callback.clone();
                    match self.call_react_callback(&cb, vec![Value::str(remaining)]) {
                        Err(e) if e.is_react_done() => return Ok(true),
                        other => {
                            other?;
                        }
                    }
                }
                for cb in react_subs[i].last_callbacks.clone() {
                    self.call_react_callback(&cb, Vec::new())?;
                }
                react_subs[i].done = true;
            }
        }
        Ok(false)
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
    /// promise-built subscriptions poll through here; `policy` selects how each
    /// emitted value is dispatched and when the loop completes (see
    /// [`SupplyDrivePolicy`]).
    pub(crate) fn drive_react_subscriptions_nested(
        &mut self,
        mut react_subs: Vec<ReactSubscription>,
        mut policy: SupplyDrivePolicy,
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

        // Event loop: poll all subscriptions
        let timeout = Duration::from_millis(10);
        'react_loop: loop {
            // GC park point: an idle react loop polls `recv_timeout` without
            // dispatching bytecode, so it would never reach the backedge
            // safepoint — park here so a stop-the-world can proceed while the
            // loop waits for events. (Unconditional: `gc_safepoint` below only
            // parks when a trigger is armed.)
            crate::gc::gc_park_point();
            // GC safepoint (§9.2a `react_poll`): one drive-loop poll unit.
            crate::gc::gc_safepoint(crate::gc::SafepointKind::ReactPoll);
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
                if std::time::Instant::now() >= *deadline {
                    promise.keep(Value::NIL, String::new(), String::new());
                    return Ok(());
                }
            }
            // Phase 1: deliver all buffered supplier values in global emit order
            // (so sibling `whenever $s.grep(...)` supplies interleave correctly),
            // and honour supplier done/quit.
            if self.drain_supplier_subs_ordered(&mut react_subs)? {
                break 'react_loop;
            }
            // Phase 2: poll the non-supplier subscriptions (on-demand / channel /
            // receiver sources).
            let mut all_done = true;
            for si in 0..react_subs.len() {
                let sub = &mut react_subs[si];
                if sub.done {
                    continue;
                }
                // Supplier-backed subs were fully serviced in phase 1.
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
                        }
                        Ok(None) => {
                            // No value available yet; if channel is closed, mark done
                            if !ch.can_send() {
                                for callback in &sub.last_callbacks {
                                    self.call_react_callback(&callback.clone(), Vec::new())?;
                                }
                                sub.done = true;
                            }
                        }
                        Err(_err) => {
                            sub.done = true;
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
                // Poll the receiver with a short timeout. The `Result` is owned, so
                // the borrow of the receiver ends on this line — freeing
                // `react_subs` for the pre-drain below.
                let poll = react_subs[si]
                    .receiver
                    .as_ref()
                    .map(|r| r.recv_timeout(timeout));
                // Raku ordering guarantee: values `emit`ted into a supplier
                // *before* the event this receiver just delivered are causally
                // earlier and must reach their `whenever`s first — even when that
                // event's callback ends the react (e.g. `whenever start { emit … }`
                // finishing while a sibling `whenever` calls `done`, so the sibling
                // supplier's already-emitted values would otherwise be lost). Drain
                // every supplier subscription before running this receiver's
                // consumer, so their pending values are delivered in source order.
                if matches!(poll, Some(Ok(SupplyEvent::Emit(_))))
                    && matches!(policy, SupplyDrivePolicy::React)
                    && self.drain_supplier_subs_ordered(&mut react_subs)?
                {
                    break 'react_loop;
                }
                let sub = &mut react_subs[si];
                // Try to receive with a short timeout
                match poll {
                    Some(Ok(SupplyEvent::Emit(value))) => match &mut policy {
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
                                    promise.keep(last_value.clone(), String::new(), String::new());
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
                    },
                    Some(Ok(SupplyEvent::Done)) => {
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
                    Some(Err(mpsc::RecvTimeoutError::Timeout)) | None => {}
                    Some(Err(mpsc::RecvTimeoutError::Disconnected)) => {
                        sub.done = true;
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
        }

        Self::run_react_close_callbacks(self, &react_subs);
        Ok(())
    }
}
