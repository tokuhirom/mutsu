//! Shared react/supply subscription drive loop (`drive_react_subscriptions_nested`),
//! split from `vm_react_loop` (§7-8 file split).
use super::*;
use crate::runtime::native_methods::{
    SupplyEvent, supplier_snapshot, take_promise_combinator_sources,
};
use crate::runtime::subtest::{ReactSubscription, SupplyDrivePolicy};
use std::sync::mpsc;
use std::time::Duration;

impl Interpreter {
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
                    promise.keep(Value::Nil, String::new(), String::new());
                    return Ok(());
                }
            }
            let mut all_done = true;
            for sub in react_subs.iter_mut() {
                if sub.done {
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
                if let Some(supplier_id) = sub.supplier_id {
                    let (values, done, quit) = supplier_snapshot(supplier_id);
                    while sub.supplier_next_index < values.len() {
                        // Check head_limit before processing more values
                        if let Some(limit) = sub.head_limit
                            && sub.emit_count >= limit
                        {
                            // Reached the head limit — mark as done
                            for callback in &sub.last_callbacks {
                                self.call_react_callback(&callback.clone(), Vec::new())?;
                            }
                            sub.done = true;
                            break;
                        }
                        let value = values[sub.supplier_next_index].clone();
                        sub.supplier_next_index += 1;
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
                                sub.emit_count += 1;
                                if let Some(limit) = sub.head_limit
                                    && sub.emit_count >= limit
                                {
                                    for callback in &sub.last_callbacks {
                                        self.call_react_callback(&callback.clone(), Vec::new())?;
                                    }
                                    sub.done = true;
                                    break;
                                }
                            }
                        } else {
                            if self.run_react_consumer(sub, value)? {
                                break 'react_loop;
                            }
                            // `last` in the body marked this whenever done; stop
                            // pulling further values from the source.
                            if sub.done {
                                break;
                            }
                            sub.emit_count += 1;
                            if let Some(limit) = sub.head_limit
                                && sub.emit_count >= limit
                            {
                                for callback in &sub.last_callbacks {
                                    self.call_react_callback(&callback.clone(), Vec::new())?;
                                }
                                sub.done = true;
                                break;
                            }
                        }
                    }
                    if let Some(error) = quit {
                        let mut handled = false;
                        for quit_cb in &sub.quit_callbacks {
                            self.call_supply_quit_handler(quit_cb.clone(), error.clone())?;
                            handled = true;
                        }
                        if handled {
                            sub.done = true;
                            continue;
                        }
                        Self::run_react_close_callbacks(self, &react_subs);
                        let quit_err =
                            crate::runtime::Interpreter::runtime_error_from_supply_reason(error);
                        return Err(crate::runtime::Interpreter::wrap_react_died(quit_err));
                    }
                    if done {
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
                    continue;
                }
                let Some(receiver) = sub.receiver.as_ref() else {
                    sub.done = true;
                    continue;
                };
                if let Some(promise) = sub.promise.as_ref()
                    && let Some(sources) = take_promise_combinator_sources(promise)
                {
                    for source in sources {
                        source.result_blocking();
                    }
                    continue;
                }
                // Try to receive with a short timeout
                match receiver.recv_timeout(timeout) {
                    Ok(SupplyEvent::Emit(value)) => match &mut policy {
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
                    Ok(SupplyEvent::Done) => {
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
                    Ok(SupplyEvent::Quit(error)) => {
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
                    Err(mpsc::RecvTimeoutError::Timeout) => {}
                    Err(mpsc::RecvTimeoutError::Disconnected) => {
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
                        promise.keep(Value::Nil, String::new(), String::new());
                        return Ok(());
                    }
                }
            }
        }

        Self::run_react_close_callbacks(self, &react_subs);
        Ok(())
    }
}
