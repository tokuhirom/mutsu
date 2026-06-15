//! VM-side react/supply drive loop (Stage 2).
//!
//! These methods were moved from `impl Interpreter` (`runtime/subtest.rs`) onto
//! `impl VM` (Stage 2 PR1) so the `whenever`-body callbacks can run **compiled
//! bytecode** instead of the tree-walking `call_sub_value`. The VM owns the
//! `Interpreter` by value, so a `&mut Interpreter` method cannot construct a VM
//! — the loop itself must live here.
//!
//! All `whenever`-body / `LAST` / `QUIT` / `CLOSE` callback dispatch goes through
//! [`VM::call_react_callback`], which runs the (on-the-fly compiled) closure via
//! `vm_call_map_block` with the triggering value bound as the block topic `$_`.
//! Loop-control signals (`done` / `next` / `last`) surface as `Err` just as the
//! old tree-walk path produced them, so the signal mapping is unchanged. Supply
//! `QUIT` handlers now dispatch natively too, via [`VM::call_supply_quit_handler`]
//! (Stage 3 follow-up) — no drive-loop callback routes back through the
//! Interpreter's tree-walk `call_sub_value` anymore.
//!
//! The `await $supply` / `$supply.Promise` path reaches this loop through a thin
//! `Interpreter::drive_react_subscriptions` bridge (see `runtime/supply_promise.rs`)
//! that uses the established `mem::take` / `VM::new` / `into_interpreter` dance.
//!
//! See PLAN.md Track C and the react-loop row of the VM/interpreter ledger.

use super::*;
use crate::runtime::native_methods::{
    SupplyEvent, next_supplier_id, supplier_register_promise, supplier_snapshot,
    take_promise_combinator_sources, take_supply_channel,
};
use crate::runtime::subtest::{ReactSubscription, StreamConsumer, SupplyDrivePolicy};
use std::sync::mpsc;
use std::time::Duration;

impl VM {
    /// Dispatch a `whenever` body or one of its `LAST` / `QUIT` / `CLOSE` phaser
    /// callbacks as **compiled bytecode** (Stage 2). The first argument, when
    /// present, is the triggering value: it is bound as the block topic `$_`
    /// (and a lone pointy param) via `vm_call_map_block`'s explicit-topic path.
    /// This reproduces the tree-walk `call_sub_value` topic semantics — the
    /// on-the-fly routine-body compile would otherwise reset `$_` to `Any` and
    /// drop the topic. Loop-control signals (`done` / `next` / `last`) still
    /// surface as `Err` exactly as the tree-walk path produced them, so the
    /// drive loop's signal mapping (`run_react_consumer` etc.) is unchanged.
    fn call_react_callback(&mut self, cb: &Value, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let topic = args.first().cloned();
        self.vm_call_map_block(cb, args, topic, false)
    }

    /// VM-native supply `QUIT` handler dispatch. Mirrors
    /// `Interpreter::call_supply_quit_handler` but runs the `QUIT` phaser body as
    /// **compiled bytecode** via [`Self::call_react_callback`] (with `reason`
    /// bound as `$_`) instead of the tree-walking `call_sub_value`. This is the
    /// last drive-loop callback that routed back through the Interpreter; with it
    /// gone the VM react loop dispatches every `whenever`/`LAST`/`QUIT`/`CLOSE`
    /// callback natively. A `when`/`default`/`succeed` inside the body counts as
    /// handled; any other error propagates.
    fn call_supply_quit_handler(
        &mut self,
        quit_cb: Value,
        reason: Value,
    ) -> Result<(), RuntimeError> {
        let saved_when = self.interpreter.when_matched();
        loan_env!(self, set_when_matched(false));
        match self.call_react_callback(&quit_cb, vec![reason]) {
            Ok(_) => {
                loan_env!(self, set_when_matched(saved_when));
                Ok(())
            }
            Err(err) if err.is_succeed => {
                loan_env!(self, set_when_matched(saved_when));
                Ok(())
            }
            Err(err) => {
                loan_env!(self, set_when_matched(saved_when));
                Err(err)
            }
        }
    }

    /// Run the react event loop: poll all registered subscriptions
    /// until all are done.
    /// Drain any queued react subscriptions without running the event loop.
    /// Used when `done;` was called in the react body and we just need to
    /// clean up without processing events.
    pub(crate) fn run_react_event_loop_drain(&mut self) {
        let _ = self.interpreter.supply_emit_buffer.pop();
    }

    /// Deliver one value to a `whenever` subscription's callback, mapping the
    /// loop-control signals a `whenever` body may raise:
    /// - `done` (`is_react_done`) ends the whole react — returns `Ok(true)` so
    ///   the caller breaks the event loop.
    /// - `next` (`is_next`) skips the rest of the body for this value — the
    ///   callback already unwound, so just continue (`Ok(false)`).
    /// - `last` (`is_last`) stops only this `whenever`: fire its LAST phasers
    ///   (with the triggering value as topic) and mark the subscription done.
    ///   The react keeps driving any other subscriptions.
    ///
    /// Any other error propagates.
    fn run_react_consumer(
        &mut self,
        sub: &mut ReactSubscription,
        value: Value,
    ) -> Result<bool, RuntimeError> {
        match self.call_react_callback(&sub.callback.clone(), vec![value.clone()]) {
            Ok(_) => Ok(false),
            Err(e) if e.is_react_done => Ok(true),
            Err(e) if e.is_next => Ok(false),
            Err(e) if e.is_last => {
                for cb in sub.last_callbacks.clone() {
                    match self.call_react_callback(&cb, vec![value.clone()]) {
                        Err(le) if le.is_react_done => {
                            sub.done = true;
                            return Ok(true);
                        }
                        other => {
                            other?;
                        }
                    }
                }
                sub.done = true;
                Ok(false)
            }
            Err(e) => Err(e),
        }
    }

    pub(crate) fn run_react_event_loop(&mut self) -> Result<(), RuntimeError> {
        // Take the subscriptions collected during the react body
        let subscriptions = self
            .interpreter
            .supply_emit_buffer
            .pop()
            .unwrap_or_default();
        if subscriptions.is_empty() {
            return Ok(());
        }

        // Extract the subscriptions from the buffer
        // Each entry is a tuple of (receiver_key, callback) stored as Values
        // We need to reconstruct the actual receivers
        let mut react_subs: Vec<ReactSubscription> = Vec::new();

        for sub_val in &subscriptions {
            if let Value::Array(items, ..) = sub_val
                && items.len() >= 2
            {
                let source = &items[0];
                let callback = items[1].clone();

                match source {
                    // Supply with a channel
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "Supply" => {
                        // Find the supply channel
                        let supply_id = self.resolve_supply_channel_id(&(attributes).as_map());
                        let is_lines =
                            matches!(attributes.as_map().get("is_lines"), Some(Value::Bool(true)));
                        let head_limit = Self::extract_head_limit(&(attributes).as_map());
                        if let Some(sid) = supply_id
                            && let Some(rx) = take_supply_channel(sid)
                        {
                            let last_callbacks = items
                                .get(2)
                                .and_then(crate::runtime::Interpreter::value_array_items)
                                .unwrap_or_default();
                            let quit_callbacks = items
                                .get(3)
                                .and_then(crate::runtime::Interpreter::value_array_items)
                                .unwrap_or_default();
                            react_subs.push(ReactSubscription {
                                receiver: Some(rx),
                                close_callbacks: Self::extract_supply_on_close_callbacks(
                                    &(attributes).as_map(),
                                ),
                                last_callbacks,
                                quit_callbacks,
                                is_lines,
                                head_limit,
                                ..ReactSubscription::new(callback)
                            });
                            continue;
                        }
                        if let Some(Value::Int(supplier_id)) =
                            attributes.as_map().get("supplier_id")
                        {
                            let last_callbacks = items
                                .get(2)
                                .and_then(crate::runtime::Interpreter::value_array_items)
                                .unwrap_or_default();
                            let quit_callbacks = items
                                .get(3)
                                .and_then(crate::runtime::Interpreter::value_array_items)
                                .unwrap_or_default();
                            react_subs.push(ReactSubscription {
                                supplier_id: Some(*supplier_id as u64),
                                close_callbacks: Self::extract_supply_on_close_callbacks(
                                    &(attributes).as_map(),
                                ),
                                last_callbacks,
                                quit_callbacks,
                                is_lines,
                                head_limit,
                                ..ReactSubscription::new(callback)
                            });
                            continue;
                        }
                        // Handle on-demand supplies: execute the callback to produce values
                        if let Some(on_demand_cb) = attributes.as_map().get("on_demand_callback") {
                            // Execute the on-demand callback, which calls emit on the
                            // emitter. Use a tracked emitter supplier id so that `done`
                            // inside the supply block (rewritten to `$emitter.done()`)
                            // marks this emitter done instead of raising the react-done
                            // signal; the event loop below watches it to complete the
                            // flattened subscriptions. If the callback dies, propagate
                            // as X::React::Died.
                            let emitter_supplier_id = next_supplier_id();
                            // Register a done-signal promise on the emitter BEFORE
                            // running the body. `$emitter.done()` resolves all pending
                            // promises (line in supplier_done) and only then resets the
                            // supplier's done flag, so this promise is the only thing
                            // that survives to tell the loop the supply completed.
                            let done_promise = crate::value::SharedPromise::new();
                            supplier_register_promise(emitter_supplier_id, done_promise.clone());
                            // Register a streaming consumer so that `emit` inside
                            // the supply body delivers values to this whenever's
                            // callback synchronously. This lets a synchronously
                            // infinite body (`supply { loop { emit(...) } }`) be
                            // terminated when the consumer signals `done`, instead
                            // of buffering every emitted value (which would never
                            // return). Direct emits stream live; inner `whenever`
                            // registrations still flow through `supply_emit_buffer`
                            // and are set up as ReactSubscriptions below.
                            self.interpreter
                                .supply_stream_consumers
                                .push(StreamConsumer {
                                    supplier_id: emitter_supplier_id,
                                    consumer_cb: callback.clone(),
                                    done: false,
                                });
                            let (od_res, emitted, body_ran_done) = loan_env!(
                                self,
                                run_on_demand_body(on_demand_cb.clone(), Some(emitter_supplier_id),)
                            );
                            let streamed_done = self
                                .interpreter
                                .supply_stream_consumers
                                .pop()
                                .map(|c| c.done)
                                .unwrap_or(false);
                            if let Err(od_err) = od_res
                                && !od_err.is_react_done
                            {
                                return Err(crate::runtime::Interpreter::wrap_react_died(od_err));
                            }
                            // If the streaming consumer signalled `done`, the
                            // whole react has been satisfied by this supply — fire
                            // its LAST callbacks and stop (don't set up the inner
                            // subscriptions or keep polling).
                            if streamed_done {
                                let last_cbs = items
                                    .get(2)
                                    .and_then(crate::runtime::Interpreter::value_array_items)
                                    .unwrap_or_default();
                                for last_cb in &last_cbs {
                                    match self.call_react_callback(&last_cb.clone(), Vec::new()) {
                                        Err(e) if e.is_react_done => return Ok(()),
                                        _ => {}
                                    }
                                }
                                return Ok(());
                            }
                            // The emitted items may include subscription registrations
                            // from `whenever` inside the supply body. Convert them to
                            // ReactSubscriptions for the event loop, tagging each with
                            // the done-signal promise so the loop can complete them
                            // when the supply body calls `done`.
                            for v in emitted {
                                if crate::runtime::Interpreter::is_supply_subscription_registration(
                                    &v,
                                ) {
                                    if let Some(mut rsub) =
                                        self.interpreter.value_to_react_subscription(&v)
                                    {
                                        rsub.on_demand_done = Some(done_promise.clone());
                                        react_subs.push(rsub);
                                    }
                                } else {
                                    let _ = self.call_react_callback(&callback.clone(), vec![v]);
                                }
                            }
                            // Supply.on-demand(..., closing => { ... }): the
                            // `closing` callback runs when the supply is closed.
                            let close_cbs =
                                Self::extract_supply_on_close_callbacks(&attributes.as_map());
                            if !close_cbs.is_empty() {
                                if body_ran_done {
                                    // Synchronous body that ran `done` — closed now.
                                    for close_cb in close_cbs {
                                        let _ = self.call_react_callback(&close_cb, Vec::new());
                                    }
                                } else {
                                    // Async body (e.g. `start { emit; done }`): the
                                    // supply closes later. Register a source-less
                                    // subscription carrying the close callbacks so
                                    // run_react_close_callbacks fires them when the
                                    // react ends.
                                    react_subs.push(ReactSubscription {
                                        close_callbacks: close_cbs,
                                        ..ReactSubscription::new(callback.clone())
                                    });
                                }
                            }
                        } else {
                            // No channel, no on-demand - replay static values.
                            // replay_static_supply handles `last`/`next`/`done`
                            // in the whenever body and fires this whenever's LAST
                            // phasers itself, so skip the shared post-LAST below.
                            let last_cbs = items
                                .get(2)
                                .and_then(crate::runtime::Interpreter::value_array_items)
                                .unwrap_or_default();
                            if self.replay_static_supply(
                                &(attributes).as_map(),
                                &callback,
                                &last_cbs,
                            )? {
                                return Ok(());
                            }
                            continue;
                        }
                        // Fire LAST callbacks after the on-demand supply completes
                        let last_cbs = items
                            .get(2)
                            .and_then(crate::runtime::Interpreter::value_array_items)
                            .unwrap_or_default();
                        for last_cb in &last_cbs {
                            match self.call_react_callback(&last_cb.clone(), Vec::new()) {
                                Err(e) if e.is_react_done => return Ok(()),
                                _ => {}
                            }
                        }
                    }
                    // Promise source
                    Value::Promise(shared) => {
                        // Create a one-shot channel for the promise
                        let (tx, rx) = mpsc::channel();
                        let shared_clone = shared.clone();
                        std::thread::spawn(move || {
                            let (result, _, _) = shared_clone.wait();
                            let _ = tx.send(SupplyEvent::Emit(result));
                            let _ = tx.send(SupplyEvent::Done);
                        });
                        react_subs.push(ReactSubscription {
                            receiver: Some(rx),
                            promise: Some(shared.clone()),
                            ..ReactSubscription::new(callback)
                        });
                    }
                    // Channel source: poll values directly from the channel
                    Value::Channel(ch) => {
                        let last_callbacks = items
                            .get(2)
                            .and_then(crate::runtime::Interpreter::value_array_items)
                            .unwrap_or_default();
                        react_subs.push(ReactSubscription {
                            last_callbacks,
                            channel: Some(ch.clone()),
                            ..ReactSubscription::new(callback)
                        });
                    }
                    _ => {}
                }
            }
        }

        self.drive_react_subscriptions(react_subs, SupplyDrivePolicy::React)
    }

    /// Shared subscription drive loop backing both `react { ... }` and the
    /// `await $supply` / `$supply.Promise` paths. `react`-built and
    /// promise-built subscriptions poll through here; `policy` selects how each
    /// emitted value is dispatched and when the loop completes (see
    /// [`SupplyDrivePolicy`]).
    pub(crate) fn drive_react_subscriptions(
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
                            Err(e) if e.is_react_done => break 'react_loop,
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
                                Err(e) if e.is_react_done => break 'react_loop,
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
                            self.interpreter.supply_emit_buffer.push(Vec::new());
                            let cb_result =
                                self.call_react_callback(&sub.callback.clone(), vec![value]);
                            let emitted = self
                                .interpreter
                                .supply_emit_buffer
                                .pop()
                                .unwrap_or_default();
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
                                if err.is_react_done || err.is_last {
                                    promise.keep(last_value.clone(), String::new(), String::new());
                                    return Ok(());
                                }
                                // `next`/`redo` are loop control, not completion.
                                if !err.is_next && !err.is_redo {
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
                                    Err(e) if e.is_react_done => break 'react_loop,
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

    fn extract_head_limit(attributes: &HashMap<String, Value>) -> Option<usize> {
        if let Some(Value::Int(n)) = attributes.get("head_limit") {
            Some(*n as usize)
        } else {
            None
        }
    }

    fn extract_supply_on_close_callbacks(attributes: &HashMap<String, Value>) -> Vec<Value> {
        if let Some(Value::Array(callbacks, ..)) = attributes.get("on_close_callbacks") {
            callbacks.to_vec()
        } else {
            Vec::new()
        }
    }

    fn run_react_close_callbacks(&mut self, react_subs: &[ReactSubscription]) {
        for sub in react_subs {
            for callback in &sub.close_callbacks {
                let _ = self.call_react_callback(&callback.clone(), Vec::new());
            }
        }
    }

    /// Resolve the supply_id to use for channel lookup.
    /// For a "lines" supply, use the parent_supply_id.
    fn resolve_supply_channel_id(&self, attributes: &HashMap<String, Value>) -> Option<u64> {
        // If this is a "lines" supply, use the parent's supply_id
        if let Some(Value::Int(parent_id)) = attributes.get("parent_supply_id") {
            return Some(*parent_id as u64);
        }
        // Otherwise use this supply's own supply_id
        if let Some(Value::Int(id)) = attributes.get("supply_id") {
            return Some(*id as u64);
        }
        None
    }

    /// Replay static supply values (non-streaming supplies)
    /// Replay a static supply's values through a `whenever` callback, honouring
    /// the loop-control signals the body may raise: `done` ends the react
    /// (returns `Ok(true)`), `next` skips to the next value, `last` stops the
    /// whenever early. This whenever's LAST phasers are fired here — with the
    /// triggering value as topic when stopped via `last`, otherwise with no
    /// topic on natural completion. Returns `Ok(true)` iff `done` was signalled.
    fn replay_static_supply(
        &mut self,
        attributes: &HashMap<String, Value>,
        callback: &Value,
        last_callbacks: &[Value],
    ) -> Result<bool, RuntimeError> {
        let mut last_topic: Option<Value> = None;
        if let Some(Value::Array(values, ..)) = attributes.get("values") {
            for v in values.iter() {
                match self.call_react_callback(&callback.clone(), vec![v.clone()]) {
                    Ok(_) => {}
                    Err(e) if e.is_react_done => return Ok(true),
                    Err(e) if e.is_next => continue,
                    Err(e) if e.is_last => {
                        last_topic = Some(v.clone());
                        break;
                    }
                    Err(e) => return Err(e),
                }
            }
        }
        for cb in last_callbacks {
            let args = match &last_topic {
                Some(v) => vec![v.clone()],
                None => Vec::new(),
            };
            match self.call_react_callback(&cb.clone(), args) {
                Err(e) if e.is_react_done => return Ok(true),
                other => {
                    other?;
                }
            }
        }
        Ok(false)
    }
}
