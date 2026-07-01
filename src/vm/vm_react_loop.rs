//! Interpreter-side react/supply drive loop (Stage 2).
//!
//! These methods were moved from `impl Interpreter` (`runtime/subtest.rs`) onto
//! `impl Interpreter` (Stage 2 PR1) so the `whenever`-body callbacks can run **compiled
//! bytecode** instead of the tree-walking `call_sub_value`. The Interpreter owns the
//! `Interpreter` by value, so a `&mut Interpreter` method cannot construct a Interpreter
//! — the loop itself must live here.
//!
//! All `whenever`-body / `LAST` / `QUIT` / `CLOSE` callback dispatch goes through
//! [`Interpreter::call_react_callback`], which runs the (on-the-fly compiled) closure via
//! `vm_call_map_block` with the triggering value bound as the block topic `$_`.
//! Loop-control signals (`done` / `next` / `last`) surface as `Err` just as the
//! old tree-walk path produced them, so the signal mapping is unchanged. Supply
//! `QUIT` handlers now dispatch natively too, via [`Interpreter::call_supply_quit_handler`]
//! (Stage 3 follow-up) — no drive-loop callback routes back through the
//! Interpreter's tree-walk `call_sub_value` anymore.
//!
//! The `await $supply` / `$supply.Promise` path reaches this loop through a thin
//! `Interpreter::drive_react_subscriptions` bridge (see `runtime/supply_promise.rs`)
//! that uses the established `mem::take` / `Interpreter::new` / `into_interpreter` dance.
//!
//! See PLAN.md Track C and the react-loop row of the Interpreter/interpreter ledger.

use super::*;
use crate::runtime::native_methods::{
    SupplyEvent, next_supplier_id, supplier_register_promise, take_supply_channel,
};
use crate::runtime::subtest::{ReactSubscription, StreamConsumer, SupplyDrivePolicy};
use std::sync::mpsc;

impl Interpreter {
    /// Dispatch a `whenever` body or one of its `LAST` / `QUIT` / `CLOSE` phaser
    /// callbacks as **compiled bytecode** (Stage 2). The first argument, when
    /// present, is the triggering value: it is bound as the block topic `$_`
    /// (and a lone pointy param) via `vm_call_map_block`'s explicit-topic path.
    /// This reproduces the tree-walk `call_sub_value` topic semantics — the
    /// on-the-fly routine-body compile would otherwise reset `$_` to `Any` and
    /// drop the topic. Loop-control signals (`done` / `next` / `last`) still
    /// surface as `Err` exactly as the tree-walk path produced them, so the
    /// drive loop's signal mapping (`run_react_consumer` etc.) is unchanged.
    pub(super) fn call_react_callback(
        &mut self,
        cb: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // A `whenever`/`LAST`/`QUIT` callback shares the enclosing react block's
        // lexicals. Each closure call persists its captured-outer free vars as
        // per-instance state (keyed by the callback's Sub id) and restores that
        // snapshot on re-entry. For a react callback that is wrong: on re-entry it
        // would restore a *stale* snapshot of a shared lexical (e.g. `my $order`
        // that a sibling `whenever` just updated), clobbering the sibling's write.
        // Drop this callback's per-instance state so it reads the shared lexical
        // from the live caller env — which every sibling writes back to.
        if let Value::Sub(data) = cb {
            self.clear_closure_captured_state_for(data.id);
        }
        let topic = args.first().cloned();
        self.vm_call_map_block(cb, args, topic, false)
    }

    /// Interpreter-native supply `QUIT` handler dispatch. Mirrors
    /// `Interpreter::call_supply_quit_handler` but runs the `QUIT` phaser body as
    /// **compiled bytecode** via [`Self::call_react_callback`] (with `reason`
    /// bound as `$_`) instead of the tree-walking `call_sub_value`. This is the
    /// last drive-loop callback that routed back through the Interpreter; with it
    /// gone the Interpreter react loop dispatches every `whenever`/`LAST`/`QUIT`/`CLOSE`
    /// callback natively. A `when`/`default`/`succeed` inside the body counts as
    /// handled; any other error propagates.
    pub(crate) fn call_supply_quit_handler(
        &mut self,
        quit_cb: Value,
        reason: Value,
    ) -> Result<(), RuntimeError> {
        let saved_when = self.when_matched();
        loan_env!(self, set_when_matched(false));
        match self.call_react_callback(&quit_cb, vec![reason]) {
            Ok(_) => {
                loan_env!(self, set_when_matched(saved_when));
                Ok(())
            }
            Err(err) if err.is_succeed() => {
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
        let _ = self.supply_emit_buffer.pop();
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
    pub(super) fn run_react_consumer(
        &mut self,
        sub: &mut ReactSubscription,
        value: Value,
    ) -> Result<bool, RuntimeError> {
        match self.call_react_callback(&sub.callback.clone(), vec![value.clone()]) {
            Ok(_) => Ok(false),
            Err(e) if e.is_react_done() => Ok(true),
            Err(e) if e.is_next() => Ok(false),
            Err(e) if e.is_last() => {
                for cb in sub.last_callbacks.clone() {
                    match self.call_react_callback(&cb, vec![value.clone()]) {
                        Err(le) if le.is_react_done() => {
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
        let subscriptions = self.supply_emit_buffer.pop().unwrap_or_default();
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
                            self.supply_stream_consumers.push(StreamConsumer {
                                supplier_id: emitter_supplier_id,
                                consumer_cb: callback.clone(),
                                done: false,
                            });
                            let stream_idx = self.supply_stream_consumers.len() - 1;
                            let (od_res, emitted, body_ran_done) = loan_env!(
                                self,
                                run_on_demand_body(on_demand_cb.clone(), Some(emitter_supplier_id),)
                            );
                            // Peek `done` (don't pop yet): the streaming consumer
                            // must stay registered while we replay any finite inner
                            // `whenever` sources below, so that `emit`s from those
                            // inner subscriptions route back to this consumer (the
                            // outer whenever's callback) via `try_stream_emit`.
                            let streamed_done = self
                                .supply_stream_consumers
                                .get(stream_idx)
                                .map(|c| c.done)
                                .unwrap_or(false);
                            if let Err(od_err) = od_res
                                && !od_err.is_react_done()
                            {
                                self.supply_stream_consumers.truncate(stream_idx);
                                return Err(crate::runtime::Interpreter::wrap_react_died(od_err));
                            }
                            // If the streaming consumer signalled `done`, the
                            // whole react has been satisfied by this supply — fire
                            // its LAST callbacks and stop (don't set up the inner
                            // subscriptions or keep polling).
                            if streamed_done {
                                self.supply_stream_consumers.truncate(stream_idx);
                                let last_cbs = items
                                    .get(2)
                                    .and_then(crate::runtime::Interpreter::value_array_items)
                                    .unwrap_or_default();
                                for last_cb in &last_cbs {
                                    match self.call_react_callback(&last_cb.clone(), Vec::new()) {
                                        Err(e) if e.is_react_done() => return Ok(()),
                                        _ => {}
                                    }
                                }
                                return Ok(());
                            }
                            // The emitted items may include subscription registrations
                            // from `whenever` inside the supply body. Live sources
                            // (channel / supplier_id) become ReactSubscriptions polled
                            // by the event loop. A finite source (`Supply.from-list`)
                            // has neither, so `value_to_react_subscription` returns
                            // None — replay it inline now. The streaming consumer is
                            // still registered, so `emit`s from the inner whenever body
                            // (an `emit` re-routed to the supply's emitter) reach the
                            // outer whenever's callback via `try_stream_emit`. This is
                            // what makes a `supply { whenever $up { emit ... } }`
                            // transform actually pass values downstream (e.g. Cro
                            // pipelines).
                            let mut early_done = false;
                            for v in emitted {
                                if crate::runtime::Interpreter::is_supply_subscription_registration(
                                    &v,
                                ) {
                                    if let Some(mut rsub) = self.value_to_react_subscription(&v) {
                                        rsub.on_demand_done = Some(done_promise.clone());
                                        react_subs.push(rsub);
                                    } else if self.replay_inner_static_subscription(&v)?
                                        == Some(true)
                                    {
                                        early_done = true;
                                        break;
                                    }
                                } else {
                                    let _ = self.call_react_callback(&callback.clone(), vec![v]);
                                }
                            }
                            self.supply_stream_consumers.truncate(stream_idx);
                            if early_done {
                                return Ok(());
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
                                Err(e) if e.is_react_done() => return Ok(()),
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

        self.drive_react_subscriptions_nested(react_subs, SupplyDrivePolicy::React)
    }
}
