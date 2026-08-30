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
        if let ValueView::Sub(data) = cb.view()
            && !self.nested_react_callbacks.contains(&data.id)
        {
            self.clear_closure_captured_state_for(data.id);
        }
        // Every `whenever`/`LAST`/`QUIT`/`CLOSE` callback body dispatches
        // through here, on whichever thread actually runs it, so a `done`
        // raised anywhere in its dynamic extent (directly or via a nested
        // sub call) has a react/supply drive loop to terminate — see
        // `runtime::react_done_handler_depth`.
        let _react_done_handler =
            crate::runtime::react_done_handler_depth::ReactDoneHandlerGuard::new();
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
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
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
        // Lowest `supply_stream_consumers` index this react registered. The
        // consumers stay registered for the whole event loop: an inner
        // `whenever` of a `supply { }` source keeps firing after the body has
        // been run once, and its `emit` reaches the outer whenever's callback
        // only while that supply's StreamConsumer is still on the stack.
        let mut stream_base: Option<usize> = None;
        let finished =
            self.build_react_subscriptions(&subscriptions, &mut react_subs, &mut stream_base)?;
        // Any `whenever <Promise>` nested inside a `supply { }` body was
        // rewritten into a stand-in supplier above. Arming happens inside the
        // drive loop, AFTER the stand-ins' sinks are registered on the loop's
        // waker — arming here let an already-resolved promise's arm closure
        // emit+done into the sink-less stand-in, whose `done` handler reset the
        // buffered value away before the sink replay could see it. Only the
        // already-finished path (no drive loop will run) arms here.
        if finished {
            self.arm_pending_promise_whenevers();
            if let Some(base) = stream_base {
                self.supply_stream_consumers.truncate(base);
            }
            return Ok(());
        }

        let result = self.drive_react_subscriptions_nested(react_subs, SupplyDrivePolicy::React);
        if let Some(base) = stream_base {
            self.supply_stream_consumers.truncate(base);
        }
        result
    }

    /// Turn the `whenever` markers a react/supply body registered into
    /// [`ReactSubscription`]s. Returns `Ok(true)` when the react is already
    /// finished (a `done` fired while replaying a static source), in which case
    /// the caller must not drive the loop.
    ///
    /// Called once for the markers the body itself registered, and again from
    /// the drive loop for every marker a running `whenever` body adds — a
    /// `whenever` nested inside another `whenever`'s body is registered only
    /// when that body runs, which is long after the initial batch.
    pub(crate) fn build_react_subscriptions(
        &mut self,
        subscriptions: &[Value],
        react_subs: &mut Vec<ReactSubscription>,
        stream_base: &mut Option<usize>,
    ) -> Result<bool, RuntimeError> {
        for sub_val in subscriptions {
            if let ValueView::Array(items, ..) = sub_val.view()
                && items.len() >= 2
            {
                let source = &items[0];
                let callback = items[1].clone();
                let quit_callbacks = items
                    .get(3)
                    .and_then(crate::runtime::Interpreter::value_array_items)
                    .unwrap_or_default();
                let whenever_id = items.get(4).and_then(|value| match value.view() {
                    ValueView::Int(id) if id >= 0 => Some(id as u64),
                    _ => None,
                });

                match source.view() {
                    // Supply with a channel
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "Supply" => {
                        // Find the supply channel
                        let supply_id = self.resolve_supply_channel_id(&(attributes).as_map());
                        let is_lines = matches!(
                            attributes.as_map().get("is_lines").map(Value::view),
                            Some(ValueView::Bool(true))
                        );
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
                                whenever_id,
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
                        if let Some(ValueView::Int(supplier_id)) =
                            attributes.as_map().get("supplier_id").map(Value::view)
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
                                whenever_id,
                                supplier_id: Some(supplier_id as u64),
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
                            stream_base.get_or_insert(stream_idx);
                            let (od_res, emitted, body_ran_done) = loan_env!(
                                self,
                                run_on_demand_body(on_demand_cb.clone(), Some(emitter_supplier_id),)
                            );
                            // A `whenever <Promise>` registered by the body is not
                            // itself a `Supply`, so the marker walk below (which
                            // only recognizes a `Supply`-sourced registration via
                            // `value_to_react_subscription` /
                            // `register_nested_on_demand_source`) would silently
                            // drop it — a broken/kept promise nested inside a
                            // `supply { }` body then never reached this react loop
                            // at all (Cro::TCP::Connector.establish's `supply {
                            // whenever self.connect(...) { ... } }`). Rewrite it
                            // into a supplier-backed stand-in `Supply` first, same
                            // as the `.tap()` path already does, so the ordinary
                            // `supplier_id` handling below drives it.
                            let emitted = self.normalize_promise_whenever_markers(emitted);
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
                                self.supply_stream_consumers
                                    .truncate(stream_base.unwrap_or(stream_idx));
                                return Err(crate::runtime::Interpreter::wrap_react_died(od_err));
                            }
                            // If the streaming consumer signalled `done`, the
                            // whole react has been satisfied by this supply — fire
                            // its LAST callbacks and stop (don't set up the inner
                            // subscriptions or keep polling).
                            if streamed_done {
                                self.supply_stream_consumers
                                    .truncate(stream_base.unwrap_or(stream_idx));
                                let last_cbs = items
                                    .get(2)
                                    .and_then(crate::runtime::Interpreter::value_array_items)
                                    .unwrap_or_default();
                                for last_cb in &last_cbs {
                                    match self.call_react_callback(&last_cb.clone(), Vec::new()) {
                                        Err(e) if e.is_react_done() => return Ok(true),
                                        _ => {}
                                    }
                                }
                                return Ok(true);
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
                                        // Only tag this nested subscription as
                                        // emitter-owned when there is an outer
                                        // QUIT handler to actually route a die
                                        // to (the shadow subscription pushed
                                        // below, which alone polls
                                        // `on_demand_done`) — otherwise leave
                                        // it None so a LAST-phaser die keeps
                                        // propagating raw exactly as before,
                                        // with no shadow entry to observe it.
                                        if !quit_callbacks.is_empty() {
                                            rsub.emitter_supplier_id = Some(emitter_supplier_id);
                                        }
                                        react_subs.push(rsub);
                                    } else if let Some(early) =
                                        self.register_nested_on_demand_source(&v, react_subs, 0)?
                                    {
                                        // A chained `supply { }` stage: wired up
                                        // with its own emitter so the pipeline
                                        // streams rather than being replayed once.
                                        if early {
                                            early_done = true;
                                            break;
                                        }
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
                            // NOTE: the StreamConsumer registered above is left in
                            // place — it is truncated after the event loop (see
                            // `stream_base`), so a value arriving later on an inner
                            // `whenever`'s live source can still be re-emitted to
                            // this whenever's callback.
                            if early_done {
                                self.supply_stream_consumers
                                    .truncate(stream_base.unwrap_or(stream_idx));
                                return Ok(true);
                            }
                            // Supply.on-demand(..., closing => { ... }): the
                            // `closing` callback runs when the supply is closed.
                            let close_cbs =
                                Self::extract_supply_on_close_callbacks(&attributes.as_map());
                            if !close_cbs.is_empty() || !quit_callbacks.is_empty() {
                                if body_ran_done {
                                    // Synchronous body that ran `done` — closed now.
                                    for close_cb in close_cbs {
                                        let _ = self.call_react_callback(&close_cb, Vec::new());
                                    }
                                } else {
                                    // Async body (e.g. `start { emit; done }`): the
                                    // supply closes later, or a live-source
                                    // `whenever` nested in the body may die and
                                    // `supplier_quit` this emitter (see
                                    // `ReactSubscription::emitter_supplier_id`).
                                    // Register a source-less subscription carrying
                                    // the close/quit callbacks and this emitter's
                                    // own done-signal promise, so
                                    // `run_react_close_callbacks`/the `on_demand_done`
                                    // poll in `vm_react_subscriptions.rs` can reach
                                    // them.
                                    react_subs.push(ReactSubscription {
                                        whenever_id,
                                        close_callbacks: close_cbs,
                                        quit_callbacks: quit_callbacks.clone(),
                                        on_demand_done: Some(done_promise.clone()),
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
                                return Ok(true);
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
                                Err(e) if e.is_react_done() => return Ok(true),
                                _ => {}
                            }
                        }
                    }
                    // Promise source
                    ValueView::Promise(shared) => {
                        // Create a one-shot channel for the promise
                        let (tx, rx) =
                            crate::runtime::native_methods::supply_channel::supply_event_channel();
                        let shared_clone = shared.clone();
                        // Registered spawn: `wait()` clones the resolved
                        // result `Value` and the promise handle drops at
                        // thread exit — both are `Gc` mutations that must not
                        // race a cycle scan (the wait itself is STW-aware).
                        crate::runtime::builtins_system::spawn_gc_helper_thread(
                            "promise-wait",
                            move || {
                                let (result, _, _) = shared_clone.wait();
                                if shared_clone.status() == "Broken" {
                                    let _ = tx.send(SupplyEvent::Quit(result));
                                } else {
                                    let _ = tx.send(SupplyEvent::Emit(result));
                                    let _ = tx.send(SupplyEvent::Done);
                                }
                            },
                        );
                        react_subs.push(ReactSubscription {
                            whenever_id,
                            receiver: Some(rx),
                            promise: Some(shared.clone()),
                            ..ReactSubscription::new(callback)
                        });
                    }
                    // Channel source: poll values directly from the channel
                    ValueView::Channel(ch) => {
                        let last_callbacks = items
                            .get(2)
                            .and_then(crate::runtime::Interpreter::value_array_items)
                            .unwrap_or_default();
                        react_subs.push(ReactSubscription {
                            whenever_id,
                            last_callbacks,
                            channel: Some(ch.clone()),
                            ..ReactSubscription::new(callback)
                        });
                    }
                    _ => {}
                }
            }
        }
        Ok(false)
    }
}
