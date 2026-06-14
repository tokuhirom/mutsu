use super::*;
use crate::runtime::native_methods::{
    SupplyEvent, next_supplier_id, supplier_register_promise, supplier_snapshot,
    take_promise_combinator_sources, take_supply_channel,
};
use crate::symbol::Symbol;
use std::sync::mpsc;
use std::time::Duration;

/// A subscription registered by a `whenever` block inside a `react` block.
pub(crate) struct ReactSubscription {
    pub receiver: Option<mpsc::Receiver<SupplyEvent>>,
    pub supplier_id: Option<u64>,
    pub supplier_next_index: usize,
    pub callback: Value,
    pub close_callbacks: Vec<Value>,
    pub last_callbacks: Vec<Value>,
    pub quit_callbacks: Vec<Value>,
    pub done: bool,
    /// When true, split incoming chunks into lines before calling the callback.
    pub is_lines: bool,
    /// Buffer for incomplete lines (when is_lines is true).
    pub line_buffer: String,
    /// Optional limit on number of emissions (from .head(N))
    pub head_limit: Option<usize>,
    /// Count of emissions so far (used with head_limit)
    pub emit_count: usize,
    /// Direct Channel source (for `whenever $channel { ... }`)
    pub channel: Option<crate::value::SharedChannel>,
    pub promise: Option<crate::value::SharedPromise>,
    /// When this subscription was flattened out of an on-demand `supply { ... }`
    /// body, a promise that resolves when the body's emitter is done. The inner
    /// `whenever`'s `done` is rewritten to `$emitter.done()`, which marks the
    /// emitter done (and resolves this promise) rather than raising the
    /// react-done signal — and the emitter's done state is reset immediately
    /// afterwards, so the loop cannot observe it by polling the supplier. The
    /// promise survives that reset, so the event loop watches it to complete
    /// the subscription once the emitter is done.
    pub on_demand_done: Option<crate::value::SharedPromise>,
}

/// A streaming consumer for an on-demand `supply { ... }` body driven by `react`.
/// While registered (keyed by the emitter's `supplier_id`), each `emit` in the
/// supply body delivers its value to `consumer_cb` synchronously rather than
/// buffering. This lets a synchronously infinite body be terminated when the
/// consumer signals `done`: the consumer's `done` marks the stream `done`, and
/// the next `emit` to a `done` consumer unwinds the body.
pub(crate) struct StreamConsumer {
    pub supplier_id: u64,
    pub consumer_cb: Value,
    pub done: bool,
}

/// Completion policy for the shared subscription drive loop
/// (`drive_react_subscriptions`).
///
/// The same poll loop backs both `react { ... }` and `await $supply` /
/// `$supply.Promise`; they differ only in how a `whenever` value is dispatched
/// and when the loop terminates:
/// - `React` runs every subscription to completion via `run_react_consumer`
///   (which maps `done`/`next`/`last`), then fires the CLOSE phasers.
/// - `Promise` drives the subscriptions only until the awaited promise resolves
///   — the supply body's inner `done` keeps it through the supplier registry —
///   or the deadline elapses, keeping it with the last emitted value.
pub(crate) enum SupplyDrivePolicy {
    React,
    Promise {
        promise: crate::value::SharedPromise,
        deadline: std::time::Instant,
        last_value: Value,
    },
}

impl Interpreter {
    /// If a streaming consumer is registered for `supplier_id`, deliver `value`
    /// to it synchronously and return `Some(result)`; otherwise return `None`
    /// so the caller falls back to the normal buffering path.
    ///
    /// - If the consumer is already `done`, this is an emit-to-dead-consumer:
    ///   return a benign react-done signal that unwinds the supply body.
    /// - Otherwise run the consumer callback. If it signals `done`, mark the
    ///   stream done and fire the body's CLOSE phasers synchronously (so e.g.
    ///   `until my $done { ... } CLOSE { $done = True }` terminates cleanly),
    ///   then return `Ok` so the current `emit` completes normally.
    pub(super) fn try_stream_emit(
        &mut self,
        supplier_id: u64,
        value: &Value,
    ) -> Option<Result<(), RuntimeError>> {
        let idx = self
            .supply_stream_consumers
            .iter()
            .position(|c| c.supplier_id == supplier_id)?;
        if self.supply_stream_consumers[idx].done {
            return Some(Err(RuntimeError::supply_terminate_signal()));
        }
        let cb = self.supply_stream_consumers[idx].consumer_cb.clone();
        match self.call_sub_value(cb, vec![value.clone()], true) {
            Err(e) if e.is_react_done => {
                self.supply_stream_consumers[idx].done = true;
                for close_cb in
                    crate::runtime::native_methods::take_supplier_close_callbacks(supplier_id)
                {
                    if let Err(ce) = self.call_sub_value(close_cb, Vec::new(), true) {
                        return Some(Err(ce));
                    }
                }
                Some(Ok(()))
            }
            Err(e) => Some(Err(e)),
            Ok(_) => Some(Ok(())),
        }
    }
}

impl Interpreter {
    fn split_whenever_body_phasers(body: &[Stmt]) -> (Vec<Stmt>, Vec<Vec<Stmt>>, Vec<Vec<Stmt>>) {
        let mut main = Vec::new();
        let mut last = Vec::new();
        let mut quit = Vec::new();
        for stmt in body {
            if let Stmt::Phaser { kind, body } = stmt {
                match kind {
                    PhaserKind::Last => last.push(body.clone()),
                    PhaserKind::Quit => quit.push(body.clone()),
                    _ => main.push(stmt.clone()),
                }
            } else {
                main.push(stmt.clone());
            }
        }
        (main, last, quit)
    }

    pub(crate) fn begin_subtest(&mut self) -> SubtestContext {
        let parent_output = std::mem::take(&mut self.output_sink_mut().output);
        let parent_halted = self.halted;
        // Stash the parent TAP state, install a fresh one, and push the subtest
        // stack (defaulting the callable kind to Sub; callers override after).
        let parent_test_state = self.tap.begin_subtest();
        self.halted = false;
        SubtestContext {
            parent_test_state,
            parent_output,
            parent_halted,
        }
    }

    pub(crate) fn finish_subtest(
        &mut self,
        ctx: SubtestContext,
        label: &str,
        run_result: Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let mut subtest_output = std::mem::take(&mut self.output_sink_mut().output);
        let subtest_state = self.tap.take_state();
        let subtest_failed = subtest_state.as_ref().map(|s| s.failed).unwrap_or(0);
        let subtest_ran = subtest_state
            .as_ref()
            .map(|s| s.effective_ran())
            .unwrap_or(0);
        let subtest_planned = subtest_state.as_ref().and_then(|s| s.planned);
        let has_plan = subtest_planned.is_some();
        // A subtest that declares a plan must run exactly that many tests. When it
        // under/over-runs, rakudo reports it as a failure with a diagnostic line.
        let plan_mismatch = matches!(subtest_planned, Some(p) if p != subtest_ran);

        self.tap.set_state(ctx.parent_test_state);
        self.output_sink_mut().output = ctx.parent_output;
        self.halted = ctx.parent_halted;
        self.tap.end_subtest();
        let parent_forced_todo_reason = self.tap.state().and_then(|state| {
            let next = state.ran + 1;
            state
                .force_todo
                .iter()
                .find(|range| next >= range.start && next <= range.end)
                .map(|range| range.reason.clone())
        });

        self.emit_output(&format!("# Subtest: {}\n", label));
        if !has_plan {
            subtest_output.push_str(&format!("1..{}\n", subtest_ran));
        } else if let Some(planned) = subtest_planned
            && plan_mismatch
        {
            let plural = if planned == 1 { "" } else { "s" };
            subtest_output.push_str(&format!(
                "# You planned {} test{}, but ran {}\n",
                planned, plural, subtest_ran
            ));
        }

        let mut rendered_lines = Vec::new();
        for raw_line in subtest_output.lines() {
            let mut line = raw_line.to_string();
            if let Some(reason) = parent_forced_todo_reason.as_deref()
                && raw_line.trim_start().starts_with("not ok ")
                && !raw_line.contains("# TODO")
            {
                if reason.is_empty() {
                    line.push_str(" # TODO");
                } else {
                    line.push_str(" # TODO ");
                    line.push_str(reason);
                }
            }
            rendered_lines.push(line);
        }
        let subtest_had_not_ok = rendered_lines
            .iter()
            .any(|line| line.trim_start().starts_with("not ok "));
        let parent_historical_todo_reason = self.tap.state().and_then(|state| {
            state
                .force_todo
                .iter()
                .rev()
                .find(|range| !range.reason.is_empty())
                .map(|range| range.reason.clone())
        });
        let uses_parent_historical_reason = parent_historical_todo_reason
            .as_deref()
            .map(|reason| {
                let marker = format!("# TODO {}", reason);
                rendered_lines.iter().any(|line| line.contains(&marker))
            })
            .unwrap_or(false);

        for line in rendered_lines {
            let indented = format!("    {}\n", line);
            self.emit_output(&indented);
        }

        let inherited_parent_todo = parent_forced_todo_reason.is_none()
            && subtest_failed == 0
            && subtest_had_not_ok
            && uses_parent_historical_reason;
        let ok = if parent_forced_todo_reason.is_some() {
            run_result.is_ok() && !subtest_had_not_ok && !plan_mismatch
        } else if inherited_parent_todo {
            false
        } else {
            run_result.is_ok() && subtest_failed == 0 && !plan_mismatch
        };
        self.test_ok(ok, label, inherited_parent_todo)?;
        Ok(())
    }

    /// Enter react mode: whenever blocks will register subscriptions
    /// instead of executing immediately.
    pub(crate) fn enter_react(&mut self) {
        self.supply_emit_buffer.push(Vec::new()); // Use supply_emit_buffer as react subscription storage marker
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
    fn run_react_consumer(
        &mut self,
        sub: &mut ReactSubscription,
        value: Value,
    ) -> Result<bool, RuntimeError> {
        match self.call_sub_value(sub.callback.clone(), vec![value.clone()], true) {
            Ok(_) => Ok(false),
            Err(e) if e.is_react_done => Ok(true),
            Err(e) if e.is_next => Ok(false),
            Err(e) if e.is_last => {
                for cb in sub.last_callbacks.clone() {
                    match self.call_sub_value(cb, vec![value.clone()], true) {
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
                                .and_then(Self::value_array_items)
                                .unwrap_or_default();
                            let quit_callbacks = items
                                .get(3)
                                .and_then(Self::value_array_items)
                                .unwrap_or_default();
                            react_subs.push(ReactSubscription {
                                receiver: Some(rx),
                                supplier_id: None,
                                supplier_next_index: 0,
                                callback,
                                close_callbacks: Self::extract_supply_on_close_callbacks(
                                    &(attributes).as_map(),
                                ),
                                last_callbacks,
                                quit_callbacks,
                                done: false,
                                is_lines,
                                line_buffer: String::new(),
                                head_limit,
                                emit_count: 0,
                                channel: None,
                                promise: None,
                                on_demand_done: None,
                            });
                            continue;
                        }
                        if let Some(Value::Int(supplier_id)) =
                            attributes.as_map().get("supplier_id")
                        {
                            let last_callbacks = items
                                .get(2)
                                .and_then(Self::value_array_items)
                                .unwrap_or_default();
                            let quit_callbacks = items
                                .get(3)
                                .and_then(Self::value_array_items)
                                .unwrap_or_default();
                            react_subs.push(ReactSubscription {
                                receiver: None,
                                supplier_id: Some(*supplier_id as u64),
                                supplier_next_index: 0,
                                callback,
                                close_callbacks: Self::extract_supply_on_close_callbacks(
                                    &(attributes).as_map(),
                                ),
                                last_callbacks,
                                quit_callbacks,
                                done: false,
                                is_lines,
                                line_buffer: String::new(),
                                head_limit,
                                emit_count: 0,
                                channel: None,
                                promise: None,
                                on_demand_done: None,
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
                            let (od_res, emitted, body_ran_done) = self.run_on_demand_body(
                                on_demand_cb.clone(),
                                Some(emitter_supplier_id),
                            );
                            let streamed_done = self
                                .supply_stream_consumers
                                .pop()
                                .map(|c| c.done)
                                .unwrap_or(false);
                            if let Err(od_err) = od_res
                                && !od_err.is_react_done
                            {
                                return Err(Self::wrap_react_died(od_err));
                            }
                            // If the streaming consumer signalled `done`, the
                            // whole react has been satisfied by this supply — fire
                            // its LAST callbacks and stop (don't set up the inner
                            // subscriptions or keep polling).
                            if streamed_done {
                                let last_cbs = items
                                    .get(2)
                                    .and_then(Self::value_array_items)
                                    .unwrap_or_default();
                                for last_cb in &last_cbs {
                                    match self.call_sub_value(last_cb.clone(), Vec::new(), true) {
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
                                if Self::is_supply_subscription_registration(&v) {
                                    if let Some(mut rsub) = self.value_to_react_subscription(&v) {
                                        rsub.on_demand_done = Some(done_promise.clone());
                                        react_subs.push(rsub);
                                    }
                                } else {
                                    let _ = self.call_sub_value(callback.clone(), vec![v], true);
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
                                        let _ = self.call_sub_value(close_cb, Vec::new(), true);
                                    }
                                } else {
                                    // Async body (e.g. `start { emit; done }`): the
                                    // supply closes later. Register a source-less
                                    // subscription carrying the close callbacks so
                                    // run_react_close_callbacks fires them when the
                                    // react ends.
                                    react_subs.push(ReactSubscription {
                                        receiver: None,
                                        supplier_id: None,
                                        supplier_next_index: 0,
                                        callback: callback.clone(),
                                        close_callbacks: close_cbs,
                                        last_callbacks: Vec::new(),
                                        quit_callbacks: Vec::new(),
                                        done: false,
                                        is_lines: false,
                                        line_buffer: String::new(),
                                        head_limit: None,
                                        emit_count: 0,
                                        channel: None,
                                        promise: None,
                                        on_demand_done: None,
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
                                .and_then(Self::value_array_items)
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
                            .and_then(Self::value_array_items)
                            .unwrap_or_default();
                        for last_cb in &last_cbs {
                            match self.call_sub_value(last_cb.clone(), Vec::new(), true) {
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
                            supplier_id: None,
                            supplier_next_index: 0,
                            callback,
                            close_callbacks: Vec::new(),
                            last_callbacks: Vec::new(),
                            quit_callbacks: Vec::new(),
                            done: false,
                            is_lines: false,
                            line_buffer: String::new(),
                            head_limit: None,
                            emit_count: 0,
                            channel: None,
                            promise: Some(shared.clone()),
                            on_demand_done: None,
                        });
                    }
                    // Channel source: poll values directly from the channel
                    Value::Channel(ch) => {
                        let last_callbacks = items
                            .get(2)
                            .and_then(Self::value_array_items)
                            .unwrap_or_default();
                        react_subs.push(ReactSubscription {
                            receiver: None,
                            supplier_id: None,
                            supplier_next_index: 0,
                            callback,
                            close_callbacks: Vec::new(),
                            last_callbacks,
                            quit_callbacks: Vec::new(),
                            done: false,
                            is_lines: false,
                            line_buffer: String::new(),
                            head_limit: None,
                            emit_count: 0,
                            channel: Some(ch.clone()),
                            promise: None,
                            on_demand_done: None,
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
                        match self.call_sub_value(callback.clone(), Vec::new(), true) {
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
                                    self.call_sub_value(callback.clone(), Vec::new(), true)?;
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
                                self.call_sub_value(callback.clone(), Vec::new(), true)?;
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
                                        self.call_sub_value(callback.clone(), Vec::new(), true)?;
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
                                    self.call_sub_value(callback.clone(), Vec::new(), true)?;
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
                        let quit_err = Self::runtime_error_from_supply_reason(error);
                        return Err(Self::wrap_react_died(quit_err));
                    }
                    if done {
                        if sub.is_lines && !sub.line_buffer.is_empty() {
                            let remaining = std::mem::take(&mut sub.line_buffer);
                            match self.call_sub_value(
                                sub.callback.clone(),
                                vec![Value::str(remaining)],
                                true,
                            ) {
                                Err(e) if e.is_react_done => break 'react_loop,
                                other => {
                                    other?;
                                }
                            }
                        }
                        for callback in &sub.last_callbacks {
                            self.call_sub_value(callback.clone(), Vec::new(), true)?;
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
                                self.call_sub_value(sub.callback.clone(), vec![value], true);
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
                                match self.call_sub_value(
                                    sub.callback.clone(),
                                    vec![Value::str(remaining)],
                                    true,
                                ) {
                                    Err(e) if e.is_react_done => break 'react_loop,
                                    other => {
                                        other?;
                                    }
                                }
                            }
                            for callback in &sub.last_callbacks {
                                self.call_sub_value(callback.clone(), Vec::new(), true)?;
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
                                let ch_quit_err = Self::runtime_error_from_supply_reason(error);
                                return Err(Self::wrap_react_died(ch_quit_err));
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

    pub(super) fn value_array_items(value: &Value) -> Option<Vec<Value>> {
        if let Value::Array(items, ..) = value {
            Some(items.to_vec())
        } else {
            None
        }
    }

    fn run_react_close_callbacks(&mut self, react_subs: &[ReactSubscription]) {
        for sub in react_subs {
            for callback in &sub.close_callbacks {
                let _ = self.call_sub_value(callback.clone(), Vec::new(), true);
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
                match self.call_sub_value(callback.clone(), vec![v.clone()], true) {
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
            match self.call_sub_value(cb.clone(), args, true) {
                Err(e) if e.is_react_done => return Ok(true),
                other => {
                    other?;
                }
            }
        }
        Ok(false)
    }

    pub(crate) fn run_whenever_with_value(
        &mut self,
        supply_val: Value,
        target_var: Option<&str>,
        param: &Option<String>,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // If the source is a Supplier, convert it to its associated Supply
        // so that subscription registration and tap dispatch work correctly.
        // Without this, `whenever $supplier { ... }` inside a `supply` block
        // would pass the Supplier object itself as the subscription source,
        // which the tap dispatch code does not recognize (it expects Supply).
        let supply_val = if let Value::Instance { class_name, .. } = &supply_val
            && (class_name == "Supplier" || class_name == "Supplier::Preserving")
        {
            self.call_method_with_values(supply_val, "Supply", vec![])?
        } else {
            supply_val
        };

        let (main_body, last_bodies, quit_bodies) = Self::split_whenever_body_phasers(body);
        let callback = Value::make_sub(
            Symbol::intern(&self.current_package()),
            Symbol::intern(""),
            param.iter().cloned().collect(),
            Vec::new(),
            main_body,
            false,
            self.env.clone(),
        );
        let last_callbacks: Vec<Value> = last_bodies
            .into_iter()
            .map(|body| {
                Value::make_sub(
                    Symbol::intern(&self.current_package()),
                    Symbol::intern(""),
                    Vec::new(),
                    Vec::new(),
                    body,
                    false,
                    self.env.clone(),
                )
            })
            .collect();
        let quit_callbacks: Vec<Value> = quit_bodies
            .into_iter()
            .map(|body| {
                Value::make_sub(
                    Symbol::intern(&self.current_package()),
                    Symbol::intern(""),
                    vec!["_".to_string()],
                    Vec::new(),
                    body,
                    false,
                    self.env.clone(),
                )
            })
            .collect();

        // Check if we're in a react block (supply_emit_buffer has an entry)
        if !self.supply_emit_buffer.is_empty() {
            if let Value::Instance { class_name, .. } = &supply_val
                && class_name == "IO::Socket::Async::Listener"
            {
                let tap = self.call_method_with_values(
                    supply_val.clone(),
                    "act",
                    vec![callback.clone()],
                )?;
                if let Some(name) = target_var {
                    self.env.insert(name.to_string(), tap);
                }
                return Ok(());
            }
            // In react mode: register the subscription for the event loop
            let sub = Value::array(vec![
                supply_val.clone(),
                callback.clone(),
                Value::array(last_callbacks.clone()),
                Value::array(quit_callbacks.clone()),
            ]);
            if let Some(last) = self.supply_emit_buffer.last_mut() {
                last.push(sub);
            }

            // Also register taps on the supply for non-react backward compat
            if let Value::Instance {
                class_name,
                attributes,
                ..
            } = &supply_val
                && class_name == "Supply"
            {
                if let Some(Value::Int(sid)) = attributes.as_map().get("supply_id") {
                    crate::runtime::native_methods::register_supply_tap(
                        *sid as u64,
                        callback.clone(),
                    );
                }
                // Also register on parent for lines supplies
                if let Some(Value::Int(pid)) = attributes.as_map().get("parent_supply_id") {
                    crate::runtime::native_methods::register_supply_tap(
                        *pid as u64,
                        callback.clone(),
                    );
                }
            }

            if let Some(name) = target_var {
                self.env.insert(name.to_string(), supply_val);
            }
            return Ok(());
        }

        // Not in react mode: original behavior
        if let Value::Instance {
            class_name,
            attributes: _,
            ..
        } = &supply_val
            && class_name == "Supply"
        {
            let mut tap_args = vec![callback.clone()];
            if let Some(done_cb) = last_callbacks.first().cloned() {
                tap_args.push(Value::Pair("done".to_string(), Box::new(done_cb)));
            }
            if let Some(quit_cb) = quit_callbacks.first().cloned() {
                tap_args.push(Value::Pair("quit".to_string(), Box::new(quit_cb)));
            }
            let updated = self.call_method_with_values(supply_val.clone(), "tap", tap_args)?;
            if let Some(name) = target_var {
                self.env.insert(name.to_string(), updated);
            }
        } else if let Value::Instance { class_name, .. } = &supply_val
            && class_name == "IO::Socket::Async::Listener"
        {
            let tap = self.call_method_with_values(supply_val.clone(), "act", vec![callback])?;
            if let Some(name) = target_var {
                self.env.insert(name.to_string(), tap);
            }
        }
        Ok(())
    }
}
