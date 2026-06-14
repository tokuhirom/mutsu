use super::*;
use crate::runtime::native_methods::SupplyEvent;
use crate::symbol::Symbol;
use std::sync::mpsc;

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

impl ReactSubscription {
    /// A subscription with no source wired up and no phasers: every field at
    /// its inert default except `callback`. Callers fill in the source field
    /// (`receiver` / `supplier_id` / `channel` / `promise`) and any collected
    /// phaser callbacks via struct-update syntax (`..ReactSubscription::new(cb)`).
    pub(crate) fn new(callback: Value) -> Self {
        ReactSubscription {
            receiver: None,
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
            promise: None,
            on_demand_done: None,
        }
    }
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

    pub(crate) fn value_array_items(value: &Value) -> Option<Vec<Value>> {
        if let Value::Array(items, ..) = value {
            Some(items.to_vec())
        } else {
            None
        }
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
