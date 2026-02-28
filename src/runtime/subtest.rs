use super::*;
use crate::runtime::native_methods::{SupplyEvent, take_supply_channel};
use std::sync::mpsc;
use std::time::Duration;

/// A subscription registered by a `whenever` block inside a `react` block.
pub(crate) struct ReactSubscription {
    pub receiver: mpsc::Receiver<SupplyEvent>,
    pub callback: Value,
    pub done: bool,
    /// When true, split incoming chunks into lines before calling the callback.
    pub is_lines: bool,
    /// Buffer for incomplete lines (when is_lines is true).
    pub line_buffer: String,
}

impl Interpreter {
    pub(crate) fn begin_subtest(&mut self) -> SubtestContext {
        let parent_test_state = self.test_state.take();
        let force_todo_inner = parent_test_state
            .as_ref()
            .map(|state| {
                let next = state.ran + 1;
                state
                    .force_todo
                    .iter()
                    .any(|(start, end)| next >= *start && next <= *end)
            })
            .unwrap_or(false);
        let parent_output = std::mem::take(&mut self.output);
        let parent_halted = self.halted;
        let mut subtest_state = TestState::new();
        if force_todo_inner {
            subtest_state.force_todo.push((1, usize::MAX));
        }
        self.test_state = Some(subtest_state);
        self.halted = false;
        self.subtest_depth += 1;
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
        let subtest_output = std::mem::take(&mut self.output);
        let subtest_state = self.test_state.take();
        let subtest_failed = subtest_state.as_ref().map(|s| s.failed).unwrap_or(0);

        self.test_state = ctx.parent_test_state;
        self.output = ctx.parent_output;
        self.halted = ctx.parent_halted;
        self.subtest_depth = self.subtest_depth.saturating_sub(1);

        for line in subtest_output.lines() {
            let indented = format!("    {}\n", line);
            self.emit_output(&indented);
        }

        let ok = run_result.is_ok() && subtest_failed == 0;
        self.test_ok(ok, label, false)?;
        Ok(())
    }

    /// Enter react mode: whenever blocks will register subscriptions
    /// instead of executing immediately.
    pub(crate) fn enter_react(&mut self) {
        self.supply_emit_buffer.push(Vec::new()); // Use supply_emit_buffer as react subscription storage marker
    }

    /// Run the react event loop: poll all registered subscriptions
    /// until all are done.
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
                        let supply_id = self.resolve_supply_channel_id(attributes);
                        let is_lines =
                            matches!(attributes.get("is_lines"), Some(Value::Bool(true)));
                        if let Some(sid) = supply_id
                            && let Some(rx) = take_supply_channel(sid)
                        {
                            react_subs.push(ReactSubscription {
                                receiver: rx,
                                callback,
                                done: false,
                                is_lines,
                                line_buffer: String::new(),
                            });
                            continue;
                        }
                        // Handle on-demand supplies: execute the callback to produce values
                        if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
                            let mut emitter_attrs = HashMap::new();
                            emitter_attrs.insert("emitted".to_string(), Value::array(Vec::new()));
                            emitter_attrs.insert("done".to_string(), Value::Bool(false));
                            let emitter =
                                Value::make_instance("Supplier".to_string(), emitter_attrs);
                            // Execute the on-demand callback, which calls emit on the emitter
                            self.supply_emit_buffer.push(Vec::new());
                            let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                            let emitted = self.supply_emit_buffer.pop().unwrap_or_default();
                            // Replay emitted values
                            for v in emitted {
                                let _ = self.call_sub_value(callback.clone(), vec![v], true);
                            }
                        } else {
                            // No channel, no on-demand - replay static values
                            self.replay_static_supply(attributes, &callback)?;
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
                            receiver: rx,
                            callback,
                            done: false,
                            is_lines: false,
                            line_buffer: String::new(),
                        });
                    }
                    _ => {}
                }
            }
        }

        if react_subs.is_empty() {
            return Ok(());
        }

        // Event loop: poll all subscriptions
        let timeout = Duration::from_millis(10);
        'react_loop: loop {
            let mut all_done = true;
            for sub in react_subs.iter_mut() {
                if sub.done {
                    continue;
                }
                all_done = false;
                // Try to receive with a short timeout
                match sub.receiver.recv_timeout(timeout) {
                    Ok(SupplyEvent::Emit(value)) => {
                        if sub.is_lines {
                            let chunk = value.to_string_value();
                            sub.line_buffer.push_str(&chunk);
                            while let Some(pos) = sub.line_buffer.find('\n') {
                                let line = sub.line_buffer[..pos].to_string();
                                sub.line_buffer = sub.line_buffer[pos + 1..].to_string();
                                match self.call_sub_value(
                                    sub.callback.clone(),
                                    vec![Value::Str(line)],
                                    true,
                                ) {
                                    Err(e) if e.is_react_done => break 'react_loop,
                                    other => {
                                        other?;
                                    }
                                }
                            }
                        } else {
                            match self.call_sub_value(sub.callback.clone(), vec![value], true) {
                                Err(e) if e.is_react_done => break 'react_loop,
                                other => {
                                    other?;
                                }
                            }
                        }
                    }
                    Ok(SupplyEvent::Done) => {
                        if sub.is_lines && !sub.line_buffer.is_empty() {
                            let remaining = std::mem::take(&mut sub.line_buffer);
                            match self.call_sub_value(
                                sub.callback.clone(),
                                vec![Value::Str(remaining)],
                                true,
                            ) {
                                Err(e) if e.is_react_done => break 'react_loop,
                                other => {
                                    other?;
                                }
                            }
                        }
                        sub.done = true;
                    }
                    Ok(SupplyEvent::Quit(error)) => {
                        sub.done = true;
                        let msg = error.to_string_value();
                        let mut err = RuntimeError::new(msg);
                        err.exception = Some(Box::new(error));
                        return Err(err);
                    }
                    Err(mpsc::RecvTimeoutError::Timeout) => {}
                    Err(mpsc::RecvTimeoutError::Disconnected) => {
                        sub.done = true;
                    }
                }
            }
            if all_done || react_subs.iter().all(|s| s.done) {
                break;
            }
        }

        Ok(())
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
    fn replay_static_supply(
        &mut self,
        attributes: &HashMap<String, Value>,
        callback: &Value,
    ) -> Result<(), RuntimeError> {
        if let Some(Value::Array(values, ..)) = attributes.get("values") {
            for v in values.iter() {
                self.call_sub_value(callback.clone(), vec![v.clone()], true)?;
            }
        }
        Ok(())
    }

    pub(crate) fn run_whenever_with_value(
        &mut self,
        supply_val: Value,
        target_var: Option<&str>,
        param: &Option<String>,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let callback = Value::make_sub(
            self.current_package.clone(),
            String::new(),
            param.iter().cloned().collect(),
            Vec::new(),
            body.to_vec(),
            false,
            self.env.clone(),
        );

        // Check if we're in a react block (supply_emit_buffer has an entry)
        if !self.supply_emit_buffer.is_empty() {
            // In react mode: register the subscription for the event loop
            let sub = Value::array(vec![supply_val.clone(), callback.clone()]);
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
                if let Some(Value::Int(sid)) = attributes.get("supply_id") {
                    crate::runtime::native_methods::register_supply_tap(
                        *sid as u64,
                        callback.clone(),
                    );
                }
                // Also register on parent for lines supplies
                if let Some(Value::Int(pid)) = attributes.get("parent_supply_id") {
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
            attributes,
            ..
        } = supply_val
            && class_name == "Supply"
        {
            let mut attrs = (*attributes).clone();
            if let Some(Value::Array(items, ..)) = attrs.get("taps") {
                let mut new_items = items.to_vec();
                new_items.push(callback.clone());
                attrs.insert("taps".to_string(), Value::array(new_items));
            } else {
                attrs.insert("taps".to_string(), Value::array(vec![callback.clone()]));
            }
            if let Some(Value::Array(values, ..)) = attrs.get("values") {
                for v in values.iter() {
                    let _ = self.call_sub_value(callback.clone(), vec![v.clone()], true);
                }
            }
            let updated = Value::make_instance(class_name, attrs);
            if let Some(name) = target_var {
                self.env.insert(name.to_string(), updated);
            }
        }
        Ok(())
    }
}
