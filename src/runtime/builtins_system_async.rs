use super::*;

impl Interpreter {
    pub(super) fn builtin_kill(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let signal = args.first().map(super::to_int).unwrap_or(15);
        let mut success = true;
        let mut had_pid = false;
        for val in args.iter().skip(1) {
            had_pid = true;
            let pid = super::to_int(val);
            success &= Self::send_signal(pid, signal);
        }
        if !had_pid {
            success = false;
        }
        Ok(Value::Bool(success))
    }

    pub(super) fn builtin_syscall(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let Some(val) = args.first() else {
            return Ok(Value::Nil);
        };
        let num = super::to_int(val);
        if num == 0 {
            let pid = self
                .env
                .get("*PID")
                .and_then(|v| match v {
                    Value::Int(i) => Some(*i),
                    _ => None,
                })
                .unwrap_or_else(|| {
                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        std::process::id() as i64
                    }
                    #[cfg(target_arch = "wasm32")]
                    {
                        0
                    }
                });
            return Ok(Value::Int(pid));
        }
        Ok(Value::Int(-1))
    }

    pub(super) fn builtin_sleep(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let secs = Self::seconds_from_value(args.first().cloned());
        match secs {
            None | Some(f64::INFINITY) => {
                // sleep with no args or Inf sleeps indefinitely.
                // Poll for global exit flag so that `start { exit }` can
                // wake us up.
                loop {
                    thread::sleep(Duration::from_millis(100));
                    if let Some(code) = super::builtins_control_flow::global_exit_requested() {
                        self.halted = true;
                        self.exit_code = code;
                        return Ok(Value::Nil);
                    }
                }
            }
            Some(s) => {
                let duration = Self::duration_from_seconds(Some(s));
                // For large sleep durations, poll periodically so that
                // a global exit() from another thread can interrupt us.
                if duration > Duration::from_secs(10) {
                    Self::interruptible_sleep(duration);
                    if let Some(code) = super::builtins_control_flow::global_exit_requested() {
                        self.halted = true;
                        self.exit_code = code;
                        return Ok(Value::Nil);
                    }
                } else {
                    thread::sleep(duration);
                }
                self.sync_shared_vars_to_env();
                Ok(Value::Nil)
            }
        }
    }

    /// Sleep for the given duration, but check for global exit every 100ms.
    fn interruptible_sleep(total: Duration) {
        let start = std::time::Instant::now();
        while start.elapsed() < total {
            let remaining = total.saturating_sub(start.elapsed());
            let chunk = remaining.min(Duration::from_millis(100));
            thread::sleep(chunk);
            if super::builtins_control_flow::global_exit_requested().is_some() {
                return;
            }
        }
    }

    pub(super) fn builtin_sleep_timer(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let duration = Self::duration_from_seconds(Self::seconds_from_value(args.first().cloned()));
        let start = Instant::now();
        thread::sleep(duration);
        let elapsed = start.elapsed();
        let remaining = duration.checked_sub(elapsed).unwrap_or_default();
        Ok(crate::builtins::arith::make_duration_value(
            remaining.as_secs_f64(),
        ))
    }

    pub(super) fn builtin_sleep_till(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let arg = args.first().cloned();
        // Extract the target POSIX timestamp, handling Instant (TAI) values
        let target_posix = match &arg {
            Some(Value::Instance {
                class_name,
                attributes,
                ..
            }) if class_name.resolve() == "Instant" => {
                // Instant stores TAI time; convert back to POSIX
                attributes
                    .as_map()
                    .get("value")
                    .and_then(super::to_float_value)
                    .map(crate::builtins::methods_0arg::temporal::instant_to_posix)
            }
            Some(Value::Instance {
                class_name,
                attributes,
                ..
            }) if class_name.resolve() == "DateTime" => {
                // Compute posix from DateTime attributes
                use crate::builtins::methods_0arg::temporal::{datetime_attrs, datetime_to_posix};
                let (year, month, day, hour, minute, second, timezone) =
                    datetime_attrs(&(attributes).as_map());
                Some(datetime_to_posix(
                    year, month, day, hour, minute, second, timezone,
                ))
            }
            _ => Self::seconds_from_value(arg),
        };
        if let Some(target) = target_posix {
            let now = crate::value::current_time_secs_f64();
            if target <= now {
                return Ok(Value::Bool(false));
            }
            let diff_secs = target - now;
            thread::sleep(Duration::from_secs_f64(diff_secs));
            return Ok(Value::Bool(true));
        }
        Ok(Value::Bool(false))
    }

    /// `start { ... }` — spawn a thread to execute the block and return a Promise.
    pub(super) fn builtin_start(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let block = args.into_iter().next().unwrap_or(Value::Nil);
        Ok(self.spawn_callable_promise(block, Symbol::intern("Promise")))
    }

    /// `await` — block until Promise(s) resolve, then return their results.
    /// Build the `RuntimeError` carrying an `X::Await::Died` exception for a
    /// broken Promise observed by `await`. Raku mixes the `X::Await::Died` role
    /// INTO the original failure cause (`.^name` is e.g. `X::AdHoc+{X::Await::Died}`),
    /// so it keeps its class, message and backtrace while also doing
    /// `X::Await::Died`. We mirror that: when the cause is an exception instance,
    /// mark it with `__mutsu_does_await_died` (recognized by `isa_check`) and
    /// re-raise it unchanged, preserving its gist/backtrace. A plain (non-instance)
    /// cause is wrapped in a fresh `X::Await::Died`.
    /// Format the current routine stack (the point where `await` was called) as
    /// a Raku-style backtrace string, innermost frame first. Used to append the
    /// await/re-throw location to a broken-Promise exception's backtrace.
    fn await_site_backtrace(&self) -> String {
        let stack = self.routine_stack();
        let mut lines = Vec::new();
        for frame in stack.iter().rev() {
            let loc = match (frame.file.as_deref(), frame.line) {
                (Some(f), Some(l)) => format!(" at {} line {}", f, l),
                (Some(f), None) => format!(" at {}", f),
                _ => String::new(),
            };
            if frame.is_block || frame.name.is_empty() || frame.name == "<unit>" {
                lines.push(format!("  in block <unit>{}", loc));
            } else {
                lines.push(format!("  in sub {}{}", frame.name, loc));
            }
        }
        lines.join("\n")
    }

    fn await_died_error(&self, cause: Value) -> RuntimeError {
        // Append the await call-site to the exception's backtrace, so the gist
        // surfaces both where the Promise's code died (preserved) and where it
        // was awaited (the re-throw location) — matching Raku's X::Await::Died.
        if let Value::Instance { attributes, .. } = &cause {
            let await_bt = self.await_site_backtrace();
            if !await_bt.is_empty()
                && let Some(Value::Instance {
                    class_name: bt_cn,
                    attributes: bt_attrs,
                    ..
                }) = attributes.as_map().get("backtrace").cloned()
                && bt_cn == "Backtrace"
            {
                let old = bt_attrs
                    .as_map()
                    .get("text")
                    .map(Value::to_string_value)
                    .unwrap_or_default();
                let combined = if old.is_empty() {
                    await_bt
                } else {
                    format!("{old}\n{await_bt}")
                };
                bt_attrs.insert("text".to_string(), Value::str(combined));
            }
        }
        let msg = match &cause {
            Value::Instance { attributes, .. } => {
                attributes.insert("__mutsu_does_await_died".to_string(), Value::Bool(true));
                attributes
                    .as_map()
                    .get("message")
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| cause.to_string_value())
            }
            other => other.to_string_value(),
        };
        let mut err = RuntimeError::new(msg.clone());
        let exc = if matches!(cause, Value::Instance { .. }) {
            cause
        } else {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            attrs.insert("payload".to_string(), Value::str(msg));
            attrs.insert("__mutsu_does_await_died".to_string(), Value::Bool(true));
            Value::make_instance(Symbol::intern("X::Await::Died"), attrs)
        };
        err.exception = Some(Box::new(exc));
        err
    }

    /// Normalize a single `await` target. A `Supply` is awaited via its
    /// `.Promise` (kept with the last emitted value when the supply completes,
    /// broken if it quits) — `await $supply` ≡ `await $supply.Promise`. All other
    /// values pass through unchanged (Promises and Channels are handled inline).
    fn await_normalize(&mut self, v: Value) -> Result<Value, RuntimeError> {
        if let Value::Instance { class_name, .. } = &v
            && class_name == "Supply"
        {
            return self.call_method_with_values(v, "Promise", vec![]);
        }
        Ok(v)
    }

    pub(super) fn builtin_await(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new(
                "Must specify a Promise or list of Promises to await",
            ));
        }
        let mut await_targets: Vec<Value> = Vec::new();
        for arg in args {
            if let Some(items) = arg.as_list_items() {
                let items: Vec<Value> = items.to_vec();
                for it in items {
                    await_targets.push(self.await_normalize(it)?);
                }
            } else {
                let normalized = self.await_normalize(arg.clone())?;
                await_targets.push(normalized);
            }
        }
        let mut results = Vec::new();
        for arg in &await_targets {
            match arg {
                Value::Promise(shared) => {
                    let (result, output, stderr) = shared.wait();
                    self.emit_output(&output);
                    self.output_sink_mut().stderr_output.push_str(&stderr);
                    if let Some(payload) = shared.take_thread_payload()
                        && let Ok(payload) = payload.downcast::<ThreadPromisePayload>()
                    {
                        let ThreadPromisePayload {
                            new_handles,
                            next_handle_id,
                        } = *payload;
                        let mut table = self.io_handles_mut();
                        for (id, state) in new_handles {
                            table.map.entry(id).or_insert(state);
                        }
                        if next_handle_id > table.next_id {
                            table.next_id = next_handle_id;
                        }
                    }
                    if shared.status() == "Broken" {
                        self.sync_shared_vars_to_env();
                        // Raku wraps the broken Promise's cause in X::Await::Died.
                        return Err(self.await_died_error(result));
                    }
                    // Replay deferred Proc::Async taps
                    if let Value::Instance {
                        ref class_name,
                        ref attributes,
                        ..
                    } = result
                        && class_name == "Proc"
                    {
                        self.replay_proc_taps(attributes);
                    }
                    let result = Self::unwrap_async_status_result(result)?;
                    results.push(result);
                    // Sync shared variables after each thread completes
                    self.sync_shared_vars_to_env();
                }
                // Backward compat: Instance-based Promise
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Promise" => {
                    let result = attributes
                        .as_map()
                        .get("result")
                        .cloned()
                        .unwrap_or(Value::Nil);
                    results.push(result);
                }
                Value::Array(elems, ..) => {
                    for elem in elems.iter() {
                        match elem {
                            Value::Promise(shared) => {
                                let (result, output, stderr) = shared.wait();
                                self.emit_output(&output);
                                self.output_sink_mut().stderr_output.push_str(&stderr);
                                if shared.status() == "Broken" {
                                    self.sync_shared_vars_to_env();
                                    return Err(self.await_died_error(result));
                                }
                                let result = Self::unwrap_async_status_result(result)?;
                                results.push(result);
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if class_name == "Promise" => {
                                let result = attributes
                                    .as_map()
                                    .get("result")
                                    .cloned()
                                    .unwrap_or(Value::Nil);
                                results.push(result);
                            }
                            _ => results.push(elem.clone()),
                        }
                    }
                }
                // `await $channel` blocks for the next value (like `.receive`):
                // returns it, or throws X::Channel::ReceiveOnClosed when the
                // channel is drained and closed, or the failure cause when failed.
                Value::Channel(ch) => match ch.receive_result() {
                    Ok(Value::Nil) => return Err(Self::channel_receive_closed_error()),
                    Ok(value) => results.push(value),
                    Err(cause) => {
                        let ex = Self::as_exception_value(cause);
                        let mut err = RuntimeError::new(ex.to_string_value());
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                },
                _ => results.push(arg.clone()),
            }
        }
        // Sync shared variables back from child threads
        self.sync_shared_vars_to_env();

        // Process any pending instance destroys (e.g. objects that were nilled
        // before the start block but whose DESTROY wasn't yet triggered).
        self.closure_env_overrides.clear();
        self.run_pending_instance_destroys()?;

        // Drain shared thread output buffers (concurrent output interleaved in real order).
        // Clone the Arc out so the output_sink guard is dropped before emit_output re-borrows self.
        let shared_out = self.output_sink().shared_thread_output.clone();
        if let Some(shared) = shared_out {
            let drained = std::mem::take(&mut *shared.lock().unwrap());
            if !drained.is_empty() {
                self.emit_output(&drained);
            }
        }
        let shared_err = self.output_sink().shared_thread_stderr.clone();
        if let Some(shared) = shared_err {
            let drained = std::mem::take(&mut *shared.lock().unwrap());
            if !drained.is_empty() {
                self.output_sink_mut().stderr_output.push_str(&drained);
            }
        }

        // Slip results flatten into the surrounding await result list (slip
        // semantics): `await start { (2, 3).Slip }, start { 4 }` yields
        // `(2, 3, 4)`, not `((2, 3), 4)`. A plain List/Array result is NOT a
        // Slip and stays nested, matching Raku.
        if results.iter().any(|r| matches!(r, Value::Slip(_))) {
            let mut flat = Vec::with_capacity(results.len());
            for r in results {
                if let Value::Slip(items) = r {
                    flat.extend(items.iter().cloned());
                } else {
                    flat.push(r);
                }
            }
            results = flat;
        }

        if results.len() == 1 {
            Ok(results.into_iter().next().unwrap())
        } else {
            Ok(Value::array(results))
        }
    }

    fn unwrap_async_status_result(value: Value) -> Result<Value, RuntimeError> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &value
            && class_name == "IO::Socket::Async::StatusResult"
        {
            let status = attributes
                .as_map()
                .get("status")
                .map(Value::to_string_value)
                .unwrap_or_else(|| "Broken".to_string());
            if status == "Kept" {
                return Ok(attributes
                    .as_map()
                    .get("result")
                    .cloned()
                    .unwrap_or(Value::Nil));
            }
            let cause = attributes
                .as_map()
                .get("cause")
                .cloned()
                .unwrap_or(Value::Nil);
            let message = cause.to_string_value();
            let mut err = RuntimeError::new(message.clone());
            if matches!(cause, Value::Instance { .. }) {
                err.exception = Some(Box::new(cause));
            } else {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("payload".to_string(), Value::str(message.clone()));
                attrs.insert("message".to_string(), Value::str(message));
                let ex = Value::make_instance(Symbol::intern("X::AdHoc"), attrs);
                err.exception = Some(Box::new(ex));
            }
            return Err(err);
        }
        Ok(value)
    }
    /// `signal(SIGINT, ...)` — returns a Supply that emits Signal enum values
    /// when the process receives the specified OS signals.
    pub(super) fn builtin_signal(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        use std::sync::mpsc;

        // Extract signal numbers and their enum representations
        let signals: Vec<(i64, Value)> = args
            .iter()
            .filter_map(|v| match v {
                Value::Enum { value, .. } => Some((value.as_i64(), v.clone())),
                Value::Int(i) => Some((*i, v.clone())),
                _ => None,
            })
            .collect();

        let supply_id = super::native_methods::next_supply_id();

        // Create channel for the Supply
        let (tx, rx) = mpsc::channel();

        // Register the channel in the supply channel map
        if let Ok(mut map) = super::native_methods::supply_channel_map_pub().lock() {
            map.insert(supply_id, rx);
        }

        // Set up real signal handling using pipe + sigaction
        for (signum, sig_val) in &signals {
            signal_watcher::register_signal(*signum as i32, supply_id, tx.clone(), sig_val.clone());
        }

        let mut attrs = std::collections::HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("supply_id".to_string(), Value::Int(supply_id as i64));
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }
}
