use crate::runtime::*;
use crate::symbol::Symbol;
use std::sync::atomic::Ordering;

use super::state::*;
use super::state_lock::*;
use super::state_scheduler::*;
use super::state_supplier::close_supplier_tap;

/// Parameters for a scheduled cue operation.
struct CueParams {
    callback: Value,
    delay: f64,
    every: Option<f64>,
    times: Option<usize>,
    cancel_flag: Option<std::sync::Arc<std::sync::atomic::AtomicBool>>,
    catch_cb: Option<Value>,
    stop_cb: Option<Value>,
}

impl Interpreter {
    fn cancellation_instance() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "cancellation-id".to_string(),
            Value::Int(next_cancellation_id() as i64),
        );
        Value::make_instance(Symbol::intern("Cancellation"), attrs)
    }

    fn scheduler_times_arg(args: &[Value]) -> Result<Option<usize>, RuntimeError> {
        let Some(value) = Self::named_value(args, "times") else {
            return Ok(None);
        };
        let count = match value {
            Value::Int(i) => i,
            Value::Num(f) if f.is_finite() => f as i64,
            Value::Bool(b) => i64::from(b),
            Value::Str(s) => s.trim().parse::<i64>().map_err(|_| {
                RuntimeError::new(format!(
                    "Scheduler.cue: :times must be numeric, got '{}'",
                    s
                ))
            })?,
            other => {
                return Err(RuntimeError::new(format!(
                    "Scheduler.cue: :times must be numeric, got '{}'",
                    other.to_string_value()
                )));
            }
        };
        Ok(Some(count.max(0) as usize))
    }

    /// Compute the delay in seconds from `:in` and `:at` named args.
    /// `:in` is a relative delay in seconds, `:at` is an absolute time (Instant).
    /// Returns the delay in seconds (clamped to >= 0 for negative results).
    fn scheduler_delay(args: &[Value]) -> Result<f64, RuntimeError> {
        if let Some(in_val) = Self::named_value(args, "in") {
            let v = in_val.to_f64();
            if v.is_nan() {
                return Err(Self::cue_nan_error());
            }
            return Ok(v);
        }
        if let Some(at_val) = Self::named_value(args, "at") {
            let at_f64 = at_val.to_f64();
            if at_f64.is_nan() {
                return Err(Self::cue_nan_error());
            }
            if at_f64.is_infinite() {
                return Ok(at_f64); // propagate Inf/-Inf
            }
            // :at is an absolute TAI time; compute delay = at - now
            let now_posix = crate::value::current_time_secs_f64();
            let now_tai = crate::builtins::methods_0arg::temporal::posix_to_instant(now_posix);
            let delay = at_f64 - now_tai;
            return Ok(if delay < 0.0 { 0.0 } else { delay });
        }
        Ok(0.0)
    }

    /// Check if `:every` value is NaN and return error if so.
    fn scheduler_every(args: &[Value]) -> Result<Option<f64>, RuntimeError> {
        let Some(val) = Self::named_value(args, "every") else {
            return Ok(None);
        };
        let v = val.to_f64();
        if v.is_nan() {
            return Err(Self::cue_nan_error());
        }
        Ok(Some(v))
    }

    fn cue_nan_error() -> RuntimeError {
        let mut attrs = HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str_from("Cannot pass NaN as a number of seconds"),
        );
        let ex = Value::make_instance(Symbol::intern("X::Scheduler::CueInNaNSeconds"), attrs);
        RuntimeError {
            exception: Some(Box::new(ex)),
            ..RuntimeError::new("Cannot pass NaN as a number of seconds")
        }
    }

    /// Helper: sleep for the given delay, handling Inf (don't run) and -Inf/negative (immediate).
    /// Returns true if we should proceed with execution, false if we should skip (Inf delay).
    fn scheduler_sleep(delay: f64) -> bool {
        if delay == f64::INFINITY {
            return false; // never run
        }
        if delay == f64::NEG_INFINITY || delay <= 0.0 {
            return true; // run immediately
        }
        std::thread::sleep(std::time::Duration::from_secs_f64(delay));
        true
    }

    pub(in crate::runtime) fn native_tap(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cancel" | "close" => {
                if let Some(Value::Int(listener_id)) = attributes.get("listener-id") {
                    close_async_listener(*listener_id as u64);
                }
                if let (Some(Value::Int(supplier_id)), Some(Value::Int(tap_id))) =
                    (attributes.get("supplier_id"), attributes.get("tap_id"))
                {
                    close_supplier_tap(*supplier_id as u64, *tap_id as u64);
                }
                Ok(Value::Nil)
            }
            "socket-port" => Ok(attributes
                .get("socket-port")
                .cloned()
                .unwrap_or(Value::Promise(SharedPromise::new()))),
            "socket-host" => Ok(attributes
                .get("socket-host")
                .cloned()
                .unwrap_or(Value::Promise(SharedPromise::new()))),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Tap",
                method
            ))),
        }
    }

    pub(in crate::runtime) fn native_cancellation(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cancel" => {
                if let Some(Value::Int(id)) = attributes.get("cancellation-id")
                    && *id > 0
                    && let Some(flag) = cancellation_state(*id as u64)
                {
                    flag.store(true, Ordering::Relaxed);
                }
                Ok(Value::Nil)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Cancellation",
                method
            ))),
        }
    }

    pub(in crate::runtime) fn native_scheduler(
        &mut self,
        _attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
        is_current_thread: bool,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cue" => {
                let callback = args.first().cloned().unwrap_or(Value::Nil);
                let times_explicit = Self::scheduler_times_arg(&args)?;
                let delay = Self::scheduler_delay(&args)?;
                let every = Self::scheduler_every(&args)?;
                let catch_cb = Self::named_value(&args, "catch");
                let stop_cb = Self::named_value(&args, "stop");

                // When :every is set without :times, repeat indefinitely
                // When :every is not set, default :times to 1
                let times: Option<usize> = match (every.is_some(), times_explicit) {
                    (_, Some(t)) => Some(t),
                    (true, None) => None, // infinite repeats
                    (false, None) => Some(1),
                };

                let cancellation = Self::cancellation_instance();
                let cancellation_id = match &cancellation {
                    Value::Instance { attributes, .. } => {
                        match attributes.get("cancellation-id").cloned() {
                            Some(Value::Int(id)) if id > 0 => id as u64,
                            _ => 0,
                        }
                    }
                    _ => 0,
                };
                let cancel_flag = cancellation_state(cancellation_id);

                let params = CueParams {
                    callback,
                    delay,
                    every,
                    times,
                    cancel_flag,
                    catch_cb,
                    stop_cb,
                };

                if is_current_thread {
                    self.scheduler_run_sync(params)?;
                } else {
                    let mut thread_interp = self.clone_for_thread();
                    std::thread::spawn(move || {
                        thread_interp.scheduler_run_async(params);
                    });
                }
                Ok(cancellation)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Scheduler",
                method
            ))),
        }
    }

    /// Check if the stop callback returns true.
    /// Reads captured variable values from shared_vars so the callback sees
    /// updates from other threads (e.g. parent setting `$stop = True`).
    fn scheduler_check_stop(&mut self, stop_cb: &Option<Value>) -> bool {
        if let Some(stop) = stop_cb {
            // Sync all shared vars so the callback closure sees updated values
            self.full_sync_shared_vars_to_env();
            if let Ok(result) = self.call_sub_value(stop.clone(), Vec::new(), true) {
                return result.truthy();
            }
        }
        false
    }

    /// Sync ALL shared vars to env, not just dirty ones.
    /// Needed for scheduler callbacks where the parent thread may have updated
    /// variables that the dirty tracking didn't capture.
    fn full_sync_shared_vars_to_env(&mut self) {
        let updates: Vec<(String, Value)> = {
            let sv = self.shared_vars.read().unwrap();
            sv.iter()
                .filter(|(k, _)| {
                    !k.starts_with("__mutsu_") && !k.starts_with('&') && k.as_str() != "_"
                })
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect()
        };
        for (key, val) in updates {
            self.env.insert(key, val);
        }
    }

    /// Check if cancelled
    fn scheduler_is_cancelled(
        cancel_flag: &Option<std::sync::Arc<std::sync::atomic::AtomicBool>>,
    ) -> bool {
        cancel_flag
            .as_ref()
            .is_some_and(|flag| flag.load(Ordering::Relaxed))
    }

    /// Run a callback, catching errors if a catch callback is provided.
    /// Returns Ok(true) on success, Ok(false) on caught error.
    fn scheduler_call_with_catch(
        &mut self,
        callback: &Value,
        catch_cb: &Option<Value>,
    ) -> Result<bool, RuntimeError> {
        let result = self.call_sub_value(callback.clone(), Vec::new(), true);
        match result {
            Ok(_) => Ok(true),
            Err(e) => {
                if let Some(catch) = catch_cb {
                    let exception = e
                        .exception
                        .map(|boxed| *boxed)
                        .unwrap_or_else(|| Value::str(e.message));
                    let _ = self.call_sub_value(catch.clone(), vec![exception], true);
                }
                Ok(false)
            }
        }
    }

    /// Run the repeating loop for :every, used by both sync and async paths.
    /// Returns Ok(()) for sync, errors propagated only in sync mode.
    fn scheduler_run_every_loop(
        &mut self,
        p: &CueParams,
        propagate_errors: bool,
    ) -> Result<(), RuntimeError> {
        let interval = p.every.unwrap_or(0.0);
        let mut count = 0;
        loop {
            if Self::scheduler_is_cancelled(&p.cancel_flag) {
                break;
            }
            if self.scheduler_check_stop(&p.stop_cb) {
                break;
            }
            if propagate_errors {
                self.scheduler_call_with_catch(&p.callback, &p.catch_cb)?;
            } else {
                let _ = self.scheduler_call_with_catch(&p.callback, &p.catch_cb);
            }
            count += 1;
            if p.times.is_some_and(|max| count >= max) {
                break;
            }
            if interval == f64::INFINITY {
                break;
            } else if interval > 0.0 && interval.is_finite() {
                std::thread::sleep(std::time::Duration::from_secs_f64(interval));
            }
        }
        Ok(())
    }

    /// Synchronous scheduler execution (CurrentThreadScheduler)
    fn scheduler_run_sync(&mut self, p: CueParams) -> Result<(), RuntimeError> {
        if !Self::scheduler_sleep(p.delay) {
            // Inf delay: for :every(Inf), run once then stop
            if p.every.is_some() {
                self.scheduler_call_with_catch(&p.callback, &p.catch_cb)?;
            }
            return Ok(());
        }

        if p.every.is_some() {
            self.scheduler_run_every_loop(&p, true)?;
        } else {
            let count = p.times.unwrap_or(1);
            for _ in 0..count {
                if Self::scheduler_is_cancelled(&p.cancel_flag) {
                    break;
                }
                self.scheduler_call_with_catch(&p.callback, &p.catch_cb)?;
            }
        }
        Ok(())
    }

    /// Async scheduler execution (ThreadPoolScheduler) - runs in spawned thread
    fn scheduler_run_async(&mut self, p: CueParams) {
        if !Self::scheduler_sleep(p.delay) {
            // Inf delay: for :every(Inf), run once then stop
            if p.every.is_some() {
                let _ = self.scheduler_call_with_catch(&p.callback, &p.catch_cb);
            }
            return;
        }

        if p.every.is_some() {
            let _ = self.scheduler_run_every_loop(&p, false);
        } else {
            let count = p.times.unwrap_or(1);
            for _ in 0..count {
                if Self::scheduler_is_cancelled(&p.cancel_flag) {
                    break;
                }
                let _ = self.scheduler_call_with_catch(&p.callback, &p.catch_cb);
            }
        }
    }

    pub(in crate::runtime) fn native_fake_scheduler(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let sched_id = match attributes.get("scheduler_id") {
            Some(Value::Int(id)) if *id > 0 => *id as u64,
            _ => {
                return Err(RuntimeError::new(
                    "FakeScheduler called without scheduler_id",
                ));
            }
        };
        match method {
            "cue" => {
                let callback = args.first().cloned().unwrap_or(Value::Nil);
                let delay = Self::named_value(&args, "at")
                    .or_else(|| Self::named_value(&args, "in"))
                    .map(|v| v.to_f64())
                    .unwrap_or(0.0);
                let every = Self::named_value(&args, "every").map(|v| v.to_f64());
                fake_scheduler_cue(sched_id, callback, every, delay);
                Ok(Self::cancellation_instance())
            }
            "progress-by" => {
                let duration = args.first().map(|v| v.to_f64()).unwrap_or(0.0);
                let (callbacks, counter_values) = fake_scheduler_progress_by(sched_id, duration);
                for cb in callbacks {
                    let _ = self.call_sub_value(cb, Vec::new(), true);
                }
                if !counter_values.is_empty() {
                    // Push counter values into the active supply emit buffer
                    // so tap-ok (with :virtual-time scheduler-driven supplies)
                    // can collect them.
                    if let Some(buf) = self.supply_emit_buffer.last_mut() {
                        for v in &counter_values {
                            buf.push(Value::Int(*v));
                        }
                    }
                    return Ok(Value::array(
                        counter_values.into_iter().map(Value::Int).collect(),
                    ));
                }
                Ok(Value::Nil)
            }
            "time" => {
                let (_, _) = fake_scheduler_progress_by(sched_id, 0.0);
                Ok(Value::Num(0.0))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on FakeScheduler",
                method
            ))),
        }
    }
}
