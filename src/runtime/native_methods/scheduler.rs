use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;
use std::sync::atomic::Ordering;

use super::interval_timer;
use super::state::*;
use super::state_lock::*;
use super::state_scheduler::{self, *};
use super::state_supplier::{close_supplier_tap, take_supplier_close_callbacks};
use crate::value::AttrMap;

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
            Value::int(next_cancellation_id() as i64),
        );
        Value::make_instance(Symbol::intern("Cancellation"), attrs)
    }

    fn scheduler_times_arg(args: &[Value]) -> Result<Option<usize>, RuntimeError> {
        let Some(value) = Self::named_value(args, "times") else {
            return Ok(None);
        };
        let count = match value.view() {
            ValueView::Int(i) => i,
            ValueView::Num(f) if f.is_finite() => f as i64,
            ValueView::Bool(b) => i64::from(b),
            ValueView::Str(s) => s.trim().parse::<i64>().map_err(|_| {
                RuntimeError::new(format!(
                    "Scheduler.cue: :times must be numeric, got '{}'",
                    *s
                ))
            })?,
            _ => {
                return Err(RuntimeError::new(format!(
                    "Scheduler.cue: :times must be numeric, got '{}'",
                    value.to_string_value()
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
        // Quiescent: a registered thread's raw sleep would starve a GC
        // stop-the-world rendezvous.
        crate::gc::block_quiescent(|| {
            crate::runtime::thread_compat::sleep(interval_timer::clamp_delay_secs(delay))
        });
        true
    }

    /// Close every upstream subscription recorded in a `[[supplier_id,
    /// tap_id], ...]` (mixed with nested Tap-handle entries) value, the same
    /// list `Tap.close`/`.cancel` cascades through. Shared with a `whenever`
    /// body's `done` completing its enclosing supply via the emitter, which
    /// must tear the same upstream subscriptions down (see
    /// `native_supply_methods::invoke_done_callback`'s `__SupplyOnDemandComplete`
    /// arm) — otherwise the source keeps delivering to a body whose output
    /// nobody can see.
    pub(in crate::runtime) fn close_upstream_taps(
        &mut self,
        entries: &Value,
    ) -> Result<(), RuntimeError> {
        if let ValueView::Array(entries, ..) = entries.view() {
            for entry in entries.iter().cloned().collect::<Vec<_>>() {
                match entry.view() {
                    ValueView::Array(pair, ..) if pair.len() == 2 => {
                        if let (ValueView::Int(sid), ValueView::Int(tid)) =
                            (pair[0].view(), pair[1].view())
                        {
                            close_supplier_tap(sid as u64, tid as u64);
                        }
                    }
                    // A chained on-demand source's own Tap handle: recurse so
                    // its `whenever`s close in turn.
                    ValueView::Instance {
                        class_name,
                        attributes: inner,
                        ..
                    } if class_name == "Tap" => {
                        let inner = inner.as_map().clone();
                        self.native_tap(&inner, "close")?;
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    pub(in crate::runtime) fn native_tap(
        &mut self,
        attributes: &AttrMap,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cancel" | "close" => {
                if let Some(ValueView::Int(whenever_id)) =
                    attributes.get("whenever_id").map(Value::view)
                {
                    close_whenever(whenever_id as u64);
                }
                if let Some(ValueView::Int(listener_id)) =
                    attributes.get("listener-id").map(Value::view)
                {
                    let lid = listener_id as u64;
                    close_async_listener(lid);
                    set_listener_closed(lid);
                }
                if let (Some(ValueView::Int(supplier_id)), Some(ValueView::Int(tap_id))) = (
                    attributes.get("supplier_id").map(Value::view),
                    attributes.get("tap_id").map(Value::view),
                ) {
                    close_supplier_tap(supplier_id as u64, tap_id as u64);
                }
                // ADR-0028: reclaim a `.schedule-on(ThreadPoolScheduler)` tap's
                // pump — dropping the sender disconnects the channel, so the
                // drain worker's blocking `recv()` observes end-of-stream and
                // exits instead of parking forever.
                if let Some(ValueView::Int(pump_id)) = attributes.get("pump_id").map(Value::view) {
                    super::state_scheduled_pump::drop_scheduled_pump(pump_id as u64);
                }
                // Stop every channel-backed act-loop worker this tap spawned:
                // setting the shared close flag makes the worker's bounded
                // wait exit and the flagged sender refuse further sends, so
                // an interval-timer entry feeding the channel retires on its
                // next tick. Without this the workers (and the timer) ran
                // until process exit — 4000 closed taps in
                // roast/S17-supply/syntax.t test 63 left ~4000 live threads
                // burning ~10 cores for the rest of the file.
                if let Some(ValueView::Array(ids, ..)) =
                    attributes.get("act_loop_close_ids").map(Value::view)
                {
                    for id in ids.iter() {
                        if let ValueView::Int(id) = id.view() {
                            close_act_loop(id as u64);
                        }
                    }
                }
                // Cascade upstream. In raku, closing a tap closes the supply
                // block that produced it, which closes the `whenever`
                // subscriptions inside it, which closes *their* sources — all
                // the way down to the original Supplier or listener. Without
                // this the block kept running: its CLOSE phasers never fired and
                // values still reached the (closed) tap callback, so
                // `Cro::Service.stop` left the old listener serving and a second
                // server on the same port never got a request.
                if let Some(entries) = attributes.get("upstream_taps") {
                    self.close_upstream_taps(entries)?;
                }
                // Fire any CLOSE-phaser callbacks registered on this tap's
                // supply emitter (run once — taking empties the list, so a
                // later normal termination won't run them again). After the
                // cascade, so a chain's CLOSE phasers run source-first, as raku
                // does.
                if let Some(ValueView::Int(cid)) =
                    attributes.get("close_supplier_id").map(Value::view)
                {
                    for cb in take_supplier_close_callbacks(cid as u64) {
                        self.call_sub_value(cb, vec![], true)?;
                    }
                }
                Ok(Value::TRUE)
            }
            "socket-port" => Ok(attributes
                .get("socket-port")
                .cloned()
                .unwrap_or(Value::promise(SharedPromise::new()))),
            "socket-host" => Ok(attributes
                .get("socket-host")
                .cloned()
                .unwrap_or(Value::promise(SharedPromise::new()))),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Tap",
                method
            ))),
        }
    }

    pub(in crate::runtime) fn native_cancellation(
        &self,
        attributes: &AttrMap,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cancel" => {
                if let Some(ValueView::Int(id)) = attributes.get("cancellation-id").map(Value::view)
                    && id > 0
                    && let Some(flag) = cancellation_state(id as u64)
                {
                    flag.store(true, Ordering::Relaxed);
                    // A timer-driven `:every` cue may have an iteration in
                    // flight (dispatched before the flag was set). Wait for it
                    // (bounded) so no callback side effect lands after
                    // `.cancel` returns: an in-flight `cas $a` completing
                    // after the caller re-declares a same-named lexical
                    // resurrects the dead cue's count through the bare-name
                    // atomic lane (roast S17-scheduler/every.t). Skip the wait
                    // when the callback itself called `.cancel` (same thread —
                    // waiting would deadlock until the timeout).
                    if let Some(busy_state) = cancellation_busy(id as u64) {
                        let self_cancel = busy_state
                            .running_thread
                            .lock()
                            .ok()
                            .is_some_and(|g| *g == Some(std::thread::current().id()));
                        if !self_cancel {
                            crate::gc::block_quiescent(|| {
                                let deadline = std::time::Instant::now()
                                    + std::time::Duration::from_millis(100);
                                while busy_state.busy.load(Ordering::Acquire)
                                    && std::time::Instant::now() < deadline
                                {
                                    std::thread::yield_now();
                                }
                            });
                        }
                        drop_cancellation_busy(id as u64);
                    }
                }
                Ok(Value::NIL)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Cancellation",
                method
            ))),
        }
    }

    pub(in crate::runtime) fn native_scheduler(
        &mut self,
        _attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
        is_current_thread: bool,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cue" => {
                // Validate mutually-exclusive scheduling adverbs up front (raku
                // throws synchronously here, before the callback is ever run).
                let has_in = Self::named_value(&args, "in").is_some();
                let has_at = Self::named_value(&args, "at").is_some();
                let has_every = Self::named_value(&args, "every").is_some();
                let has_times = Self::named_value(&args, "times").is_some();
                let has_stop = Self::named_value(&args, "stop").is_some();
                if has_in && has_at {
                    return Err(RuntimeError::new(
                        "Cannot specify :in and :at at the same time",
                    ));
                }
                if has_every && has_times && has_stop {
                    return Err(RuntimeError::new(
                        "Cannot specify :every, :times and :stop at the same time",
                    ));
                }
                if is_current_thread && has_every {
                    return Err(RuntimeError::new(
                        "Cannot specify :every in a CurrentThreadScheduler",
                    ));
                }
                let callback = args.first().cloned().unwrap_or(Value::NIL);
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
                let cancellation_id = match cancellation.view() {
                    ValueView::Instance { attributes, .. } => {
                        match attributes.as_map().get("cancellation-id").map(Value::view) {
                            Some(ValueView::Int(id)) if id > 0 => id as u64,
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
                } else if params.every.is_some_and(|e| e != f64::INFINITY)
                    && params.delay != f64::INFINITY
                {
                    // A finite (or -Inf, clamped) `:every` cue is a deadline-heap
                    // timer entry that enqueues each iteration onto the worker
                    // pool (ADR-0020 slice 2) — no dedicated sleep-loop thread.
                    // `:every(Inf)` and an Inf `:in` delay keep the one-shot
                    // path, which preserves their historical run-once handling.
                    self.cue_every_timer(params, cancellation_id)?;
                } else {
                    let mut thread_interp = self.clone_for_thread();
                    // Track the spawned task so `$*SCHEDULER.loads` reflects it
                    // until the callback finishes (mark started before spawn so a
                    // racing `.loads` never undercounts).
                    state_scheduler::scheduler_task_started();
                    // A finite :in/:at delay waits on the shared deadline-heap
                    // timer instead of a sleeping worker thread; the worker is
                    // spawned only once due. (An Inf delay keeps the direct
                    // path — scheduler_sleep handles its run-once-for-:every
                    // special case without sleeping.)
                    let mut params = params;
                    let delay = params.delay;
                    let deferred = delay.is_finite() && delay > 0.0;
                    if deferred {
                        params.delay = 0.0;
                    }
                    // One-shot cues run on the ADR-0020 worker pool (slice 1).
                    let run = move || {
                        crate::runtime::worker_pool::submit(move || {
                            thread_interp.scheduler_run_async(params);
                            state_scheduler::scheduler_task_finished();
                        });
                    };
                    if deferred {
                        interval_timer::register_once(
                            interval_timer::clamp_delay_secs(delay),
                            Box::new(run),
                        );
                    } else {
                        run();
                    }
                }
                Ok(cancellation)
            }
            "uncaught_handler" => {
                // Getter: return current uncaught_handler or Nil
                Ok(state_scheduler::get_uncaught_handler().unwrap_or(Value::NIL))
            }
            "loads" => {
                // Number of outstanding (spawned-but-unfinished) scheduled tasks.
                // A CurrentThreadScheduler cue runs inline (never increments), so
                // this is 0 once the scheduler is idle.
                Ok(Value::int(state_scheduler::scheduler_loads() as i64))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Scheduler",
                method
            ))),
        }
    }

    /// Mutable dispatch for Scheduler: handles uncaught_handler setter.
    /// Returns (result_value, updated_attributes).
    pub(in crate::runtime) fn native_scheduler_mut(
        attributes: AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        match method {
            "uncaught_handler" => {
                let handler = args.into_iter().next().unwrap_or(Value::NIL);
                state_scheduler::set_uncaught_handler(handler.clone());
                Ok((handler, attributes))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Scheduler",
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
            self.shared_vars
                .visible_entries()
                .into_iter()
                .filter(|(k, _)| {
                    !k.starts_with("__mutsu_") && !k.starts_with('&') && k.as_str() != "_"
                })
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
                } else if let Some(handler) = state_scheduler::get_uncaught_handler() {
                    // No per-cue :catch: an uncaught exception goes to the
                    // scheduler's uncaught_handler (raku semantics). Pass a real
                    // exception object so `$exception.message` works.
                    let exception = e.exception.map(|boxed| *boxed).unwrap_or_else(|| {
                        let mut attrs = HashMap::new();
                        attrs.insert("message".to_string(), Value::str(e.message.clone()));
                        Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
                    });
                    let _ = self.call_sub_value(handler, vec![exception], true);
                }
                Ok(false)
            }
        }
    }

    /// Drive a finite `:every` cue from the shared deadline-heap timer
    /// (ADR-0020 slice 2): each tick enqueues one callback run onto the worker
    /// pool, skipping the tick while the previous run is still going. The cue
    /// stops owning a thread — the retired implementation was a dedicated
    /// worker sleeping through a repeat loop for the cue's whole lifetime.
    fn cue_every_timer(
        &mut self,
        params: CueParams,
        cancellation_id: u64,
    ) -> Result<(), RuntimeError> {
        use std::sync::Arc;
        use std::sync::atomic::AtomicBool;
        let every = params.every.unwrap_or(0.0);
        // Rakudo parity: the timer has a 1ms minimum resolution; sub-1ms (and
        // zero/negative/-Inf) intervals are clamped with the same warning.
        let interval = if every < 0.001 {
            let shown_ms = if every.is_finite() {
                every * 1000.0
            } else {
                0.0
            };
            self.raise_resumable_warning(
                &format!("Minimum timer resolution is 1ms; using that instead of {shown_ms}ms"),
                Value::NIL,
            )?;
            0.001
        } else {
            every
        };
        let delay = params.delay;
        let cancel_flag = params.cancel_flag.clone();
        // `:every` with both `:times` and `:stop` is rejected up front, so the
        // dispatch count is exact even though a `:stop` probe consumes a tick.
        // The retired loop ran once even for `:times(0)` (it checked the count
        // AFTER the call) — `max(1)` preserves that.
        let times = params.times.map(|t| t.max(1));
        // Block-aware clone: the callback's own captured scalars go through
        // per-binding closure cells, not the bare-name shared lane, matching
        // the `start` spawn path.
        let thread_interp = self.clone_for_thread_for_block(&params.callback);
        // Track the cue so `$*SCHEDULER.loads` reflects it until it dies.
        state_scheduler::scheduler_task_started();
        let state = Arc::new(std::sync::Mutex::new((thread_interp, params)));
        let busy = Arc::new(AtomicBool::new(false));
        let running_thread = Arc::new(std::sync::Mutex::new(None));
        // Register the in-flight state under the Cancellation id so `.cancel`
        // can wait for a dispatched iteration to finish (see
        // `native_cancellation`).
        register_cancellation_busy(
            cancellation_id,
            Arc::new(CancellationBusy {
                busy: busy.clone(),
                running_thread: running_thread.clone(),
            }),
        );
        let stopped = Arc::new(AtomicBool::new(false));
        let mut dispatched: usize = 0;
        interval_timer::register_entry(
            interval_timer::clamp_delay_secs(delay),
            Box::new(move || {
                // Driver-thread rules: cheap checks and a pool enqueue only —
                // `:stop` and the callback are user code and run in the task.
                if stopped.load(Ordering::Relaxed)
                    || cancel_flag
                        .as_ref()
                        .is_some_and(|flag| flag.load(Ordering::Relaxed))
                    || times.is_some_and(|max| dispatched >= max)
                {
                    state_scheduler::scheduler_task_finished();
                    return None;
                }
                // Skip the tick while the previous iteration still runs (the
                // timer's fixed-rate reschedule then naturally falls back to
                // "next period after now").
                if !busy.swap(true, Ordering::AcqRel) {
                    dispatched += 1;
                    let state = state.clone();
                    let busy = busy.clone();
                    let stopped = stopped.clone();
                    let running_thread = running_thread.clone();
                    crate::runtime::worker_pool::submit(move || {
                        if let Ok(mut g) = running_thread.lock() {
                            *g = Some(std::thread::current().id());
                        }
                        if let Ok(mut guard) = state.lock() {
                            let (interp, p) = &mut *guard;
                            // Re-check cancellation at execution time: the
                            // dispatch-to-execution window (pool queue + lock)
                            // is far wider than the retired loop's
                            // check-to-call gap, and `.cancel` only waits for
                            // iterations it could see dispatched.
                            let is_cancelled = || {
                                p.cancel_flag
                                    .as_ref()
                                    .is_some_and(|flag| flag.load(Ordering::Relaxed))
                            };
                            if !is_cancelled() {
                                if interp.scheduler_check_stop(&p.stop_cb) {
                                    stopped.store(true, Ordering::Relaxed);
                                } else if !is_cancelled() {
                                    // Second check right before the call: the
                                    // stop probe above does a full shared-var
                                    // env sync, which is most of the window.
                                    let _ =
                                        interp.scheduler_call_with_catch(&p.callback, &p.catch_cb);
                                }
                            }
                        }
                        if let Ok(mut g) = running_thread.lock() {
                            *g = None;
                        }
                        busy.store(false, Ordering::Release);
                    });
                }
                Some(std::time::Duration::from_secs_f64(interval))
            }),
        );
        Ok(())
    }

    /// Synchronous scheduler execution (CurrentThreadScheduler). `:every` is
    /// rejected up front on a CurrentThreadScheduler, so there is no repeat
    /// handling here.
    fn scheduler_run_sync(&mut self, p: CueParams) -> Result<(), RuntimeError> {
        if !Self::scheduler_sleep(p.delay) {
            return Ok(());
        }
        let count = p.times.unwrap_or(1);
        for _ in 0..count {
            if Self::scheduler_is_cancelled(&p.cancel_flag) {
                break;
            }
            self.scheduler_call_with_catch(&p.callback, &p.catch_cb)?;
        }
        Ok(())
    }

    /// Async scheduler execution (ThreadPoolScheduler) — runs in a pooled
    /// worker task. Finite `:every` never reaches this (it is timer-driven,
    /// `cue_every_timer`); an `:every(Inf)` cue or an Inf `:in` delay lands
    /// here and runs once, preserving the retired repeat loop's break-after-
    /// one-run on an infinite interval.
    fn scheduler_run_async(&mut self, p: CueParams) {
        if !Self::scheduler_sleep(p.delay) {
            // Inf delay: for :every, run once then stop
            if p.every.is_some() {
                let _ = self.scheduler_call_with_catch(&p.callback, &p.catch_cb);
            }
            return;
        }

        if p.every.is_some() {
            if !Self::scheduler_is_cancelled(&p.cancel_flag)
                && !self.scheduler_check_stop(&p.stop_cb)
            {
                let _ = self.scheduler_call_with_catch(&p.callback, &p.catch_cb);
            }
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
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let sched_id = match attributes.get("scheduler_id").map(Value::view) {
            Some(ValueView::Int(id)) if id > 0 => id as u64,
            _ => {
                return Err(RuntimeError::new(
                    "FakeScheduler called without scheduler_id",
                ));
            }
        };
        match method {
            "cue" => {
                let callback = args.first().cloned().unwrap_or(Value::NIL);
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
                for cb in fake_scheduler_progress_by(sched_id, duration) {
                    let _ = self.call_sub_value(cb, Vec::new(), true);
                }
                Ok(Value::NIL)
            }
            "time" => {
                let _ = fake_scheduler_progress_by(sched_id, 0.0);
                Ok(Value::num(0.0))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on FakeScheduler",
                method
            ))),
        }
    }
}
