//! Split out of native_supply_methods.rs. See that file for the shared
//! helpers and the `QuitOutcome` enum.
use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;

/// Block until the control supplier emits a `limit:N` directive (or the 30s
/// bound elapses), consuming it through a push sink — registration replays a
/// directive that was emitted before we started listening, and later emits
/// wake the wait instantly (the old 10ms snapshot poll both lagged and could
/// miss a directive cleared by a supplier reset).
fn wait_for_control_limit(ctrl_id: u64) -> i64 {
    let waker = crate::value::waker::ReactWaker::new();
    let sink_id = supplier_sink_register(ctrl_id, 0, &waker);
    let mut current_limit: i64 = 0;
    let start = crate::runtime::thread_compat::Instant::now();
    while current_limit == 0 && start.elapsed() < std::time::Duration::from_secs(30) {
        for (_, event) in waker.drain() {
            if let crate::value::waker::SinkEvent::Emit(val) = event
                && let Some(rest) = val.to_string_value().strip_prefix("limit:")
                && let Ok(n) = rest.trim().parse::<i64>()
            {
                current_limit = n;
            }
        }
        if current_limit == 0 {
            waker.wait_activity(std::time::Duration::from_millis(100));
        }
    }
    supplier_sink_unregister(ctrl_id, sink_id);
    current_limit
}

impl Interpreter {
    /// Implement Supply.throttle($limit, $seconds-or-block, :$control, :$status)
    pub(super) fn supply_throttle(
        &mut self,
        attributes: &AttrMap,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        use crate::value::SharedPromise;
        let source_values = self.supply_get_values(attributes)?;
        let positionals = Self::positional_values(&args);
        let limit = positionals.first().map(|v| v.to_f64() as i64).unwrap_or(1);
        let mut process_block: Option<Value> = None;
        let mut seconds: f64 = 0.0;
        if let Some(second_arg) = positionals.get(1) {
            if matches!(second_arg.view(), ValueView::Sub(_) | ValueView::WeakSub(_)) {
                process_block = Some((*second_arg).clone());
            } else {
                seconds = second_arg.to_f64();
            }
        }
        let control_supplier_id = Self::named_value(&args, "control").and_then(|v| {
            if let ValueView::Instance { attributes, .. } = v.view() {
                supplier_id_from_attrs(&(attributes).as_map())
            } else {
                None
            }
        });
        let status_supplier_id = Self::named_value(&args, "status").and_then(|v| {
            if let ValueView::Instance { attributes, .. } = v.view() {
                supplier_id_from_attrs(&(attributes).as_map())
            } else {
                None
            }
        });
        if let Some(block) = process_block {
            if let Some(ctrl_id) = control_supplier_id
                && limit == 0
            {
                let out_supplier_id = next_supplier_id();
                let status_sid = status_supplier_id;
                let vals = source_values;
                let blk = block;
                let mut thread_interp = self.clone_for_thread();
                // Runs a full interpreter (VM safepoints park it during
                // execution): registered GC mutator; the control wait and the
                // worker joins are quiescent safe regions.
                crate::runtime::builtins_system::spawn_user_thread(move || {
                    let current_limit = wait_for_control_limit(ctrl_id);
                    let effective_limit = if current_limit > 0 {
                        current_limit
                    } else {
                        vals.len() as i64
                    };
                    // Run the process block with `effective_limit`-way
                    // concurrency (Raku semantics: throttle limits how many
                    // jobs run at once, it does not serialize them). Each
                    // completed job emits its kept promise immediately, so
                    // emission order is completion order; delivery itself is
                    // serialized per Raku's per-tap guarantee.
                    let n_workers = (effective_limit.max(1) as usize).min(vals.len().max(1));
                    let next_idx = Arc::new(std::sync::atomic::AtomicUsize::new(0));
                    let emit_lock = Arc::new(std::sync::Mutex::new(()));
                    let emitted_count = Arc::new(std::sync::atomic::AtomicUsize::new(0));
                    let vals = Arc::new(vals);
                    let mut handles = Vec::with_capacity(n_workers);
                    for _ in 0..n_workers {
                        let mut winterp = thread_interp.clone_for_thread();
                        let vals = Arc::clone(&vals);
                        let blk = blk.clone();
                        let next_idx = Arc::clone(&next_idx);
                        let emit_lock = Arc::clone(&emit_lock);
                        let emitted_count = Arc::clone(&emitted_count);
                        handles.push(crate::runtime::builtins_system::spawn_user_thread(
                            move || {
                                loop {
                                    let idx =
                                        next_idx.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                    if idx >= vals.len() {
                                        break;
                                    }
                                    if let Ok(result) = winterp.call_sub_value(
                                        blk.clone(),
                                        vec![vals[idx].clone()],
                                        false,
                                    ) {
                                        let promise = SharedPromise::new_kept(result);
                                        let pval = Value::promise(promise);
                                        let _guard = emit_lock.lock().unwrap();
                                        supplier_emit(out_supplier_id, pval.clone());
                                        let actions =
                                            supplier_emit_callbacks(out_supplier_id, &pval);
                                        for action in actions {
                                            if let SupplierEmitAction::Call(tap, emitted, delay) =
                                                action
                                            {
                                                Self::sleep_for_supply_delay(delay);
                                                let _ = winterp.call_sub_value(
                                                    tap,
                                                    vec![emitted],
                                                    true,
                                                );
                                            }
                                        }
                                        emitted_count
                                            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                    }
                                }
                            },
                        ));
                    }
                    for handle in handles {
                        let _ = crate::gc::block_quiescent(|| handle.join());
                    }
                    let emitted_count =
                        emitted_count.load(std::sync::atomic::Ordering::Relaxed) as i64;
                    if let Some(status_id) = status_sid {
                        let mut status_hash = HashMap::new();
                        status_hash.insert("allowed".to_string(), Value::int(effective_limit));
                        status_hash.insert("bled".to_string(), Value::int(0));
                        status_hash.insert("buffered".to_string(), Value::int(0));
                        status_hash.insert("emitted".to_string(), Value::int(emitted_count));
                        status_hash.insert("id".to_string(), Value::str("done".to_string()));
                        status_hash.insert("limit".to_string(), Value::int(effective_limit));
                        status_hash.insert("running".to_string(), Value::int(0));
                        let status_value = Value::hash(status_hash);
                        supplier_emit(status_id, status_value.clone());
                        let actions = supplier_emit_callbacks(status_id, &status_value);
                        for action in actions {
                            if let SupplierEmitAction::Call(tap, emitted, delay) = action {
                                Self::sleep_for_supply_delay(delay);
                                let _ = thread_interp.call_sub_value(tap, vec![emitted], true);
                            }
                        }
                    }
                    supplier_done(out_supplier_id);
                    for done_cb in take_supplier_done_callbacks(out_supplier_id) {
                        let _ = thread_interp.call_sub_value(done_cb, Vec::new(), true);
                    }
                });
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(Vec::new()));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::TRUE);
                new_attrs.insert(
                    "supplier_id".to_string(),
                    Value::int(out_supplier_id as i64),
                );
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            } else {
                let mut promises = Vec::with_capacity(source_values.len());
                for val in &source_values {
                    let result = self.call_sub_value(block.clone(), vec![val.clone()], false)?;
                    let promise = SharedPromise::new_kept(result);
                    promises.push(Value::promise(promise));
                }
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(promises));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::FALSE);
                if seconds > 0.0 {
                    new_attrs.insert("delay_seconds".to_string(), Value::num(seconds));
                    new_attrs.insert("throttle_limit".to_string(), Value::int(limit));
                }
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
        } else if let Some(ctrl_id) = control_supplier_id
            && limit == 0
        {
            let out_supplier_id = next_supplier_id();
            let vals = source_values;
            let secs = seconds;
            let mut thread_interp = self.clone_for_thread();
            // Registered GC mutator (runs a full interpreter); the control
            // wait is a quiescent safe region.
            crate::runtime::builtins_system::spawn_user_thread(move || {
                let current_limit = wait_for_control_limit(ctrl_id);
                let effective_limit = if current_limit > 0 {
                    current_limit as usize
                } else {
                    vals.len()
                };
                for (idx, val) in vals.iter().enumerate() {
                    // The first batch vents immediately once the limit is
                    // known (Rakudo emits as soon as capacity allows); the
                    // inter-batch delay applies between subsequent batches.
                    if secs > 0.0 && effective_limit > 0 && idx > 0 && idx % effective_limit == 0 {
                        Self::sleep_for_supply_delay(secs);
                    }
                    supplier_emit(out_supplier_id, val.clone());
                    let actions = supplier_emit_callbacks(out_supplier_id, val);
                    for action in actions {
                        if let SupplierEmitAction::Call(tap, emitted, delay) = action {
                            Self::sleep_for_supply_delay(delay);
                            let _ = thread_interp.call_sub_value(tap, vec![emitted], true);
                        }
                    }
                }
                supplier_done(out_supplier_id);
                for done_cb in take_supplier_done_callbacks(out_supplier_id) {
                    let _ = thread_interp.call_sub_value(done_cb, Vec::new(), true);
                }
            });
            let mut new_attrs = HashMap::new();
            new_attrs.insert("values".to_string(), Value::array(Vec::new()));
            new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            new_attrs.insert("live".to_string(), Value::TRUE);
            new_attrs.insert(
                "supplier_id".to_string(),
                Value::int(out_supplier_id as i64),
            );
            Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
        } else {
            let mut new_attrs = HashMap::new();
            new_attrs.insert("values".to_string(), Value::array(source_values));
            new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            new_attrs.insert("live".to_string(), Value::FALSE);
            if seconds > 0.0 {
                new_attrs.insert("delay_seconds".to_string(), Value::num(seconds));
                new_attrs.insert("throttle_limit".to_string(), Value::int(limit));
            }
            Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
        }
    }

    /// Create a new non-live Supply from a list of values.
    pub(super) fn make_supply_from_values(
        &self,
        values: Vec<Value>,
        _source_attrs: &AttrMap,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        Value::make_instance(Symbol::intern("Supply"), attrs)
    }

    /// Dispatch a transform method on a Supply instance.
    /// This is called from methods.rs for methods that need
    /// special Supply handling but are dispatched there.
    pub(crate) fn dispatch_supply_transform(
        &mut self,
        target: Value,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "Supply"
        {
            if method == "stable" {
                let seconds = args.first().map(Value::to_f64).unwrap_or(0.0);
                // `.stable(0)` is a no-op: return the same Supply so that
                // `$s === $s.stable(0)` holds.
                if seconds <= 0.0 {
                    return Ok(target.clone());
                }
                // Best-effort debounce for a materialized (non-live) Supply:
                // when every value is emitted within the stable window (as with
                // from-list, which emits instantaneously), only the final value
                // survives.
                // TODO: proper timer-based debounce for live Suppliers needs the
                // async scheduler; that path is exercised only by fudge-skipped
                // tests today.
                let attrs = attributes.as_map();
                let values = self.supply_get_values(&attrs)?;
                let out: Vec<Value> = values.last().cloned().into_iter().collect();
                return Ok(self.make_supply_from_values(out, &attrs));
            }
            self.native_supply(&(attributes).as_map(), method, args.to_vec())
        } else {
            Err(RuntimeError::new(format!(
                "Cannot call .{} on non-Supply",
                method
            )))
        }
    }

    /// Check if a key is already in the unique filter's seen list.
    /// Used by SupplierEmitAction::UniqueCheck when :as/:with are specified.
    pub(super) fn supplier_unique_check_seen(
        &mut self,
        supplier_id: u64,
        tap_index: usize,
        key: &Value,
        with_fn: &Option<Value>,
    ) -> Result<bool, RuntimeError> {
        let seen_keys = supplier_unique_get_seen(supplier_id, tap_index);
        for seen in &seen_keys {
            let is_same = if let Some(func) = with_fn {
                self.call_sub_value(func.clone(), vec![seen.clone(), key.clone()], true)?
                    .truthy()
            } else {
                values_identical(seen, key)
            };
            if is_same {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Run a Supply.start block in a background thread.
    /// Immediately emits a live inner Supply to the output supplier's taps,
    /// then spawns a thread that runs the block. When the block completes,
    /// the result is emitted into the inner Supply.
    pub(crate) fn run_start_call_in_thread(
        &mut self,
        callable: Value,
        value: Value,
        output_supplier_id: u64,
    ) {
        // Create a live inner Supply immediately
        let inner_supplier_id = next_supplier_id();
        let mut inner_attrs = HashMap::new();
        inner_attrs.insert("values".to_string(), Value::array(Vec::new()));
        inner_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        inner_attrs.insert(
            "supplier_id".to_string(),
            Value::int(inner_supplier_id as i64),
        );
        inner_attrs.insert("live".to_string(), Value::TRUE);
        let supply_val = Value::make_instance(Symbol::intern("Supply"), inner_attrs);

        // Emit the inner Supply to the output supplier synchronously
        supplier_emit(output_supplier_id, supply_val.clone());
        let out_actions = supplier_emit_callbacks(output_supplier_id, &supply_val);
        for out_action in out_actions {
            if let SupplierEmitAction::Call(tap, emitted, delay) = out_action {
                Self::sleep_for_supply_delay(delay);
                let _ = self.call_sub_value(tap, vec![emitted], true);
            }
        }

        // Spawn a thread to run the block asynchronously
        let mut thread_interp = self.clone_for_thread();
        crate::runtime::builtins_system::spawn_user_thread(move || {
            let result_val = thread_interp
                .call_sub_value(callable, vec![value], true)
                .unwrap_or(Value::NIL);
            // Emit the result into the inner Supply
            supplier_emit(inner_supplier_id, result_val.clone());
            let inner_actions = supplier_emit_callbacks(inner_supplier_id, &result_val);
            for action in inner_actions {
                if let SupplierEmitAction::Call(tap, emitted, delay) = action {
                    Self::sleep_for_supply_delay(delay);
                    let _ = thread_interp.call_sub_value(tap, vec![emitted], true);
                }
            }
            // Mark the inner supply as done
            supplier_done(inner_supplier_id);
            for done_cb in take_supplier_done_callbacks(inner_supplier_id) {
                let _ = thread_interp.invoke_done_callback(done_cb);
            }
        });
    }
}
