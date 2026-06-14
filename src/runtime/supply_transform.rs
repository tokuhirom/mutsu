//! Split out of native_supply_methods.rs. See that file for the shared
//! helpers and the `QuitOutcome` enum.
use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Implement Supply.throttle($limit, $seconds-or-block, :$control, :$status)
    pub(super) fn supply_throttle(
        &mut self,
        attributes: &HashMap<String, Value>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        use crate::value::SharedPromise;
        let source_values = self.supply_get_values(attributes)?;
        let positionals = Self::positional_values(&args);
        let limit = positionals.first().map(|v| v.to_f64() as i64).unwrap_or(1);
        let mut process_block: Option<Value> = None;
        let mut seconds: f64 = 0.0;
        if let Some(second_arg) = positionals.get(1) {
            if matches!(second_arg, Value::Sub(_) | Value::WeakSub(_)) {
                process_block = Some((*second_arg).clone());
            } else {
                seconds = second_arg.to_f64();
            }
        }
        let control_supplier_id = Self::named_value(&args, "control").and_then(|v| {
            if let Value::Instance { attributes, .. } = &v {
                supplier_id_from_attrs(&(attributes).as_map())
            } else {
                None
            }
        });
        let status_supplier_id = Self::named_value(&args, "status").and_then(|v| {
            if let Value::Instance { attributes, .. } = &v {
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
                std::thread::spawn(move || {
                    let mut current_limit: i64 = 0;
                    let start = std::time::Instant::now();
                    while current_limit == 0 && start.elapsed() < std::time::Duration::from_secs(30)
                    {
                        let (emitted, _, _) = supplier_snapshot(ctrl_id);
                        for val in &emitted {
                            let s = val.to_string_value();
                            if let Some(rest) = s.strip_prefix("limit:")
                                && let Ok(n) = rest.trim().parse::<i64>()
                            {
                                current_limit = n;
                            }
                        }
                        if current_limit == 0 {
                            std::thread::sleep(std::time::Duration::from_millis(10));
                        }
                    }
                    let effective_limit = if current_limit > 0 {
                        current_limit
                    } else {
                        vals.len() as i64
                    };
                    let mut emitted_count = 0i64;
                    for val in &vals {
                        if let Ok(result) =
                            thread_interp.call_sub_value(blk.clone(), vec![val.clone()], false)
                        {
                            let promise = SharedPromise::new_kept(result);
                            let pval = Value::Promise(promise);
                            supplier_emit(out_supplier_id, pval.clone());
                            let actions = supplier_emit_callbacks(out_supplier_id, &pval);
                            for action in actions {
                                if let SupplierEmitAction::Call(tap, emitted, delay) = action {
                                    Self::sleep_for_supply_delay(delay);
                                    let _ = thread_interp.call_sub_value(tap, vec![emitted], true);
                                }
                            }
                            emitted_count += 1;
                        }
                    }
                    if let Some(status_id) = status_sid {
                        let mut status_hash = HashMap::new();
                        status_hash.insert("allowed".to_string(), Value::Int(effective_limit));
                        status_hash.insert("bled".to_string(), Value::Int(0));
                        status_hash.insert("buffered".to_string(), Value::Int(0));
                        status_hash.insert("emitted".to_string(), Value::Int(emitted_count));
                        status_hash.insert("id".to_string(), Value::str("done".to_string()));
                        status_hash.insert("limit".to_string(), Value::Int(effective_limit));
                        status_hash.insert("running".to_string(), Value::Int(0));
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
                new_attrs.insert("live".to_string(), Value::Bool(true));
                new_attrs.insert(
                    "supplier_id".to_string(),
                    Value::Int(out_supplier_id as i64),
                );
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            } else {
                let mut promises = Vec::with_capacity(source_values.len());
                for val in &source_values {
                    let result = self.call_sub_value(block.clone(), vec![val.clone()], false)?;
                    let promise = SharedPromise::new_kept(result);
                    promises.push(Value::Promise(promise));
                }
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(promises));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::Bool(false));
                if seconds > 0.0 {
                    new_attrs.insert("delay_seconds".to_string(), Value::Num(seconds));
                    new_attrs.insert("throttle_limit".to_string(), Value::Int(limit));
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
            std::thread::spawn(move || {
                let mut current_limit: i64 = 0;
                let start = std::time::Instant::now();
                while current_limit == 0 && start.elapsed() < std::time::Duration::from_secs(30) {
                    let (emitted, _, _) = supplier_snapshot(ctrl_id);
                    for val in &emitted {
                        let s = val.to_string_value();
                        if let Some(rest) = s.strip_prefix("limit:")
                            && let Ok(n) = rest.trim().parse::<i64>()
                        {
                            current_limit = n;
                        }
                    }
                    if current_limit == 0 {
                        std::thread::sleep(std::time::Duration::from_millis(10));
                    }
                }
                let effective_limit = if current_limit > 0 {
                    current_limit as usize
                } else {
                    vals.len()
                };
                for (idx, val) in vals.iter().enumerate() {
                    if secs > 0.0 && effective_limit > 0 && idx % effective_limit == 0 {
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
            new_attrs.insert("live".to_string(), Value::Bool(true));
            new_attrs.insert(
                "supplier_id".to_string(),
                Value::Int(out_supplier_id as i64),
            );
            Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
        } else {
            let mut new_attrs = HashMap::new();
            new_attrs.insert("values".to_string(), Value::array(source_values));
            new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            new_attrs.insert("live".to_string(), Value::Bool(false));
            if seconds > 0.0 {
                new_attrs.insert("delay_seconds".to_string(), Value::Num(seconds));
                new_attrs.insert("throttle_limit".to_string(), Value::Int(limit));
            }
            Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
        }
    }

    /// Create a new non-live Supply from a list of values.
    pub(super) fn make_supply_from_values(
        &self,
        values: Vec<Value>,
        _source_attrs: &HashMap<String, Value>,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::Bool(false));
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
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Supply"
        {
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
            Value::Int(inner_supplier_id as i64),
        );
        inner_attrs.insert("live".to_string(), Value::Bool(true));
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
                .unwrap_or(Value::Nil);
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
