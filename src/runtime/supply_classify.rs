//! Split out of native_supply_methods.rs. See that file for the shared
//! helpers and the `QuitOutcome` enum.
use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Supply.unique implementation — filters duplicate values.
    /// Supports `:as`, `:with`, and `:expires` named parameters.
    pub(super) fn supply_unique(
        &mut self,
        attributes: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let as_fn = Self::named_value(args, "as");
        let with_fn = Self::named_value(args, "with");
        let expires = Self::named_value(args, "expires");

        let values = match attributes.get("values") {
            Some(Value::Array(items, ..)) => items.to_vec(),
            _ => Vec::new(),
        };

        let has_supplier = attributes.get("supplier_id").is_some();

        if has_supplier {
            // For supplier-backed supplies, store unique filter params in attributes
            // so the tap handler can apply filtering in real-time during emission.
            let mut new_attrs = HashMap::new();
            // Copy supplier_id and related attributes
            if let Some(sid) = attributes.get("supplier_id") {
                new_attrs.insert("supplier_id".to_string(), sid.clone());
            }
            if let Some(d) = attributes.get("supplier_done") {
                new_attrs.insert("supplier_done".to_string(), d.clone());
            }
            new_attrs.insert("values".to_string(), Value::array(Vec::new()));
            new_attrs.insert("live".to_string(), Value::Bool(false));
            new_attrs.insert("unique_filter".to_string(), Value::Bool(true));
            if let Some(ref f) = as_fn {
                new_attrs.insert("unique_as".to_string(), f.clone());
            }
            if let Some(ref f) = with_fn {
                new_attrs.insert("unique_with".to_string(), f.clone());
            }
            if let Some(ref e) = expires {
                new_attrs.insert("unique_expires".to_string(), e.clone());
            }
            new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            return Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs));
        }

        // For non-supplier or non-expires case, filter values directly
        let mut seen_keys: Vec<Value> = Vec::new();
        let mut unique_items: Vec<Value> = Vec::new();
        for item in values {
            let key = if let Some(ref func) = as_fn {
                self.call_sub_value(func.clone(), vec![item.clone()], true)?
            } else {
                item.clone()
            };

            let mut duplicate = false;
            for seen in &seen_keys {
                let is_same = if let Some(ref func) = with_fn {
                    self.call_sub_value(func.clone(), vec![seen.clone(), key.clone()], true)?
                        .truthy()
                } else {
                    values_identical(seen, &key)
                };
                if is_same {
                    duplicate = true;
                    break;
                }
            }

            if !duplicate {
                seen_keys.push(key);
                unique_items.push(item);
            }
        }

        let mut new_attrs = HashMap::new();
        new_attrs.insert("values".to_string(), Value::array(unique_items));
        new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        new_attrs.insert(
            "live".to_string(),
            attributes
                .get("live")
                .cloned()
                .unwrap_or(Value::Bool(false)),
        );
        Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
    }

    /// Supply.classify(mapper) — returns a Supply that emits Pair(key, Supply) for each
    /// classification group. The mapper can be a Block, Hash, or Array.
    pub(super) fn supply_classify(
        &mut self,
        attributes: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mapper = args.first().cloned().unwrap_or(Value::Nil);

        if let Some(Value::Int(source_supplier_id)) = attributes.get("supplier_id") {
            let source_sid = *source_supplier_id as u64;
            // Create a new supplier for the classify output
            let classify_supplier_id = next_supplier_id();
            // Register a classify tap on the source supplier
            register_supplier_classify_tap(source_sid, mapper, classify_supplier_id);
            // Return a Supply backed by the classify supplier
            let mut supply_attrs = HashMap::new();
            supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
            supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            supply_attrs.insert("live".to_string(), Value::Bool(true));
            supply_attrs.insert(
                "supplier_id".to_string(),
                Value::Int(classify_supplier_id as i64),
            );
            return Ok(Value::make_instance(Symbol::intern("Supply"), supply_attrs));
        }

        // Non-live supply: process all values upfront
        let values = match attributes.get("values") {
            Some(Value::Array(items, ..)) => items.to_vec(),
            _ => Vec::new(),
        };
        let mut keys_order: Vec<Value> = Vec::new();
        let mut buckets: HashMap<String, Vec<Value>> = HashMap::new();
        for val in values {
            let key = self.apply_classify_mapper(&mapper, &val)?;
            let key_str = key.to_string_value();
            if !buckets.contains_key(&key_str) {
                keys_order.push(key);
            }
            buckets.entry(key_str).or_default().push(val);
        }
        // Build result as array of pairs (key => Supply)
        let mut result_values = Vec::new();
        for key in keys_order {
            let key_str = key.to_string_value();
            let items = buckets.remove(&key_str).unwrap_or_default();
            let mut sub_attrs = HashMap::new();
            sub_attrs.insert("values".to_string(), Value::array(items));
            sub_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            sub_attrs.insert("live".to_string(), Value::Bool(false));
            let sub_supply = Value::make_instance(Symbol::intern("Supply"), sub_attrs);
            result_values.push(Value::ValuePair(Box::new(key), Box::new(sub_supply)));
        }
        let mut new_attrs = HashMap::new();
        new_attrs.insert("values".to_string(), Value::array(result_values));
        new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        new_attrs.insert("live".to_string(), Value::Bool(false));
        Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
    }

    /// Apply a classify mapper (Block/Hash/Array) to a value.
    pub(super) fn apply_classify_mapper(
        &mut self,
        mapper: &Value,
        value: &Value,
    ) -> Result<Value, RuntimeError> {
        match mapper {
            Value::Hash(map) => {
                let key = value.to_string_value();
                if let Some(v) = map.get(&key) {
                    Ok(v.clone())
                } else {
                    // Hash with `is default(0)` — default value
                    Ok(Value::Int(0))
                }
            }
            Value::Array(items, ..) => {
                let idx = value.to_f64() as usize;
                if idx < items.len() {
                    Ok(items[idx].clone())
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => {
                // Assume callable (Sub, block, etc.)
                self.call_sub_value(mapper.clone(), vec![value.clone()], true)
            }
        }
    }

    /// Handle a ClassifyCheck action during supplier emit.
    pub(super) fn handle_classify_emit(
        &mut self,
        source_supplier_id: u64,
        tap_index: usize,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let (mapper, classify_supplier_id, mut seen_keys, mut key_supplier_ids) =
            match get_classify_state(source_supplier_id, tap_index) {
                Some(state) => state,
                None => return Ok(()),
            };

        let key = self.apply_classify_mapper(&mapper, &value)?;
        let key_str = key.to_string_value();

        // Find or create sub-supplier for this key
        let sub_supplier_id = if let Some((_, sid)) = key_supplier_ids
            .iter()
            .find(|(k, _)| k.to_string_value() == key_str)
        {
            *sid
        } else {
            // New key — create sub-supplier and its Supply
            let sub_sid = next_supplier_id();
            seen_keys.push(key.clone());
            key_supplier_ids.push((key.clone(), sub_sid));

            // Update state before emitting (so it's visible to callbacks)
            update_classify_state(source_supplier_id, tap_index, seen_keys, key_supplier_ids);

            // Create a Supply for this sub-supplier
            let mut sub_supply_attrs = HashMap::new();
            sub_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
            sub_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            sub_supply_attrs.insert("live".to_string(), Value::Bool(true));
            sub_supply_attrs.insert("supplier_id".to_string(), Value::Int(sub_sid as i64));
            let sub_supply = Value::make_instance(Symbol::intern("Supply"), sub_supply_attrs);

            // Emit Pair(key, supply) to the classify supplier's taps
            let pair = Value::ValuePair(Box::new(key), Box::new(sub_supply));
            supplier_emit(classify_supplier_id, pair.clone());
            let actions = supplier_emit_callbacks(classify_supplier_id, &pair);
            for action in actions {
                match action {
                    SupplierEmitAction::Call(tap, emitted, delay_seconds) => {
                        Self::sleep_for_supply_delay(delay_seconds);
                        let _ = self.call_sub_value(tap, vec![emitted], true);
                    }
                    SupplierEmitAction::HeadLimitReached { supplier_id: sid2 } => {
                        let deferred_promises = supplier_done_deferred(sid2);
                        for done_cb in take_supplier_done_callbacks(sid2) {
                            let _ = self.invoke_done_callback(done_cb);
                        }
                        for (promise, result) in deferred_promises {
                            promise.keep(result, String::new(), String::new());
                        }
                    }
                    _ => {}
                }
            }

            sub_sid
        };

        // Emit the value to the sub-supplier
        supplier_emit(sub_supplier_id, value.clone());
        let actions = supplier_emit_callbacks(sub_supplier_id, &value);
        for action in actions {
            match action {
                SupplierEmitAction::Call(tap, emitted, delay_seconds) => {
                    Self::sleep_for_supply_delay(delay_seconds);
                    let _ = self.call_sub_value(tap, vec![emitted], true);
                }
                SupplierEmitAction::HeadLimitReached { supplier_id: sid2 } => {
                    let deferred_promises = supplier_done_deferred(sid2);
                    for done_cb in take_supplier_done_callbacks(sid2) {
                        let _ = self.invoke_done_callback(done_cb);
                    }
                    for (promise, result) in deferred_promises {
                        promise.keep(result, String::new(), String::new());
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }
}
