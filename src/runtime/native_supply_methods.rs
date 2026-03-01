use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use unicode_segmentation::UnicodeSegmentation;

impl Interpreter {
    pub(super) fn native_supply(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "decode" => {
                let encoding = args
                    .first()
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "utf-8".to_string());
                let source_values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };

                let mut emitted: Vec<Value> = Vec::new();
                let mut carry = String::new();
                for chunk in source_values {
                    let bytes = self.supply_chunk_to_bytes(&chunk, &encoding);
                    let decoded = self.decode_bytes_for_supply(&bytes, &encoding)?;
                    let normalized = self.translate_newlines_for_decode_native(&decoded);
                    let mut merged = String::new();
                    merged.push_str(&carry);
                    merged.push_str(&normalized);
                    let graphemes: Vec<&str> = merged.graphemes(true).collect();
                    if graphemes.is_empty() {
                        carry.clear();
                        continue;
                    }
                    let keep = graphemes.len().saturating_sub(1);
                    if keep > 0 {
                        emitted.push(Value::Str(graphemes[..keep].concat()));
                    }
                    carry = graphemes[keep].to_string();
                }
                if !carry.is_empty() {
                    emitted.push(Value::Str(carry));
                }

                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(emitted));
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
            "tail" => {
                let values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                let tail_count = self.resolve_supply_tail_count(args.first(), values.len())?;
                let start = values.len().saturating_sub(tail_count);
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(values[start..].to_vec()));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::Bool(false));
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
            "delayed" => {
                let delay_seconds = self.resolve_supply_delay_seconds(args.first())?;
                if delay_seconds <= 0.0 {
                    return Ok(Value::Instance {
                        class_name: Symbol::intern("Supply"),
                        attributes: Arc::new(attributes.clone()),
                        id: 0,
                    });
                }
                let mut new_attrs = attributes.clone();
                new_attrs.insert("delay_seconds".to_string(), Value::Num(delay_seconds));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
            "min" => {
                let mapper = args.first().cloned();
                if let Some(ref m) = mapper
                    && !matches!(m, Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. })
                {
                    return Err(RuntimeError::new(
                        "Supply.min: mapper must be code if specified",
                    ));
                }

                let values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };

                let mut result = Vec::new();
                let mut best_key: Option<Value> = None;
                for value in values {
                    let key = if let Some(ref map_fn) = mapper {
                        self.call_sub_value(map_fn.clone(), vec![value.clone()], true)?
                    } else {
                        value.clone()
                    };
                    let is_new_min = match &best_key {
                        None => true,
                        Some(current) => compare_values(&key, current) < 0,
                    };
                    if is_new_min {
                        best_key = Some(key);
                        result.push(value);
                    }
                }

                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(result));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::Bool(false));
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
            "split" => {
                let needle = Self::positional_value_required(&args, 0, "split requires a needle")?;
                let limit = Self::positional_value(&args, 1);
                let skip_empty = Self::named_bool(&args, "skip-empty");

                let max_parts = match limit {
                    None => None,
                    Some(Value::Int(i)) => {
                        if *i <= 0 {
                            return Ok(Value::make_instance(Symbol::intern("Supply"), {
                                let mut attrs = HashMap::new();
                                attrs.insert("values".to_string(), Value::array(Vec::new()));
                                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                                attrs.insert("live".to_string(), Value::Bool(false));
                                attrs
                            }));
                        }
                        Some(*i as usize)
                    }
                    Some(Value::Num(f)) if f.is_infinite() && f.is_sign_positive() => None,
                    Some(Value::Num(f)) => {
                        if *f <= 0.0 {
                            return Ok(Value::make_instance(Symbol::intern("Supply"), {
                                let mut attrs = HashMap::new();
                                attrs.insert("values".to_string(), Value::array(Vec::new()));
                                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                                attrs.insert("live".to_string(), Value::Bool(false));
                                attrs
                            }));
                        }
                        Some(*f as usize)
                    }
                    Some(Value::Str(s)) => {
                        let t = s.trim();
                        if t.eq_ignore_ascii_case("inf") {
                            None
                        } else if t.eq_ignore_ascii_case("-inf") {
                            return Ok(Value::make_instance(Symbol::intern("Supply"), {
                                let mut attrs = HashMap::new();
                                attrs.insert("values".to_string(), Value::array(Vec::new()));
                                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                                attrs.insert("live".to_string(), Value::Bool(false));
                                attrs
                            }));
                        } else {
                            match t.parse::<i64>() {
                                Ok(n) if n > 0 => Some(n as usize),
                                _ => {
                                    return Ok(Value::make_instance(Symbol::intern("Supply"), {
                                        let mut attrs = HashMap::new();
                                        attrs
                                            .insert("values".to_string(), Value::array(Vec::new()));
                                        attrs.insert("taps".to_string(), Value::array(Vec::new()));
                                        attrs.insert("live".to_string(), Value::Bool(false));
                                        attrs
                                    }));
                                }
                            }
                        }
                    }
                    Some(other) => {
                        let n = other.to_f64();
                        if n <= 0.0 {
                            return Ok(Value::make_instance(Symbol::intern("Supply"), {
                                let mut attrs = HashMap::new();
                                attrs.insert("values".to_string(), Value::array(Vec::new()));
                                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                                attrs.insert("live".to_string(), Value::Bool(false));
                                attrs
                            }));
                        }
                        Some(n as usize)
                    }
                };

                let source = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<_>>()
                        .join(""),
                    _ => String::new(),
                };

                let mut parts: Vec<Value> = match needle {
                    Value::Regex(pat) => {
                        let matches = self.regex_find_all(pat, &source);
                        if matches.is_empty() {
                            vec![Value::Str(source)]
                        } else {
                            let chars: Vec<char> = source.chars().collect();
                            let mut out = Vec::new();
                            let mut last_end = 0usize;
                            for (start, end) in matches {
                                let piece: String = chars[last_end..start].iter().collect();
                                out.push(Value::Str(piece));
                                last_end = end;
                            }
                            let tail: String = chars[last_end..].iter().collect();
                            out.push(Value::Str(tail));
                            out
                        }
                    }
                    other => {
                        let sep = other.to_string_value();
                        source
                            .split(&sep)
                            .map(|s| Value::Str(s.to_string()))
                            .collect()
                    }
                };

                if skip_empty {
                    parts.retain(|v| !v.to_string_value().is_empty());
                }
                if let Some(limit) = max_parts {
                    parts.truncate(limit);
                }

                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(parts));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::Bool(false));
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
            "reverse" => {
                let values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => {
                        let mut v = items.to_vec();
                        v.reverse();
                        v
                    }
                    _ => Vec::new(),
                };
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(values));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::Bool(false));
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
            "repeated" => {
                let as_fn = Self::named_value(&args, "as");
                let with_fn = Self::named_value(&args, "with");
                let values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                let mut seen_keys: Vec<Value> = Vec::new();
                let mut result = Vec::new();
                for val in &values {
                    let key = if let Some(ref f) = as_fn {
                        self.call_sub_value(f.clone(), vec![val.clone()], true)?
                    } else {
                        val.clone()
                    };
                    let found = seen_keys.iter().any(|s| {
                        if let Some(ref f) = with_fn {
                            self.call_sub_value(f.clone(), vec![s.clone(), key.clone()], true)
                                .map(|v| v.truthy())
                                .unwrap_or(false)
                        } else {
                            s == &key
                        }
                    });
                    if found {
                        result.push(val.clone());
                    } else {
                        seen_keys.push(key);
                    }
                }
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(result));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::Bool(false));
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
            "tap" | "act" => {
                // Immutable tap: emit all values and return a Tap instance
                let tap_cb = args.first().cloned().unwrap_or(Value::Nil);
                let done_cb = Self::named_value(&args, "done");
                let delay_seconds = Self::supply_delay_seconds(attributes);

                if let Some(Value::Int(supplier_id)) = attributes.get("supplier_id") {
                    let is_lines = matches!(attributes.get("is_lines"), Some(Value::Bool(true)));
                    if is_lines {
                        let chomp = attributes
                            .get("line_chomp")
                            .map(Value::truthy)
                            .unwrap_or(true);
                        register_supplier_lines_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            chomp,
                            delay_seconds,
                        );
                    } else {
                        register_supplier_tap(*supplier_id as u64, tap_cb.clone(), delay_seconds);
                    }
                }

                // If this Supply has a supply_id, register the tap globally
                // so that .start (Proc::Async) can find and call it later
                if let Some(Value::Int(sid)) = attributes.get("supply_id") {
                    register_supply_tap(*sid as u64, tap_cb.clone());
                }

                // For live/async supplies (e.g., signal), spawn a background thread
                // to consume events from the channel and call the callback.
                if let Some(Value::Int(sid)) = attributes.get("supply_id")
                    && let Some(rx) = take_supply_channel(*sid as u64)
                {
                    let mut thread_interp = self.clone_for_thread();
                    let cb = tap_cb.clone();
                    let delay = delay_seconds;
                    std::thread::spawn(move || {
                        Self::run_supply_act_loop(&mut thread_interp, &rx, &cb, delay);
                    });
                    return Ok(Value::make_instance(Symbol::intern("Tap"), HashMap::new()));
                }

                // For on-demand supplies, execute the callback to produce values
                let values = if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
                    let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    // Use supply_emit_buffer to collect emitted values
                    self.supply_emit_buffer.push(Vec::new());
                    let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                    self.supply_emit_buffer.pop().unwrap_or_default()
                } else if let Some(Value::Array(v, ..)) = attributes.get("values") {
                    v.to_vec()
                } else {
                    Vec::new()
                };

                // Call do_callbacks and tap callback for each value
                let do_cbs = attributes.get("do_callbacks").and_then(|v| {
                    if let Value::Array(a, ..) = v {
                        Some(a.to_vec())
                    } else {
                        None
                    }
                });
                for v in &values {
                    Self::sleep_for_supply_delay(delay_seconds);
                    if let Some(ref cbs) = do_cbs {
                        for cb in cbs {
                            let _ = self.call_sub_value(cb.clone(), vec![v.clone()], true);
                        }
                    }
                    let _ = self.call_sub_value(tap_cb.clone(), vec![v.clone()], true);
                }

                if let Some(done_fn) = done_cb {
                    if let Some(Value::Int(supplier_id)) = attributes.get("supplier_id") {
                        let supplier_is_done = attributes
                            .get("supplier_done")
                            .map(Value::truthy)
                            .unwrap_or(false);
                        if supplier_is_done {
                            let _ = self.call_sub_value(done_fn, vec![], true);
                        } else {
                            register_supplier_done_callback(*supplier_id as u64, done_fn);
                        }
                    } else {
                        let _ = self.call_sub_value(done_fn, vec![], true);
                    }
                }
                Ok(Value::make_instance(Symbol::intern("Tap"), HashMap::new()))
            }
            "do" => {
                // Supply.do($callback) — create a new Supply that calls $callback
                // as a side-effect for each value, passing values through
                let callback = args.first().cloned().unwrap_or(Value::Nil);
                let values = attributes
                    .get("values")
                    .cloned()
                    .unwrap_or(Value::array(Vec::new()));
                let live = attributes
                    .get("live")
                    .cloned()
                    .unwrap_or(Value::Bool(false));
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), values);
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("live".to_string(), live);
                // Accumulate do_callbacks chain
                let mut do_cbs =
                    if let Some(Value::Array(existing, ..)) = attributes.get("do_callbacks") {
                        existing.to_vec()
                    } else {
                        Vec::new()
                    };
                do_cbs.push(callback);
                new_attrs.insert("do_callbacks".to_string(), Value::array(do_cbs));
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
            "Promise" => {
                let promise = SharedPromise::new();
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    supplier_register_promise(supplier_id, promise.clone());
                } else if let Some(reason) = attributes.get("quit_reason").cloned() {
                    promise.break_with(reason, String::new(), String::new());
                } else {
                    let live = attributes.get("live").map(Value::truthy).unwrap_or(false);
                    if !live {
                        let result = match attributes.get("values") {
                            Some(Value::Array(items, ..)) => {
                                items.last().cloned().unwrap_or(Value::Nil)
                            }
                            _ => Value::Nil,
                        };
                        promise.keep(result, String::new(), String::new());
                    }
                }
                Ok(Value::Promise(promise))
            }
            "schedule-on" => {
                let scheduler = args.first().cloned().unwrap_or(Value::Nil);
                if !self.type_matches_value("Scheduler", &scheduler) {
                    return Err(RuntimeError::new(
                        "Supply.schedule-on expects a Scheduler argument",
                    ));
                }
                let mut new_attrs = attributes.clone();
                new_attrs.insert("scheduler".to_string(), scheduler);
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
            "lines" => {
                // Create a derived Supply that splits incoming chunks into lines.
                // `:chomp` defaults to true.
                let chomp = Self::named_value(&args, "chomp")
                    .map(|v| v.truthy())
                    .unwrap_or(true);
                let source_values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                let new_id = next_supply_id();
                let mut new_attrs = HashMap::new();
                new_attrs.insert(
                    "values".to_string(),
                    Value::array(split_supply_chunks_into_lines(&source_values, chomp)),
                );
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("supply_id".to_string(), Value::Int(new_id as i64));
                new_attrs.insert("is_lines".to_string(), Value::Bool(true));
                new_attrs.insert("line_chomp".to_string(), Value::Bool(chomp));
                new_attrs.insert("live".to_string(), Value::Bool(false));
                if let Some(parent_id) = attributes.get("supply_id") {
                    new_attrs.insert("parent_supply_id".to_string(), parent_id.clone());
                }
                if let Some(supplier_id) = attributes.get("supplier_id") {
                    new_attrs.insert("supplier_id".to_string(), supplier_id.clone());
                }
                if let Some(done) = attributes.get("supplier_done") {
                    new_attrs.insert("supplier_done".to_string(), done.clone());
                }
                if let Some(reason) = attributes.get("quit_reason") {
                    new_attrs.insert("quit_reason".to_string(), reason.clone());
                }
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
            "merge" => {
                // Supply.merge: merge multiple supplies into one
                // For now, just concatenate values from all supplies
                let new_id = next_supply_id();
                let mut all_values = Vec::new();
                if let Some(Value::Array(items, ..)) = attributes.get("values") {
                    all_values.extend(items.iter().cloned());
                }
                for arg in &args {
                    if let Value::Instance { attributes, .. } = arg
                        && let Some(Value::Array(items, ..)) = attributes.get("values")
                    {
                        all_values.extend(items.iter().cloned());
                    }
                }
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(all_values));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("supply_id".to_string(), Value::Int(new_id as i64));
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
            }
            "Supply" | "supply" => {
                // .Supply on a Supply is identity (noop) — return self
                // Preserve the same id for === identity check
                Ok(Value::Instance {
                    class_name: Symbol::intern("Supply"),
                    attributes: Arc::new(attributes.clone()),
                    id: 0, // placeholder — identity is checked via container, not id
                })
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Supply",
                method
            ))),
        }
    }

    // --- Supplier immutable ---

    pub(super) fn native_supplier(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "Supply" => {
                // Return a Supply backed by this Supplier
                let supplier_id = supplier_id_from_attrs(attributes).unwrap_or_else(next_supply_id);
                let (values, done, quit_reason) = supplier_snapshot(supplier_id);
                let mut supply_attrs = HashMap::new();
                supply_attrs.insert("values".to_string(), Value::array(values));
                supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                supply_attrs.insert(
                    "live".to_string(),
                    Value::Bool(!done && quit_reason.is_none()),
                );
                supply_attrs.insert("supplier_id".to_string(), Value::Int(supplier_id as i64));
                supply_attrs.insert("supplier_done".to_string(), Value::Bool(done));
                if let Some(reason) = quit_reason {
                    supply_attrs.insert("quit_reason".to_string(), reason);
                }
                Ok(Value::make_instance(Symbol::intern("Supply"), supply_attrs))
            }
            "emit" => {
                // Push to supply_emit_buffer (works for on-demand callbacks)
                let value = args.first().cloned().unwrap_or(Value::Nil);
                if let Some(buf) = self.supply_emit_buffer.last_mut() {
                    buf.push(value.clone());
                }
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    supplier_emit(supplier_id, value);
                }
                Ok(Value::Nil)
            }
            "done" => {
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    supplier_done(supplier_id);
                }
                Ok(Value::Nil)
            }
            "quit" => {
                let reason = args
                    .first()
                    .cloned()
                    .unwrap_or_else(|| Value::Str("Died".to_string()));
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    supplier_quit(supplier_id, reason);
                }
                Ok(Value::Nil)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Supplier",
                method
            ))),
        }
    }

    // --- Supplier mutable ---

    pub(super) fn native_supplier_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "emit" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                // Push to supply_emit_buffer if active
                if let Some(buf) = self.supply_emit_buffer.last_mut() {
                    buf.push(value.clone());
                }
                if let Some(supplier_id) = supplier_id_from_attrs(&attrs) {
                    supplier_emit(supplier_id, value.clone());
                }
                if let Some(Value::Array(items, ..)) = attrs.get_mut("emitted") {
                    Arc::make_mut(items).push(args.first().cloned().unwrap_or(Value::Nil));
                } else {
                    attrs.insert(
                        "emitted".to_string(),
                        Value::array(vec![args.first().cloned().unwrap_or(Value::Nil)]),
                    );
                }
                if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                    for (tap, emitted, delay_seconds) in
                        supplier_emit_callbacks(*supplier_id as u64, &value)
                    {
                        Self::sleep_for_supply_delay(delay_seconds);
                        let _ = self.call_sub_value(tap, vec![emitted], true);
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "done" => {
                attrs.insert("done".to_string(), Value::Bool(true));
                if let Some(supplier_id) = supplier_id_from_attrs(&attrs) {
                    supplier_done(supplier_id);
                }
                if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                    for (tap, emitted) in flush_supplier_line_taps(*supplier_id as u64) {
                        let _ = self.call_sub_value(tap, vec![emitted], true);
                    }
                    for done_cb in take_supplier_done_callbacks(*supplier_id as u64) {
                        let _ = self.call_sub_value(done_cb, Vec::new(), true);
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "quit" => {
                let reason = args
                    .first()
                    .cloned()
                    .unwrap_or_else(|| Value::Str("Died".to_string()));
                attrs.insert("done".to_string(), Value::Bool(true));
                attrs.insert("quit_reason".to_string(), reason.clone());
                if let Some(supplier_id) = supplier_id_from_attrs(&attrs) {
                    supplier_quit(supplier_id, reason);
                }
                if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                    for (tap, emitted) in flush_supplier_line_taps(*supplier_id as u64) {
                        let _ = self.call_sub_value(tap, vec![emitted], true);
                    }
                    for done_cb in take_supplier_done_callbacks(*supplier_id as u64) {
                        let _ = self.call_sub_value(done_cb, Vec::new(), true);
                    }
                }
                Ok((Value::Nil, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Supplier",
                method
            ))),
        }
    }

    // --- Supply mutable ---

    pub(super) fn native_supply_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "emit" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                if let Some(Value::Array(items, ..)) = attrs.get_mut("values") {
                    Arc::make_mut(items).push(value.clone());
                } else {
                    attrs.insert("values".to_string(), Value::array(vec![value.clone()]));
                }
                if let Some(Value::Array(taps, ..)) = attrs.get_mut("taps") {
                    for tap in taps.iter().cloned().collect::<Vec<_>>() {
                        let _ = self.call_sub_value(tap, vec![value.clone()], true);
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "tap" | "act" => {
                let tap_cb = args.first().cloned().unwrap_or(Value::Nil);
                let done_cb = Self::named_value(&args, "done");
                let delay_seconds = Self::supply_delay_seconds(&attrs);

                if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                    let is_lines = matches!(attrs.get("is_lines"), Some(Value::Bool(true)));
                    if is_lines {
                        let chomp = attrs.get("line_chomp").map(Value::truthy).unwrap_or(true);
                        register_supplier_lines_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            chomp,
                            delay_seconds,
                        );
                    } else {
                        register_supplier_tap(*supplier_id as u64, tap_cb.clone(), delay_seconds);
                    }
                }

                // If this Supply has a supply_id (belongs to Proc::Async),
                // register tap in the global registry so .start can find it
                if let Some(Value::Int(sid)) = attrs.get("supply_id") {
                    register_supply_tap(*sid as u64, tap_cb.clone());
                }

                // For live/async supplies (e.g., signal), spawn a background thread
                // to consume events from the channel and call the callback.
                if let Some(Value::Int(sid)) = attrs.get("supply_id")
                    && let Some(rx) = take_supply_channel(*sid as u64)
                {
                    let mut thread_interp = self.clone_for_thread();
                    let cb = tap_cb.clone();
                    let delay = delay_seconds;
                    std::thread::spawn(move || {
                        Self::run_supply_act_loop(&mut thread_interp, &rx, &cb, delay);
                    });
                    let tap_instance = Value::make_instance(Symbol::intern("Tap"), HashMap::new());
                    return Ok((tap_instance, attrs));
                }

                // For on-demand supplies, execute the callback to produce values
                let values = if let Some(on_demand_cb) = attrs.get("on_demand_callback").cloned() {
                    let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    self.supply_emit_buffer.push(Vec::new());
                    let _ = self.call_sub_value(on_demand_cb, vec![emitter], false);
                    self.supply_emit_buffer.pop().unwrap_or_default()
                } else {
                    if let Some(Value::Array(items, ..)) = attrs.get_mut("taps") {
                        Arc::make_mut(items).push(tap_cb.clone());
                    } else {
                        attrs.insert("taps".to_string(), Value::array(vec![tap_cb.clone()]));
                    }
                    if let Some(Value::Array(values, ..)) = attrs.get("values") {
                        values.to_vec()
                    } else {
                        Vec::new()
                    }
                };

                // Call do_callbacks and tap callback for each value
                let do_cbs = attrs.get("do_callbacks").and_then(|v| {
                    if let Value::Array(a, ..) = v {
                        Some(a.to_vec())
                    } else {
                        None
                    }
                });
                for v in &values {
                    Self::sleep_for_supply_delay(delay_seconds);
                    if let Some(ref cbs) = do_cbs {
                        for cb in cbs {
                            let _ = self.call_sub_value(cb.clone(), vec![v.clone()], true);
                        }
                    }
                    let _ = self.call_sub_value(tap_cb.clone(), vec![v.clone()], true);
                }

                // Call done callback after all values emitted
                if let Some(done_fn) = done_cb {
                    if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                        let supplier_is_done = attrs
                            .get("supplier_done")
                            .map(Value::truthy)
                            .unwrap_or(false);
                        if supplier_is_done {
                            let _ = self.call_sub_value(done_fn, vec![], true);
                        } else {
                            register_supplier_done_callback(*supplier_id as u64, done_fn);
                        }
                    } else {
                        let _ = self.call_sub_value(done_fn, vec![], true);
                    }
                }
                let tap_instance = Value::make_instance(Symbol::intern("Tap"), HashMap::new());
                Ok((tap_instance, attrs))
            }
            "repeated" => {
                let as_fn = Self::named_value(&args, "as");
                let with_fn = Self::named_value(&args, "with");
                let values = match attrs.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                let mut seen_keys: Vec<Value> = Vec::new();
                let mut result = Vec::new();
                for val in &values {
                    let key = if let Some(ref f) = as_fn {
                        self.call_sub_value(f.clone(), vec![val.clone()], true)?
                    } else {
                        val.clone()
                    };
                    let found = seen_keys.iter().any(|s| {
                        if let Some(ref f) = with_fn {
                            self.call_sub_value(f.clone(), vec![s.clone(), key.clone()], true)
                                .map(|v| v.truthy())
                                .unwrap_or(false)
                        } else {
                            s == &key
                        }
                    });
                    if found {
                        result.push(val.clone());
                    } else {
                        seen_keys.push(key);
                    }
                }
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(result));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("live".to_string(), Value::Bool(false));
                Ok((
                    Value::make_instance(Symbol::intern("Supply"), new_attrs),
                    attrs,
                ))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Supply",
                method
            ))),
        }
    }
}
