use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use unicode_segmentation::UnicodeSegmentation;

impl Interpreter {
    fn supply_has_active_callback(callback: &Value) -> bool {
        !matches!(callback, Value::Nil)
    }

    pub(super) fn runtime_error_from_supply_reason(reason: Value) -> RuntimeError {
        let message = reason.to_string_value();
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(reason));
        err
    }

    fn supply_is_terminated(attributes: &HashMap<String, Value>) -> bool {
        supplier_id_from_attrs(attributes)
            .map(|supplier_id| {
                let (_, done, quit_reason) = supplier_snapshot(supplier_id);
                done || quit_reason.is_some()
            })
            .unwrap_or_else(|| {
                attributes.get("done").map(Value::truthy).unwrap_or(false)
                    || attributes.contains_key("quit_reason")
            })
    }

    pub(super) fn call_supply_quit_handler(
        &mut self,
        quit_cb: Value,
        reason: Value,
    ) -> Result<(), RuntimeError> {
        let saved_when = self.when_matched();
        self.set_when_matched(false);
        let handled = match self.call_sub_value(quit_cb, vec![reason.clone()], true) {
            Ok(_) => true,
            Err(err) if err.is_succeed => true,
            Err(err) => {
                self.set_when_matched(saved_when);
                return Err(err);
            }
        };
        self.set_when_matched(saved_when);
        if handled {
            Ok(())
        } else {
            Err(Self::runtime_error_from_supply_reason(reason))
        }
    }

    pub(super) fn native_supply(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "encode" => {
                let source_values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                let mut emitted: Vec<Value> = Vec::with_capacity(source_values.len());
                for chunk in source_values {
                    emitted.push(self.call_method_with_values(chunk, "encode", args.clone())?);
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
                        emitted.push(Value::str(graphemes[..keep].concat()));
                    }
                    carry = graphemes[keep].to_string();
                }
                if !carry.is_empty() {
                    emitted.push(Value::str(carry));
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
            "native-descriptor" => Ok(attributes
                .get("native_descriptor_promise")
                .cloned()
                .unwrap_or_else(|| {
                    let promise = SharedPromise::new();
                    promise.keep(Value::Int(1), String::new(), String::new());
                    Value::Promise(promise)
                })),
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
                    return Ok(Value::make_instance_with_id(
                        Symbol::intern("Supply"),
                        attributes.clone(),
                        0,
                    ));
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
                            vec![Value::str(source)]
                        } else {
                            let chars: Vec<char> = source.chars().collect();
                            let mut out = Vec::new();
                            let mut last_end = 0usize;
                            for (start, end) in matches {
                                let piece: String = chars[last_end..start].iter().collect();
                                out.push(Value::str(piece));
                                last_end = end;
                            }
                            let tail: String = chars[last_end..].iter().collect();
                            out.push(Value::str(tail));
                            out
                        }
                    }
                    other => {
                        let sep = other.to_string_value();
                        source
                            .split(&sep)
                            .map(|s| Value::str(s.to_string()))
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
                let tap_cb = Self::positional_value(&args, 0)
                    .cloned()
                    .unwrap_or(Value::Nil);
                let done_cb = Self::named_value(&args, "done");
                let quit_cb = Self::named_value(&args, "quit");
                let delay_seconds = Self::supply_delay_seconds(attributes);

                if let Some(Value::Int(supplier_id)) = attributes.get("supplier_id") {
                    let has_unique =
                        matches!(attributes.get("unique_filter"), Some(Value::Bool(true)));
                    let is_lines = matches!(attributes.get("is_lines"), Some(Value::Bool(true)));
                    let is_words = matches!(attributes.get("is_words"), Some(Value::Bool(true)));
                    let is_elems =
                        matches!(attributes.get("elems_filter"), Some(Value::Bool(true)));
                    if !Self::supply_has_active_callback(&tap_cb) {
                        // done/quit-only taps do not register a value callback
                    } else if is_lines {
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
                    } else if is_words {
                        register_supplier_words_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                        );
                    } else if has_unique {
                        let as_fn = attributes.get("unique_as").cloned();
                        let with_fn = attributes.get("unique_with").cloned();
                        let expires = attributes.get("unique_expires").map(|v| v.to_f64());
                        register_supplier_unique_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            as_fn,
                            with_fn,
                            expires,
                        );
                    } else if is_elems {
                        let interval = attributes
                            .get("elems_interval")
                            .map(Value::to_f64)
                            .unwrap_or(0.0);
                        let initial_count = attributes
                            .get("elems_initial_count")
                            .and_then(|v| match v {
                                Value::Int(i) => Some(*i),
                                _ => None,
                            })
                            .unwrap_or(0);
                        register_supplier_elems_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            interval,
                            initial_count,
                        );
                    } else if let Some(Value::Int(limit)) = attributes.get("head_limit") {
                        register_supplier_tap_with_head_limit(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            *limit as usize,
                        );
                    } else if let Some(produce_callable) =
                        attributes.get("produce_callable").cloned()
                    {
                        register_supplier_produce_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            produce_callable,
                        );
                    } else {
                        register_supplier_tap(*supplier_id as u64, tap_cb.clone(), delay_seconds);
                    }
                }

                // Build a Tap handle referencing the registered subscription so
                // `.close` can stop it later.
                let tap_handle_attrs =
                    if let Some(Value::Int(supplier_id)) = attributes.get("supplier_id") {
                        let sid = *supplier_id as u64;
                        let mut h = HashMap::new();
                        h.insert("supplier_id".to_string(), Value::Int(sid as i64));
                        if let Some(tid) = last_supplier_tap_id(sid) {
                            h.insert("tap_id".to_string(), Value::Int(tid as i64));
                        }
                        h
                    } else {
                        HashMap::new()
                    };

                // If this Supply has a supply_id, register the tap globally
                // so that .start (Proc::Async) can find and call it later
                if let Some(Value::Int(sid)) = attributes.get("supply_id")
                    && Self::supply_has_active_callback(&tap_cb)
                {
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
                if let Some(Value::Int(sid)) = attributes.get("supply_id")
                    && let Some(collected) = get_supply_collected_output(*sid as u64)
                    && !collected.is_empty()
                {
                    if Self::supply_has_active_callback(&tap_cb) {
                        let _ =
                            self.call_sub_value(tap_cb.clone(), vec![Value::str(collected)], true);
                    }
                    return Ok(Value::make_instance(Symbol::intern("Tap"), HashMap::new()));
                }

                // For on-demand supplies, execute the callback to produce values
                let mut on_demand_quit: Option<Value> = None;
                let values = if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
                    let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    // Use supply_emit_buffer to collect emitted values
                    self.supply_emit_buffer.push(Vec::new());
                    let callback_result =
                        self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                    let emitted = self.supply_emit_buffer.pop().unwrap_or_default();
                    if let Err(err) = callback_result {
                        on_demand_quit = Some(
                            err.exception
                                .as_deref()
                                .cloned()
                                .unwrap_or_else(|| Value::str(err.message)),
                        );
                    }
                    emitted
                } else if let Some(Value::Int(sid)) = attributes.get("supplier_id") {
                    // For live (hot) supplier-backed supplies, new taps should
                    // only see future emits, not replayed past values. For
                    // cold supplier-backed supplies, replay the captured
                    // snapshot values so taps see them.
                    let is_live = matches!(attributes.get("live"), Some(Value::Bool(true)));
                    if is_live {
                        Vec::new()
                    } else {
                        let (snap_values, _, _) = supplier_snapshot(*sid as u64);
                        if !snap_values.is_empty() {
                            snap_values
                        } else if let Some(Value::Array(v, ..)) = attributes.get("values") {
                            v.to_vec()
                        } else {
                            Vec::new()
                        }
                    }
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
                            self.call_sub_value(cb.clone(), vec![v.clone()], true)?;
                        }
                    }
                    if Self::supply_has_active_callback(&tap_cb) {
                        self.call_sub_value(tap_cb.clone(), vec![v.clone()], true)?;
                    }
                }

                if let Some(quit_reason) = on_demand_quit {
                    if let Some(quit_fn) = quit_cb {
                        self.call_supply_quit_handler(quit_fn, quit_reason)?;
                    } else {
                        return Err(Self::runtime_error_from_supply_reason(quit_reason));
                    }
                    return Ok(Value::make_instance(Symbol::intern("Tap"), HashMap::new()));
                }

                if let Some(done_fn) = done_cb {
                    if let Some(Value::Int(supplier_id)) = attributes.get("supplier_id") {
                        let sid = *supplier_id as u64;
                        let (_, is_done, _) = supplier_snapshot(sid);
                        if is_done {
                            let _ = self.call_sub_value(done_fn, vec![], true);
                        } else {
                            register_supplier_done_callback(sid, done_fn);
                        }
                    } else {
                        let _ = self.call_sub_value(done_fn, vec![], true);
                    }
                }
                if let Some(quit_fn) = quit_cb {
                    if let Some(Value::Int(supplier_id)) = attributes.get("supplier_id") {
                        let (_, _, quit_reason) = supplier_snapshot(*supplier_id as u64);
                        if let Some(reason) = quit_reason {
                            self.call_supply_quit_handler(quit_fn, reason)?;
                        } else {
                            register_supplier_quit_callback(*supplier_id as u64, quit_fn);
                        }
                    } else if let Some(reason) = attributes.get("quit_reason").cloned() {
                        self.call_supply_quit_handler(quit_fn, reason)?;
                    }
                }
                Ok(Value::make_instance(
                    Symbol::intern("Tap"),
                    tap_handle_attrs,
                ))
            }
            "on-close" => {
                let close_cb = args.first().cloned().unwrap_or(Value::Nil);
                let mut new_attrs = attributes.clone();
                let mut callbacks = if let Some(Value::Array(existing, ..)) =
                    attributes.get("on_close_callbacks")
                {
                    existing.to_vec()
                } else {
                    Vec::new()
                };
                callbacks.push(close_cb);
                new_attrs.insert("on_close_callbacks".to_string(), Value::array(callbacks));
                Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
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
            "unique" => self.supply_unique(attributes, &args),
            "classify" => self.supply_classify(attributes, &args),
            "sort" => {
                let source_values = self.supply_get_values(attributes)?;
                let mut sorted = source_values.clone();
                if let Some(comparator) = args.first() {
                    let comp = comparator.clone();
                    let mut err: Option<RuntimeError> = None;
                    sorted.sort_by(|a, b| {
                        if err.is_some() {
                            return std::cmp::Ordering::Equal;
                        }
                        match self.call_sub_value(comp.clone(), vec![a.clone(), b.clone()], false) {
                            Ok(result) => {
                                // Handle Order enum (Less=-1, Same=0, More=1)
                                let n = match &result {
                                    Value::Enum { value, .. } => value.as_i64() as f64,
                                    other => other.to_f64(),
                                };
                                if n < 0.0 {
                                    std::cmp::Ordering::Less
                                } else if n > 0.0 {
                                    std::cmp::Ordering::Greater
                                } else {
                                    std::cmp::Ordering::Equal
                                }
                            }
                            Err(e) => {
                                err = Some(e);
                                std::cmp::Ordering::Equal
                            }
                        }
                    });
                    if let Some(e) = err {
                        return Err(e);
                    }
                } else {
                    sorted.sort_by(|a, b| {
                        let cmp = super::utils::compare_values(a, b);
                        if cmp < 0 {
                            std::cmp::Ordering::Less
                        } else if cmp > 0 {
                            std::cmp::Ordering::Greater
                        } else {
                            std::cmp::Ordering::Equal
                        }
                    });
                }
                Ok(self.make_supply_from_values(sorted, attributes))
            }
            "squish" => {
                let source_values = self.supply_get_values(attributes)?;
                let squished = self.dispatch_squish(Value::array(source_values), &args)?;
                let items = match squished {
                    Value::Seq(items) | Value::Array(items, ..) => items.to_vec(),
                    other => vec![other],
                };
                Ok(self.make_supply_from_values(items, attributes))
            }
            "head" => {
                let has_supplier = attributes.get("supplier_id").is_some();
                let source_values = self.supply_get_values(attributes)?;
                let count = if args.is_empty() {
                    1
                } else {
                    match args.first() {
                        Some(Value::Whatever) => {
                            if has_supplier {
                                usize::MAX
                            } else {
                                source_values.len()
                            }
                        }
                        Some(Value::Num(f)) if f.is_infinite() => {
                            if has_supplier {
                                usize::MAX
                            } else {
                                source_values.len()
                            }
                        }
                        Some(Value::Int(n)) => {
                            if *n < 0 {
                                0
                            } else {
                                *n as usize
                            }
                        }
                        Some(val) => {
                            // WhateverCode like *-3
                            let total = source_values.len() as i64;
                            let result =
                                self.call_sub_value(val.clone(), vec![Value::Int(total)], true)?;
                            let n = result.to_f64() as i64;
                            if n < 0 { 0 } else { n as usize }
                        }
                        None => 1,
                    }
                };
                if has_supplier {
                    // For live (supplier-backed) supplies, create a transformed supply
                    // that preserves the supplier connection but limits emissions.
                    let mut new_attrs = attributes.clone();
                    new_attrs.insert("head_limit".to_string(), Value::Int(count as i64));
                    new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
                } else {
                    let taken: Vec<Value> = source_values.into_iter().take(count).collect();
                    Ok(self.make_supply_from_values(taken, attributes))
                }
            }
            "flat" => {
                // For live (Supplier-backed) supplies, set up a flat tap chain:
                // incoming values are flattened and each element is re-emitted
                // to a new downstream supplier.
                if let Some(Value::Int(source_supplier_id)) = attributes.get("supplier_id") {
                    let source_sid = *source_supplier_id as u64;
                    let downstream_sid = next_supplier_id();
                    register_supplier_flat_tap(source_sid, downstream_sid);

                    let mut new_attrs = HashMap::new();
                    new_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    new_attrs.insert("supplier_id".to_string(), Value::Int(downstream_sid as i64));
                    new_attrs.insert("live".to_string(), Value::Bool(true));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs));
                }
                let source_values = self.supply_get_values(attributes)?;
                let mut flattened = Vec::new();
                for val in source_values {
                    match val {
                        Value::Array(items, kind) if !kind.is_itemized() => {
                            flattened.extend(items.iter().cloned());
                        }
                        Value::Slip(items) | Value::Seq(items) => {
                            flattened.extend(items.iter().cloned());
                        }
                        other => flattened.push(other),
                    }
                }
                Ok(self.make_supply_from_values(flattened, attributes))
            }
            "produce" => {
                let reducer = args.first().cloned().unwrap_or(Value::Nil);
                if !matches!(
                    reducer,
                    Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                ) {
                    return Err(RuntimeError::new("Supply.produce requires a code argument"));
                }
                // For live (Supplier-backed) supplies, create a derived supply
                // that stores the produce callable for deferred execution on tap.
                if attributes.get("supplier_id").is_some() {
                    let mut new_attrs = HashMap::new();
                    new_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    if let Some(sid) = attributes.get("supplier_id") {
                        new_attrs.insert("supplier_id".to_string(), sid.clone());
                    }
                    new_attrs.insert("produce_callable".to_string(), reducer);
                    new_attrs.insert("live".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs));
                }
                let source_values = self.supply_get_values(attributes)?;
                let mut produced = Vec::new();
                if !source_values.is_empty() {
                    let mut acc = source_values[0].clone();
                    produced.push(acc.clone());
                    for val in source_values.iter().skip(1) {
                        acc =
                            self.call_sub_value(reducer.clone(), vec![acc, val.clone()], false)?;
                        produced.push(acc.clone());
                    }
                }
                Ok(self.make_supply_from_values(produced, attributes))
            }
            "batch" => {
                let mut batch_elems: Option<usize> = None;
                let mut batch_seconds: Option<f64> = None;
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        match key.as_str() {
                            "elems" => batch_elems = Some(value.to_f64() as usize),
                            "seconds" => batch_seconds = Some(value.to_f64()),
                            _ => {}
                        }
                    } else {
                        batch_elems = Some(arg.to_f64() as usize);
                    }
                }

                // For supplier-backed (live) supplies, set up a batch tap chain
                if let Some(Value::Int(source_supplier_id)) = attributes.get("supplier_id") {
                    let source_sid = *source_supplier_id as u64;
                    let downstream_sid = next_supplier_id();

                    register_supplier_batch_tap(
                        source_sid,
                        downstream_sid,
                        batch_elems,
                        batch_seconds,
                    );

                    let mut new_attrs = HashMap::new();
                    new_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    new_attrs.insert("supplier_id".to_string(), Value::Int(downstream_sid as i64));
                    new_attrs.insert("live".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs));
                }

                // Non-live supply: batch eagerly
                let source_values = self.supply_get_values(attributes)?;
                let size = batch_elems.unwrap_or(source_values.len().max(1));
                let mut batched = Vec::new();
                for chunk in source_values.chunks(size) {
                    batched.push(Value::array(chunk.to_vec()));
                }
                Ok(self.make_supply_from_values(batched, attributes))
            }
            "rotor" => {
                let source_values = self.supply_get_values(attributes)?;
                let rotored = self.dispatch_rotor(Value::array(source_values), &args)?;
                let items = match rotored {
                    Value::Seq(items) | Value::Array(items, ..) => items.to_vec(),
                    other => vec![other],
                };
                Ok(self.make_supply_from_values(items, attributes))
            }
            "rotate" => {
                let source_values = self.supply_get_values(attributes)?;
                let n = args.first().map(|v| v.to_f64() as i64).unwrap_or(1);
                let len = source_values.len();
                if len == 0 {
                    return Ok(self.make_supply_from_values(Vec::new(), attributes));
                }
                let n = ((n % len as i64) + len as i64) as usize % len;
                let mut rotated = source_values[n..].to_vec();
                rotated.extend_from_slice(&source_values[..n]);
                Ok(self.make_supply_from_values(rotated, attributes))
            }
            "comb" => {
                let source_values = self.supply_get_values(attributes)?;
                // Concatenate all source string values, then comb the whole thing.
                // Supply.comb treats the stream as one logical string.
                let joined: String = source_values
                    .iter()
                    .map(|v| v.to_string_value())
                    .collect::<Vec<_>>()
                    .join("");
                let target = Value::str(joined);
                // Supply.comb ignores the :match named option (always returns strings),
                // matching Rakudo's behavior.
                let positional_args: Vec<Value> = args
                    .iter()
                    .filter(|a| !matches!(a, Value::Pair(k, _) if k == "match"))
                    .cloned()
                    .collect();
                let result = self.call_method_with_values(target, "comb", positional_args)?;
                let combed: Vec<Value> = match result {
                    Value::Seq(items) | Value::Array(items, ..) => items.iter().cloned().collect(),
                    other => vec![other],
                };
                Ok(self.make_supply_from_values(combed, attributes))
            }
            "words" => {
                let source_values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                let mut words = Vec::new();
                let mut buffer = String::new();
                for val in source_values {
                    let s = val.to_string_value();
                    buffer.push_str(&s);
                    loop {
                        let trimmed = buffer.trim_start();
                        if trimmed.is_empty() {
                            buffer.clear();
                            break;
                        }
                        if let Some(ws_pos) = trimmed.find(char::is_whitespace) {
                            let word = &trimmed[..ws_pos];
                            words.push(Value::str(word.to_string()));
                            let consumed = buffer.len() - trimmed.len() + ws_pos;
                            buffer = buffer[consumed..].to_string();
                        } else {
                            let leading_ws = buffer.len() - trimmed.len();
                            buffer = buffer[leading_ws..].to_string();
                            break;
                        }
                    }
                }
                let remaining = buffer.trim();
                if !remaining.is_empty() {
                    words.push(Value::str(remaining.to_string()));
                }
                let new_id = next_supply_id();
                let mut new_attrs = HashMap::new();
                new_attrs.insert("values".to_string(), Value::array(words));
                new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                new_attrs.insert("supply_id".to_string(), Value::Int(new_id as i64));
                new_attrs.insert("is_words".to_string(), Value::Bool(true));
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
            "snip" => {
                let source_values = self.supply_get_values(attributes)?;
                // snip takes conditions and splits the supply at points where conditions match
                let conditions: Vec<Value> = args;
                let mut result: Vec<Value> = Vec::new();
                let mut current_group: Vec<Value> = Vec::new();
                let mut cond_idx = 0;
                for val in source_values {
                    if cond_idx < conditions.len() {
                        let cond = &conditions[cond_idx];
                        // Use smartmatch (val ~~ cond) to check if the condition matches
                        let matched = self.smart_match(&val, cond);
                        if matched {
                            // Emit the current group and start a new one with this value
                            result.push(Value::array(current_group));
                            current_group = vec![val];
                            cond_idx += 1;
                            continue;
                        }
                    }
                    current_group.push(val);
                }
                if !current_group.is_empty() {
                    result.push(Value::array(current_group));
                }
                Ok(self.make_supply_from_values(result, attributes))
            }
            "minmax" => {
                let mapper = args.first().cloned();
                if let Some(ref m) = mapper
                    && !matches!(m, Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. })
                {
                    return Err(RuntimeError::new(
                        "Supply.minmax: mapper must be code if specified",
                    ));
                }
                let source_values = self.supply_get_values(attributes)?;
                let mut results: Vec<Value> = Vec::new();
                let mut cur_min: Option<Value> = None;
                let mut cur_max: Option<Value> = None;
                let mut cur_min_key: Option<Value> = None;
                let mut cur_max_key: Option<Value> = None;
                for val in source_values {
                    let key = if let Some(ref func) = mapper {
                        self.call_sub_value(func.clone(), vec![val.clone()], true)?
                    } else {
                        val.clone()
                    };
                    let mut changed = false;
                    if cur_min_key.is_none()
                        || super::utils::compare_values(&key, cur_min_key.as_ref().unwrap()) < 0
                    {
                        cur_min = Some(val.clone());
                        cur_min_key = Some(key.clone());
                        changed = true;
                    }
                    if cur_max_key.is_none()
                        || super::utils::compare_values(&key, cur_max_key.as_ref().unwrap()) > 0
                    {
                        cur_max = Some(val.clone());
                        cur_max_key = Some(key.clone());
                        changed = true;
                    }
                    if changed && let (Some(mn), Some(mx)) = (&cur_min, &cur_max) {
                        let range = match (mn, mx) {
                            (Value::Int(a), Value::Int(b)) => Value::Range(*a, *b),
                            _ => Value::generic_range(mn.clone(), mx.clone(), false, false),
                        };
                        results.push(range);
                    }
                }
                Ok(self.make_supply_from_values(results, attributes))
            }
            "zip" | "zip-latest" => {
                // Supply.zip(...) / Supply.zip-latest(...)
                // Zip this supply with others
                let source_values = self.supply_get_values(attributes)?;
                let mut other_values: Vec<Vec<Value>> = Vec::new();
                for arg in &args {
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = arg
                        && class_name == "Supply"
                    {
                        other_values.push(self.supply_get_values(attributes)?);
                    }
                }
                let min_len = std::iter::once(source_values.len())
                    .chain(other_values.iter().map(|v| v.len()))
                    .min()
                    .unwrap_or(0);
                let mut zipped = Vec::new();
                for i in 0..min_len {
                    let mut tuple = vec![source_values[i].clone()];
                    for other in &other_values {
                        tuple.push(other[i].clone());
                    }
                    zipped.push(Value::array(tuple));
                }
                Ok(self.make_supply_from_values(zipped, attributes))
            }
            "start" => {
                // Supply.start: for each emitted value, run the block and wrap
                // the result in a single-value Supply, emitting that Supply downstream.
                let block = args.first().cloned().unwrap_or(Value::Nil);

                // For live (Supplier-backed) supplies, create a derived supplier
                // and register a start-transform tap on the source.
                if let Some(source_supplier_id) = supplier_id_from_attrs(attributes) {
                    let output_supplier_id = next_supplier_id();
                    register_supplier_start_tap(source_supplier_id, block, output_supplier_id);
                    let mut new_attrs = HashMap::new();
                    new_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    new_attrs.insert(
                        "supplier_id".to_string(),
                        Value::Int(output_supplier_id as i64),
                    );
                    new_attrs.insert("live".to_string(), Value::Bool(true));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs));
                }

                // For non-live supplies, process all values eagerly
                let source_values = self.supply_get_values(attributes)?;
                let mut results = Vec::new();
                for val in source_values {
                    let result = self.call_sub_value(block.clone(), vec![val], true)?;
                    let mut inner_attrs = HashMap::new();
                    inner_attrs.insert("values".to_string(), Value::array(vec![result]));
                    inner_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    inner_attrs.insert("live".to_string(), Value::Bool(false));
                    results.push(Value::make_instance(Symbol::intern("Supply"), inner_attrs));
                }
                Ok(self.make_supply_from_values(results, attributes))
            }
            "wait" => {
                // Supply.wait: block until the underlying live supplier is done
                // (or quit), then return the last emitted value. For non-live
                // supplies this returns immediately with the last value.
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    use crate::value::SharedPromise;
                    let promise = SharedPromise::new_with_class(Symbol::intern("Promise"));
                    supplier_register_promise(supplier_id, promise.clone());
                    let (result, output, stderr) = promise.wait();
                    self.emit_output(&output);
                    self.stderr_output.push_str(&stderr);
                    self.sync_shared_vars_to_env();
                    // Drain shared thread output buffers so output produced by
                    // any background `start { }` thread that emitted into this
                    // supplier reaches the main TAP stream in chronological
                    // order.
                    if let Some(ref shared) = self.shared_thread_output {
                        let drained = std::mem::take(&mut *shared.lock().unwrap());
                        if !drained.is_empty() {
                            self.emit_output(&drained);
                        }
                    }
                    if let Some(ref shared) = self.shared_thread_stderr {
                        let drained = std::mem::take(&mut *shared.lock().unwrap());
                        if !drained.is_empty() {
                            self.stderr_output.push_str(&drained);
                        }
                    }
                    return Ok(result);
                }
                let source_values = self.supply_get_values(attributes)?;
                Ok(source_values.last().cloned().unwrap_or(Value::Nil))
            }
            "Channel" => {
                // Supply.Channel: create a Channel that receives all values
                // emitted by the underlying supplier. Existing snapshot values
                // are pushed first, then a live tap forwards future emits.
                // The channel is closed when the supplier is done, or failed
                // when the supplier quits.
                let source_values = self.supply_get_values(attributes)?;
                let ch = SharedChannel::new();
                for v in source_values {
                    ch.send(v);
                }
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    let (_, done, quit_reason) = supplier_snapshot(supplier_id);
                    if let Some(reason) = quit_reason {
                        ch.fail(reason);
                    } else if done {
                        ch.close();
                    } else {
                        register_supplier_channel_tap(supplier_id, ch.clone());
                    }
                } else {
                    ch.close();
                }
                Ok(Value::Channel(ch))
            }
            "Supply" | "supply" => {
                // .Supply on a Supply is identity (noop) — return self
                // Preserve the same id for === identity check
                Ok(Value::make_instance_with_id(
                    Symbol::intern("Supply"),
                    attributes.clone(),
                    0,
                ))
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
                if Self::supply_is_terminated(attributes) {
                    return Ok(Value::Nil);
                }
                if let Some(buf) = self.supply_emit_buffer.last_mut() {
                    buf.push(value.clone());
                }
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    supplier_emit(supplier_id, value.clone());
                    // Dispatch tap callbacks (head_limit, unique, produce, etc.)
                    let actions = supplier_emit_callbacks(supplier_id, &value);
                    for action in actions {
                        match action {
                            SupplierEmitAction::Call(tap, emitted, delay_seconds) => {
                                Self::sleep_for_supply_delay(delay_seconds);
                                let _ = self.call_sub_value(tap, vec![emitted], true);
                            }
                            SupplierEmitAction::UniqueCheck {
                                callback,
                                value: val,
                                delay_seconds,
                                ..
                            } => {
                                Self::sleep_for_supply_delay(delay_seconds);
                                let _ = self.call_sub_value(callback, vec![val], true);
                            }
                            SupplierEmitAction::ClassifyCheck {
                                value: val,
                                tap_index,
                            } => {
                                let _ = self.handle_classify_emit(supplier_id, tap_index, val);
                            }
                            SupplierEmitAction::HeadLimitReached { supplier_id: sid2 } => {
                                let deferred_promises = supplier_done_deferred(sid2);
                                for done_cb in take_supplier_done_callbacks(sid2) {
                                    let _ = self.call_sub_value(done_cb, Vec::new(), true);
                                }
                                for (promise, result) in deferred_promises {
                                    promise.keep(result, String::new(), String::new());
                                }
                            }
                            SupplierEmitAction::ProduceCall {
                                callback,
                                callable,
                                value: val,
                                accumulator,
                                delay_seconds,
                                tap_index,
                            } => {
                                let new_acc = if let Some(acc) = accumulator {
                                    self.call_sub_value(callable, vec![acc, val], false)
                                        .unwrap_or(Value::Nil)
                                } else {
                                    val
                                };
                                supplier_produce_update_acc(
                                    supplier_id,
                                    tap_index,
                                    new_acc.clone(),
                                );
                                Self::sleep_for_supply_delay(delay_seconds);
                                let _ = self.call_sub_value(callback, vec![new_acc], true);
                            }
                            SupplierEmitAction::StartCall {
                                callable,
                                value: val,
                                output_supplier_id,
                            } => {
                                self.run_start_call_in_thread(callable, val, output_supplier_id);
                            }
                            SupplierEmitAction::BatchEmit {
                                downstream_supplier_id,
                                batch,
                            } => {
                                let batch_value = Value::array(batch);
                                supplier_emit(downstream_supplier_id, batch_value.clone());
                                let ds_actions =
                                    supplier_emit_callbacks(downstream_supplier_id, &batch_value);
                                for ds_action in ds_actions {
                                    if let SupplierEmitAction::Call(tap, emitted, delay_seconds) =
                                        ds_action
                                    {
                                        Self::sleep_for_supply_delay(delay_seconds);
                                        let _ = self.call_sub_value(tap, vec![emitted], true);
                                    }
                                }
                            }
                            SupplierEmitAction::FlatEmit {
                                downstream_supplier_id,
                                items,
                            } => {
                                for item in items {
                                    supplier_emit(downstream_supplier_id, item.clone());
                                    let ds_actions =
                                        supplier_emit_callbacks(downstream_supplier_id, &item);
                                    for ds_action in ds_actions {
                                        if let SupplierEmitAction::Call(
                                            tap,
                                            emitted,
                                            delay_seconds,
                                        ) = ds_action
                                        {
                                            Self::sleep_for_supply_delay(delay_seconds);
                                            let _ = self.call_sub_value(tap, vec![emitted], true);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Ok(Value::Nil)
            }
            "done" => {
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    supplier_done(supplier_id);
                    close_supplier_channel_taps(supplier_id, None);
                    // Flush batch buffers before done
                    for (dsid, batch) in flush_supplier_batch_taps(supplier_id) {
                        let batch_value = Value::array(batch);
                        supplier_emit(dsid, batch_value.clone());
                        let ds_actions = supplier_emit_callbacks(dsid, &batch_value);
                        for ds_action in ds_actions {
                            if let SupplierEmitAction::Call(tap, emitted, delay_seconds) = ds_action
                            {
                                Self::sleep_for_supply_delay(delay_seconds);
                                let _ = self.call_sub_value(tap, vec![emitted], true);
                            }
                        }
                        supplier_done(dsid);
                        for done_cb in take_supplier_done_callbacks(dsid) {
                            let _ = self.call_sub_value(done_cb, Vec::new(), true);
                        }
                    }
                    for (tap, emitted) in flush_supplier_line_taps(supplier_id) {
                        let _ = self.call_sub_value(tap, vec![emitted], true);
                    }
                    for (tap, emitted) in flush_supplier_words_taps(supplier_id) {
                        let _ = self.call_sub_value(tap, vec![emitted], true);
                    }
                    for done_cb in take_supplier_done_callbacks(supplier_id) {
                        let _ = self.call_sub_value(done_cb, Vec::new(), true);
                    }
                    // Propagate done to start-transform output suppliers
                    for out_sid in get_start_output_supplier_ids(supplier_id) {
                        supplier_done(out_sid);
                        for done_cb in take_supplier_done_callbacks(out_sid) {
                            let _ = self.call_sub_value(done_cb, Vec::new(), true);
                        }
                    }
                }
                Ok(Value::Nil)
            }
            "quit" => {
                let reason = args
                    .first()
                    .cloned()
                    .unwrap_or_else(|| Value::str_from("Died"));
                if let Some(supplier_id) = supplier_id_from_attrs(attributes) {
                    supplier_quit(supplier_id, reason.clone());
                    close_supplier_channel_taps(supplier_id, Some(reason.clone()));
                    for (tap, emitted) in flush_supplier_line_taps(supplier_id) {
                        self.call_sub_value(tap, vec![emitted], true)?;
                    }
                    for (tap, emitted) in flush_supplier_words_taps(supplier_id) {
                        self.call_sub_value(tap, vec![emitted], true)?;
                    }
                    let (_, _, quit_reason) = supplier_snapshot(supplier_id);
                    if let Some(reason) = quit_reason {
                        for quit_cb in take_supplier_quit_callbacks(supplier_id) {
                            self.call_supply_quit_handler(quit_cb, reason.clone())?;
                        }
                    }
                    for done_cb in take_supplier_done_callbacks(supplier_id) {
                        self.call_sub_value(done_cb, Vec::new(), true)?;
                    }
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
                if Self::supply_is_terminated(&attrs) {
                    return Ok((Value::Nil, attrs));
                }
                // Push to supply_emit_buffer if active
                if let Some(buf) = self.supply_emit_buffer.last_mut() {
                    buf.push(value.clone());
                }
                if let Some(buf) = self.supply_emit_timed_buffer.last_mut() {
                    buf.push((value.clone(), std::time::Instant::now()));
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
                    let sid = *supplier_id as u64;
                    let actions = supplier_emit_callbacks(sid, &value);
                    for action in actions {
                        match action {
                            SupplierEmitAction::Call(tap, emitted, delay_seconds) => {
                                Self::sleep_for_supply_delay(delay_seconds);
                                self.call_sub_value(tap, vec![emitted], true)?;
                            }
                            SupplierEmitAction::UniqueCheck {
                                callback,
                                value: val,
                                delay_seconds,
                                as_fn,
                                with_fn,
                                tap_index,
                            } => {
                                let key = if let Some(f) = as_fn {
                                    self.call_sub_value(f, vec![val.clone()], true)?
                                } else {
                                    val.clone()
                                };
                                // Check against seen keys
                                let is_dup = self
                                    .supplier_unique_check_seen(sid, tap_index, &key, &with_fn)?;
                                if !is_dup {
                                    supplier_unique_mark_seen(sid, tap_index, key);
                                    Self::sleep_for_supply_delay(delay_seconds);
                                    self.call_sub_value(callback, vec![val], true)?;
                                }
                            }
                            SupplierEmitAction::ClassifyCheck {
                                value: val,
                                tap_index,
                            } => {
                                self.handle_classify_emit(sid, tap_index, val)?;
                            }
                            SupplierEmitAction::HeadLimitReached { supplier_id: sid2 } => {
                                let deferred_promises = supplier_done_deferred(sid2);
                                for done_cb in take_supplier_done_callbacks(sid2) {
                                    let _ = self.call_sub_value(done_cb, Vec::new(), true);
                                }
                                for (promise, result) in deferred_promises {
                                    promise.keep(result, String::new(), String::new());
                                }
                            }
                            SupplierEmitAction::ProduceCall {
                                callback,
                                callable,
                                value: val,
                                accumulator,
                                delay_seconds,
                                tap_index,
                            } => {
                                let new_acc = if let Some(acc) = accumulator {
                                    self.call_sub_value(callable, vec![acc, val], false)
                                        .unwrap_or(Value::Nil)
                                } else {
                                    val
                                };
                                supplier_produce_update_acc(sid, tap_index, new_acc.clone());
                                Self::sleep_for_supply_delay(delay_seconds);
                                self.call_sub_value(callback, vec![new_acc], true)?;
                            }
                            SupplierEmitAction::StartCall {
                                callable,
                                value: val,
                                output_supplier_id,
                            } => {
                                self.run_start_call_in_thread(callable, val, output_supplier_id);
                            }
                            SupplierEmitAction::BatchEmit {
                                downstream_supplier_id,
                                batch,
                            } => {
                                let batch_value = Value::array(batch);
                                supplier_emit(downstream_supplier_id, batch_value.clone());
                                let ds_actions =
                                    supplier_emit_callbacks(downstream_supplier_id, &batch_value);
                                for ds_action in ds_actions {
                                    if let SupplierEmitAction::Call(tap, emitted, delay_seconds) =
                                        ds_action
                                    {
                                        Self::sleep_for_supply_delay(delay_seconds);
                                        self.call_sub_value(tap, vec![emitted], true)?;
                                    }
                                }
                            }
                            SupplierEmitAction::FlatEmit {
                                downstream_supplier_id,
                                items,
                            } => {
                                for item in items {
                                    supplier_emit(downstream_supplier_id, item.clone());
                                    let ds_actions =
                                        supplier_emit_callbacks(downstream_supplier_id, &item);
                                    for ds_action in ds_actions {
                                        if let SupplierEmitAction::Call(
                                            tap,
                                            emitted,
                                            delay_seconds,
                                        ) = ds_action
                                        {
                                            Self::sleep_for_supply_delay(delay_seconds);
                                            self.call_sub_value(tap, vec![emitted], true)?;
                                        }
                                    }
                                }
                            }
                        }
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
                    let sid = *supplier_id as u64;
                    close_supplier_channel_taps(sid, None);
                    // Flush batch buffers before done
                    for (dsid, batch) in flush_supplier_batch_taps(sid) {
                        let batch_value = Value::array(batch);
                        supplier_emit(dsid, batch_value.clone());
                        let ds_actions = supplier_emit_callbacks(dsid, &batch_value);
                        for ds_action in ds_actions {
                            if let SupplierEmitAction::Call(tap, emitted, delay_seconds) = ds_action
                            {
                                Self::sleep_for_supply_delay(delay_seconds);
                                self.call_sub_value(tap, vec![emitted], true)?;
                            }
                        }
                        // Propagate done to downstream batch suppliers
                        supplier_done(dsid);
                        for done_cb in take_supplier_done_callbacks(dsid) {
                            let _ = self.call_sub_value(done_cb, Vec::new(), true);
                        }
                    }
                    // Propagate done to classify sub-suppliers
                    let classify_subs = get_classify_sub_supplier_ids(sid);
                    for sub_sid in classify_subs {
                        supplier_done(sub_sid);
                        for done_cb in take_supplier_done_callbacks(sub_sid) {
                            let _ = self.call_sub_value(done_cb, Vec::new(), true);
                        }
                    }
                    for (tap, emitted) in flush_supplier_line_taps(sid) {
                        self.call_sub_value(tap, vec![emitted], true)?;
                    }
                    for (tap, emitted) in flush_supplier_words_taps(sid) {
                        self.call_sub_value(tap, vec![emitted], true)?;
                    }
                    for done_cb in take_supplier_done_callbacks(sid) {
                        self.call_sub_value(done_cb, Vec::new(), true)?;
                    }
                    // Propagate done to start-transform output suppliers
                    for out_sid in get_start_output_supplier_ids(sid) {
                        supplier_done(out_sid);
                        for done_cb in take_supplier_done_callbacks(out_sid) {
                            let _ = self.call_sub_value(done_cb, Vec::new(), true);
                        }
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "quit" => {
                let reason = args
                    .first()
                    .cloned()
                    .unwrap_or_else(|| Value::str_from("Died"));
                attrs.insert("done".to_string(), Value::Bool(true));
                attrs.insert("quit_reason".to_string(), reason.clone());
                if let Some(supplier_id) = supplier_id_from_attrs(&attrs) {
                    supplier_quit(supplier_id, reason.clone());
                }
                if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                    let sid = *supplier_id as u64;
                    close_supplier_channel_taps(sid, Some(reason.clone()));
                    for (tap, emitted) in flush_supplier_line_taps(sid) {
                        self.call_sub_value(tap, vec![emitted], true)?;
                    }
                    for (tap, emitted) in flush_supplier_words_taps(sid) {
                        self.call_sub_value(tap, vec![emitted], true)?;
                    }
                    for quit_cb in take_supplier_quit_callbacks(sid) {
                        self.call_supply_quit_handler(quit_cb, reason.clone())?;
                    }
                    for done_cb in take_supplier_done_callbacks(sid) {
                        self.call_sub_value(done_cb, Vec::new(), true)?;
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
                        if Self::supply_has_active_callback(&tap) {
                            let _ = self.call_sub_value(tap, vec![value.clone()], true);
                        }
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "tap" | "act" => {
                let tap_cb = Self::positional_value(&args, 0)
                    .cloned()
                    .unwrap_or(Value::Nil);
                let done_cb = Self::named_value(&args, "done");
                let quit_cb = Self::named_value(&args, "quit");
                let delay_seconds = Self::supply_delay_seconds(&attrs);

                if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                    let has_unique = matches!(attrs.get("unique_filter"), Some(Value::Bool(true)));
                    let is_lines = matches!(attrs.get("is_lines"), Some(Value::Bool(true)));
                    let is_words = matches!(attrs.get("is_words"), Some(Value::Bool(true)));
                    let is_elems = matches!(attrs.get("elems_filter"), Some(Value::Bool(true)));
                    if !Self::supply_has_active_callback(&tap_cb) {
                        // done/quit-only taps do not register a value callback
                    } else if is_lines {
                        let chomp = attrs.get("line_chomp").map(Value::truthy).unwrap_or(true);
                        register_supplier_lines_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            chomp,
                            delay_seconds,
                        );
                    } else if is_words {
                        register_supplier_words_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                        );
                    } else if has_unique {
                        let as_fn = attrs.get("unique_as").cloned();
                        let with_fn = attrs.get("unique_with").cloned();
                        let expires = attrs.get("unique_expires").map(|v| v.to_f64());
                        register_supplier_unique_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            as_fn,
                            with_fn,
                            expires,
                        );
                    } else if is_elems {
                        let interval = attrs
                            .get("elems_interval")
                            .map(Value::to_f64)
                            .unwrap_or(0.0);
                        let initial_count = attrs
                            .get("elems_initial_count")
                            .and_then(|v| match v {
                                Value::Int(i) => Some(*i),
                                _ => None,
                            })
                            .unwrap_or(0);
                        register_supplier_elems_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            interval,
                            initial_count,
                        );
                    } else if let Some(Value::Int(limit)) = attrs.get("head_limit") {
                        register_supplier_tap_with_head_limit(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            *limit as usize,
                        );
                    } else if let Some(produce_callable) = attrs.get("produce_callable").cloned() {
                        register_supplier_produce_tap(
                            *supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            produce_callable,
                        );
                    } else {
                        register_supplier_tap(*supplier_id as u64, tap_cb.clone(), delay_seconds);
                    }
                }

                // Build a Tap handle referencing the registered subscription so
                // `.close` can stop it later.
                let tap_handle_attrs =
                    if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                        let sid = *supplier_id as u64;
                        let mut h = HashMap::new();
                        h.insert("supplier_id".to_string(), Value::Int(sid as i64));
                        if let Some(tid) = last_supplier_tap_id(sid) {
                            h.insert("tap_id".to_string(), Value::Int(tid as i64));
                        }
                        h
                    } else {
                        HashMap::new()
                    };

                // If this Supply has a supply_id (belongs to Proc::Async),
                // register tap in the global registry so .start can find it
                if let Some(Value::Int(sid)) = attrs.get("supply_id")
                    && Self::supply_has_active_callback(&tap_cb)
                {
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
                if let Some(Value::Int(sid)) = attrs.get("supply_id")
                    && let Some(collected) = get_supply_collected_output(*sid as u64)
                    && !collected.is_empty()
                {
                    if Self::supply_has_active_callback(&tap_cb) {
                        let _ =
                            self.call_sub_value(tap_cb.clone(), vec![Value::str(collected)], true);
                    }
                    let tap_instance = Value::make_instance(Symbol::intern("Tap"), HashMap::new());
                    return Ok((tap_instance, attrs));
                }

                // For on-demand supplies, execute the callback to produce values
                let has_unique = matches!(attrs.get("unique_filter"), Some(Value::Bool(true)));
                let mut on_demand_quit: Option<Value> = None;
                let values = if let Some(on_demand_cb) = attrs.get("on_demand_callback").cloned() {
                    let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    self.supply_emit_buffer.push(Vec::new());
                    let callback_result = self.call_sub_value(on_demand_cb, vec![emitter], false);
                    let emitted = self.supply_emit_buffer.pop().unwrap_or_default();
                    if let Err(err) = callback_result {
                        on_demand_quit = Some(
                            err.exception
                                .as_deref()
                                .cloned()
                                .unwrap_or_else(|| Value::str(err.message)),
                        );
                    }
                    emitted
                } else if has_unique {
                    if Self::supply_has_active_callback(&tap_cb) {
                        if let Some(Value::Array(items, ..)) = attrs.get_mut("taps") {
                            Arc::make_mut(items).push(tap_cb.clone());
                        } else {
                            attrs.insert("taps".to_string(), Value::array(vec![tap_cb.clone()]));
                        }
                    }
                    // For unique supplier-backed supplies, replay already-emitted
                    // values through the unique filter at tap-time. This handles
                    // the case where emissions happen before the tap is registered
                    // (e.g., tap-ok's :after-tap evaluation order).
                    if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                        let (emitted, _done, _quit) = supplier_snapshot(*supplier_id as u64);
                        let as_fn = attrs.get("unique_as").cloned();
                        let with_fn = attrs.get("unique_with").cloned();
                        let expires_secs = attrs.get("unique_expires").map(|v| v.to_f64());
                        let mut seen_keys: Vec<(Value, std::time::Instant)> = Vec::new();
                        let mut unique_items: Vec<Value> = Vec::new();
                        let now = std::time::Instant::now();
                        for item in emitted {
                            // Expire old seen values if :expires is set
                            if let Some(expire_secs) = expires_secs {
                                seen_keys.retain(|(_, ts)| {
                                    now.duration_since(*ts).as_secs_f64() < expire_secs
                                });
                            }
                            let key = if let Some(ref func) = as_fn {
                                self.call_sub_value(func.clone(), vec![item.clone()], true)?
                            } else {
                                item.clone()
                            };
                            let mut duplicate = false;
                            for (seen, _) in &seen_keys {
                                let is_same = if let Some(ref func) = with_fn {
                                    self.call_sub_value(
                                        func.clone(),
                                        vec![seen.clone(), key.clone()],
                                        true,
                                    )?
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
                                seen_keys.push((key.clone(), now));
                                unique_items.push(item);
                                // Also mark as seen in the global unique filter state
                                // so future real-time emissions know about these
                                let sid = *supplier_id as u64;
                                let tap_count = supplier_tap_count(sid);
                                if tap_count > 0 {
                                    supplier_unique_mark_seen(sid, tap_count - 1, key);
                                }
                            }
                        }
                        unique_items
                    } else {
                        Vec::new()
                    }
                } else {
                    if Self::supply_has_active_callback(&tap_cb) {
                        if let Some(Value::Array(items, ..)) = attrs.get_mut("taps") {
                            Arc::make_mut(items).push(tap_cb.clone());
                        } else {
                            attrs.insert("taps".to_string(), Value::array(vec![tap_cb.clone()]));
                        }
                    }
                    if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                        let is_live = matches!(attrs.get("live"), Some(Value::Bool(true)));
                        if is_live {
                            Vec::new()
                        } else {
                            let (snap_values, _, _) = supplier_snapshot(*supplier_id as u64);
                            if !snap_values.is_empty() {
                                snap_values
                            } else if let Some(Value::Array(values, ..)) = attrs.get("values") {
                                values.to_vec()
                            } else {
                                Vec::new()
                            }
                        }
                    } else if let Some(Value::Array(values, ..)) = attrs.get("values") {
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
                            self.call_sub_value(cb.clone(), vec![v.clone()], true)?;
                        }
                    }
                    if Self::supply_has_active_callback(&tap_cb) {
                        self.call_sub_value(tap_cb.clone(), vec![v.clone()], true)?;
                    }
                }

                if let Some(quit_reason) = on_demand_quit {
                    if let Some(quit_fn) = quit_cb {
                        self.call_supply_quit_handler(quit_fn, quit_reason)?;
                    } else {
                        return Err(Self::runtime_error_from_supply_reason(quit_reason));
                    }
                    let tap_instance = Value::make_instance(Symbol::intern("Tap"), HashMap::new());
                    return Ok((tap_instance, attrs));
                }

                // Call done callback after all values emitted
                if let Some(done_fn) = done_cb {
                    if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                        // Check both the attribute and the global supplier state
                        let supplier_is_done = attrs
                            .get("supplier_done")
                            .map(Value::truthy)
                            .unwrap_or(false)
                            || {
                                let (_, done, _) = supplier_snapshot(*supplier_id as u64);
                                done
                            };
                        if supplier_is_done {
                            let _ = self.call_sub_value(done_fn, vec![], true);
                        } else {
                            register_supplier_done_callback(*supplier_id as u64, done_fn);
                        }
                    } else {
                        let _ = self.call_sub_value(done_fn, vec![], true);
                    }
                }
                if let Some(quit_fn) = quit_cb {
                    if let Some(Value::Int(supplier_id)) = attrs.get("supplier_id") {
                        let (_, _, quit_reason) = supplier_snapshot(*supplier_id as u64);
                        if let Some(reason) = quit_reason {
                            self.call_supply_quit_handler(quit_fn, reason)?;
                        } else {
                            register_supplier_quit_callback(*supplier_id as u64, quit_fn);
                        }
                    } else if let Some(reason) = attrs.get("quit_reason").cloned() {
                        self.call_supply_quit_handler(quit_fn, reason)?;
                    }
                }
                let tap_instance = Value::make_instance(Symbol::intern("Tap"), tap_handle_attrs);
                Ok((tap_instance, attrs))
            }
            "on-close" => {
                let close_cb = args.first().cloned().unwrap_or(Value::Nil);
                let mut new_attrs = attrs.clone();
                let mut callbacks =
                    if let Some(Value::Array(existing, ..)) = attrs.get("on_close_callbacks") {
                        existing.to_vec()
                    } else {
                        Vec::new()
                    };
                callbacks.push(close_cb);
                new_attrs.insert("on_close_callbacks".to_string(), Value::array(callbacks));
                Ok((
                    Value::make_instance(Symbol::intern("Supply"), new_attrs),
                    attrs,
                ))
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
            "unique" => {
                let result = self.supply_unique(&attrs, &args)?;
                Ok((result, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Supply",
                method
            ))),
        }
    }

    /// Supply.unique implementation — filters duplicate values.
    /// Supports `:as`, `:with`, and `:expires` named parameters.
    fn supply_unique(
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
    fn supply_classify(
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
    fn apply_classify_mapper(
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
                            let _ = self.call_sub_value(done_cb, Vec::new(), true);
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
                        let _ = self.call_sub_value(done_cb, Vec::new(), true);
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

    /// Extract source values from a Supply's attributes.
    fn supply_get_values(
        &mut self,
        attributes: &HashMap<String, Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
            let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                let mut a = HashMap::new();
                a.insert("emitted".to_string(), Value::array(Vec::new()));
                a.insert("done".to_string(), Value::Bool(false));
                a
            });
            self.supply_emit_buffer.push(Vec::new());
            let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
            Ok(self.supply_emit_buffer.pop().unwrap_or_default())
        } else {
            Ok(match attributes.get("values") {
                Some(Value::Array(items, ..)) => items.to_vec(),
                _ => Vec::new(),
            })
        }
    }

    /// Create a new non-live Supply from a list of values.
    fn make_supply_from_values(
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
            self.native_supply(attributes, method, args.to_vec())
        } else {
            Err(RuntimeError::new(format!(
                "Cannot call .{} on non-Supply",
                method
            )))
        }
    }

    /// Check if a key is already in the unique filter's seen list.
    /// Used by SupplierEmitAction::UniqueCheck when :as/:with are specified.
    fn supplier_unique_check_seen(
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
        std::thread::spawn(move || {
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
                let _ = thread_interp.call_sub_value(done_cb, Vec::new(), true);
            }
        });
    }
}
