//! Split out of native_supply_methods.rs. See that file for the shared
//! helpers and the `QuitOutcome` enum.
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
                // Delegate to the canonical (mut) tap/act implementation so
                // there is a single source of truth. The &self path does not
                // write the receiver attributes back, so the returned map is
                // discarded (matching the prior immutable-tap behavior).
                let (tap, _) = self.native_supply_mut(attributes.clone(), method, args)?;
                Ok(tap)
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
                } else if attributes.contains_key("on_demand_callback") {
                    // For on-demand supplies (created by `supply { ... }` blocks),
                    // run the supply body through the react event loop to handle
                    // async `whenever` subscriptions (e.g. Supply.interval).
                    // Collect emitted values and keep the promise with the last one.
                    self.supply_promise_on_demand(attributes, &promise)?;
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
                        && let Some(Value::Array(items, ..)) = attributes.as_map().get("values")
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
            "classify" => self.supply_classify(attributes, &args, false),
            "categorize" => self.supply_classify(attributes, &args, true),
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
                    Value::Array(items, ..) => items.to_vec(),
                    Value::Seq(items) => items.to_vec(),
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
            "throttle" => self.supply_throttle(attributes, args),
            "rotor" => {
                let source_values = self.supply_get_values(attributes)?;
                let rotored = self.dispatch_rotor(Value::array(source_values), &args)?;
                let items = match rotored {
                    Value::Array(items, ..) => items.to_vec(),
                    Value::Seq(items) => items.to_vec(),
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
                    Value::Array(items, ..) => items.iter().cloned().collect(),
                    Value::Seq(items) => items.iter().cloned().collect(),
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
                let is_latest = method == "zip-latest";

                // Extract :with and :initial named args
                let mut with_fn: Option<Value> = None;
                let mut initial: Option<Vec<Value>> = None;
                let mut other_supplies: Vec<Value> = Vec::new();
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        if key == "with" {
                            with_fn = Some(*value.clone());
                        } else if key == "initial" {
                            if let Value::Array(items, ..) = &**value {
                                initial = Some(items.to_vec());
                            }
                        } else {
                            other_supplies.push(arg.clone());
                        }
                    } else {
                        other_supplies.push(arg.clone());
                    }
                }

                // Check if any input supplies are live (supplier-backed or channel-based)
                let self_is_live = supplier_id_from_attrs(attributes).is_some()
                    || attributes.contains_key("supply_id");
                let any_live = self_is_live
                    || other_supplies.iter().any(|s| {
                        if let Value::Instance { attributes, .. } = s {
                            supplier_id_from_attrs(&(attributes).as_map()).is_some()
                                || attributes.contains_key("supply_id")
                        } else {
                            false
                        }
                    });

                if any_live {
                    let output_supplier_id = next_supplier_id();
                    let source_count = 1 + other_supplies.len();

                    // Check if any source is channel-based (has supply_id)
                    let has_channel = attributes.contains_key("supply_id")
                        || other_supplies.iter().any(|s| {
                            matches!(s, Value::Instance { attributes: a, .. } if a.contains_key("supply_id"))
                        });

                    if is_latest && has_channel {
                        // Channel-based zip-latest: spawn coordination thread
                        use crate::runtime::native_methods::{
                            SupplyEvent, next_supply_id, supply_channel_map_pub,
                            take_supply_channel,
                        };
                        use std::sync::mpsc;

                        let zip_supply_id = next_supply_id();
                        let (zip_tx, zip_rx) = mpsc::channel();

                        enum SourceKind {
                            Channel(mpsc::Receiver<SupplyEvent>),
                            Static(Vec<Value>),
                        }

                        // Collect all sources (self + others). Own each map so we
                        // don't hold attribute read guards across the loop below.
                        let mut all_supplies: Vec<HashMap<String, Value>> =
                            vec![attributes.clone()];
                        for supply in &other_supplies {
                            if let Value::Instance { attributes: a, .. } = supply {
                                all_supplies.push(a.to_map());
                            }
                        }

                        let mut sources: Vec<SourceKind> = Vec::with_capacity(all_supplies.len());
                        for a in &all_supplies {
                            if let Some(Value::Int(sid)) = a.get("supply_id")
                                && let Some(rx) = take_supply_channel(*sid as u64)
                            {
                                sources.push(SourceKind::Channel(rx));
                                continue;
                            }
                            let items = match a.get("values") {
                                Some(Value::Array(items, ..)) => items.to_vec(),
                                _ => Vec::new(),
                            };
                            sources.push(SourceKind::Static(items));
                        }

                        std::thread::spawn(move || {
                            let n = sources.len();
                            let mut latest: Vec<Option<Value>> = vec![None; n];
                            let mut done: Vec<bool> = vec![false; n];
                            let timeout_dur = std::time::Duration::from_millis(10);

                            // Pre-fill from static sources
                            for (i, source) in sources.iter().enumerate() {
                                if let SourceKind::Static(vals) = source {
                                    if let Some(last) = vals.last() {
                                        latest[i] = Some(last.clone());
                                    }
                                    done[i] = true;
                                }
                            }

                            loop {
                                // Poll channel sources
                                for (i, source) in sources.iter().enumerate() {
                                    if done[i] {
                                        continue;
                                    }
                                    if let SourceKind::Channel(rx) = source {
                                        match rx.recv_timeout(timeout_dur) {
                                            Ok(SupplyEvent::Emit(v)) => {
                                                latest[i] = Some(v);
                                                // Emit if all sources have a value
                                                if latest.iter().all(|v| v.is_some()) {
                                                    let tuple: Vec<Value> = latest
                                                        .iter()
                                                        .map(|v| v.clone().unwrap())
                                                        .collect();
                                                    if zip_tx
                                                        .send(SupplyEvent::Emit(Value::array(
                                                            tuple,
                                                        )))
                                                        .is_err()
                                                    {
                                                        return;
                                                    }
                                                }
                                            }
                                            Ok(SupplyEvent::Done) => {
                                                done[i] = true;
                                            }
                                            Ok(SupplyEvent::Quit(_)) => {
                                                done[i] = true;
                                            }
                                            Err(mpsc::RecvTimeoutError::Timeout) => {}
                                            Err(mpsc::RecvTimeoutError::Disconnected) => {
                                                done[i] = true;
                                            }
                                        }
                                    }
                                }

                                // If all done, finish
                                if done.iter().all(|d| *d) {
                                    let _ = zip_tx.send(SupplyEvent::Done);
                                    return;
                                }
                            }
                        });

                        if let Ok(mut map) = supply_channel_map_pub().lock() {
                            map.insert(zip_supply_id, zip_rx);
                        }

                        let mut new_attrs = HashMap::new();
                        new_attrs.insert("values".to_string(), Value::array(Vec::new()));
                        new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                        new_attrs.insert("supply_id".to_string(), Value::Int(zip_supply_id as i64));
                        new_attrs.insert("live".to_string(), Value::Bool(false));
                        return Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs));
                    } else if is_latest {
                        let zip_latest_state_id = register_zip_latest_state(
                            source_count,
                            output_supplier_id,
                            with_fn,
                            initial,
                        );
                        if let Some(sid) = supplier_id_from_attrs(attributes) {
                            register_supplier_zip_latest_tap(sid, zip_latest_state_id, 0);
                        }
                        for (i, supply) in other_supplies.iter().enumerate() {
                            if let Value::Instance {
                                attributes: other_attrs,
                                ..
                            } = supply
                            {
                                let source_index = i + 1;
                                if let Some(sid) = supplier_id_from_attrs(&(other_attrs).as_map()) {
                                    register_supplier_zip_latest_tap(
                                        sid,
                                        zip_latest_state_id,
                                        source_index,
                                    );
                                }
                            }
                        }
                    } else {
                        let zip_state_id =
                            register_zip_state(source_count, output_supplier_id, with_fn);
                        if let Some(sid) = supplier_id_from_attrs(attributes) {
                            register_supplier_zip_tap(sid, zip_state_id, 0);
                        }
                        for (i, supply) in other_supplies.iter().enumerate() {
                            if let Value::Instance {
                                attributes: other_attrs,
                                ..
                            } = supply
                            {
                                let source_index = i + 1;
                                if let Some(sid) = supplier_id_from_attrs(&(other_attrs).as_map()) {
                                    register_supplier_zip_tap(sid, zip_state_id, source_index);
                                }
                            }
                        }
                    }

                    let mut new_attrs = HashMap::new();
                    new_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    new_attrs.insert(
                        "supplier_id".to_string(),
                        Value::Int(output_supplier_id as i64),
                    );
                    new_attrs.insert("live".to_string(), Value::Bool(false));
                    Ok(Value::make_instance(Symbol::intern("Supply"), new_attrs))
                } else {
                    // Non-live path
                    let source_values = self.supply_get_values(attributes)?;
                    let mut other_values: Vec<Vec<Value>> = Vec::new();
                    for arg in &other_supplies {
                        if let Value::Instance {
                            class_name,
                            attributes,
                            ..
                        } = arg
                            && class_name == "Supply"
                        {
                            other_values.push(self.supply_get_values(&(attributes).as_map())?);
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
                        if let Some(ref wf) = with_fn {
                            let combined = self.call_sub_value(wf.clone(), tuple, false)?;
                            zipped.push(combined);
                        } else {
                            zipped.push(Value::array(tuple));
                        }
                    }
                    Ok(self.make_supply_from_values(zipped, attributes))
                }
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
                    self.output_sink_mut().stderr_output.push_str(&stderr);
                    self.sync_shared_vars_to_env();
                    // Drain shared thread output buffers so output produced by
                    // any background `start { }` thread that emitted into this
                    // supplier reaches the main TAP stream in chronological
                    // order.
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
}
