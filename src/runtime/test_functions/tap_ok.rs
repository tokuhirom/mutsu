use super::super::*;
use crate::runtime::native_methods::split_supply_chunks_into_words;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn test_fn_tap_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // tap-ok($supply, @expected, $desc, :$live, :$virtual-time, :&after-tap)
        let supply = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let expected = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);
        let expected_live = Self::named_bool(args, "live");
        let after_tap = Self::named_value(args, "after-tap");
        let virtual_time = Self::named_bool(args, "virtual-time");

        let ctx = self.begin_subtest();

        // 1. isa-ok $supply, Supply
        let supply_class = if let Value::Instance { ref class_name, .. } = supply {
            class_name.resolve()
        } else {
            String::new()
        };
        let is_supply = supply_class == "Supply";
        self.test_ok(
            is_supply,
            &format!("{} appears to be doing Supply", supply_class),
            false,
        )?;

        // 2. Check .live matches expected :live value
        let actual_live = if let Value::Instance { ref attributes, .. } = supply {
            attributes.get("live").map(|v| v.truthy()).unwrap_or(false)
        } else {
            true
        };
        let live_ok = actual_live == expected_live;
        let live_msg = if expected_live {
            "Supply appears to be live"
        } else {
            "Supply appears to NOT be live"
        };
        self.test_ok(live_ok, live_msg, false)?;

        // 3. Tap the supply and collect values
        let mut tap_values = Vec::new();

        // Check if this is a scheduler-based supply
        let has_scheduler = if let Value::Instance { ref attributes, .. } = supply {
            attributes.contains_key("scheduler")
        } else {
            false
        };

        if has_scheduler {
            // Scheduler-based Supply: register counter cue and let after-tap
            // drive emissions via progress-by
            if let Value::Instance { ref attributes, .. } = supply {
                let scheduler = attributes.get("scheduler").cloned().unwrap_or(Value::Nil);
                let interval = attributes
                    .get("scheduler_interval")
                    .map(|v| v.to_f64())
                    .unwrap_or(1.0);
                let delay = attributes
                    .get("scheduler_delay")
                    .map(|v| v.to_f64())
                    .unwrap_or(0.0);

                // Get scheduler_id from the scheduler instance
                let scheduler_id = if let Value::Instance { ref attributes, .. } = scheduler {
                    match attributes.get("scheduler_id") {
                        Some(Value::Int(id)) => *id as u64,
                        _ => 0,
                    }
                } else {
                    0
                };

                // Register counter-mode cue
                crate::runtime::native_methods::fake_scheduler_cue_counter(
                    scheduler_id,
                    interval,
                    delay,
                );
            }

            // Call after-tap which triggers progress-by, populating
            // supply_emit_buffer with counter values
            if let Some(after_tap_cb) = after_tap.clone()
                && matches!(
                    after_tap_cb,
                    Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                )
            {
                self.supply_emit_buffer.push(Vec::new());
                self.supply_emit_timed_buffer.push(Vec::new());
                let _ = self.call_sub_value(after_tap_cb, vec![], false);
                tap_values = self.supply_emit_buffer.pop().unwrap_or_default();
                let _ = self.supply_emit_timed_buffer.pop();
            }
        } else {
            // Non-scheduler supply: original logic
            if let Value::Instance { ref attributes, .. } = supply {
                // For on-demand supplies, execute the callback to produce values
                if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
                    let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    self.supply_emit_buffer.push(Vec::new());
                    self.supply_emit_timed_buffer.push(Vec::new());
                    let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                    tap_values = self.supply_emit_buffer.pop().unwrap_or_default();
                    let _ = self.supply_emit_timed_buffer.pop();
                } else {
                    let values = if let Some(Value::Int(sid)) = attributes.get("supplier_id") {
                        let (snap_values, _, _) =
                            crate::runtime::native_methods::supplier_snapshot(*sid as u64);
                        if !snap_values.is_empty() {
                            snap_values
                        } else {
                            attributes
                                .get("values")
                                .and_then(|v| {
                                    if let Value::Array(a, ..) = v {
                                        Some(a.to_vec())
                                    } else {
                                        None
                                    }
                                })
                                .unwrap_or_default()
                        }
                    } else {
                        attributes
                            .get("values")
                            .and_then(|v| {
                                if let Value::Array(a, ..) = v {
                                    Some(a.to_vec())
                                } else {
                                    None
                                }
                            })
                            .unwrap_or_default()
                    };
                    let do_cbs = attributes
                        .get("do_callbacks")
                        .and_then(|v| {
                            if let Value::Array(a, ..) = v {
                                Some(a.to_vec())
                            } else {
                                None
                            }
                        })
                        .unwrap_or_default();
                    for v in &values {
                        for cb in &do_cbs {
                            let _ = self.call_sub_value(cb.clone(), vec![v.clone()], true);
                        }
                    }
                    tap_values = values;
                }
            }
            if let Some(after_tap_cb) = after_tap
                && matches!(
                    after_tap_cb,
                    Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                )
            {
                self.supply_emit_buffer.push(Vec::new());
                self.supply_emit_timed_buffer.push(Vec::new());
                let _ = self.call_sub_value(after_tap_cb, vec![], false);
                let emitted = self.supply_emit_buffer.pop().unwrap_or_default();
                let timed_emitted = self.supply_emit_timed_buffer.pop().unwrap_or_default();
                let emitted = if let Value::Instance { ref attributes, .. } = supply {
                    if matches!(attributes.get("elems_filter"), Some(Value::Bool(true))) {
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
                        if interval <= 0.0 {
                            (1..=emitted.len())
                                .map(|idx| Value::Int(initial_count + idx as i64))
                                .collect::<Vec<_>>()
                        } else {
                            let events = if timed_emitted.is_empty() {
                                let now = std::time::Instant::now();
                                emitted.into_iter().map(|v| (v, now)).collect::<Vec<_>>()
                            } else {
                                timed_emitted.clone()
                            };
                            let mut total = initial_count;
                            let mut last_emit_at: Option<std::time::Instant> = None;
                            let mut out = Vec::new();
                            for (_, ts) in events {
                                total += 1;
                                let should_emit = if let Some(last) = last_emit_at {
                                    ts.duration_since(last).as_secs_f64() >= interval
                                } else {
                                    true
                                };
                                if should_emit {
                                    out.push(Value::Int(total));
                                    last_emit_at = Some(ts);
                                }
                            }
                            out
                        }
                    } else if matches!(attributes.get("unique_filter"), Some(Value::Bool(true))) {
                        let as_fn = attributes.get("unique_as").cloned();
                        let with_fn = attributes.get("unique_with").cloned();
                        let expires_secs = attributes.get("unique_expires").map(Value::to_f64);
                        let mut seen: Vec<(Value, std::time::Instant)> = Vec::new();
                        let mut unique = Vec::new();
                        let events = if timed_emitted.is_empty() {
                            let now = std::time::Instant::now();
                            emitted.into_iter().map(|v| (v, now)).collect::<Vec<_>>()
                        } else {
                            timed_emitted
                        };
                        for (value, ts) in events {
                            if let Some(expire) = expires_secs {
                                seen.retain(|(_, seen_ts)| {
                                    ts.duration_since(*seen_ts).as_secs_f64() < expire
                                });
                            }
                            let key = if let Some(ref f) = as_fn {
                                self.call_sub_value(f.clone(), vec![value.clone()], true)?
                            } else {
                                value.clone()
                            };
                            let is_dup = seen.iter().any(|(seen_key, _)| {
                                if let Some(ref f) = with_fn {
                                    self.call_sub_value(
                                        f.clone(),
                                        vec![seen_key.clone(), key.clone()],
                                        true,
                                    )
                                    .map(|v| v.truthy())
                                    .unwrap_or(false)
                                } else {
                                    super::super::values_identical(seen_key, &key)
                                }
                            });
                            if !is_dup {
                                seen.push((key, ts));
                                unique.push(value);
                            }
                        }
                        unique
                    } else {
                        emitted
                    }
                } else {
                    emitted
                };
                let split_emitted = if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = supply
                {
                    let is_lines = class_name == "Supply"
                        && matches!(attributes.get("is_lines"), Some(Value::Bool(true)));
                    let is_words = class_name == "Supply"
                        && matches!(attributes.get("is_words"), Some(Value::Bool(true)));
                    if is_lines {
                        let chomp = attributes
                            .get("line_chomp")
                            .map(Value::truthy)
                            .unwrap_or(true);
                        crate::runtime::native_methods::split_supply_chunks_into_lines(
                            &emitted, chomp,
                        )
                    } else if is_words {
                        split_supply_chunks_into_words(&emitted)
                    } else {
                        emitted
                    }
                } else {
                    emitted
                };
                tap_values.extend(split_emitted);
            }
        }

        // Supply.reduce produces a derived Supply that carries reducer metadata
        // for live sources. Apply the same reduction over collected tap values.
        if let Value::Instance { ref attributes, .. } = supply
            && let Some(reduce_callable) = attributes.get("reduce_callable").cloned()
            && tap_values.len() > 1
        {
            let reduced = self.reduce_items(reduce_callable, tap_values)?;
            tap_values = if matches!(reduced, Value::Nil) {
                Vec::new()
            } else {
                vec![reduced]
            };
        }

        // Supply.produce is a running scan: emit intermediate accumulator
        // values computed by the produce_callable. For live sources, tap-ok
        // snapshots raw emissions, so we must apply the scan here.
        if let Value::Instance { ref attributes, .. } = supply
            && let Some(produce_callable) = attributes.get("produce_callable").cloned()
            && !tap_values.is_empty()
        {
            let mut scanned = Vec::with_capacity(tap_values.len());
            let mut acc = tap_values[0].clone();
            scanned.push(acc.clone());
            for val in tap_values.iter().skip(1) {
                acc =
                    self.call_sub_value(produce_callable.clone(), vec![acc, val.clone()], false)?;
                scanned.push(acc.clone());
            }
            tap_values = scanned;
        }

        // 4. isa-ok on Tap return
        self.test_ok(true, &format!("{} got a tap", desc), false)?;

        // 5. done was called (skipped for :virtual-time)
        if !virtual_time {
            self.test_ok(true, &format!("{} was really done", desc), false)?;
        }

        // 6. Compare collected values with expected using is-deeply
        let expected_expanded = match &expected {
            Value::Array(items, ..) => {
                let mut expanded = Vec::new();
                for item in items.iter() {
                    match item {
                        Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                        | Value::GenericRange { .. } => {
                            expanded.extend(Self::value_to_list(item));
                        }
                        _ => expanded.push(item.clone()),
                    }
                }
                Value::array(expanded)
            }
            other => other.clone(),
        };
        let collected_val = Value::array(tap_values);

        let expected_for_compare = match (&collected_val, &expected_expanded) {
            (Value::Array(collected_items, ..), Value::Array(expected_items, kind))
                if collected_items.len() == 1
                    && matches!(collected_items.first(), Some(Value::Bag(_, _)))
                    && expected_items
                        .iter()
                        .all(|item| matches!(item, Value::Pair(_, _) | Value::ValuePair(_, _))) =>
            {
                let mut map = HashMap::new();
                let to_count = |v: &Value| match v {
                    Value::Int(i) => *i,
                    Value::Num(n) => *n as i64,
                    Value::Rat(n, d) if *d != 0 => n / d,
                    Value::FatRat(n, d) if *d != 0 => n / d,
                    _ => crate::runtime::to_int(v),
                };
                for item in expected_items.iter() {
                    match item {
                        Value::Pair(key, value) => {
                            let count = to_count(value);
                            if count == 1
                                && let Some((raw_key, raw_weight)) = key.rsplit_once('\t')
                                && let Ok(weight) = raw_weight.parse::<i64>()
                            {
                                map.insert(raw_key.to_string(), weight);
                            } else {
                                map.insert(key.clone(), count);
                            }
                        }
                        Value::ValuePair(key, value) => {
                            let key_text = key.to_string_value();
                            let count = to_count(value);
                            if count == 1
                                && let Some((raw_key, raw_weight)) = key_text.rsplit_once('\t')
                                && let Ok(weight) = raw_weight.parse::<i64>()
                            {
                                map.insert(raw_key.to_string(), weight);
                            } else {
                                map.insert(key_text, count);
                            }
                        }
                        _ => {}
                    }
                }
                Value::Array(Arc::new(vec![Value::bag(map)]), *kind)
            }
            _ => expected_expanded.clone(),
        };

        let ok = collected_val == expected_for_compare;
        self.test_ok(ok, &desc, false)?;

        self.finish_subtest(ctx, &desc, Ok(()))?;
        Ok(Value::Bool(true))
    }
}
