//! Split out of native_supply_methods.rs. See that file for the shared
//! helpers and the `QuitOutcome` enum.
use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
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
                let mut tap_handle_attrs =
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
                // The on-demand emitter (created below) owns any CLOSE-phaser
                // callbacks; remember its id on the Tap so `.close` fires them.
                let mut close_supplier_id: Option<u64> = None;

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
                    crate::runtime::builtins_system::spawn_user_thread(move || {
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
                let mut done_group_marker: Option<Value> = None;
                let values = if let Some(on_demand_cb) = attrs.get("on_demand_callback").cloned() {
                    // Give the emitter a supplier_id so that when a `whenever`
                    // body calls `$emitter.emit(val)`, the value can be dispatched
                    // to taps registered on this emitter. The outer tap_cb will
                    // be registered lazily below only when `whenever` subscriptions
                    // are found, to avoid double-delivery for plain `emit` calls.
                    let emitter_supplier_id = next_supplier_id();
                    close_supplier_id = Some(emitter_supplier_id);
                    let (callback_result, emitted, body_ran_done) =
                        self.run_on_demand_body(on_demand_cb, Some(emitter_supplier_id));
                    if let Err(err) = callback_result {
                        on_demand_quit = Some(
                            err.exception
                                .as_deref()
                                .cloned()
                                .unwrap_or_else(|| Value::str(err.message)),
                        );
                    }

                    // Separate whenever subscription registrations from plain
                    // emitted values (same logic as immutable tap path).

                    // Pre-count supplier-backed whenever subscriptions
                    let whenever_supplier_count = emitted
                        .iter()
                        .filter(|item| {
                            if let Value::Array(arr, ..) = item
                                && arr.len() == 4
                                && let Value::Instance {
                                    class_name,
                                    attributes,
                                    ..
                                } = &arr[0]
                                && *class_name == "Supply"
                                && attributes.contains_key("supplier_id")
                            {
                                true
                            } else {
                                false
                            }
                        })
                        .count();

                    if whenever_supplier_count > 0 {
                        done_group_marker = done_cb.as_ref().map(|df| {
                            let group_id =
                                create_whenever_done_group(whenever_supplier_count, df.clone());
                            Self::make_whenever_done_group_marker(group_id)
                        });
                    }

                    let mut plain_values = Vec::new();
                    let mut outer_tap_registered = false;
                    // on-close callbacks from each whenever source supply,
                    // fired when the supply completes via `done`.
                    let mut whenever_on_close: Vec<Value> = Vec::new();
                    // true when the block body itself ran `done`.
                    let body_done = body_ran_done;
                    for item in emitted {
                        if let Value::Array(ref arr, ..) = item
                            && arr.len() == 4
                            && matches!(&arr[0], Value::Instance { class_name, .. } if class_name == "Supply")
                        {
                            let inner_supply = &arr[0];
                            let body_cb = arr[1].clone();

                            if let Value::Instance {
                                attributes: inner_attrs,
                                ..
                            } = inner_supply
                                && let Some(Value::Int(sid)) =
                                    inner_attrs.as_map().get("supplier_id")
                            {
                                let supplier_id = *sid as u64;
                                register_supplier_tap(supplier_id, body_cb, 0.0);
                                // Register the outer tap callback on the emitter
                                // so that `$emitter.emit(val)` inside the body
                                // forwards values to the outer tap subscriber.
                                // Only register once even with multiple whenevers.
                                if !outer_tap_registered
                                    && Self::supply_has_active_callback(&tap_cb)
                                {
                                    register_supplier_tap(
                                        emitter_supplier_id,
                                        tap_cb.clone(),
                                        delay_seconds,
                                    );
                                    outer_tap_registered = true;
                                }
                                if let Some(ref qf) = quit_cb {
                                    register_supplier_quit_callback(supplier_id, qf.clone());
                                }
                                // Register the whenever's own LAST phaser
                                // callbacks (arr[2]) as done callbacks on the
                                // inner supplier, BEFORE the group marker, so
                                // that when the inner supplier signals done the
                                // LAST blocks run first. Their `emit` routes to
                                // the supply block's emitter (and out to the
                                // outer tap) just like the main body's emit.
                                if let Value::Array(last_arr, ..) = &arr[2] {
                                    for last_cb in last_arr.iter() {
                                        register_supplier_done_callback(
                                            supplier_id,
                                            last_cb.clone(),
                                        );
                                    }
                                }
                                // Register the whenever's own QUIT phaser
                                // callbacks (arr[3]) as whenever-quit handlers.
                                // On quit they run before the downstream quit
                                // handler; if one handles the exception the
                                // supply completes with done instead of quit.
                                if let Value::Array(quit_arr, ..) = &arr[3] {
                                    for qcb in quit_arr.iter() {
                                        register_supplier_whenever_quit_callback(
                                            supplier_id,
                                            qcb.clone(),
                                        );
                                    }
                                }
                                // Register done callback: use the group marker
                                // so done only fires when ALL whenevers complete.
                                if let Some(ref marker) = done_group_marker {
                                    register_supplier_done_callback(supplier_id, marker.clone());
                                }
                                // Fire CLOSE phasers when this source signals
                                // done (normal termination), in addition to an
                                // explicit tap close.
                                if let Some(cid) = close_supplier_id {
                                    register_supplier_done_callback(
                                        supplier_id,
                                        Self::make_supply_close_marker(cid),
                                    );
                                }
                                // Collect this source's on-close callbacks so a
                                // `done`-driven supply completion can fire them.
                                if let Some(Value::Array(cbs, ..)) =
                                    inner_attrs.as_map().get("on_close_callbacks")
                                {
                                    whenever_on_close.extend(cbs.iter().cloned());
                                }
                            } else {
                                let inner_vals =
                                    if let Value::Instance { attributes: a, .. } = inner_supply {
                                        self.supply_list_values(&a.as_map(), true)?
                                    } else {
                                        self.supply_list_values(&HashMap::new(), true)?
                                    };
                                for v in inner_vals {
                                    match self.call_sub_value(body_cb.clone(), vec![v], true) {
                                        Ok(_) => {}
                                        Err(err) => {
                                            let reason = err
                                                .exception
                                                .as_deref()
                                                .cloned()
                                                .unwrap_or_else(|| Value::str(err.message));
                                            if let Some(ref qf) = quit_cb {
                                                self.call_supply_quit_handler(qf.clone(), reason)?;
                                            } else {
                                                return Err(
                                                    Self::runtime_error_from_supply_reason(reason),
                                                );
                                            }
                                            return Ok((
                                                Value::make_instance(
                                                    Symbol::intern("Tap"),
                                                    HashMap::new(),
                                                ),
                                                attrs,
                                            ));
                                        }
                                    }
                                }
                            }
                        } else {
                            plain_values.push(item);
                        }
                    }
                    // A `done` that terminates the whole supply (an explicit
                    // `done` in the block body, or a `done` inside a whenever
                    // body) fires every whenever source's on-close plus the
                    // downstream done. If the body itself ran `done`, complete
                    // now; otherwise register the marker so a later `done`
                    // inside a whenever body (via the emitter's done) fires it.
                    // Only whenever-backed supplies need this completion path;
                    // a cold supply's done callback is handled below as usual.
                    if whenever_supplier_count > 0 {
                        let complete_marker = Self::make_on_demand_complete_marker(
                            done_cb.clone(),
                            std::mem::take(&mut whenever_on_close),
                        );
                        if body_done {
                            self.invoke_done_callback(complete_marker)?;
                        } else {
                            register_supplier_done_callback(emitter_supplier_id, complete_marker);
                        }
                    }
                    plain_values
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
                let throttle_limit = attrs.get("throttle_limit").and_then(|v| {
                    if let Value::Int(n) = v {
                        Some(*n as usize)
                    } else {
                        None
                    }
                });
                for (idx, v) in values.iter().enumerate() {
                    if let Some(limit) = throttle_limit {
                        if limit > 0 && idx % limit == 0 {
                            Self::sleep_for_supply_delay(delay_seconds);
                        }
                    } else {
                        Self::sleep_for_supply_delay(delay_seconds);
                    }
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
                    } else if done_group_marker.is_none() {
                        // Only call done immediately when there are no
                        // supplier-backed whenever subscriptions tracking
                        // done via the group mechanism. Cold (on-demand)
                        // whenevers complete synchronously, so done can
                        // fire immediately in that case.
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
                if let Some(cid) = close_supplier_id {
                    tap_handle_attrs
                        .insert("close_supplier_id".to_string(), Value::Int(cid as i64));
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
}
