//! Split out of native_supply_methods.rs. See that file for the shared
//! helpers and the `QuitOutcome` enum.
use super::native_methods::*;
use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;

impl Interpreter {
    pub(super) fn native_supply_mut(
        &mut self,
        mut attrs: AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        match method {
            "emit" => {
                let value = args.first().cloned().unwrap_or(Value::NIL);
                let pushed = attrs
                    .get_mut("values")
                    .and_then(|v| {
                        v.with_array_mut(|items, _| {
                            crate::gc::Gc::make_mut(items).push(value.clone());
                        })
                    })
                    .is_some();
                if !pushed {
                    attrs.insert("values".to_string(), Value::array(vec![value.clone()]));
                }
                if let Some(ValueView::Array(taps, ..)) = attrs.get("taps").map(Value::view) {
                    for tap in taps.iter().cloned().collect::<Vec<_>>() {
                        if Self::supply_has_active_callback(&tap) {
                            let _ = self.call_sub_value(tap, vec![value.clone()], true);
                        }
                    }
                }
                Ok((Value::NIL, attrs))
            }
            "tap" | "act" => {
                let tap_cb = Self::positional_value(&args, 0)
                    .cloned()
                    .unwrap_or(Value::NIL);
                let done_cb = Self::named_value(&args, "done");
                let quit_cb = Self::named_value(&args, "quit");
                let delay_seconds = Self::supply_delay_seconds(&attrs);

                if let Some(ValueView::Int(supplier_id)) = attrs.get("supplier_id").map(Value::view)
                {
                    let has_unique = matches!(
                        attrs.get("unique_filter").map(Value::view),
                        Some(ValueView::Bool(true))
                    );
                    let is_lines = matches!(
                        attrs.get("is_lines").map(Value::view),
                        Some(ValueView::Bool(true))
                    );
                    let is_words = matches!(
                        attrs.get("is_words").map(Value::view),
                        Some(ValueView::Bool(true))
                    );
                    let is_elems = matches!(
                        attrs.get("elems_filter").map(Value::view),
                        Some(ValueView::Bool(true))
                    );
                    if !Self::supply_has_active_callback(&tap_cb) {
                        // done/quit-only taps do not register a value callback
                    } else if is_lines {
                        let chomp = attrs.get("line_chomp").map(Value::truthy).unwrap_or(true);
                        register_supplier_lines_tap(
                            supplier_id as u64,
                            tap_cb.clone(),
                            chomp,
                            delay_seconds,
                        );
                    } else if is_words {
                        register_supplier_words_tap(
                            supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                        );
                    } else if has_unique {
                        let as_fn = attrs.get("unique_as").cloned();
                        let with_fn = attrs.get("unique_with").cloned();
                        let expires = attrs.get("unique_expires").map(|v| v.to_f64());
                        register_supplier_unique_tap(
                            supplier_id as u64,
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
                            .and_then(|v| match v.view() {
                                ValueView::Int(i) => Some(i),
                                _ => None,
                            })
                            .unwrap_or(0);
                        register_supplier_elems_tap(
                            supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            interval,
                            initial_count,
                        );
                    } else if let Some(ValueView::Int(limit)) =
                        attrs.get("head_limit").map(Value::view)
                    {
                        register_supplier_tap_with_head_limit(
                            supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            limit as usize,
                        );
                    } else if let Some(produce_callable) = attrs.get("produce_callable").cloned() {
                        register_supplier_produce_tap(
                            supplier_id as u64,
                            tap_cb.clone(),
                            delay_seconds,
                            produce_callable,
                        );
                    } else {
                        register_supplier_tap(supplier_id as u64, tap_cb.clone(), delay_seconds);
                    }
                }

                // For a `migrate` Supply, also subscribe the outer migrate tap
                // on the master supply-of-supplies so that emitted inner supplies
                // switch the forwarded source (the user tap above was registered
                // on the migrate output supplier `supplier_id`).
                if let (Some(ValueView::Int(master_sid)), Some(ValueView::Int(ds_sid))) = (
                    attrs.get("migrate_source").map(Value::view),
                    attrs.get("supplier_id").map(Value::view),
                ) {
                    register_supplier_migrate_tap(master_sid as u64, ds_sid as u64);
                }

                // Build a Tap handle referencing the registered subscription so
                // `.close` can stop it later.
                let mut tap_handle_attrs = if let Some(ValueView::Int(supplier_id)) =
                    attrs.get("supplier_id").map(Value::view)
                {
                    let sid = supplier_id as u64;
                    let mut h = HashMap::new();
                    h.insert("supplier_id".to_string(), Value::int(sid as i64));
                    if let Some(tid) = last_supplier_tap_id(sid) {
                        h.insert("tap_id".to_string(), Value::int(tid as i64));
                    }
                    h
                } else {
                    HashMap::new()
                };
                // The on-demand emitter (created below) owns any CLOSE-phaser
                // callbacks; remember its id on the Tap so `.close` fires them.
                let mut close_supplier_id: Option<u64> = None;

                // A live (Supplier-backed) supply carries `.on-close(...)` /
                // `closing =>` callbacks in its attributes. Register them on
                // the source supplier and point the Tap handle at it so
                // `Tap.close` (and a supplier `done`) fires them; previously
                // they were never wired on this path.
                if !attrs.contains_key("on_demand_callback")
                    && let Some(ValueView::Int(sid)) = attrs.get("supplier_id").map(Value::view)
                    && let Some(ValueView::Array(cbs, ..)) =
                        attrs.get("on_close_callbacks").map(Value::view)
                    && !cbs.is_empty()
                {
                    for cb in cbs.iter() {
                        register_supplier_close_callback(sid as u64, cb.clone());
                    }
                    close_supplier_id = Some(sid as u64);
                }

                // If this Supply has a supply_id (belongs to Proc::Async),
                // register tap in the global registry so .start can find it
                if let Some(ValueView::Int(sid)) = attrs.get("supply_id").map(Value::view) {
                    let sid = sid as u64;
                    if Self::supply_has_active_callback(&tap_cb) {
                        register_supply_tap(sid, tap_cb.clone());
                    }
                    // A `quit =>` handler on a Proc::Async output Supply fires only
                    // when the stream ends in a decode error; record it so the
                    // await-time replay can invoke it.
                    if let Some(ref qf) = quit_cb {
                        register_supply_quit_tap(sid, qf.clone());
                    }
                    // Remember the effective decode encoding (a per-tap
                    // `stdout(:enc(...))` overrides the constructor `:enc`), so the
                    // replay decodes the raw bytes correctly.
                    if let Some(enc) = attrs.get("enc").map(Value::to_string_value) {
                        set_supply_enc(sid, enc);
                    }
                }

                // A Proc::Async output supply (`proc_output` marker) is delivered
                // exactly once, by the await/result-time `replay_proc_taps` — the
                // tap was registered in the global registry above. Consuming the
                // live channel (or the collected output) here as well would
                // deliver the same output twice once the tap closure's captured
                // lexicals are shared cells (S17-procasync/basic.t test 37), and
                // taking the channel would starve `react whenever` / `bind-stdin`
                // consumers of the same stream.
                let is_proc_output = attrs.contains_key("proc_output");

                // For live/async supplies (e.g., signal), spawn a background thread
                // to consume events from the channel and call the callback.
                if !is_proc_output
                    && let Some(ValueView::Int(sid)) = attrs.get("supply_id").map(Value::view)
                    && let Some(rx) = take_supply_channel(sid as u64)
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
                if !is_proc_output
                    && let Some(ValueView::Int(sid)) = attrs.get("supply_id").map(Value::view)
                    && let Some(collected) = get_supply_collected_output(sid as u64)
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
                let has_unique = matches!(
                    attrs.get("unique_filter").map(Value::view),
                    Some(ValueView::Bool(true))
                );
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
                    // When tapped by a nested `whenever` during a running react,
                    // register a done-signal promise BEFORE running the body so an
                    // async `start { emit; done }` body's later `done` (on a worker
                    // thread) is observable on the main thread — `supplier_done`
                    // resolves this promise, and the resolution survives the
                    // `supplier_reset` that clears the raw done flag afterward.
                    let close_done_promise = if self.react_active > 0 {
                        let p = crate::value::SharedPromise::new();
                        supplier_register_promise(emitter_supplier_id, p.clone());
                        Some(p)
                    } else {
                        None
                    };
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
                            if let ValueView::Array(arr, ..) = item.view()
                                && arr.len() == 4
                                && let ValueView::Instance {
                                    class_name,
                                    attributes,
                                    ..
                                } = arr[0].view()
                                && class_name == "Supply"
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
                        if let ValueView::Array(arr, ..) = item.view()
                            && arr.len() == 4
                            && matches!(arr[0].view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
                        {
                            let inner_supply = &arr[0];
                            let body_cb = arr[1].clone();

                            if let ValueView::Instance {
                                attributes: inner_attrs,
                                ..
                            } = inner_supply.view()
                                && let Some(ValueView::Int(sid)) =
                                    inner_attrs.as_map().get("supplier_id").map(Value::view)
                            {
                                let supplier_id = sid as u64;
                                register_supplier_tap(supplier_id, body_cb, 0.0);
                                // Raku serializes all `whenever` handlers of one
                                // supply block ("only in one whenever block at a
                                // time"). Tag this source trigger with the block's
                                // emitter id so a cross-thread emit into it holds
                                // the block's serialize lock while the handler runs.
                                crate::runtime::native_methods::set_supplier_serialize_group(
                                    supplier_id,
                                    emitter_supplier_id,
                                );
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
                                if let ValueView::Array(last_arr, ..) = arr[2].view() {
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
                                if let ValueView::Array(quit_arr, ..) = arr[3].view() {
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
                                if let Some(ValueView::Array(cbs, ..)) = inner_attrs
                                    .as_map()
                                    .get("on_close_callbacks")
                                    .map(Value::view)
                                {
                                    whenever_on_close.extend(cbs.iter().cloned());
                                }
                            } else {
                                // Cold (supplier-less) whenever source: replay it
                                // synchronously, capturing the body's emissions so
                                // they reach the tap subscriber (and do_callbacks)
                                // via plain_values below, in source order. Before
                                // this the body's `$emitter.emit` had no registered
                                // tap and the values were silently dropped. When a
                                // preceding supplier-backed whenever already
                                // registered the outer tap on the emitter, the
                                // emissions were dispatched live during the replay,
                                // so the captured copies are discarded.
                                let last_cbs = Self::value_array_items(&arr[2]).unwrap_or_default();
                                let quit_cbs = Self::value_array_items(&arr[3]).unwrap_or_default();
                                let (mut captured, unhandled_quit) = self
                                    .replay_cold_whenever_capture(
                                        inner_supply,
                                        &body_cb,
                                        &last_cbs,
                                        &quit_cbs,
                                    );
                                if !outer_tap_registered {
                                    plain_values.append(&mut captured);
                                }
                                if let Some(reason) = unhandled_quit {
                                    if let Some(ref qf) = quit_cb {
                                        self.call_supply_quit_handler(qf.clone(), reason)?;
                                    } else {
                                        return Err(Self::runtime_error_from_supply_reason(reason));
                                    }
                                    return Ok((
                                        Value::make_instance(Symbol::intern("Tap"), HashMap::new()),
                                        attrs,
                                    ));
                                }
                            }
                        } else {
                            plain_values.push(item.clone());
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
                    // The tapped on-demand supply's own `closing => { ... }`
                    // callbacks (stored on the Supply as `on_close_callbacks`) fire
                    // when it closes. A synchronous body that already ran `done`
                    // fires them now; an async body (e.g. `start { emit; done }`)
                    // registers them to fire when the emitter later signals `done`,
                    // and — since a plain async body registers no inner whenever —
                    // the outer tap must also be registered on the emitter so the
                    // thread's `emit`s reach the tap subscriber.
                    let own_close_cbs: Vec<Value> =
                        match attrs.get("on_close_callbacks").map(Value::view) {
                            Some(ValueView::Array(cbs, ..)) => cbs.to_vec(),
                            _ => Vec::new(),
                        };
                    if !own_close_cbs.is_empty() {
                        let (_, emitter_done, _) = supplier_snapshot(emitter_supplier_id);
                        if self.react_active > 0 {
                            // Tapped by a nested `whenever` while a react drive
                            // loop is running (`whenever $outer { whenever $sod {} }`).
                            // Fire the `closing => { ... }` callbacks on the main
                            // react thread so a write to a captured react-block
                            // lexical is not lost on an async `start { emit; done }`
                            // body's worker thread.
                            if body_done {
                                // Synchronous body already closed: fire now.
                                for cb in &own_close_cbs {
                                    let _ = self.call_sub_value(cb.clone(), vec![], true);
                                }
                            } else if let Some(p) = close_done_promise {
                                // Async body: the drive loop fires the callbacks
                                // once the emitter's done-signal promise resolves.
                                // Wake the loop when that happens so it doesn't
                                // wait out its idle cap to notice.
                                if let Some(w) = &self.current_react_waker {
                                    let w = w.clone();
                                    let _ = p.on_resolve(Box::new(move |_, _, _, _| w.notify()));
                                }
                                self.pending_tap_closes.push((p, own_close_cbs));
                                if !outer_tap_registered
                                    && Self::supply_has_active_callback(&tap_cb)
                                {
                                    register_supplier_tap(
                                        emitter_supplier_id,
                                        tap_cb.clone(),
                                        delay_seconds,
                                    );
                                }
                            }
                        } else if body_done || emitter_done {
                            for cb in &own_close_cbs {
                                self.call_sub_value(cb.clone(), vec![], true)?;
                            }
                        } else {
                            for cb in &own_close_cbs {
                                register_supplier_close_callback(emitter_supplier_id, cb.clone());
                            }
                            register_supplier_done_callback(
                                emitter_supplier_id,
                                Self::make_supply_close_marker(emitter_supplier_id),
                            );
                            if !outer_tap_registered && Self::supply_has_active_callback(&tap_cb) {
                                register_supplier_tap(
                                    emitter_supplier_id,
                                    tap_cb.clone(),
                                    delay_seconds,
                                );
                            }
                        }
                    }
                    plain_values
                } else if has_unique {
                    if Self::supply_has_active_callback(&tap_cb) {
                        let pushed = attrs
                            .get_mut("taps")
                            .and_then(|v| {
                                v.with_array_mut(|items, _| {
                                    crate::gc::Gc::make_mut(items).push(tap_cb.clone());
                                })
                            })
                            .is_some();
                        if !pushed {
                            attrs.insert("taps".to_string(), Value::array(vec![tap_cb.clone()]));
                        }
                    }
                    // For unique supplier-backed supplies, replay already-emitted
                    // values through the unique filter at tap-time. This handles
                    // the case where emissions happen before the tap is registered
                    // (e.g., tap-ok's :after-tap evaluation order).
                    if let Some(ValueView::Int(supplier_id)) =
                        attrs.get("supplier_id").map(Value::view)
                    {
                        let (emitted, _done, _quit) = supplier_snapshot(supplier_id as u64);
                        let as_fn = attrs.get("unique_as").cloned();
                        let with_fn = attrs.get("unique_with").cloned();
                        let expires_secs = attrs.get("unique_expires").map(|v| v.to_f64());
                        let mut seen_keys: Vec<(Value, crate::runtime::thread_compat::Instant)> =
                            Vec::new();
                        let mut unique_items: Vec<Value> = Vec::new();
                        let now = crate::runtime::thread_compat::Instant::now();
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
                                let sid = supplier_id as u64;
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
                        let pushed = attrs
                            .get_mut("taps")
                            .and_then(|v| {
                                v.with_array_mut(|items, _| {
                                    crate::gc::Gc::make_mut(items).push(tap_cb.clone());
                                })
                            })
                            .is_some();
                        if !pushed {
                            attrs.insert("taps".to_string(), Value::array(vec![tap_cb.clone()]));
                        }
                    }
                    if let Some(ValueView::Int(supplier_id)) =
                        attrs.get("supplier_id").map(Value::view)
                    {
                        let is_live = matches!(
                            attrs.get("live").map(Value::view),
                            Some(ValueView::Bool(true))
                        );
                        if is_live {
                            Vec::new()
                        } else {
                            let (snap_values, _, _) = supplier_snapshot(supplier_id as u64);
                            if !snap_values.is_empty() {
                                snap_values
                            } else if let Some(ValueView::Array(values, ..)) =
                                attrs.get("values").map(Value::view)
                            {
                                values.to_vec()
                            } else {
                                Vec::new()
                            }
                        }
                    } else if let Some(ValueView::Array(values, ..)) =
                        attrs.get("values").map(Value::view)
                    {
                        values.to_vec()
                    } else {
                        Vec::new()
                    }
                };

                // Call do_callbacks and tap callback for each value
                let do_cbs = attrs.get("do_callbacks").and_then(|v| {
                    if let ValueView::Array(a, ..) = v.view() {
                        Some(a.to_vec())
                    } else {
                        None
                    }
                });
                let throttle_limit = attrs.get("throttle_limit").and_then(|v| {
                    if let ValueView::Int(n) = v.view() {
                        Some(n as usize)
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
                        // A `done`/`last` inside the tap callback completes the tap
                        // cleanly: stop emitting and fall through to the done
                        // callback. It must not surface as a runtime error (this is
                        // how `(1..Inf).Supply.tap({ ...; done if ... })` terminates).
                        match self.call_sub_value(tap_cb.clone(), vec![v.clone()], true) {
                            Ok(_) => {}
                            Err(err) if err.is_react_done() || err.is_last() => break,
                            Err(err) => return Err(err),
                        }
                    }
                }

                // The tap callback is an immediately-invoked closure run on this
                // thread, so escape analysis never boxes the outer lexicals it
                // captures-and-writes (`$min`/`$max`/`$before` in the throttle
                // timing tests). Record its captured-outer writes so the enclosing
                // `.tap` call site drains them back into the caller's locals
                // (mirrors the lazy-map / gather / cross-shortcircuit carriers) —
                // without this they are lost once the blanket reconcile is removed
                // (docs/captured-outer-cell-sharing.md §7.2).
                if let ValueView::Sub(data) = tap_cb.view()
                    && let Some(code) = data.compiled_code.clone()
                {
                    self.record_eager_block_free_var_writeback(&code, &data.params);
                }

                if let Some(quit_reason) = on_demand_quit {
                    if let Some(quit_fn) = quit_cb {
                        self.call_supply_quit_handler(quit_fn, quit_reason)?;
                        // Dispatching the quit callback clears
                        // `pending_rw_writeback_sources`
                        // (call_compiled_closure_with_topic), which drops the tap
                        // callback's captured-outer writes recorded above. Re-record
                        // them after the handler so the enclosing `.tap` call site
                        // still drains the tap block's scalar writes (e.g.
                        // `$emits-run++` in `supply { emit ...; die }`).
                        if let ValueView::Sub(data) = tap_cb.view()
                            && let Some(code) = data.compiled_code.clone()
                        {
                            self.record_eager_block_free_var_writeback(&code, &data.params);
                        }
                    } else {
                        return Err(Self::runtime_error_from_supply_reason(quit_reason));
                    }
                    let tap_instance = Value::make_instance(Symbol::intern("Tap"), HashMap::new());
                    return Ok((tap_instance, attrs));
                }

                // Call done callback after all values emitted
                if let Some(done_fn) = done_cb {
                    if let Some(ValueView::Int(supplier_id)) =
                        attrs.get("supplier_id").map(Value::view)
                    {
                        // Check both the attribute and the global supplier state
                        let supplier_is_done = attrs
                            .get("supplier_done")
                            .map(Value::truthy)
                            .unwrap_or(false)
                            || {
                                let (_, done, _) = supplier_snapshot(supplier_id as u64);
                                done
                            };
                        if supplier_is_done {
                            let _ = self.call_sub_value(done_fn, vec![], true);
                        } else {
                            register_supplier_done_callback(supplier_id as u64, done_fn);
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
                    if let Some(ValueView::Int(supplier_id)) =
                        attrs.get("supplier_id").map(Value::view)
                    {
                        let (_, _, quit_reason) = supplier_snapshot(supplier_id as u64);
                        if let Some(reason) = quit_reason {
                            self.call_supply_quit_handler(quit_fn, reason)?;
                        } else {
                            register_supplier_quit_callback(supplier_id as u64, quit_fn);
                        }
                    } else if let Some(reason) = attrs.get("quit_reason").cloned() {
                        self.call_supply_quit_handler(quit_fn, reason)?;
                    }
                }
                if let Some(cid) = close_supplier_id {
                    tap_handle_attrs
                        .insert("close_supplier_id".to_string(), Value::int(cid as i64));
                }
                let tap_instance = Value::make_instance(Symbol::intern("Tap"), tap_handle_attrs);
                Ok((tap_instance, attrs))
            }
            "on-close" => {
                let close_cb = args.first().cloned().unwrap_or(Value::NIL);
                let mut new_attrs = attrs.clone();
                let mut callbacks = if let Some(ValueView::Array(existing, ..)) =
                    attrs.get("on_close_callbacks").map(Value::view)
                {
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
                let values = match attrs.get("values").map(Value::view) {
                    Some(ValueView::Array(items, ..)) => items.to_vec(),
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
                new_attrs.insert("live".to_string(), Value::FALSE);
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
