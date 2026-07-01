//! Split out of native_supply_methods.rs. See that file for the shared
//! helpers and the `QuitOutcome` enum.
use super::native_methods::*;
use super::native_supply_methods::QuitOutcome;
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
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
            "__mutsu_register_close_phaser" => {
                // A CLOSE phaser in a supply block registers its body here, on
                // the emitter's supplier_id, to run when the tap closes or the
                // supply terminates (see take_supplier_close_callbacks).
                if let Some(supplier_id) = supplier_id_from_attrs(attributes)
                    && let Some(cb) = args.into_iter().next()
                {
                    register_supplier_close_callback(supplier_id, cb);
                }
                Ok(Value::Nil)
            }
            "emit" => {
                // Push to supply_emit_buffer (works for on-demand callbacks)
                let value = args.first().cloned().unwrap_or(Value::Nil);
                if Self::supply_is_terminated(attributes) {
                    return Ok(Value::Nil);
                }
                // Streaming on-demand react path (see native_supplier_mut emit).
                if let Some(sid) = supplier_id_from_attrs(attributes)
                    && let Some(res) = self.try_stream_emit(sid, &value)
                {
                    return res.map(|_| Value::Nil);
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
                                if let Err(err) = self.call_sub_value(tap, vec![emitted], true) {
                                    // Route errors from tap callbacks to the
                                    // supplier's quit handlers (e.g. die inside
                                    // a whenever body in a supply block).
                                    let reason = err
                                        .exception
                                        .as_deref()
                                        .cloned()
                                        .unwrap_or_else(|| Value::str(err.message));
                                    let quit_cbs = take_supplier_quit_callbacks(supplier_id);
                                    if !quit_cbs.is_empty() {
                                        for qcb in quit_cbs {
                                            self.call_supply_quit_handler(qcb, reason.clone())?;
                                        }
                                    }
                                }
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
                                    self.call_sub_value(f, vec![val.clone()], true)
                                        .unwrap_or(val.clone())
                                } else {
                                    val.clone()
                                };
                                let is_dup = self
                                    .supplier_unique_check_seen(
                                        supplier_id,
                                        tap_index,
                                        &key,
                                        &with_fn,
                                    )
                                    .unwrap_or(false);
                                if !is_dup {
                                    supplier_unique_mark_seen(supplier_id, tap_index, key);
                                    Self::sleep_for_supply_delay(delay_seconds);
                                    let _ = self.call_sub_value(callback, vec![val], true);
                                }
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
                                    let _ = self.invoke_done_callback(done_cb);
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
                            SupplierEmitAction::ZipBuffer {
                                zip_state_id: zid,
                                source_index: si,
                                value: val,
                            } => {
                                let result = zip_buffer_value(zid, si, val);
                                if let ZipAction::Emit(tuple_val) = result {
                                    let (output_sid, wf) = zip_state_info(zid);
                                    let emit_val = if let Some(wfn) = wf {
                                        if let Value::Array(items, ..) = &tuple_val {
                                            self.call_sub_value(wfn, items.to_vec(), false)
                                                .unwrap_or(tuple_val)
                                        } else {
                                            tuple_val
                                        }
                                    } else {
                                        tuple_val
                                    };
                                    supplier_emit(output_sid, emit_val.clone());
                                    let ds_actions = supplier_emit_callbacks(output_sid, &emit_val);
                                    for da in ds_actions {
                                        if let SupplierEmitAction::Call(
                                            tap,
                                            emitted,
                                            delay_seconds,
                                        ) = da
                                        {
                                            Self::sleep_for_supply_delay(delay_seconds);
                                            let _ = self.call_sub_value(tap, vec![emitted], true);
                                        }
                                    }
                                }
                            }
                            SupplierEmitAction::ZipLatestBuffer {
                                zip_latest_state_id: zid,
                                source_index: si,
                                value: val,
                            } => {
                                let result = zip_latest_buffer_value(zid, si, val);
                                if let ZipAction::Emit(tuple_val) = result {
                                    let (output_sid, wf) = zip_latest_state_info(zid);
                                    let emit_val = if let Some(wfn) = wf {
                                        if let Value::Array(items, ..) = &tuple_val {
                                            self.call_sub_value(wfn, items.to_vec(), false)
                                                .unwrap_or(tuple_val)
                                        } else {
                                            tuple_val
                                        }
                                    } else {
                                        tuple_val
                                    };
                                    supplier_emit(output_sid, emit_val.clone());
                                    let ds_actions = supplier_emit_callbacks(output_sid, &emit_val);
                                    for da in ds_actions {
                                        if let SupplierEmitAction::Call(
                                            tap,
                                            emitted,
                                            delay_seconds,
                                        ) = da
                                        {
                                            Self::sleep_for_supply_delay(delay_seconds);
                                            let _ = self.call_sub_value(tap, vec![emitted], true);
                                        }
                                    }
                                }
                            }
                            SupplierEmitAction::Migrate {
                                value: val,
                                master_supplier_id,
                                downstream_supplier_id,
                                tap_index,
                            } => {
                                self.handle_supply_migrate(
                                    val,
                                    master_supplier_id,
                                    downstream_supplier_id,
                                    tap_index,
                                )?;
                            }
                            SupplierEmitAction::ForwardEmit {
                                downstream_supplier_id,
                                value: val,
                            } => {
                                self.handle_supply_forward(downstream_supplier_id, val)?;
                            }
                        }
                    }
                }
                Ok(Value::Nil)
            }
            "done" => {
                // Bumped so the on-demand tap handler can tell (by comparing
                // the count before/after running the block body) that the body
                // itself called `done` — the supplier's own done state is reset
                // below, so it can't be read back later.
                bump_supplier_done_count();
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
                            let _ = self.invoke_done_callback(done_cb);
                        }
                    }
                    for (tap, emitted) in flush_supplier_line_taps(supplier_id) {
                        let _ = self.call_sub_value(tap, vec![emitted], true);
                    }
                    for (tap, emitted) in flush_supplier_words_taps(supplier_id) {
                        let _ = self.call_sub_value(tap, vec![emitted], true);
                    }
                    for done_cb in take_supplier_done_callbacks(supplier_id) {
                        self.invoke_done_callback(done_cb)?;
                    }
                    // Propagate done to start-transform output suppliers
                    for out_sid in get_start_output_supplier_ids(supplier_id) {
                        supplier_done(out_sid);
                        for done_cb in take_supplier_done_callbacks(out_sid) {
                            let _ = self.invoke_done_callback(done_cb);
                        }
                    }
                    // Propagate done to zip output suppliers
                    for zid in get_supplier_zip_state_ids(supplier_id) {
                        let (action, output_sid) = zip_source_done(zid);
                        if matches!(action, ZipAction::AllDone) {
                            supplier_done(output_sid);
                            for done_cb in take_supplier_done_callbacks(output_sid) {
                                let _ = self.invoke_done_callback(done_cb);
                            }
                        }
                    }
                    // Propagate done to zip-latest output suppliers
                    for zid in get_supplier_zip_latest_state_ids(supplier_id) {
                        let (action, output_sid) = zip_latest_source_done(zid);
                        if matches!(action, ZipAction::AllDone) {
                            supplier_done(output_sid);
                            for done_cb in take_supplier_done_callbacks(output_sid) {
                                let _ = self.invoke_done_callback(done_cb);
                            }
                        }
                    }
                    close_all_supplier_taps(supplier_id);
                    supplier_reset(supplier_id);
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
                        // Run any `whenever` QUIT phasers first. If one handles
                        // the exception, the supply completes with done and the
                        // downstream quit handler is suppressed.
                        let mut handled = false;
                        let mut via_done = false;
                        for qcb in take_supplier_whenever_quit_callbacks(supplier_id) {
                            match self.run_whenever_quit_phaser(qcb, reason.clone()) {
                                QuitOutcome::HandledViaDone => {
                                    handled = true;
                                    via_done = true;
                                }
                                QuitOutcome::Handled => handled = true,
                                QuitOutcome::Unhandled => {}
                            }
                        }
                        if handled {
                            // When the QUIT called `done`, the emitter completion
                            // already fired the downstream done — just drop the
                            // pending done callbacks so it does not fire twice.
                            if via_done {
                                let _ = take_supplier_done_callbacks(supplier_id);
                            } else {
                                for done_cb in take_supplier_done_callbacks(supplier_id) {
                                    let _ = self.invoke_done_callback(done_cb);
                                }
                            }
                        } else {
                            for quit_cb in take_supplier_quit_callbacks(supplier_id) {
                                self.call_supply_quit_handler(quit_cb, reason.clone())?;
                            }
                            let _ = take_supplier_done_callbacks(supplier_id);
                        }
                    } else {
                        let _ = take_supplier_done_callbacks(supplier_id);
                    }
                    close_all_supplier_taps(supplier_id);
                    supplier_reset_keep_quit(supplier_id);
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
                // Streaming on-demand react path: deliver synchronously to the
                // consumer instead of buffering, so an infinite synchronous body
                // can be terminated on emit-to-dead-consumer.
                if let Some(sid) = supplier_id_from_attrs(&attrs)
                    && let Some(res) = self.try_stream_emit(sid, &value)
                {
                    return res.map(|_| (Value::Nil, attrs));
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
                                if let Err(err) = self.call_sub_value(tap, vec![emitted], true) {
                                    // Route errors from tap callbacks to the
                                    // supplier's quit handlers.
                                    let reason = err
                                        .exception
                                        .as_deref()
                                        .cloned()
                                        .unwrap_or_else(|| Value::str(err.message));
                                    let quit_cbs = take_supplier_quit_callbacks(sid);
                                    if !quit_cbs.is_empty() {
                                        for qcb in quit_cbs {
                                            self.call_supply_quit_handler(qcb, reason.clone())?;
                                        }
                                    } else {
                                        return Err(Self::runtime_error_from_supply_reason(reason));
                                    }
                                }
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
                                    let _ = self.invoke_done_callback(done_cb);
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
                            SupplierEmitAction::ZipBuffer {
                                zip_state_id: zid,
                                source_index: si,
                                value: val,
                            } => {
                                let result = zip_buffer_value(zid, si, val);
                                if let ZipAction::Emit(tuple_val) = result {
                                    let (output_sid, wf) = zip_state_info(zid);
                                    let emit_val = if let Some(wfn) = wf {
                                        if let Value::Array(items, ..) = &tuple_val {
                                            self.call_sub_value(wfn, items.to_vec(), false)
                                                .unwrap_or(tuple_val)
                                        } else {
                                            tuple_val
                                        }
                                    } else {
                                        tuple_val
                                    };
                                    supplier_emit(output_sid, emit_val.clone());
                                    let ds_actions = supplier_emit_callbacks(output_sid, &emit_val);
                                    for da in ds_actions {
                                        if let SupplierEmitAction::Call(
                                            tap,
                                            emitted,
                                            delay_seconds,
                                        ) = da
                                        {
                                            Self::sleep_for_supply_delay(delay_seconds);
                                            let _ = self.call_sub_value(tap, vec![emitted], true);
                                        }
                                    }
                                }
                            }
                            SupplierEmitAction::ZipLatestBuffer {
                                zip_latest_state_id: zid,
                                source_index: si,
                                value: val,
                            } => {
                                let result = zip_latest_buffer_value(zid, si, val);
                                if let ZipAction::Emit(tuple_val) = result {
                                    let (output_sid, wf) = zip_latest_state_info(zid);
                                    let emit_val = if let Some(wfn) = wf {
                                        if let Value::Array(items, ..) = &tuple_val {
                                            self.call_sub_value(wfn, items.to_vec(), false)
                                                .unwrap_or(tuple_val)
                                        } else {
                                            tuple_val
                                        }
                                    } else {
                                        tuple_val
                                    };
                                    supplier_emit(output_sid, emit_val.clone());
                                    let ds_actions = supplier_emit_callbacks(output_sid, &emit_val);
                                    for da in ds_actions {
                                        if let SupplierEmitAction::Call(
                                            tap,
                                            emitted,
                                            delay_seconds,
                                        ) = da
                                        {
                                            Self::sleep_for_supply_delay(delay_seconds);
                                            let _ = self.call_sub_value(tap, vec![emitted], true);
                                        }
                                    }
                                }
                            }
                            SupplierEmitAction::Migrate {
                                value: val,
                                master_supplier_id,
                                downstream_supplier_id,
                                tap_index,
                            } => {
                                self.handle_supply_migrate(
                                    val,
                                    master_supplier_id,
                                    downstream_supplier_id,
                                    tap_index,
                                )?;
                            }
                            SupplierEmitAction::ForwardEmit {
                                downstream_supplier_id,
                                value: val,
                            } => {
                                self.handle_supply_forward(downstream_supplier_id, val)?;
                            }
                        }
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "done" => {
                bump_supplier_done_count();
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
                            let _ = self.invoke_done_callback(done_cb);
                        }
                    }
                    // Propagate done to classify sub-suppliers
                    let classify_subs = get_classify_sub_supplier_ids(sid);
                    for sub_sid in classify_subs {
                        supplier_done(sub_sid);
                        for done_cb in take_supplier_done_callbacks(sub_sid) {
                            let _ = self.invoke_done_callback(done_cb);
                        }
                    }
                    for (tap, emitted) in flush_supplier_line_taps(sid) {
                        self.call_sub_value(tap, vec![emitted], true)?;
                    }
                    for (tap, emitted) in flush_supplier_words_taps(sid) {
                        self.call_sub_value(tap, vec![emitted], true)?;
                    }
                    for done_cb in take_supplier_done_callbacks(sid) {
                        self.invoke_done_callback(done_cb)?;
                    }
                    // Propagate done to start-transform output suppliers
                    for out_sid in get_start_output_supplier_ids(sid) {
                        supplier_done(out_sid);
                        for done_cb in take_supplier_done_callbacks(out_sid) {
                            let _ = self.invoke_done_callback(done_cb);
                        }
                    }
                    // Propagate done to zip output suppliers
                    for zid in get_supplier_zip_state_ids(sid) {
                        let (action, output_sid) = zip_source_done(zid);
                        if matches!(action, ZipAction::AllDone) {
                            supplier_done(output_sid);
                            for done_cb in take_supplier_done_callbacks(output_sid) {
                                let _ = self.invoke_done_callback(done_cb);
                            }
                        }
                    }
                    // Propagate done to zip-latest output suppliers
                    for zid in get_supplier_zip_latest_state_ids(sid) {
                        let (action, output_sid) = zip_latest_source_done(zid);
                        if matches!(action, ZipAction::AllDone) {
                            supplier_done(output_sid);
                            for done_cb in take_supplier_done_callbacks(output_sid) {
                                let _ = self.invoke_done_callback(done_cb);
                            }
                        }
                    }
                    close_all_supplier_taps(sid);
                    supplier_reset(sid);
                }
                attrs.insert("done".to_string(), Value::Bool(false));
                attrs.remove("emitted");
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
                    // Run any `whenever` QUIT phasers first. If one handles the
                    // exception (a when/default matched, or it called done), the
                    // supply completes with done and the downstream quit handler
                    // is suppressed.
                    let mut handled = false;
                    let mut via_done = false;
                    for qcb in take_supplier_whenever_quit_callbacks(sid) {
                        match self.run_whenever_quit_phaser(qcb, reason.clone()) {
                            QuitOutcome::HandledViaDone => {
                                handled = true;
                                via_done = true;
                            }
                            QuitOutcome::Handled => handled = true,
                            QuitOutcome::Unhandled => {}
                        }
                    }
                    if handled {
                        // When the QUIT called `done`, the emitter completion
                        // already fired the downstream done — just drop the
                        // pending done callbacks so it does not fire twice.
                        if via_done {
                            let _ = take_supplier_done_callbacks(sid);
                        } else {
                            for done_cb in take_supplier_done_callbacks(sid) {
                                let _ = self.invoke_done_callback(done_cb);
                            }
                        }
                    } else {
                        for quit_cb in take_supplier_quit_callbacks(sid) {
                            self.call_supply_quit_handler(quit_cb, reason.clone())?;
                        }
                        let _ = take_supplier_done_callbacks(sid);
                    }
                    close_all_supplier_taps(sid);
                    supplier_reset_keep_quit(sid);
                }
                attrs.insert("done".to_string(), Value::Bool(false));
                attrs.remove("emitted");
                Ok((Value::Nil, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Supplier",
                method
            ))),
        }
    }

    // --- Supply mutable ---
}
