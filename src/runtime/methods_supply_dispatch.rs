use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Supply.merge(...) as a class method
    pub(super) fn dispatch_supply_merge(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Validate all args are Supply instances
        for arg in args {
            if !matches!(arg.view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
            {
                let mut ex_attrs = HashMap::new();
                ex_attrs.insert("combinator".to_string(), Value::str("merge".to_string()));
                ex_attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Can only merge Supply objects, got {}",
                        crate::value::types::what_type_name(arg)
                    )),
                );
                let ex = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Supply::Combinator"),
                    ex_attrs,
                );
                let mut err = RuntimeError::new(format!(
                    "Can only merge Supply objects, got {}",
                    crate::value::types::what_type_name(arg)
                ));
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
        }
        if args.len() == 1 {
            // Merging one supply is a noop — return the same supply
            return Ok(args[0].clone());
        }
        let mut all_values = Vec::new();
        for arg in args {
            if let ValueView::Instance { attributes, .. } = arg.view()
                && let Some(ValueView::Array(items, ..)) =
                    attributes.as_map().get("values").map(Value::view)
            {
                all_values.extend(items.iter().cloned());
            }
        }
        let mut new_attrs = HashMap::new();
        new_attrs.insert("values".to_string(), Value::array(all_values));
        new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        new_attrs.insert("live".to_string(), Value::FALSE);
        Ok(Value::make_instance(
            crate::symbol::Symbol::intern("Supply"),
            new_attrs,
        ))
    }

    /// Supply.zip(...) as a class method
    pub(super) fn dispatch_supply_zip_class(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Separate named args (Pair values like :with) from positional args
        let mut with_fn: Option<Value> = None;
        let mut supplies: Vec<Value> = Vec::new();
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view()
                && key == "with"
            {
                with_fn = Some(value.clone());
            } else {
                supplies.push(arg.clone());
            }
        }

        if supplies.is_empty() {
            return Ok(Value::make_instance(
                crate::symbol::Symbol::intern("Supply"),
                {
                    let mut a = HashMap::new();
                    a.insert("values".to_string(), Value::array(Vec::new()));
                    a.insert("taps".to_string(), Value::array(Vec::new()));
                    a.insert("live".to_string(), Value::FALSE);
                    a
                },
            ));
        }

        // Validate all args are Supply instances
        for arg in &supplies {
            if !matches!(arg.view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
            {
                let mut ex_attrs = HashMap::new();
                ex_attrs.insert("combinator".to_string(), Value::str("zip".to_string()));
                ex_attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Can only zip Supply objects, got {}",
                        crate::value::types::what_type_name(arg)
                    )),
                );
                let ex = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Supply::Combinator"),
                    ex_attrs,
                );
                let mut err = RuntimeError::new(format!(
                    "Can only zip Supply objects, got {}",
                    crate::value::types::what_type_name(arg)
                ));
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
        }

        // Single supply is a noop — return the same supply
        if supplies.len() == 1 {
            return Ok(supplies.into_iter().next().unwrap());
        }

        // Check if any supply is channel-based (has supply_id) — if so, create
        // a live zip supply that coordinates via channels.
        let has_channel_supply = supplies.iter().any(|s| {
            if let ValueView::Instance { attributes, .. } = s.view() {
                attributes.contains_key("supply_id")
            } else {
                false
            }
        });

        if has_channel_supply {
            return Self::create_channel_zip_supply(&supplies);
        }

        // All-static path: collect values from all supplies
        let mut supply_values: Vec<Vec<Value>> = Vec::new();
        for arg in &supplies {
            if let ValueView::Instance { attributes, .. } = arg.view() {
                let items = match attributes.as_map().get("values").map(Value::view) {
                    Some(ValueView::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                supply_values.push(items);
            }
        }
        let min_len = supply_values.iter().map(|v| v.len()).min().unwrap_or(0);
        let mut zipped = Vec::new();
        for i in 0..min_len {
            let tuple: Vec<Value> = supply_values.iter().map(|sv| sv[i].clone()).collect();
            if let Some(ref wf) = with_fn {
                let combined = self.call_sub_value(wf.clone(), tuple, false)?;
                zipped.push(combined);
            } else {
                zipped.push(Value::array(tuple));
            }
        }
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(zipped));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        Ok(Value::make_instance(
            crate::symbol::Symbol::intern("Supply"),
            attrs,
        ))
    }

    /// Create a channel-based zip supply when at least one source is channel-based.
    /// Spawns a coordination thread that reads from all sources and emits zipped values.
    fn create_channel_zip_supply(supplies: &[Value]) -> Result<Value, RuntimeError> {
        use super::native_methods::{
            SupplyEvent, next_supply_id, supply_channel_map_pub, take_supply_channel,
        };
        use std::sync::mpsc;

        let zip_supply_id = next_supply_id();
        let (zip_tx, zip_rx) = mpsc::channel();

        enum SourceKind {
            Channel(mpsc::Receiver<SupplyEvent>),
            Static(Vec<Value>),
        }
        let mut sources: Vec<SourceKind> = Vec::with_capacity(supplies.len());

        for supply in supplies {
            if let ValueView::Instance { attributes, .. } = supply.view() {
                if let Some(ValueView::Int(sid)) =
                    attributes.as_map().get("supply_id").map(Value::view)
                    && let Some(rx) = take_supply_channel(sid as u64)
                {
                    sources.push(SourceKind::Channel(rx));
                    continue;
                }
                let items = match attributes.as_map().get("values").map(Value::view) {
                    Some(ValueView::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                sources.push(SourceKind::Static(items));
            }
        }

        // Buffers and emits `Value`s (Gc nodes): registered GC mutator; the
        // poll loop parks at a GC park point each round so a stop-the-world
        // can proceed while it idles.
        crate::runtime::builtins_system::spawn_user_thread(move || {
            let n = sources.len();
            let mut buffers: Vec<std::collections::VecDeque<Value>> =
                vec![std::collections::VecDeque::new(); n];
            let mut done: Vec<bool> = vec![false; n];

            // Pre-fill buffers from static sources
            for (i, source) in sources.iter().enumerate() {
                if let SourceKind::Static(vals) = source {
                    for v in vals {
                        buffers[i].push_back(v.clone());
                    }
                    done[i] = true;
                }
            }

            let timeout = std::time::Duration::from_millis(10);

            loop {
                crate::gc::gc_park_point();
                // Emit zipped values while all buffers have at least one value
                while buffers.iter().all(|b| !b.is_empty()) {
                    let tuple: Vec<Value> =
                        buffers.iter_mut().map(|b| b.pop_front().unwrap()).collect();
                    if zip_tx.send(SupplyEvent::Emit(Value::array(tuple))).is_err() {
                        return;
                    }
                }

                // If any done source has an empty buffer, zip is complete
                let any_done_empty = (0..n).any(|i| done[i] && buffers[i].is_empty());
                if any_done_empty {
                    let _ = zip_tx.send(SupplyEvent::Done);
                    return;
                }

                // Poll channel sources for new values
                for (i, source) in sources.iter().enumerate() {
                    if done[i] {
                        continue;
                    }
                    if let SourceKind::Channel(rx) = source {
                        match rx.recv_timeout(timeout) {
                            Ok(SupplyEvent::Emit(v)) => {
                                buffers[i].push_back(v);
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

                // If all sources are done, finish
                if done.iter().all(|d| *d) {
                    let _ = zip_tx.send(SupplyEvent::Done);
                    return;
                }
            }
        });

        if let Ok(mut map) = supply_channel_map_pub().lock() {
            map.insert(zip_supply_id, zip_rx);
        }

        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("supply_id".to_string(), Value::int(zip_supply_id as i64));
        attrs.insert("live".to_string(), Value::FALSE);
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply.from-list class method
    pub(super) fn dispatch_supply_from_list(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Implement +@ single-arg rule:
        // - Single array/list arg: iterate its elements
        // - Multiple args: each arg is one value
        let values = if args.len() == 1 {
            match args[0].view() {
                ValueView::Array(items, ..) => items.to_vec(),
                ValueView::Slip(items) | ValueView::Seq(items) => items.to_vec(),
                ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. } => Self::value_to_list(&args[0]),
                _ => vec![args[0].clone()],
            }
        } else {
            let mut values = Vec::new();
            for arg in args {
                match arg.view() {
                    ValueView::Slip(items) => {
                        values.extend(items.iter().cloned());
                    }
                    _ => values.push(arg.clone()),
                }
            }
            values
        };
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply.on-demand class method
    pub(super) fn dispatch_supply_on_demand(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let callback = args.first().cloned().unwrap_or(Value::NIL);
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        attrs.insert("on_demand_callback".to_string(), callback);
        // `Supply.on-demand(..., closing => { ... })`: the `closing` callback runs
        // when the supply is closed (its tap is closed, or it sends `done`).
        // Store it on the on-close list so the react runtime fires it on
        // completion (see run_react_event_loop's on-demand branch).
        for arg in args.iter().skip(1) {
            if let ValueView::Pair(key, value) = arg.view()
                && key == "closing"
            {
                attrs.insert(
                    "on_close_callbacks".to_string(),
                    Value::array(vec![value.clone()]),
                );
            }
        }
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply.signal class method
    pub(super) fn dispatch_supply_signal(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::TRUE);
        attrs.insert("signals".to_string(), Value::array(args.to_vec()));
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply instance .grab method
    pub(super) fn dispatch_supply_grab(
        &mut self,
        attributes: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let values = match attributes.get("values").map(Value::view) {
            Some(ValueView::Array(items, ..)) => items.to_vec(),
            _ => Vec::new(),
        };
        let func = args.first().cloned().unwrap_or(Value::NIL);
        let values_list = Value::array(values);
        let result = self.eval_call_on_value(func, vec![values_list])?;
        let result_values = Self::value_to_list(&result);
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(result_values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply instance .skip method
    pub(super) fn dispatch_supply_skip(
        &self,
        attributes: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let n = if args.is_empty() {
            1usize
        } else {
            let arg = &args[0];
            match arg.view() {
                ValueView::Int(i) => i as usize,
                ValueView::Num(f) => f as usize,
                ValueView::Str(s) => s.parse::<usize>().map_err(|_| {
                    RuntimeError::new(format!(
                        "X::Str::Numeric: Cannot convert string '{}' to a number",
                        *s
                    ))
                })?,
                _ => arg.to_f64() as usize,
            }
        };
        let values = match attributes.get("values").map(Value::view) {
            Some(ValueView::Array(items, ..)) => items.iter().skip(n).cloned().collect::<Vec<_>>(),
            _ => Vec::new(),
        };
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply.elems method
    pub(super) fn dispatch_supply_elems(
        &mut self,
        attributes: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let interval = args.first().map(Value::to_f64).unwrap_or(0.0).max(0.0);

        if let Some(ValueView::Int(supplier_id)) = attributes.get("supplier_id").map(Value::view)
            && supplier_id > 0
        {
            let (emitted, done, quit_reason) =
                crate::runtime::native_methods::supplier_snapshot(supplier_id as u64);
            let mut elems_attrs = HashMap::new();
            elems_attrs.insert("values".to_string(), Value::array(Vec::new()));
            elems_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            elems_attrs.insert("live".to_string(), Value::FALSE);
            elems_attrs.insert("supplier_id".to_string(), Value::int(supplier_id));
            elems_attrs.insert("supplier_done".to_string(), Value::truth(done));
            elems_attrs.insert("elems_filter".to_string(), Value::TRUE);
            elems_attrs.insert("elems_interval".to_string(), Value::num(interval));
            elems_attrs.insert(
                "elems_initial_count".to_string(),
                Value::int(emitted.len() as i64),
            );
            if let Some(reason) = quit_reason {
                elems_attrs.insert("quit_reason".to_string(), reason);
            }
            return Ok(Value::make_instance(Symbol::intern("Supply"), elems_attrs));
        }

        let source_values = self.supply_list_values(attributes, true)?;
        let elems_values = if interval > 0.0 {
            Vec::new()
        } else {
            (1..=source_values.len())
                .map(|idx| Value::int(idx as i64))
                .collect()
        };
        let mut elems_attrs = HashMap::new();
        elems_attrs.insert("values".to_string(), Value::array(elems_values));
        elems_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        elems_attrs.insert("live".to_string(), Value::FALSE);
        Ok(Value::make_instance(Symbol::intern("Supply"), elems_attrs))
    }

    /// Handle Supply.map method
    pub(super) fn dispatch_supply_map(
        &mut self,
        attributes: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mapper = args.first().cloned().unwrap_or(Value::NIL);
        // Live (Supplier-backed) supply: register a transform tap so the map
        // stays live and forwards each mapped value to the derived supply.
        if let Some(live) = self.make_live_transform_supply(
            attributes,
            mapper.clone(),
            crate::runtime::native_methods::TransformMode::Map,
        ) {
            return Ok(live);
        }
        let source_values = if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
            let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                let mut a = HashMap::new();
                a.insert("emitted".to_string(), Value::array(Vec::new()));
                a.insert("done".to_string(), Value::FALSE);
                a
            });
            self.supply_emit_buffer.push(Vec::new());
            let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
            self.supply_emit_buffer.pop().unwrap_or_default()
        } else {
            self.supply_list_values(attributes, true)?
        };

        let mut mapped_values = Vec::with_capacity(source_values.len());
        for value in source_values {
            mapped_values.push(self.call_sub_value(mapper.clone(), vec![value], true)?);
        }

        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(mapped_values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert(
            "live".to_string(),
            attributes.get("live").cloned().unwrap_or(Value::FALSE),
        );
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// If `attributes` describes a live (Supplier-backed) supply, register a
    /// `grep`/`map` transform tap on it and return a new live derived Supply
    /// that receives the filtered/mapped values. Returns `None` for a
    /// materialized (snapshot) supply so the caller applies the transform
    /// eagerly instead.
    pub(in crate::runtime) fn make_live_transform_supply(
        &mut self,
        attributes: &HashMap<String, Value>,
        callable: Value,
        mode: crate::runtime::native_methods::TransformMode,
    ) -> Option<Value> {
        let source_sid = crate::runtime::native_methods::supplier_id_from_attrs(attributes)?;
        let downstream_sid = crate::runtime::native_methods::next_supplier_id();
        crate::runtime::native_methods::register_supplier_transform_tap(
            source_sid,
            downstream_sid,
            callable,
            mode,
        );
        let mut new_attrs = HashMap::new();
        new_attrs.insert("values".to_string(), Value::array(Vec::new()));
        new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        new_attrs.insert("supplier_id".to_string(), Value::int(downstream_sid as i64));
        new_attrs.insert("live".to_string(), Value::TRUE);
        Some(Value::make_instance(Symbol::intern("Supply"), new_attrs))
    }

    /// Handle Supply.reduce method
    pub(super) fn dispatch_supply_reduce(
        &mut self,
        target: Value,
        attributes: &HashMap<String, Value>,
        callable: Value,
    ) -> Result<Value, RuntimeError> {
        if !matches!(
            callable.view(),
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
        ) {
            return Err(RuntimeError::new("must be code if specified"));
        }
        if attributes.get("supplier_id").is_some() || attributes.get("on_demand_callback").is_some()
        {
            let mut reduce_attrs = HashMap::new();
            reduce_attrs.insert("values".to_string(), Value::array(Vec::new()));
            reduce_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            reduce_attrs.insert("live".to_string(), Value::FALSE);
            reduce_attrs.insert("reduce_source".to_string(), target);
            reduce_attrs.insert("reduce_callable".to_string(), callable);
            return Ok(Value::make_instance(Symbol::intern("Supply"), reduce_attrs));
        }
        let items = self.supply_list_values(attributes, true)?;
        let reduced = self.reduce_items(callable, items)?;
        let values = if reduced.is_nil() {
            Vec::new()
        } else {
            vec![reduced]
        };
        let mut reduce_attrs = HashMap::new();
        reduce_attrs.insert("values".to_string(), Value::array(values));
        reduce_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        reduce_attrs.insert("live".to_string(), Value::FALSE);
        Ok(Value::make_instance(Symbol::intern("Supply"), reduce_attrs))
    }

    /// Handle a `Migrate` emit action: the master supply-of-supplies emitted a
    /// value. It must be a Supply; switch the forwarded inner supply to it. If
    /// it is not a Supply, throw X::Supply::Migrate::Needs (propagated to the
    /// `emit` caller).
    pub(in crate::runtime) fn handle_supply_migrate(
        &mut self,
        value: Value,
        master_supplier_id: u64,
        downstream_supplier_id: u64,
        tap_index: usize,
    ) -> Result<(), RuntimeError> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = value.view()
            && class_name == "Supply"
        {
            if let Some(ValueView::Int(inner_sid)) =
                attributes.as_map().get("supplier_id").map(Value::view)
            {
                crate::runtime::native_methods::migrate_switch_inner(
                    master_supplier_id,
                    tap_index,
                    inner_sid as u64,
                    downstream_supplier_id,
                );
            } else {
                // A cold (non-live) supply: forward its already-known values now.
                let vals = self.supply_list_values(&attributes.as_map(), true)?;
                for v in vals {
                    self.handle_supply_forward(downstream_supplier_id, v)?;
                }
            }
            return Ok(());
        }
        let mut ex_attrs = HashMap::new();
        ex_attrs.insert(
            "message".to_string(),
            Value::str(".migrate needs Supplies to be emitted".to_string()),
        );
        let ex = Value::make_instance(Symbol::intern("X::Supply::Migrate::Needs"), ex_attrs);
        let mut err = RuntimeError::new(".migrate needs Supplies to be emitted");
        err.exception = Some(Box::new(ex));
        Err(err)
    }

    /// Handle a `ForwardEmit` action: re-emit a value verbatim into the
    /// downstream supplier and run its (plain) tap callbacks.
    pub(in crate::runtime) fn handle_supply_forward(
        &mut self,
        downstream_supplier_id: u64,
        value: Value,
    ) -> Result<(), RuntimeError> {
        use crate::runtime::native_methods::{
            SupplierEmitAction, supplier_emit, supplier_emit_callbacks,
        };
        supplier_emit(downstream_supplier_id, value.clone());
        let ds_actions = supplier_emit_callbacks(downstream_supplier_id, &value);
        for da in ds_actions {
            if let SupplierEmitAction::Call(tap, emitted, delay_seconds) = da {
                Self::sleep_for_supply_delay(delay_seconds);
                self.call_sub_value(tap, vec![emitted], true)?;
            }
        }
        Ok(())
    }

    /// Handle a `grep`/`map`/`do` transform tap emission on a live supply: run
    /// the callable on the value, then forward the (filtered, mapped, or
    /// original) result to the downstream supplier and drive its taps.
    pub(in crate::runtime) fn handle_supply_transform_emit(
        &mut self,
        downstream_supplier_id: u64,
        callable: Value,
        mode: crate::runtime::native_methods::TransformMode,
        value: Value,
    ) -> Result<(), RuntimeError> {
        use crate::runtime::native_methods::TransformMode;
        match mode {
            TransformMode::Grep => {
                // `grep` uses smart-match semantics (`$_ ~~ matcher`), so a
                // Callable is called, a type object type-checks, a Regex
                // matches, etc.
                let keep = self.smart_match_values(&value, &callable);
                if keep {
                    self.handle_supply_forward(downstream_supplier_id, value)?;
                }
            }
            TransformMode::Map => {
                let mapped = self.call_sub_value(callable, vec![value], true)?;
                self.handle_supply_forward(downstream_supplier_id, mapped)?;
            }
            TransformMode::Do => {
                // Side-effect only: the original value passes through.
                self.call_sub_value(callable, vec![value.clone()], true)?;
                self.handle_supply_forward(downstream_supplier_id, value)?;
            }
        }
        Ok(())
    }

    /// Supply.zip-latest(...) as a class method
    pub(super) fn dispatch_supply_zip_latest_class(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut with_fn: Option<Value> = None;
        let mut initial: Option<Vec<Value>> = None;
        let mut supplies: Vec<Value> = Vec::new();
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                if key == "with" {
                    with_fn = Some(value.clone());
                } else if key == "initial" {
                    if let ValueView::Array(items, ..) = value.view() {
                        initial = Some(items.to_vec());
                    }
                } else {
                    supplies.push(arg.clone());
                }
            } else {
                supplies.push(arg.clone());
            }
        }

        if supplies.is_empty() {
            return Ok(Value::make_instance(Symbol::intern("Supply"), {
                let mut a = HashMap::new();
                a.insert("values".to_string(), Value::array(Vec::new()));
                a.insert("taps".to_string(), Value::array(Vec::new()));
                a.insert("live".to_string(), Value::FALSE);
                a
            }));
        }

        // Validate all args are Supply instances
        for arg in &supplies {
            if !matches!(arg.view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
            {
                let mut ex_attrs = HashMap::new();
                ex_attrs.insert(
                    "combinator".to_string(),
                    Value::str("zip-latest".to_string()),
                );
                ex_attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Can only zip-latest Supply objects, got {}",
                        crate::value::types::what_type_name(arg)
                    )),
                );
                let ex = Value::make_instance(Symbol::intern("X::Supply::Combinator"), ex_attrs);
                let mut err = RuntimeError::new(format!(
                    "Can only zip-latest Supply objects, got {}",
                    crate::value::types::what_type_name(arg)
                ));
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
        }

        // Single supply is a noop
        if supplies.len() == 1 {
            return Ok(supplies.into_iter().next().unwrap());
        }

        // Delegate to instance method on first supply
        let first = supplies.remove(0);
        if let ValueView::Instance { attributes, .. } = first.view() {
            let mut method_args: Vec<Value> = supplies;
            if let Some(wf) = with_fn {
                method_args.push(Value::pair("with".to_string(), wf));
            }
            if let Some(init) = initial {
                method_args.push(Value::pair("initial".to_string(), Value::array(init)));
            }
            self.native_supply(&(attributes).as_map(), "zip-latest", method_args)
        } else {
            Err(RuntimeError::new(
                "Cannot call zip-latest on non-Supply".to_string(),
            ))
        }
    }
}
