use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Supply.merge(...) as a class method
    pub(super) fn dispatch_supply_merge(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Validate all args are Supply instances
        for arg in args {
            if !matches!(arg, Value::Instance { class_name, .. } if class_name == "Supply") {
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
            if let Value::Instance { attributes, .. } = arg
                && let Some(Value::Array(items, ..)) = attributes.get("values")
            {
                all_values.extend(items.iter().cloned());
            }
        }
        let mut new_attrs = HashMap::new();
        new_attrs.insert("values".to_string(), Value::array(all_values));
        new_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        new_attrs.insert("live".to_string(), Value::Bool(false));
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
            if let Value::Pair(key, value) = arg
                && key == "with"
            {
                with_fn = Some(*value.clone());
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
                    a.insert("live".to_string(), Value::Bool(false));
                    a
                },
            ));
        }

        // Validate all args are Supply instances
        for arg in &supplies {
            if !matches!(arg, Value::Instance { class_name, .. } if class_name == "Supply") {
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
            if let Value::Instance { attributes, .. } = s {
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
            if let Value::Instance { attributes, .. } = arg {
                let items = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
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
        attrs.insert("live".to_string(), Value::Bool(false));
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
            if let Value::Instance { attributes, .. } = supply {
                if let Some(Value::Int(sid)) = attributes.get("supply_id")
                    && let Some(rx) = take_supply_channel(*sid as u64)
                {
                    sources.push(SourceKind::Channel(rx));
                    continue;
                }
                let items = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                sources.push(SourceKind::Static(items));
            }
        }

        std::thread::spawn(move || {
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
        attrs.insert("supply_id".to_string(), Value::Int(zip_supply_id as i64));
        attrs.insert("live".to_string(), Value::Bool(false));
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply.from-list class method
    pub(super) fn dispatch_supply_from_list(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Implement +@ single-arg rule:
        // - Single array/list arg: iterate its elements
        // - Multiple args: each arg is one value
        let values = if args.len() == 1 {
            match &args[0] {
                Value::Array(items, ..) => items.to_vec(),
                Value::Slip(items) | Value::Seq(items) => items.to_vec(),
                Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. } => Self::value_to_list(&args[0]),
                other => vec![other.clone()],
            }
        } else {
            let mut values = Vec::new();
            for arg in args {
                match arg {
                    Value::Slip(items) => {
                        values.extend(items.iter().cloned());
                    }
                    other => values.push(other.clone()),
                }
            }
            values
        };
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::Bool(false));
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply.on-demand class method
    pub(super) fn dispatch_supply_on_demand(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let callback = args.first().cloned().unwrap_or(Value::Nil);
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::Bool(false));
        attrs.insert("on_demand_callback".to_string(), callback);
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply.signal class method
    pub(super) fn dispatch_supply_signal(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::Bool(true));
        attrs.insert("signals".to_string(), Value::array(args.to_vec()));
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply instance .grab method
    pub(super) fn dispatch_supply_grab(
        &mut self,
        attributes: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let values = match attributes.get("values") {
            Some(Value::Array(items, ..)) => items.to_vec(),
            _ => Vec::new(),
        };
        let func = args.first().cloned().unwrap_or(Value::Nil);
        let values_list = Value::array(values);
        let result = self.eval_call_on_value(func, vec![values_list])?;
        let result_values = Self::value_to_list(&result);
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(result_values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::Bool(false));
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
            match arg {
                Value::Int(i) => *i as usize,
                Value::Num(f) => *f as usize,
                Value::Str(s) => s.parse::<usize>().map_err(|_| {
                    RuntimeError::new(format!(
                        "X::Str::Numeric: Cannot convert string '{}' to a number",
                        s
                    ))
                })?,
                _ => arg.to_f64() as usize,
            }
        };
        let values = match attributes.get("values") {
            Some(Value::Array(items, ..)) => items.iter().skip(n).cloned().collect::<Vec<_>>(),
            _ => Vec::new(),
        };
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::Bool(false));
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply.elems method
    pub(super) fn dispatch_supply_elems(
        &mut self,
        attributes: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let interval = args.first().map(Value::to_f64).unwrap_or(0.0).max(0.0);

        if let Some(Value::Int(supplier_id)) = attributes.get("supplier_id")
            && *supplier_id > 0
        {
            let (emitted, done, quit_reason) =
                crate::runtime::native_methods::supplier_snapshot(*supplier_id as u64);
            let mut elems_attrs = HashMap::new();
            elems_attrs.insert("values".to_string(), Value::array(Vec::new()));
            elems_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            elems_attrs.insert("live".to_string(), Value::Bool(false));
            elems_attrs.insert("supplier_id".to_string(), Value::Int(*supplier_id));
            elems_attrs.insert("supplier_done".to_string(), Value::Bool(done));
            elems_attrs.insert("elems_filter".to_string(), Value::Bool(true));
            elems_attrs.insert("elems_interval".to_string(), Value::Num(interval));
            elems_attrs.insert(
                "elems_initial_count".to_string(),
                Value::Int(emitted.len() as i64),
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
                .map(|idx| Value::Int(idx as i64))
                .collect()
        };
        let mut elems_attrs = HashMap::new();
        elems_attrs.insert("values".to_string(), Value::array(elems_values));
        elems_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        elems_attrs.insert("live".to_string(), Value::Bool(false));
        Ok(Value::make_instance(Symbol::intern("Supply"), elems_attrs))
    }

    /// Handle Supply.map method
    pub(super) fn dispatch_supply_map(
        &mut self,
        attributes: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mapper = args.first().cloned().unwrap_or(Value::Nil);
        let source_values = if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
            let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                let mut a = HashMap::new();
                a.insert("emitted".to_string(), Value::array(Vec::new()));
                a.insert("done".to_string(), Value::Bool(false));
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
            attributes
                .get("live")
                .cloned()
                .unwrap_or(Value::Bool(false)),
        );
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Handle Supply.reduce method
    pub(super) fn dispatch_supply_reduce(
        &mut self,
        target: Value,
        attributes: &HashMap<String, Value>,
        callable: Value,
    ) -> Result<Value, RuntimeError> {
        if !matches!(
            callable,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ) {
            return Err(RuntimeError::new("must be code if specified"));
        }
        if attributes.get("supplier_id").is_some() || attributes.get("on_demand_callback").is_some()
        {
            let mut reduce_attrs = HashMap::new();
            reduce_attrs.insert("values".to_string(), Value::array(Vec::new()));
            reduce_attrs.insert("taps".to_string(), Value::array(Vec::new()));
            reduce_attrs.insert("live".to_string(), Value::Bool(false));
            reduce_attrs.insert("reduce_source".to_string(), target);
            reduce_attrs.insert("reduce_callable".to_string(), callable);
            return Ok(Value::make_instance(Symbol::intern("Supply"), reduce_attrs));
        }
        let items = self.supply_list_values(attributes, true)?;
        let reduced = self.reduce_items(callable, items)?;
        let values = if matches!(reduced, Value::Nil) {
            Vec::new()
        } else {
            vec![reduced]
        };
        let mut reduce_attrs = HashMap::new();
        reduce_attrs.insert("values".to_string(), Value::array(values));
        reduce_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        reduce_attrs.insert("live".to_string(), Value::Bool(false));
        Ok(Value::make_instance(Symbol::intern("Supply"), reduce_attrs))
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
            if let Value::Pair(key, value) = arg {
                if key == "with" {
                    with_fn = Some(*value.clone());
                } else if key == "initial" {
                    if let Value::Array(items, ..) = &**value {
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
                a.insert("live".to_string(), Value::Bool(false));
                a
            }));
        }

        // Validate all args are Supply instances
        for arg in &supplies {
            if !matches!(arg, Value::Instance { class_name, .. } if class_name == "Supply") {
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
        if let Value::Instance { attributes, .. } = &first {
            let mut method_args: Vec<Value> = supplies;
            if let Some(wf) = with_fn {
                method_args.push(Value::Pair("with".to_string(), Box::new(wf)));
            }
            if let Some(init) = initial {
                method_args.push(Value::Pair(
                    "initial".to_string(),
                    Box::new(Value::array(init)),
                ));
            }
            self.native_supply((attributes).as_map(), "zip-latest", method_args)
        } else {
            Err(RuntimeError::new(
                "Cannot call zip-latest on non-Supply".to_string(),
            ))
        }
    }
}
