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
        if args.is_empty() {
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
        // Collect values from all supplies
        let mut supply_values: Vec<Vec<Value>> = Vec::new();
        for arg in args {
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
            zipped.push(Value::array(tuple));
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
}
