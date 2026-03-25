use super::*;

impl Interpreter {
    fn is_lazy_tail_target(target: &Value) -> bool {
        match target {
            Value::LazyList(_) => true,
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            Value::GenericRange { start, end, .. } => {
                let start_f = start.to_f64();
                let end_f = end.to_f64();
                !start_f.is_finite() || !end_f.is_finite() || start_f.is_nan() || end_f.is_nan()
            }
            _ => false,
        }
    }

    pub(in crate::runtime) fn dispatch_tail(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if Self::is_lazy_tail_target(&target) {
            let mut attrs = HashMap::new();
            attrs.insert("action".to_string(), Value::str_from("tail"));
            attrs.insert(
                "message".to_string(),
                Value::str_from("Cannot .tail a lazy list"),
            );
            let exception = Value::make_instance(Symbol::intern("X::Cannot::Lazy"), attrs);
            let mut err = RuntimeError::new("X::Cannot::Lazy");
            err.exception = Some(Box::new(exception));
            return Err(err);
        }

        if let Value::Seq(items) = &target
            && items.is_empty()
        {
            let seq_id = std::sync::Arc::as_ptr(items) as usize;
            let key = format!("__mutsu_predictive_seq_iter::{seq_id}");
            if let Some(iterator) = self.env.get(&key).cloned() {
                let iter_slot = "$mutsu_predictive_tail_iterator";
                let saved_iter = self.env.get(iter_slot).cloned();
                self.env.insert(iter_slot.to_string(), iterator);

                let current_iter = self.env.get(iter_slot).cloned().unwrap_or(Value::Nil);
                let count_only = self
                    .call_method_mut_with_values(
                        iter_slot,
                        current_iter.clone(),
                        "count-only",
                        vec![],
                    )
                    .or_else(|_| {
                        self.call_method_with_values(current_iter, "count-only", vec![])
                    })?;
                let total_len = count_only.to_f64().max(0.0) as usize;
                let tail_count = if args.is_empty() {
                    if total_len == 0 { 0 } else { 1 }
                } else {
                    self.resolve_supply_tail_count(args.first(), total_len)?
                };
                let skip = total_len.saturating_sub(tail_count);
                for _ in 0..skip {
                    let current_iter = self.env.get(iter_slot).cloned().unwrap_or(Value::Nil);
                    let skipped = self
                        .call_method_mut_with_values(
                            iter_slot,
                            current_iter.clone(),
                            "skip-one",
                            vec![],
                        )
                        .or_else(|_| {
                            self.call_method_with_values(current_iter, "skip-one", vec![])
                        })?;
                    if !skipped.truthy() {
                        break;
                    }
                }
                let mut pulled = Vec::with_capacity(tail_count);
                for _ in 0..tail_count {
                    let current_iter = self.env.get(iter_slot).cloned().unwrap_or(Value::Nil);
                    let value = self
                        .call_method_mut_with_values(
                            iter_slot,
                            current_iter.clone(),
                            "pull-one",
                            vec![],
                        )
                        .or_else(|_| {
                            self.call_method_with_values(current_iter, "pull-one", vec![])
                        })?;
                    if matches!(&value, Value::Str(s) if s.as_str() == "IterationEnd")
                        || matches!(&value, Value::Package(name) if *name == Symbol::intern("IterationEnd"))
                    {
                        break;
                    }
                    pulled.push(value);
                }
                if let Some(updated_iter) = self.env.get(iter_slot).cloned() {
                    if let Value::Instance { attributes, .. } = &updated_iter {
                        for (meta_key, source_name) in attributes.iter() {
                            let Some(attr_name) = meta_key.strip_prefix("__mutsu_attr_alias::")
                            else {
                                continue;
                            };
                            let Value::Str(source_name) = source_name else {
                                continue;
                            };
                            if let Some(attr_value) = attributes.get(attr_name).cloned() {
                                self.env.insert(source_name.to_string(), attr_value);
                            }
                        }
                    }
                    self.env.insert(key, updated_iter);
                }
                if let Some(prev) = saved_iter {
                    self.env.insert(iter_slot.to_string(), prev);
                } else {
                    self.env.remove(iter_slot);
                }
                if args.is_empty() {
                    return Ok(pulled.pop().unwrap_or(Value::Nil));
                }
                return Ok(Value::array(pulled));
            }
        }

        let items = crate::runtime::utils::value_to_list(&target);
        if args.is_empty() {
            return Ok(items.last().cloned().unwrap_or(Value::Nil));
        }

        let tail_count = self.resolve_supply_tail_count(args.first(), items.len())?;
        let start = items.len().saturating_sub(tail_count);
        Ok(Value::array(items[start..].to_vec()))
    }

    pub(in crate::runtime) fn callback_uses_supply_list(callback: &Value) -> bool {
        if let Value::Sub(data) = callback {
            let dbg = format!("{:?}", data.body);
            dbg.contains("\"Supply\"") && dbg.contains("\"list\"")
        } else {
            false
        }
    }

    pub(in crate::runtime) fn split_host_port_literal(input: &str) -> (String, Option<u16>) {
        let s = input.trim();
        if s.is_empty() {
            return (String::new(), None);
        }
        if let Some(stripped) = s.strip_prefix('[')
            && let Some(end_rel) = stripped.find(']')
        {
            let end = end_rel + 1;
            let host = &s[..=end];
            let rest = &s[end + 1..];
            if let Some(port_str) = rest.strip_prefix(':')
                && let Ok(port) = port_str.parse::<u16>()
            {
                return (host.to_string(), Some(port));
            }
            return (s.to_string(), None);
        }
        if let Some((host, port_str)) = s.rsplit_once(':')
            && !host.contains(':')
            && let Ok(port) = port_str.parse::<u16>()
        {
            return (host.to_string(), Some(port));
        }
        (s.to_string(), None)
    }

    pub(in crate::runtime) fn dispatch_rotate(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Multi-dim shaped arrays cannot be rotated
        if let Value::Array(_, kind) = &target
            && *kind == crate::value::ArrayKind::Shaped
            && let Some(shape) = crate::runtime::utils::shaped_array_shape(&target)
            && shape.len() > 1
        {
            return Err(RuntimeError::illegal_on_fixed_dimension_array("rotate"));
        }
        let items = if let Some(list_items) = target.as_list_items() {
            list_items.to_vec()
        } else {
            match target {
                Value::Capture { positional, .. } => positional,
                Value::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
                _ => return Ok(Value::Nil),
            }
        };
        if items.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
        let len = items.len() as i64;
        let by = match args.first() {
            Some(Value::Int(i)) => *i,
            Some(Value::Num(n)) => *n as i64,
            Some(Value::Rat(n, d)) if *d != 0 => *n / *d,
            Some(Value::BigRat(n, d)) if *d != num_bigint::BigInt::from(0) => {
                use num_traits::ToPrimitive;
                (n / d).to_i64().unwrap_or(1)
            }
            Some(other) => other.to_string_value().parse::<i64>().unwrap_or(1),
            None => 1,
        };
        let shift = ((by % len) + len) % len;
        let mut out = vec![Value::Nil; items.len()];
        for (i, item) in items.into_iter().enumerate() {
            let dst = ((i as i64 + len - shift) % len) as usize;
            out[dst] = item;
        }
        Ok(Value::array(out))
    }
}
