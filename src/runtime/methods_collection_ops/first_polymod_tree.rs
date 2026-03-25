use super::*;

impl Interpreter {
    pub(in crate::runtime) fn dispatch_first(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Separate named args (Pairs) from positional args
        let mut positional = Vec::new();
        let mut has_neg_v = false;
        let mut has_end = false;
        let mut has_k = false;
        let mut has_kv = false;
        let mut has_p = false;
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "v" => {
                    if !value.truthy() {
                        has_neg_v = true;
                    }
                }
                Value::Pair(key, value) if key == "end" => {
                    if value.truthy() {
                        has_end = true;
                    }
                }
                Value::Pair(key, value) if key == "k" => {
                    has_k = value.truthy();
                }
                Value::Pair(key, value) if key == "kv" => {
                    has_kv = value.truthy();
                }
                Value::Pair(key, value) if key == "p" => {
                    has_p = value.truthy();
                }
                _ => positional.push(arg.clone()),
            }
        }
        if has_neg_v {
            return Err(RuntimeError::new(
                "Throwing `:!v` on first is not supported",
            ));
        }
        // Check for Bool matcher (X::Match::Bool)
        if matches!(positional.first(), Some(Value::Bool(_))) {
            let mut err = RuntimeError::new("Cannot use Bool as a matcher");
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Match::Bool"),
                std::collections::HashMap::new(),
            )));
            return Err(err);
        }
        let func = positional.first().cloned();

        // Supply.first returns a new Supply containing the matched value
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Supply"
        {
            return self.dispatch_supply_first(attributes, func, has_end);
        }

        let items = crate::runtime::utils::value_to_list(&target);
        if let Some((idx, value)) = self.find_first_match_over_items(func, &items, has_end)? {
            return Ok(super::super::builtins_collection::format_first_result(
                idx, value, has_k, has_kv, has_p,
            ));
        }
        Ok(Value::Nil)
    }

    fn dispatch_supply_first(
        &mut self,
        attributes: &HashMap<String, Value>,
        func: Option<Value>,
        has_end: bool,
    ) -> Result<Value, RuntimeError> {
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
            attributes
                .get("values")
                .and_then(|v| {
                    if let Value::Array(items, ..) = v {
                        Some(items.to_vec())
                    } else {
                        None
                    }
                })
                .unwrap_or_default()
        };

        let result_values = if let Some((_, value)) =
            self.find_first_match_over_items(func, &source_values, has_end)?
        {
            vec![value]
        } else {
            Vec::new()
        };

        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(result_values));
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

    /// `$n.polymod(@divisors)` — successive modular decomposition.
    pub(in crate::runtime) fn method_polymod(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        fn val_to_f64(v: &Value) -> f64 {
            match v {
                Value::Int(n) => *n as f64,
                Value::Num(n) => *n,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::Bool(b) => {
                    if *b {
                        1.0
                    } else {
                        0.0
                    }
                }
                Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
                _ => 0.0,
            }
        }
        fn f64_to_val(n: f64) -> Value {
            if n.is_finite() && n == n.trunc() && n.abs() < i64::MAX as f64 {
                Value::Int(n as i64)
            } else {
                Value::Num(n)
            }
        }
        fn flatten_to_list(v: &Value) -> Vec<Value> {
            match v {
                Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                    items.as_ref().clone()
                }
                Value::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
                _ => vec![v.clone()],
            }
        }
        let mut n = val_to_f64(target);
        // Flatten args into a list of divisors, tracking if any source is infinite
        let mut divisors = Vec::new();
        let mut has_infinite = false;
        for arg in args {
            match arg {
                Value::LazyList(_) => {
                    has_infinite = true;
                    divisors.extend(flatten_to_list(arg));
                }
                Value::Array(..) | Value::Seq(_) | Value::Slip(_) => {
                    divisors.extend(flatten_to_list(arg));
                }
                _ => divisors.push(arg.clone()),
            }
        }
        let mut result = Vec::new();
        let mut stopped = false;
        for d in &divisors {
            let d_val = val_to_f64(d);
            if d_val == 0.0 {
                result.push(f64_to_val(n));
                n = f64::INFINITY;
                continue;
            }
            // For infinite lists, stop when n reaches 0
            if has_infinite && n == 0.0 {
                stopped = true;
                break;
            }
            // Modulo 1 always yields remainder 0 and quotient = n; for infinite lists
            // this would loop forever, so push n directly and stop
            if has_infinite && d_val == 1.0 {
                result.push(f64_to_val(n));
                stopped = true;
                break;
            }
            let rem = n % d_val;
            let quot = ((n - rem) / d_val).trunc();
            result.push(f64_to_val(rem));
            n = quot;
        }
        if !stopped {
            result.push(f64_to_val(n));
        }
        Ok(Value::array(result))
    }

    pub(in crate::runtime) fn dispatch_tree(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Non-iterable: .tree(anything) returns self
        let items = match &target {
            Value::Array(items, ..) => items.clone(),
            _ => return Ok(target),
        };

        let arg = &args[0];
        match arg {
            // .tree(0) — identity
            Value::Int(0) => Ok(target),
            // .tree(n) — tree to n levels
            Value::Int(n) if *n > 0 => Ok(Value::array(self.tree_depth(&items, *n as usize)?)),
            // .tree(*) or .tree(Inf) — full depth (same as .tree())
            Value::Whatever => Ok(Value::array(self.tree_depth(&items, usize::MAX)?)),
            Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                Ok(Value::array(self.tree_depth(&items, usize::MAX)?))
            }
            // .tree([&first, *@rest]) — array of closures form
            Value::Array(closure_list, ..) => {
                if closure_list.is_empty() {
                    return Ok(target);
                }
                let first = &closure_list[0];
                self.call_sub_value(first.clone(), vec![target], false)
            }
            // .tree(&closure, ...) — apply closures at depth levels
            Value::Sub(_) => {
                let closures: Vec<Value> = args.to_vec();
                self.tree_with_closures(&items, &closures, 0)
            }
            _ => Ok(target),
        }
    }

    pub(in crate::runtime) fn tree_depth(
        &mut self,
        items: &[Value],
        depth: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut result = Vec::new();
        for item in items {
            match item {
                Value::Array(inner, ..) if depth > 0 => {
                    result.push(Value::array(self.tree_depth(inner, depth - 1)?));
                }
                other => result.push(other.clone()),
            }
        }
        Ok(result)
    }

    pub(in crate::runtime) fn tree_with_closures(
        &mut self,
        items: &[Value],
        closures: &[Value],
        depth: usize,
    ) -> Result<Value, RuntimeError> {
        let mut processed = Vec::new();
        for item in items {
            match item {
                Value::Array(inner, ..) if closures.len() > depth + 1 => {
                    let sub_result = self.tree_with_closures(inner, closures, depth + 1)?;
                    processed.push(sub_result);
                }
                Value::Array(inner, ..) => {
                    // Apply the last closure to leaf arrays
                    if let Some(closure) = closures.last()
                        && closures.len() > 1
                    {
                        processed.push(self.call_sub_value(
                            closure.clone(),
                            vec![Value::Array(inner.clone(), ArrayKind::List)],
                            false,
                        )?);
                    } else {
                        processed.push(Value::Array(inner.clone(), ArrayKind::List)); // already Arc-wrapped
                    }
                }
                other => processed.push(other.clone()),
            }
        }
        // Apply the closure at this depth level
        if let Some(closure) = closures.get(depth) {
            self.call_sub_value(closure.clone(), vec![Value::array(processed)], false)
        } else {
            Ok(Value::array(processed))
        }
    }
}
