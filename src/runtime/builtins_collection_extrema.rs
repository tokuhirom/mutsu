use super::*;

impl Interpreter {
    fn failure_exception_from_value(value: &Value) -> Option<Value> {
        match value {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => attributes.as_map().get("exception").cloned(),
            Value::Mixin(inner, mixins) => {
                if let Some(mixed) = mixins.get("Failure")
                    && let Some(ex) = Self::failure_exception_from_value(mixed)
                {
                    return Some(ex);
                }
                Self::failure_exception_from_value(inner)
            }
            _ => None,
        }
    }

    fn is_failure_like(value: &Value) -> bool {
        Self::failure_exception_from_value(value).is_some()
    }

    /// Determine the arity of a callable for min/max/minmax by-block dispatch.
    /// Returns 1 for sort-key extractors, 2 for comparators.
    pub(crate) fn extrema_callable_arity(&self, callable: &Value) -> usize {
        match callable {
            Value::Sub(data) => {
                if data.params.is_empty() {
                    1 // implicit $_ → arity 1
                } else {
                    data.params.len()
                }
            }
            Value::WeakSub(weak) => {
                if let Some(data) = weak.upgrade() {
                    if data.params.is_empty() {
                        1
                    } else {
                        data.params.len()
                    }
                } else {
                    2
                }
            }
            Value::Routine { .. } => {
                let (params, param_defs) = self.callable_signature(callable);
                if !param_defs.is_empty() {
                    let positional_count = param_defs
                        .iter()
                        .filter(|pd| !pd.named && !pd.slurpy && !pd.double_slurpy)
                        .count();
                    if positional_count > 0 {
                        return positional_count;
                    }
                }
                if params.is_empty() { 1 } else { params.len() }
            }
            _ => 2, // default to comparator
        }
    }

    fn extrema_from_values_by(
        &mut self,
        args: &[Value],
        want_max: bool,
        by: Option<&Value>,
    ) -> Result<Value, RuntimeError> {
        // Determine if by-block is arity-1 (sort key) or arity-2 (comparator)
        // before borrowing self mutably for the block-call closure.
        let by_arity = by.map(|b| self.extrema_callable_arity(b));
        Self::extrema_from_values_generic(args, want_max, by, by_arity, |c, a| {
            self.call_sub_value(c.clone(), a, true)
        })
    }

    /// Engine-agnostic min/max fold over a flat list of values (the genuine
    /// Category-B fork is the `:by` block, which each engine supplies through
    /// `call`). Shared by the interpreter (`extrema_from_values_by`) and the VM
    /// (`try_native_extrema`) so the flatten / failure / `Any`-filter / fold
    /// logic exists once. `by_arity` is the resolved arity of `by`
    /// (`extrema_callable_arity`); 1 = sort-key mapper, otherwise a comparator.
    pub(crate) fn extrema_from_values_generic<F>(
        args: &[Value],
        want_max: bool,
        by: Option<&Value>,
        by_arity: Option<usize>,
        mut call: F,
    ) -> Result<Value, RuntimeError>
    where
        F: FnMut(&Value, Vec<Value>) -> Result<Value, RuntimeError>,
    {
        if args.is_empty() {
            return Ok(Value::Nil);
        }

        // Flatten: if a single array/seq/list arg is provided, use its items
        let expanded: Vec<Value> = if args.len() == 1 {
            if let Some(items) = args[0].as_list_items() {
                items.to_vec()
            } else {
                args.to_vec()
            }
        } else {
            args.to_vec()
        };

        if expanded.is_empty() {
            // Empty list: return Inf for min, -Inf for max
            return Ok(if want_max {
                Value::Num(f64::NEG_INFINITY)
            } else {
                Value::Num(f64::INFINITY)
            });
        }

        let failures: Vec<Value> = expanded
            .iter()
            .filter(|v| Self::is_failure_like(v))
            .cloned()
            .collect();
        if failures.len() >= 2
            && let Some(ex) = Self::failure_exception_from_value(&failures[0])
        {
            let mut err = RuntimeError::new(ex.to_string_value());
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        if failures.len() == 1 {
            return Ok(failures[0].clone());
        }

        let mut filtered: Vec<Value> = expanded
            .iter()
            .filter(|v| !matches!(v, Value::Package(name) if name == "Any"))
            .cloned()
            .collect();
        if filtered.is_empty() {
            return Ok(args[0].clone());
        }

        let mut best = filtered.remove(0);
        for value in filtered {
            let cmp = if let Some(by_block) = by {
                if by_arity == Some(1) {
                    // Arity-1: use as sort key extractor
                    let key_a = call(by_block, vec![value.clone()])?;
                    let key_b = call(by_block, vec![best.clone()])?;
                    crate::runtime::compare_values(&key_a, &key_b)
                } else {
                    // Arity-2: use as comparator ($^a, $^b) => Order
                    let result = call(by_block, vec![value.clone(), best.clone()])?;
                    Self::order_to_int(&result)
                }
            } else {
                crate::runtime::compare_values(&value, &best)
            };
            if (want_max && cmp > 0) || (!want_max && cmp < 0) {
                best = value;
            }
        }
        Ok(best)
    }

    fn extrema_from_hash(
        &mut self,
        map: &std::sync::Arc<crate::value::HashData>,
        by: Option<Value>,
        want_max: bool,
    ) -> Result<Value, RuntimeError> {
        let mut best_pair: Option<Value> = None;
        let mut best_key: Option<Value> = None;

        for (k, v) in map.as_ref().iter() {
            // Object hashes store `.WHICH` string keys; expose the original
            // typed key both in the resulting Pair and in the default ordering.
            let pair = map.typed_pair(k, v.clone());
            let key = if let Some(by_callable) = &by {
                match self.call_sub_value(by_callable.clone(), vec![pair.clone()], true) {
                    Ok(value) => value,
                    Err(_) => v.clone(),
                }
            } else {
                map.typed_key(k)
            };

            let replace = if let Some(current_key) = &best_key {
                let cmp = crate::runtime::compare_values(&key, current_key);
                (want_max && cmp > 0) || (!want_max && cmp < 0)
            } else {
                true
            };
            if replace {
                best_pair = Some(pair);
                best_key = Some(key);
            }
        }

        Ok(best_pair.unwrap_or(Value::Nil))
    }

    /// Extract named adverbs (:k, :v, :kv, :p) from args for min/max.
    pub(crate) fn extract_extrema_adverbs(args: &[Value]) -> (Option<String>, Vec<Value>) {
        let mut adverb = None;
        let mut positional = Vec::new();
        let mut by_found = false;
        for arg in args {
            match arg {
                Value::Pair(name, _) if name == "by" => {
                    by_found = true;
                    positional.push(arg.clone()); // keep by pair for later extraction
                }
                Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by") =>
                {
                    by_found = true;
                    positional.push(arg.clone());
                }
                // `:k`/`:v`/`:kv`/`:p` select what the extremum reports. A negated
                // form (`:!k`, Bool false) means "the value" (the default), but in
                // either case the adverb pair must be stripped from the positional
                // candidates so it never participates in the min/max comparison.
                Value::Pair(name, value) if matches!(name.as_str(), "k" | "v" | "kv" | "p") => {
                    if matches!(value.as_ref(), Value::Bool(true)) {
                        adverb = Some(name.clone());
                    }
                }
                _ => positional.push(arg.clone()),
            }
        }
        let _ = by_found;
        (adverb, positional)
    }

    pub(super) fn builtin_min(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let (adverb, filtered_args) = Self::extract_extrema_adverbs(args);
        if let Some(r) = self.range_extrema_sub_form(&filtered_args, "min", adverb.as_deref()) {
            return r;
        }
        let result = self.builtin_min_inner(&filtered_args)?;
        self.apply_extrema_adverb(&filtered_args, result, false, adverb.as_deref())
    }

    /// The sub forms `min($range, :k)` / `max($range, :kv)` must use the same
    /// Range-aware index logic as the method forms. When the only candidate is a
    /// Range (no `:by`), delegate to the method dispatch.
    fn range_extrema_sub_form(
        &mut self,
        filtered_args: &[Value],
        method: &str,
        adverb: Option<&str>,
    ) -> Option<Result<Value, RuntimeError>> {
        let [only] = filtered_args else { return None };
        if !Self::value_is_rangey(only) {
            return None;
        }
        let has_by = filtered_args.iter().any(|a| {
            matches!(a, Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. })
                || matches!(a, Value::Pair(n, _) if n == "by")
        });
        if has_by {
            return None;
        }
        let method_args = match adverb {
            Some(a) => vec![Value::Pair(a.to_string(), Box::new(Value::Bool(true)))],
            None => vec![],
        };
        Some(self.call_method_with_values(only.clone(), method, method_args))
    }

    fn builtin_min_inner(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let by = args.iter().find_map(|arg| match arg {
            Value::Pair(name, value) if name == "by" => Some((**value).clone()),
            Value::ValuePair(key, value)
                if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by") =>
            {
                Some((**value).clone())
            }
            _ => None,
        });
        let positional: Vec<Value> = args
            .iter()
            .filter(|arg| {
                !matches!(arg, Value::Pair(name, _) if name == "by")
                    && !matches!(arg, Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by"))
            })
            .cloned()
            .collect();

        if positional.len() == 1
            && let Value::Hash(map) = &positional[0]
        {
            return self.extrema_from_hash(map, by, false);
        }
        if positional.len() == 1
            && let Value::Instance { class_name, .. } = &positional[0]
            && class_name == "Hash"
        {
            let method_args = by.map_or_else(Vec::new, |v| vec![v]);
            return self.call_method_with_values(positional[0].clone(), "min", method_args);
        }
        self.extrema_from_values_by(&positional, false, by.as_ref())
    }

    pub(super) fn builtin_max(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let (adverb, filtered_args) = Self::extract_extrema_adverbs(args);
        if let Some(r) = self.range_extrema_sub_form(&filtered_args, "max", adverb.as_deref()) {
            return r;
        }
        let result = self.builtin_max_inner(&filtered_args)?;
        self.apply_extrema_adverb(&filtered_args, result, true, adverb.as_deref())
    }

    fn builtin_max_inner(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let by = args.iter().find_map(|arg| match arg {
            Value::Pair(name, value) if name == "by" => Some((**value).clone()),
            Value::ValuePair(key, value)
                if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by") =>
            {
                Some((**value).clone())
            }
            _ => None,
        });
        let positional: Vec<Value> = args
            .iter()
            .filter(|arg| {
                !matches!(arg, Value::Pair(name, _) if name == "by")
                    && !matches!(arg, Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by"))
            })
            .cloned()
            .collect();

        if positional.len() == 1
            && let Value::Hash(map) = &positional[0]
        {
            return self.extrema_from_hash(map, by, true);
        }
        if positional.len() == 1
            && let Value::Instance { class_name, .. } = &positional[0]
            && class_name == "Hash"
        {
            let method_args = by.map_or_else(Vec::new, |v| vec![v]);
            return self.call_method_with_values(positional[0].clone(), "max", method_args);
        }
        self.extrema_from_values_by(&positional, true, by.as_ref())
    }
}
