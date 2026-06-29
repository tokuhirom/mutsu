use super::*;

impl Interpreter {
    /// Apply :k, :v, :kv, :p adverbs to min/max result.
    /// These return index/value/both/pair information about the extremum.
    pub(crate) fn apply_extrema_adverb(
        &self,
        args: &[Value],
        result: Value,
        _want_max: bool,
        adverb: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        let adverb = match adverb {
            Some(a) => a,
            None => return Ok(result),
        };

        // Get the list items from the first positional arg. (Ranges are routed to
        // the Range-aware scalar path in `dispatch_min_max_method` /
        // `range_extrema_sub_form` and never reach here, so a plain List's
        // min/max with adverbs reports ALL matching indices as a list.)
        let positional: Vec<Value> = args
            .iter()
            .filter(|arg| {
                !matches!(arg, Value::Pair(_, _)) && !matches!(arg, Value::ValuePair(_, _))
            })
            .cloned()
            .collect();

        // Flatten list items
        let items: Vec<Value> = if positional.len() == 1 {
            if let Some(list) = positional[0].as_list_items() {
                list.to_vec()
            } else {
                positional
            }
        } else {
            positional
        };

        if items.is_empty() {
            return Ok(Value::Seq(std::sync::Arc::new(vec![])));
        }

        // Find all indices matching the extremum
        let mut matching_indices = Vec::new();
        for (i, item) in items.iter().enumerate() {
            let cmp = crate::runtime::compare_values(item, &result);
            if cmp == 0 {
                matching_indices.push(i);
            }
        }

        match adverb {
            "k" => {
                let keys: Vec<Value> = matching_indices
                    .iter()
                    .map(|&i| Value::Int(i as i64))
                    .collect();
                Ok(Value::Seq(std::sync::Arc::new(keys)))
            }
            "v" => {
                let vals: Vec<Value> = matching_indices.iter().map(|&i| items[i].clone()).collect();
                Ok(Value::Seq(std::sync::Arc::new(vals)))
            }
            "kv" => {
                let mut kvs = Vec::new();
                for &i in &matching_indices {
                    kvs.push(Value::Int(i as i64));
                    kvs.push(items[i].clone());
                }
                Ok(Value::Seq(std::sync::Arc::new(kvs)))
            }
            "p" => {
                let pairs: Vec<Value> = matching_indices
                    .iter()
                    .map(|&i| {
                        Value::ValuePair(Box::new(Value::Int(i as i64)), Box::new(items[i].clone()))
                    })
                    .collect();
                Ok(Value::Seq(std::sync::Arc::new(pairs)))
            }
            _ => Ok(result),
        }
    }

    /// Whether a value is a Range (any exclusivity) or GenericRange.
    pub(crate) fn value_is_rangey(v: &Value) -> bool {
        matches!(
            v,
            Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }
        )
    }

    pub(crate) fn collect_minmax_candidates(value: &Value, out: &mut Vec<Value>) {
        match value {
            // Unwrap Scalar containers
            Value::Scalar(inner) => Self::collect_minmax_candidates(inner, out),
            // Unwrap first-class element containers (`:=`-bound / grep rw alias).
            Value::ContainerRef(cell) => {
                Self::collect_minmax_candidates(&cell.lock().unwrap().clone(), out)
            }
            Value::Range(a, b)
            | Value::RangeExcl(a, b)
            | Value::RangeExclStart(a, b)
            | Value::RangeExclBoth(a, b) => {
                out.push(Value::Int(*a));
                out.push(Value::Int(*b));
            }
            Value::GenericRange { start, end, .. } => {
                // Skip Inf..-Inf (identity for minmax combine from empty lists)
                let is_start_inf = matches!(start.as_ref(), Value::Num(n) if n.is_infinite() && n.is_sign_positive());
                let is_end_neg_inf = matches!(end.as_ref(), Value::Num(n) if n.is_infinite() && n.is_sign_negative());
                if is_start_inf && is_end_neg_inf {
                    return; // skip identity range
                }
                out.push((**start).clone());
                out.push((**end).clone());
            }
            _ if value.as_list_items().is_some() => {
                // Recursively collect from list items so itemized sub-ranges
                // within a tuple get their endpoints extracted
                for item in value.as_list_items().unwrap().iter() {
                    Self::collect_minmax_candidates(item, out);
                }
            }
            other => out.push(other.clone()),
        }
    }

    pub(crate) fn make_inclusive_range_value(left: Value, right: Value) -> Value {
        match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::Range(*a, *b),
            (Value::Int(a), Value::Num(b)) if b.is_infinite() && b.is_sign_positive() => {
                Value::Range(*a, i64::MAX)
            }
            (Value::Int(a), Value::Whatever) => Value::Range(*a, i64::MAX),
            (Value::Num(a), Value::Int(b)) if a.is_infinite() && a.is_sign_negative() => {
                Value::Range(i64::MIN, *b)
            }
            (Value::Str(a), Value::Str(b)) => Value::GenericRange {
                start: Arc::new(Value::Str(a.clone())),
                end: Arc::new(Value::Str(b.clone())),
                excl_start: false,
                excl_end: false,
            },
            (l, r) if l.is_numeric() && r.is_numeric() => Value::GenericRange {
                start: Arc::new(l.clone()),
                end: Arc::new(r.clone()),
                excl_start: false,
                excl_end: false,
            },
            (Value::Str(a), r) if r.is_numeric() => Value::GenericRange {
                start: Arc::new(Value::Str(a.clone())),
                end: Arc::new(r.clone()),
                excl_start: false,
                excl_end: false,
            },
            (l, Value::Str(b)) if l.is_numeric() => Value::GenericRange {
                start: Arc::new(l.clone()),
                end: Arc::new(Value::Str(b.clone())),
                excl_start: false,
                excl_end: false,
            },
            (_, Value::Sub(_)) | (Value::Sub(_), _) => Value::GenericRange {
                start: Arc::new(left),
                end: Arc::new(right),
                excl_start: false,
                excl_end: false,
            },
            _ => Value::Nil,
        }
    }

    pub(super) fn builtin_minmax(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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

        // Resolve by-block arity before borrowing self mutably for the closure.
        let by_arity = by.as_ref().map(|b| self.extrema_callable_arity(b));
        Self::minmax_from_values_generic(&positional, by.as_ref(), by_arity, |c, a| {
            self.call_sub_value(c.clone(), a, true)
        })
    }

    /// Engine-agnostic `minmax` over a list: collect candidates, fold the min and
    /// the max (using the `:by` block — the genuine Category-B fork — through
    /// `call`), and build the inclusive `min..max` range. Shared by the
    /// interpreter (`builtin_minmax`) and the VM (`try_native_minmax`) so the
    /// candidate / fold / range-build logic exists once. `by_arity` is the
    /// resolved arity of `by` (1 = sort-key mapper, otherwise a comparator).
    pub(crate) fn minmax_from_values_generic<F>(
        positional: &[Value],
        by: Option<&Value>,
        by_arity: Option<usize>,
        mut call: F,
    ) -> Result<Value, RuntimeError>
    where
        F: FnMut(&Value, Vec<Value>) -> Result<Value, RuntimeError>,
    {
        let mut candidates = Vec::new();
        if by.is_some() {
            // When a comparator is provided, enumerate ranges fully
            for arg in positional {
                match arg {
                    Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. } => {
                        let items = crate::runtime::utils::value_to_list(arg);
                        candidates.extend(items);
                    }
                    _ if arg.as_list_items().is_some() => {
                        candidates.extend(arg.as_list_items().unwrap().iter().cloned());
                    }
                    other => candidates.push(other.clone()),
                }
            }
        } else {
            for arg in positional {
                Self::collect_minmax_candidates(arg, &mut candidates);
            }
        }
        if candidates.is_empty() {
            // Return Inf..-Inf for empty list (Raku behavior)
            return Ok(Self::make_inclusive_range_value(
                Value::Num(f64::INFINITY),
                Value::Num(f64::NEG_INFINITY),
            ));
        }

        if let Some(by_block) = by {
            let by_arity = by_arity.unwrap_or(2);
            let mut min_value = candidates[0].clone();
            let mut max_value = candidates[0].clone();
            for value in candidates.iter().skip(1) {
                let cmp_min = if by_arity == 1 {
                    let key_a = call(by_block, vec![value.clone()])?;
                    let key_b = call(by_block, vec![min_value.clone()])?;
                    crate::runtime::compare_values(&key_a, &key_b)
                } else {
                    let result = call(by_block, vec![value.clone(), min_value.clone()])?;
                    Self::order_to_int(&result)
                };
                let cmp_max = if by_arity == 1 {
                    let key_a = call(by_block, vec![value.clone()])?;
                    let key_b = call(by_block, vec![max_value.clone()])?;
                    crate::runtime::compare_values(&key_a, &key_b)
                } else {
                    let result = call(by_block, vec![value.clone(), max_value.clone()])?;
                    Self::order_to_int(&result)
                };
                if cmp_min < 0 {
                    min_value = value.clone();
                }
                if cmp_max > 0 {
                    max_value = value.clone();
                }
            }
            Ok(Self::make_inclusive_range_value(min_value, max_value))
        } else {
            let mut min_value = candidates[0].clone();
            let mut max_value = candidates[0].clone();
            for value in candidates.iter().skip(1) {
                if crate::runtime::compare_values(value, &min_value) < 0 {
                    min_value = value.clone();
                }
                if crate::runtime::compare_values(value, &max_value) > 0 {
                    max_value = value.clone();
                }
            }
            Ok(Self::make_inclusive_range_value(min_value, max_value))
        }
    }

    /// Convert an Order/Int/Num value from a comparator callback to an integer.
    pub(super) fn order_to_int(result: &Value) -> i32 {
        match result {
            Value::Int(n) => (*n as i32).signum(),
            Value::Num(n) => {
                if *n < 0.0 {
                    -1
                } else if *n > 0.0 {
                    1
                } else {
                    0
                }
            }
            Value::Enum {
                enum_type, value, ..
            } if enum_type.resolve() == "Order" => match value {
                crate::value::EnumValue::Int(n) => (*n as i32).signum(),
                _ => 0,
            },
            _ => result.to_f64() as i32,
        }
    }
}
