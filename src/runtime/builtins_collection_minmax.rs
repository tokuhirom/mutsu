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
                !matches!(arg.view(), ValueView::Pair(_, _))
                    && !matches!(arg.view(), ValueView::ValuePair(_, _))
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
            return Ok(Value::seq(vec![]));
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
                    .map(|&i| Value::int(i as i64))
                    .collect();
                Ok(Value::seq(keys))
            }
            "v" => {
                let vals: Vec<Value> = matching_indices.iter().map(|&i| items[i].clone()).collect();
                Ok(Value::seq(vals))
            }
            "kv" => {
                let mut kvs = Vec::new();
                for &i in &matching_indices {
                    kvs.push(Value::int(i as i64));
                    kvs.push(items[i].clone());
                }
                Ok(Value::seq(kvs))
            }
            "p" => {
                let pairs: Vec<Value> = matching_indices
                    .iter()
                    .map(|&i| Value::value_pair(Value::int(i as i64), items[i].clone()))
                    .collect();
                Ok(Value::seq(pairs))
            }
            _ => Ok(result),
        }
    }

    /// Whether a value is a Range (any exclusivity) or GenericRange.
    pub(crate) fn value_is_rangey(v: &Value) -> bool {
        matches!(
            v.view(),
            ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
        )
    }

    pub(crate) fn collect_minmax_candidates(value: &Value, out: &mut Vec<Value>) {
        match value.view() {
            // Unwrap Scalar containers
            ValueView::Scalar(inner) => Self::collect_minmax_candidates(inner, out),
            // Unwrap first-class element containers (`:=`-bound / grep rw alias).
            ValueView::ContainerRef(cell) => {
                Self::collect_minmax_candidates(&cell.lock().unwrap().clone(), out)
            }
            ValueView::Range(a, b)
            | ValueView::RangeExcl(a, b)
            | ValueView::RangeExclStart(a, b)
            | ValueView::RangeExclBoth(a, b) => {
                out.push(Value::int(a));
                out.push(Value::int(b));
            }
            ValueView::GenericRange { start, end, .. } => {
                // Skip Inf..-Inf (identity for minmax combine from empty lists)
                let is_start_inf = matches!(start.as_ref().view(), ValueView::Num(n) if n.is_infinite() && n.is_sign_positive());
                let is_end_neg_inf = matches!(end.as_ref().view(), ValueView::Num(n) if n.is_infinite() && n.is_sign_negative());
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
            _ => out.push(value.clone()),
        }
    }

    pub(crate) fn make_inclusive_range_value(left: Value, right: Value) -> Value {
        match (left.view(), right.view()) {
            (ValueView::Int(a), ValueView::Int(b)) => Value::range(a, b),
            (ValueView::Int(a), ValueView::Num(b)) if b.is_infinite() && b.is_sign_positive() => {
                Value::range(a, i64::MAX)
            }
            (ValueView::Int(a), ValueView::Whatever) => Value::range(a, i64::MAX),
            (ValueView::Num(a), ValueView::Int(b)) if a.is_infinite() && a.is_sign_negative() => {
                Value::range(i64::MIN, b)
            }
            (ValueView::Str(a), ValueView::Str(b)) => Value::generic_range(
                Value::str_arc(a.clone()),
                Value::str_arc(b.clone()),
                false,
                false,
            ),
            (_, _) if left.is_numeric() && right.is_numeric() => {
                Value::generic_range(left.clone(), right.clone(), false, false)
            }
            (ValueView::Str(a), _) if right.is_numeric() => {
                Value::generic_range(Value::str_arc(a.clone()), right.clone(), false, false)
            }
            (_, ValueView::Str(b)) if left.is_numeric() => {
                Value::generic_range(left.clone(), Value::str_arc(b.clone()), false, false)
            }
            (_, ValueView::Sub(_)) | (ValueView::Sub(_), _) => {
                Value::generic_range(left, right, false, false)
            }
            _ => Value::NIL,
        }
    }

    pub(super) fn builtin_minmax(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let by = args.iter().find_map(|arg| match arg.view() {
            ValueView::Pair(name, value) if name == "by" => Some(value.clone()),
            ValueView::ValuePair(key, value)
                if matches!(key.view(), ValueView::Str(name) if name.as_str() == "by") =>
            {
                Some(value.clone())
            }
            _ => None,
        });
        let positional: Vec<Value> = args
            .iter()
            .filter(|arg| {
                !matches!(arg.view(), ValueView::Pair(name, _) if name == "by")
                    && !matches!(arg.view(), ValueView::ValuePair(key, _) if matches!(key.view(), ValueView::Str(name) if name.as_str() == "by"))
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
                match arg.view() {
                    ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. } => {
                        let items = crate::runtime::utils::value_to_list(arg);
                        candidates.extend(items);
                    }
                    _ if arg.as_list_items().is_some() => {
                        candidates.extend(arg.as_list_items().unwrap().iter().cloned());
                    }
                    _ => candidates.push(arg.clone()),
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
                Value::num(f64::INFINITY),
                Value::num(f64::NEG_INFINITY),
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
        match result.view() {
            ValueView::Int(n) => (n as i32).signum(),
            ValueView::Num(n) => {
                if n < 0.0 {
                    -1
                } else if n > 0.0 {
                    1
                } else {
                    0
                }
            }
            ValueView::Enum {
                enum_type, value, ..
            } if enum_type.resolve() == "Order" => match value {
                crate::value::EnumValue::Int(n) => (*n as i32).signum(),
                _ => 0,
            },
            _ => result.to_f64() as i32,
        }
    }
}
