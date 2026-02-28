use super::*;
use std::collections::HashMap as StdHashMap;

/// Format the result of `first()` according to adverb flags (:k, :kv, :p).
pub(super) fn format_first_result(
    idx: usize,
    value: Value,
    has_k: bool,
    has_kv: bool,
    has_p: bool,
) -> Value {
    if has_k {
        Value::Int(idx as i64)
    } else if has_kv {
        Value::array(vec![Value::Int(idx as i64), value])
    } else if has_p {
        Value::ValuePair(Box::new(Value::Int(idx as i64)), Box::new(value))
    } else {
        value
    }
}

impl Interpreter {
    pub(super) fn builtin_end(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            let msg = format!(
                "Calling end({}) will never work with signature of the proto ($, *%)",
                std::iter::repeat_n("Int", args.len())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            let mut attrs = StdHashMap::new();
            attrs.insert("message".to_string(), Value::Str(msg.clone()));
            let ex = Value::make_instance("X::TypeCheck::Argument".to_string(), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        let elems = self.builtin_elems(args)?;
        match elems {
            Value::Int(n) => Ok(Value::Int(n - 1)),
            _ => Ok(Value::Int(0)),
        }
    }

    pub(super) fn builtin_elems(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            let msg = format!(
                "Calling elems({}) will never work with signature of the proto ($, *%)",
                std::iter::repeat_n("Int", args.len())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            let mut attrs = StdHashMap::new();
            attrs.insert("message".to_string(), Value::Str(msg.clone()));
            let ex = Value::make_instance("X::TypeCheck::Argument".to_string(), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        let val = &args[0];
        Ok(match val {
            Value::Array(items, ..) => Value::Int(items.len() as i64),
            Value::LazyList(list) => Value::Int(self.force_lazy_list(list)?.len() as i64),
            Value::Hash(items) => Value::Int(items.len() as i64),
            Value::Str(s) => Value::Int(s.chars().count() as i64),
            _ => Value::Int(1),
        })
    }

    pub(super) fn builtin_set(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut elems = HashSet::new();
        for arg in args {
            match arg {
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        elems.insert(item.to_string_value());
                    }
                }
                other => {
                    elems.insert(other.to_string_value());
                }
            }
        }
        Ok(Value::set(elems))
    }

    pub(super) fn builtin_bag(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut counts: HashMap<String, i64> = HashMap::new();
        for arg in args {
            match arg {
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        *counts.entry(item.to_string_value()).or_insert(0) += 1;
                    }
                }
                other => {
                    *counts.entry(other.to_string_value()).or_insert(0) += 1;
                }
            }
        }
        Ok(Value::bag(counts))
    }

    pub(super) fn builtin_mix(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut weights: HashMap<String, f64> = HashMap::new();
        for arg in args {
            match arg {
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        match item {
                            Value::Pair(k, v) => {
                                let w = match v.as_ref() {
                                    Value::Int(i) => *i as f64,
                                    Value::Num(n) => *n,
                                    Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                                    _ => 1.0,
                                };
                                *weights.entry(k.clone()).or_insert(0.0) += w;
                            }
                            Value::ValuePair(k, v) => {
                                let w = match v.as_ref() {
                                    Value::Int(i) => *i as f64,
                                    Value::Num(n) => *n,
                                    Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                                    _ => 1.0,
                                };
                                *weights.entry(k.to_string_value()).or_insert(0.0) += w;
                            }
                            _ => {
                                *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                            }
                        }
                    }
                }
                other => {
                    *weights.entry(other.to_string_value()).or_insert(0.0) += 1.0;
                }
            }
        }
        Ok(Value::mix(weights))
    }

    pub(super) fn builtin_hash(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut flat_values = Vec::new();
        for arg in args {
            flat_values.extend(Self::value_to_list(arg));
        }
        crate::runtime::utils::build_hash_from_items(flat_values)
    }

    pub(super) fn builtin_junction(
        &self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let kind = match name {
            "any" => JunctionKind::Any,
            "all" => JunctionKind::All,
            "one" => JunctionKind::One,
            _ => JunctionKind::None,
        };
        let mut elems = Vec::new();
        for arg in args {
            match arg {
                Value::Array(items, ..) => elems.extend(items.iter().cloned()),
                other => elems.push(other),
            }
        }
        Ok(Value::junction(kind, elems))
    }

    pub(super) fn builtin_pair(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let key = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let val = args.get(1).cloned().unwrap_or(Value::Nil);
        Ok(Value::Pair(key, Box::new(val)))
    }

    pub(super) fn builtin_keys(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "keys")
    }

    pub(super) fn builtin_values(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "values")
    }

    pub(super) fn builtin_kv(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "kv")
    }

    pub(super) fn builtin_pairs(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "pairs")
    }

    fn builtin_unary_collection_method(
        &self,
        args: &[Value],
        method: &str,
    ) -> Result<Value, RuntimeError> {
        let target = args.first().cloned().unwrap_or(Value::Nil);
        crate::builtins::native_method_0arg(&target, method)
            .unwrap_or_else(|| Ok(Value::array(Vec::new())))
    }

    pub(super) fn builtin_abs(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Int(i)) => Value::Int(i.abs()),
            Some(Value::Num(f)) => Value::Num(f.abs()),
            _ => Value::Int(0),
        })
    }

    fn failure_exception_from_value(value: &Value) -> Option<Value> {
        match value {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => attributes.get("exception").cloned(),
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

    fn extrema_from_values(
        &mut self,
        args: &[Value],
        want_max: bool,
    ) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::Nil);
        }

        let failures: Vec<Value> = args
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

        let mut filtered: Vec<Value> = args
            .iter()
            .filter(|v| !matches!(v, Value::Package(name) if name == "Any"))
            .cloned()
            .collect();
        if filtered.is_empty() {
            return Ok(args[0].clone());
        }
        let mut best = filtered.remove(0);
        for value in filtered {
            let cmp = crate::runtime::compare_values(&value, &best);
            if (want_max && cmp > 0) || (!want_max && cmp < 0) {
                best = value;
            }
        }
        Ok(best)
    }

    fn extrema_from_hash(
        &mut self,
        map: &std::sync::Arc<HashMap<String, Value>>,
        by: Option<Value>,
        want_max: bool,
    ) -> Result<Value, RuntimeError> {
        let mut best_pair: Option<Value> = None;
        let mut best_key: Option<Value> = None;

        for (k, v) in map.as_ref() {
            let pair = Value::Pair(k.clone(), Box::new(v.clone()));
            let key = if let Some(by_callable) = &by {
                match self.call_sub_value(by_callable.clone(), vec![pair.clone()], true) {
                    Ok(value) => value,
                    Err(_) => v.clone(),
                }
            } else {
                Value::Str(k.clone())
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

    pub(super) fn builtin_min(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let by = args.iter().find_map(|arg| match arg {
            Value::Pair(name, value) if name == "by" => Some((**value).clone()),
            Value::ValuePair(key, value)
                if matches!(key.as_ref(), Value::Str(name) if name == "by") =>
            {
                Some((**value).clone())
            }
            _ => None,
        });
        let positional: Vec<Value> = args
            .iter()
            .filter(|arg| {
                !matches!(arg, Value::Pair(name, _) if name == "by")
                    && !matches!(arg, Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name == "by"))
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
        self.extrema_from_values(&positional, false)
    }

    pub(super) fn builtin_max(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let by = args.iter().find_map(|arg| match arg {
            Value::Pair(name, value) if name == "by" => Some((**value).clone()),
            Value::ValuePair(key, value)
                if matches!(key.as_ref(), Value::Str(name) if name == "by") =>
            {
                Some((**value).clone())
            }
            _ => None,
        });
        let positional: Vec<Value> = args
            .iter()
            .filter(|arg| {
                !matches!(arg, Value::Pair(name, _) if name == "by")
                    && !matches!(arg, Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name == "by"))
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
        self.extrema_from_values(&positional, true)
    }

    fn collect_minmax_candidates(value: &Value, out: &mut Vec<Value>) {
        match value {
            Value::Range(a, b)
            | Value::RangeExcl(a, b)
            | Value::RangeExclStart(a, b)
            | Value::RangeExclBoth(a, b) => {
                out.push(Value::Int(*a));
                out.push(Value::Int(*b));
            }
            Value::GenericRange { start, end, .. } => {
                out.push((**start).clone());
                out.push((**end).clone());
            }
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                out.extend(items.iter().cloned());
            }
            other => out.push(other.clone()),
        }
    }

    fn make_inclusive_range_value(left: Value, right: Value) -> Value {
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
                start: Box::new(Value::Str(a.clone())),
                end: Box::new(Value::Str(b.clone())),
                excl_start: false,
                excl_end: false,
            },
            (l, r) if l.is_numeric() && r.is_numeric() => Value::GenericRange {
                start: Box::new(l.clone()),
                end: Box::new(r.clone()),
                excl_start: false,
                excl_end: false,
            },
            (Value::Str(a), r) if r.is_numeric() => Value::GenericRange {
                start: Box::new(Value::Str(a.clone())),
                end: Box::new(r.clone()),
                excl_start: false,
                excl_end: false,
            },
            (l, Value::Str(b)) if l.is_numeric() => Value::GenericRange {
                start: Box::new(l.clone()),
                end: Box::new(Value::Str(b.clone())),
                excl_start: false,
                excl_end: false,
            },
            (_, Value::Sub(_)) | (Value::Sub(_), _) => Value::GenericRange {
                start: Box::new(left),
                end: Box::new(right),
                excl_start: false,
                excl_end: false,
            },
            _ => Value::Nil,
        }
    }

    pub(super) fn builtin_minmax(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut candidates = Vec::new();
        for arg in args {
            Self::collect_minmax_candidates(arg, &mut candidates);
        }
        if candidates.is_empty() {
            return Ok(Value::Nil);
        }

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

    pub(super) fn builtin_shift(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(match args.first().cloned() {
            Some(Value::Array(mut items, ..)) => {
                if items.is_empty() {
                    Value::Nil
                } else {
                    Arc::make_mut(&mut items).remove(0)
                }
            }
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_pop(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(match args.first().cloned() {
            Some(Value::Array(mut items, ..)) => {
                Arc::make_mut(&mut items).pop().unwrap_or(Value::Nil)
            }
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_join(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let sep = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if args.len() == 2
            && let Some(Value::Array(items, ..)) = args.get(1)
        {
            let joined = items
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(&sep);
            return Ok(Value::Str(joined));
        }
        // Multi-arg: join(sep, item1, item2, ...)
        if args.len() > 1 {
            let joined = args[1..]
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(&sep);
            return Ok(Value::Str(joined));
        }
        Ok(Value::Str(String::new()))
    }

    pub(super) fn builtin_list(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut result = Vec::new();
        for arg in args {
            result.extend(Self::value_to_list(arg));
        }
        Ok(Value::array(result))
    }

    pub(super) fn builtin_flat(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut result = Vec::new();
        for arg in args {
            Self::flat_into(arg, &mut result);
        }
        Ok(Value::Seq(std::sync::Arc::new(result)))
    }

    pub(crate) fn flat_into(val: &Value, out: &mut Vec<Value>) {
        match val {
            Value::Array(items, ..) | Value::Slip(items) | Value::Seq(items) => {
                for item in items.iter() {
                    Self::flat_into(item, out);
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                out.extend(Self::value_to_list(val));
            }
            other => out.push(other.clone()),
        }
    }

    pub(super) fn builtin_slip(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut items = Vec::new();
        for arg in args {
            match arg {
                Value::Array(elems, ..) => items.extend(elems.iter().cloned()),
                Value::Seq(elems) => items.extend(elems.iter().cloned()),
                Value::Slip(elems) => items.extend(elems.iter().cloned()),
                other => items.push(other.clone()),
            }
        }
        Ok(Value::slip(items))
    }

    pub(super) fn builtin_reverse(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Array(mut items, ..)) => {
                Arc::make_mut(&mut items).reverse();
                Value::Array(items, false)
            }
            Some(Value::Str(s)) => Value::Str(s.chars().rev().collect()),
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_sort(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // sort(comparator, list) or sort(list)
        if args.len() >= 2 {
            let first = &args[0];
            // Check if first arg is a callable (comparator function)
            if matches!(first, Value::Sub(_) | Value::Routine { .. }) {
                let comparator = first.clone();
                let mut items: Vec<Value> = Vec::new();
                for arg in args.iter().skip(1) {
                    if crate::runtime::utils::is_shaped_array(arg) {
                        items.extend(crate::runtime::utils::shaped_array_leaves(arg));
                    } else {
                        match arg {
                            Value::Array(elems, ..) => items.extend(elems.iter().cloned()),
                            Value::Seq(elems) => items.extend(elems.iter().cloned()),
                            other => items.push(other.clone()),
                        }
                    }
                }
                // Delegate to dispatch_sort which handles all callable types
                return self.dispatch_sort(Value::array(items), &[comparator]);
            }
        }
        let val = args.first().cloned();
        Ok(match val {
            Some(ref v) if crate::runtime::utils::is_shaped_array(v) => {
                let mut leaves = crate::runtime::utils::shaped_array_leaves(v);
                leaves.sort_by_key(|a| a.to_string_value());
                Value::array(leaves)
            }
            Some(Value::Array(mut items, ..)) => {
                Arc::make_mut(&mut items).sort_by_key(|a| a.to_string_value());
                Value::Array(items, false)
            }
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_unique(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        let mut positional = Vec::new();
        let mut method_args = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, _) if key == "as" || key == "with" => {
                    method_args.push(arg.clone());
                }
                _ => positional.push(arg.clone()),
            }
        }

        if positional.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        let target = if positional.len() == 1 {
            positional[0].clone()
        } else {
            Value::array(positional)
        };
        self.call_method_with_values(target, "unique", method_args)
    }

    pub(super) fn builtin_map(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let func = args.first().cloned();
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            if crate::runtime::utils::is_shaped_array(arg) {
                list_items.extend(crate::runtime::utils::shaped_array_leaves(arg));
            } else {
                match arg {
                    Value::Array(items, ..) => list_items.extend(items.iter().cloned()),
                    Value::Range(a, b) => list_items.extend((*a..=*b).map(Value::Int)),
                    Value::RangeExcl(a, b) => list_items.extend((*a..*b).map(Value::Int)),
                    Value::RangeExclStart(a, b) => list_items.extend((*a + 1..=*b).map(Value::Int)),
                    Value::RangeExclBoth(a, b) => list_items.extend((*a + 1..*b).map(Value::Int)),
                    v if v.is_range() => {
                        list_items.extend(crate::runtime::utils::value_to_list(v));
                    }
                    other => list_items.push(other.clone()),
                }
            }
        }
        self.eval_map_over_items(func, list_items)
    }

    pub(super) fn builtin_grep(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let func = args.first().cloned();
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            match arg {
                Value::Array(items, ..) => list_items.extend(items.iter().cloned()),
                Value::Range(a, b) => list_items.extend((*a..=*b).map(Value::Int)),
                Value::RangeExcl(a, b) => list_items.extend((*a..*b).map(Value::Int)),
                v if v.is_range() => {
                    list_items.extend(crate::runtime::utils::value_to_list(v));
                }
                other => list_items.push(other.clone()),
            }
        }
        self.eval_grep_over_items(func, list_items)
    }

    pub(super) fn builtin_first(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Separate named args (Pairs) from positional args
        let mut positional = Vec::new();
        let mut has_v = false;
        let mut has_neg_v = false;
        let mut has_end = false;
        let mut has_k = false;
        let mut has_kv = false;
        let mut has_p = false;
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "v" => {
                    if value.truthy() {
                        has_v = true;
                    } else {
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
        let _ = has_v; // :v is the default behavior
        // Check for Bool matcher (X::Match::Bool)
        if matches!(positional.first(), Some(Value::Bool(_))) {
            let mut err = RuntimeError::new("Cannot use Bool as a matcher");
            err.exception = Some(Box::new(Value::make_instance(
                "X::Match::Bool".to_string(),
                std::collections::HashMap::new(),
            )));
            return Err(err);
        }
        let func = positional.first().cloned();
        let mut list_items = Vec::new();
        for arg in positional.iter().skip(1) {
            match arg {
                Value::Array(items, ..) => list_items.extend(items.iter().cloned()),
                Value::Range(a, b) => list_items.extend((*a..=*b).map(Value::Int)),
                Value::RangeExcl(a, b) => list_items.extend((*a..*b).map(Value::Int)),
                v if v.is_range() => {
                    list_items.extend(crate::runtime::utils::value_to_list(v));
                }
                other => list_items.push(other.clone()),
            }
        }
        if let Some((idx, value)) = self.find_first_match_over_items(func, &list_items, has_end)? {
            return Ok(format_first_result(idx, value, has_k, has_kv, has_p));
        }
        Ok(Value::Nil)
    }

    pub(super) fn builtin_classify(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let func = match args.first() {
            Some(value) => value.clone(),
            None => return Ok(Value::hash(HashMap::new())),
        };
        let mut buckets: HashMap<String, Vec<Value>> = HashMap::new();
        // Flatten list/array arguments into individual items
        let mut items = Vec::new();
        for arg in args.iter().skip(1) {
            match arg {
                Value::Array(values, ..) => items.extend(values.iter().cloned()),
                v if v.is_range() => {
                    items.extend(crate::runtime::utils::value_to_list(v));
                }
                other => items.push(other.clone()),
            }
        }
        for item in &items {
            let keys = match self.call_lambda_with_arg(&func, item.clone()) {
                Ok(Value::Array(values, ..)) => values.to_vec(),
                Ok(value) => vec![value],
                Err(_) => vec![Value::Nil],
            };
            let target_keys = if name == "classify" {
                if keys.is_empty() {
                    vec![Value::Nil]
                } else {
                    vec![keys[0].clone()]
                }
            } else {
                keys
            };
            for key in target_keys {
                let bucket_key = key.to_string_value();
                buckets.entry(bucket_key).or_default().push(item.clone());
            }
        }
        let hash_map = buckets
            .into_iter()
            .map(|(k, v)| (k, Value::array(v)))
            .collect();
        Ok(Value::hash(hash_map))
    }

    /// `cross(@a, @b, ...)` — Cartesian product of lists.
    /// With `with => &op`, applies the operator to each pair instead of making tuples.
    pub(super) fn builtin_cross(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let mut lists: Vec<Vec<Value>> = Vec::new();
        let mut with_func: Option<Value> = None;

        for arg in &args {
            match arg {
                Value::Pair(k, v) if k.as_str() == "with" => {
                    with_func = Some(v.as_ref().clone());
                }
                _ => {
                    lists.push(super::utils::value_to_list(arg));
                }
            }
        }

        if lists.is_empty() {
            return Ok(Value::array(vec![]));
        }

        // Compute Cartesian product iteratively
        let mut result: Vec<Vec<Value>> = vec![vec![]];
        for list in &lists {
            let mut new_result = Vec::new();
            for combo in &result {
                for item in list {
                    let mut new_combo = combo.clone();
                    new_combo.push(item.clone());
                    new_result.push(new_combo);
                }
            }
            result = new_result;
        }

        // Apply `with` function or create tuples
        if let Some(func) = with_func {
            let mut final_result = Vec::new();
            for combo in result {
                let val = self.call_sub_value(func.clone(), combo, false)?;
                final_result.push(val);
            }
            Ok(Value::array(final_result))
        } else {
            // Return as list of lists (tuples)
            let tuples: Vec<Value> = result.into_iter().map(Value::array).collect();
            Ok(Value::array(tuples))
        }
    }

    pub(super) fn builtin_roundrobin(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
        if args.len() == 1 {
            return Ok(args[0].clone());
        }

        let streams: Vec<Vec<Value>> = args
            .iter()
            .map(|arg| match arg {
                Value::Capture { positional, named }
                    if named.is_empty() && positional.len() == 1 =>
                {
                    vec![arg.clone()]
                }
                Value::Array(items, _) => items.iter().cloned().collect(),
                Value::Seq(items) | Value::Slip(items) => items.iter().cloned().collect(),
                Value::Range(a, b) => (*a..=*b).map(Value::Int).collect(),
                Value::RangeExcl(a, b) => (*a..*b).map(Value::Int).collect(),
                v if v.is_range() => crate::runtime::utils::value_to_list(v),
                other => vec![other.clone()],
            })
            .collect();

        let mut indices = vec![0usize; streams.len()];
        let mut rounds = Vec::new();
        loop {
            let mut tuple = Vec::new();
            let mut progressed = false;
            for (i, stream) in streams.iter().enumerate() {
                if indices[i] < stream.len() {
                    tuple.push(stream[indices[i]].clone());
                    indices[i] += 1;
                    progressed = true;
                }
            }
            if !progressed {
                break;
            }
            rounds.push(Value::array(tuple));
        }

        Ok(Value::array(rounds))
    }
}
