use super::*;
use crate::symbol::Symbol;
use std::collections::HashMap as StdHashMap;

/// Public wrapper for `collect_minmax_candidates` usable from builtins crate.
pub(crate) fn collect_minmax_candidates_pub(value: &Value, out: &mut Vec<Value>) {
    Interpreter::collect_minmax_candidates(value, out);
}

/// Public wrapper for `make_inclusive_range_value` usable from builtins crate.
pub(crate) fn make_inclusive_range_pub(left: Value, right: Value) -> Value {
    Interpreter::make_inclusive_range_value(left, right)
}

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
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
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
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
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
        // One-arg rule: when called with a single list/range argument,
        // flatten it into the junction elements. With multiple arguments,
        // each argument becomes a junction element without flattening.
        let elems = if args.len() == 1 {
            let arg = args.into_iter().next().unwrap();
            match arg {
                Value::Array(items, ..) => items.to_vec(),
                Value::Seq(items) | Value::Slip(items) => items.to_vec(),
                range @ (Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }) => Self::value_to_list(&range),
                other => vec![other],
            }
        } else {
            args
        };
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
        crate::builtins::native_method_0arg(&target, crate::symbol::Symbol::intern(method))
            .unwrap_or_else(|| Ok(Value::array(Vec::new())))
    }

    pub(super) fn builtin_abs(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Int(i)) => Value::Int(i.abs()),
            Some(Value::BigInt(n)) => Value::bigint(n.as_ref().abs()),
            Some(Value::Num(f)) => Value::Num(f.abs()),
            Some(Value::Rat(n, d)) => Value::Rat(n.abs(), d),
            Some(Value::Complex(r, i)) => Value::Num((r * r + i * i).sqrt()),
            Some(v) => Value::Num(v.to_f64().abs()),
            None => Value::Int(0),
        })
    }

    pub(super) fn builtin_sign(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args
            .first()
            .ok_or_else(|| RuntimeError::new("sign requires an argument"))?;
        // Delegate to the .sign method via the native_method_0arg path
        match crate::builtins::methods_0arg::native_method_0arg(
            val,
            crate::symbol::Symbol::intern("sign"),
        ) {
            Some(result) => result,
            None => Err(RuntimeError::new(
                "Cannot call sign on this value".to_string(),
            )),
        }
    }
}

/// Raku `val()` builtin: convert a string into an allomorphic type.
pub(super) fn builtin_val(args: &[Value]) -> Value {
    let arg = match args.first() {
        Some(v) => v,
        None => return Value::Nil,
    };
    // val() on non-Str types (List, Slip, Array) returns the value unchanged.
    match arg {
        Value::Array(..) | Value::Seq(_) | Value::Slip(_) => return arg.clone(),
        _ => {}
    }
    let original = arg.to_string_value();
    let word = original.trim();

    fn make_allomorphic(val: Value, original: &str) -> Value {
        let mut mixins = StdHashMap::new();
        // Store the original string (with whitespace) as the Str component
        mixins.insert("Str".to_string(), Value::str(original.to_string()));
        Value::mixin(val, mixins)
    }

    // Try complex (must end with 'i')
    if let Some(complex) = try_parse_complex(word) {
        return make_allomorphic(complex, &original);
    }

    // Try integer
    if let Ok(i) = word.parse::<i64>() {
        return make_allomorphic(Value::Int(i), &original);
    }

    // Try Num (scientific notation with e/E)
    if (word.contains('e') || word.contains('E')) && !word.ends_with('i') {
        // Normalize U+2212 MINUS SIGN to ASCII minus
        let normalized = word.replace('\u{2212}', "-");
        if let Ok(f) = normalized.parse::<f64>() {
            return make_allomorphic(Value::Num(f), &original);
        }
    }

    // Try Rat (fraction notation like "1/5")
    if word.contains('/') && !word.contains('.') && !word.contains('e') && !word.contains('E') {
        let parts: Vec<&str> = word.splitn(2, '/').collect();
        if parts.len() == 2
            && let (Ok(n), Ok(d)) = (
                parts[0].trim().parse::<i64>(),
                parts[1].trim().parse::<i64>(),
            )
            && d != 0
        {
            return make_allomorphic(crate::value::make_rat(n, d), &original);
        }
    }

    // Try Rat (decimal without exponent)
    if word.contains('.') && !word.contains('e') && !word.contains('E') {
        let normalized = word.replace('\u{2212}', "-");
        if let Ok(f) = normalized.parse::<f64>() {
            // Approximate as Rat: use fixed denominator approach
            let scale = 10i64.pow(
                normalized
                    .find('.')
                    .map(|p| normalized.len() - p - 1)
                    .unwrap_or(0) as u32,
            );
            let numer = (f * scale as f64).round() as i64;
            return make_allomorphic(crate::value::make_rat(numer, scale), &original);
        }
    }

    // Plain string
    Value::str(original.to_string())
}

fn try_parse_complex(word: &str) -> Option<Value> {
    // Handle both "1+2i" and "1+Inf\i" / "1-Inf\i" / "1+NaN\i" forms
    let (without_i, backslash_i) = if let Some(stripped) = word.strip_suffix("\\i") {
        (stripped, true)
    } else if let Some(stripped) = word.strip_suffix('i') {
        (stripped, false)
    } else {
        return None;
    };

    // Pure imaginary
    if let Ok(imag) = without_i.parse::<f64>() {
        return Some(Value::Complex(0.0, imag));
    }
    // Handle pure imaginary with special values: "Inf\i", "-Inf\i", "NaN\i"
    if backslash_i && let Some(imag) = parse_special_float(without_i) {
        return Some(Value::Complex(0.0, imag));
    }

    // real+imag i
    let bytes = without_i.as_bytes();
    let mut split_pos = None;
    for i in 1..bytes.len() {
        if (bytes[i] == b'+' || bytes[i] == b'-') && bytes[i - 1] != b'e' && bytes[i - 1] != b'E' {
            split_pos = Some(i);
        }
    }
    let split_pos = split_pos?;
    let real_str = &without_i[..split_pos];
    let imag_str = &without_i[split_pos..];
    let real: f64 = real_str.parse().ok()?;
    let imag: f64 = if backslash_i {
        // For \i form, imaginary part may be special float like "+Inf", "-Inf", "+NaN"
        parse_special_float(imag_str).or_else(|| imag_str.parse().ok())?
    } else {
        imag_str.parse().ok()?
    };
    Some(Value::Complex(real, imag))
}

fn parse_special_float(s: &str) -> Option<f64> {
    match s {
        "Inf" | "+Inf" => Some(f64::INFINITY),
        "-Inf" => Some(f64::NEG_INFINITY),
        "NaN" | "+NaN" | "-NaN" => Some(f64::NAN),
        _ => None,
    }
}

impl Interpreter {
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

    /// Determine the arity of a callable for min/max/minmax by-block dispatch.
    /// Returns 1 for sort-key extractors, 2 for comparators.
    fn extrema_callable_arity(&self, callable: &Value) -> usize {
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

        // Determine if by-block is arity-1 (sort key) or arity-2 (comparator)
        let by_arity = by.map(|b| self.extrema_callable_arity(b));

        let mut best = filtered.remove(0);
        for value in filtered {
            let cmp = if let Some(by_block) = by {
                if by_arity == Some(1) {
                    // Arity-1: use as sort key extractor
                    let key_a = self.call_sub_value(by_block.clone(), vec![value.clone()], true)?;
                    let key_b = self.call_sub_value(by_block.clone(), vec![best.clone()], true)?;
                    crate::runtime::compare_values(&key_a, &key_b)
                } else {
                    // Arity-2: use as comparator ($^a, $^b) => Order
                    let result = self.call_sub_value(
                        by_block.clone(),
                        vec![value.clone(), best.clone()],
                        true,
                    )?;
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
                Value::str(k.clone())
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
    fn extract_extrema_adverbs(args: &[Value]) -> (Option<String>, Vec<Value>) {
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
                Value::Pair(name, value)
                    if matches!(name.as_str(), "k" | "v" | "kv" | "p")
                        && matches!(value.as_ref(), Value::Bool(true)) =>
                {
                    adverb = Some(name.clone());
                }
                _ => positional.push(arg.clone()),
            }
        }
        let _ = by_found;
        (adverb, positional)
    }

    pub(super) fn builtin_min(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let (adverb, filtered_args) = Self::extract_extrema_adverbs(args);
        let result = self.builtin_min_inner(&filtered_args)?;
        self.apply_extrema_adverb(&filtered_args, result, false, adverb.as_deref())
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

    /// Apply :k, :v, :kv, :p adverbs to min/max result.
    /// These return index/value/both/pair information about the extremum.
    fn apply_extrema_adverb(
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

        // Get the list items from the first positional arg
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
            return Ok(Value::array(vec![]));
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
                Ok(Value::array(keys))
            }
            "v" => {
                let vals: Vec<Value> = matching_indices.iter().map(|&i| items[i].clone()).collect();
                Ok(Value::array(vals))
            }
            "kv" => {
                let mut kvs = Vec::new();
                for &i in &matching_indices {
                    kvs.push(Value::Int(i as i64));
                    kvs.push(items[i].clone());
                }
                Ok(Value::array(kvs))
            }
            "p" => {
                let pairs: Vec<Value> = matching_indices
                    .iter()
                    .map(|&i| Value::Pair(i.to_string(), Box::new(items[i].clone())))
                    .collect();
                Ok(Value::array(pairs))
            }
            _ => Ok(result),
        }
    }

    pub(crate) fn collect_minmax_candidates(value: &Value, out: &mut Vec<Value>) {
        match value {
            // Unwrap Scalar containers
            Value::Scalar(inner) => Self::collect_minmax_candidates(inner, out),
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

        let mut candidates = Vec::new();
        if by.is_some() {
            // When a comparator is provided, enumerate ranges fully
            for arg in &positional {
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
            for arg in &positional {
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

        if let Some(by_block) = &by {
            let by_arity = self.extrema_callable_arity(by_block);
            let mut min_value = candidates[0].clone();
            let mut max_value = candidates[0].clone();
            for value in candidates.iter().skip(1) {
                let cmp_min = if by_arity == 1 {
                    let key_a = self.call_sub_value(by_block.clone(), vec![value.clone()], true)?;
                    let key_b =
                        self.call_sub_value(by_block.clone(), vec![min_value.clone()], true)?;
                    crate::runtime::compare_values(&key_a, &key_b)
                } else {
                    let result = self.call_sub_value(
                        by_block.clone(),
                        vec![value.clone(), min_value.clone()],
                        true,
                    )?;
                    Self::order_to_int(&result)
                };
                let cmp_max = if by_arity == 1 {
                    let key_a = self.call_sub_value(by_block.clone(), vec![value.clone()], true)?;
                    let key_b =
                        self.call_sub_value(by_block.clone(), vec![max_value.clone()], true)?;
                    crate::runtime::compare_values(&key_a, &key_b)
                } else {
                    let result = self.call_sub_value(
                        by_block.clone(),
                        vec![value.clone(), max_value.clone()],
                        true,
                    )?;
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
    fn order_to_int(result: &Value) -> i32 {
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

    pub(super) fn builtin_shift(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            let msg = "X::TypeCheck::Argument: Calling shift(Any) will never work with declared signature ($)".to_string();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        if args.len() > 1 {
            return Err(RuntimeError::new(format!(
                "Too many positionals passed; expected 1 argument but got {}",
                args.len()
            )));
        }
        Ok(match args.first().cloned() {
            Some(Value::Array(mut items, ..)) => {
                if items.is_empty() {
                    make_empty_array_failure("shift")
                } else {
                    Arc::make_mut(&mut items).remove(0)
                }
            }
            _ => make_empty_array_failure("shift"),
        })
    }

    pub(super) fn builtin_pop(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            let msg =
                "Calling pop() will never work with signature of the proto ($, *%)".to_string();
            let mut attrs = StdHashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        if args.len() > 1 {
            return Err(RuntimeError::new(format!(
                "Too many positionals passed to pop; expected 1 argument but got {}",
                args.len()
            )));
        }
        Ok(match args.first().cloned() {
            Some(Value::Array(mut items, ..)) => {
                let items_mut = Arc::make_mut(&mut items);
                if items_mut.is_empty() {
                    make_empty_array_failure("pop")
                } else {
                    items_mut.pop().unwrap_or(Value::Nil)
                }
            }
            _ => make_empty_array_failure("pop"),
        })
    }

    pub(super) fn builtin_join(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let sep = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if args.len() == 2
            && let Some(Value::Array(items, kind)) = args.get(1)
        {
            if kind.is_itemized() {
                // Itemized array: treat as single item, stringify
                return Ok(Value::str(args[1].to_string_value()));
            }
            let joined = items
                .iter()
                .map(|v| v.to_str_context())
                .collect::<Vec<_>>()
                .join(&sep);
            return Ok(Value::str(joined));
        }
        // Multi-arg: join(sep, item1, item2, ...)
        // Flatten ranges and lists to their elements
        if args.len() > 1 {
            let mut parts = Vec::new();
            for v in &args[1..] {
                match v {
                    Value::Range(start, end) => {
                        for i in *start..=*end {
                            parts.push(Value::Int(i).to_string_value());
                        }
                    }
                    Value::RangeExcl(start, end) => {
                        for i in *start..*end {
                            parts.push(Value::Int(i).to_string_value());
                        }
                    }
                    Value::GenericRange { .. } => {
                        for item in crate::runtime::utils::value_to_list(v) {
                            parts.push(item.to_str_context());
                        }
                    }
                    Value::Array(items, kind) if !kind.is_itemized() => {
                        for item in items.iter() {
                            parts.push(item.to_str_context());
                        }
                    }
                    Value::Seq(items) | Value::Slip(items) => {
                        for item in items.as_ref() {
                            parts.push(item.to_str_context());
                        }
                    }
                    _ => {
                        parts.push(v.to_str_context());
                    }
                }
            }
            let joined = parts.join(&sep);
            return Ok(Value::str(joined));
        }
        Ok(Value::str(String::new()))
    }

    pub(super) fn builtin_list(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `list` on a single Seq is a no-op (returns the Seq as-is)
        if args.len() == 1
            && let Value::Seq(_) = &args[0]
        {
            return Ok(args[0].clone());
        }
        let mut result = Vec::new();
        for arg in args {
            result.extend(Self::value_to_list(arg));
        }
        Ok(Value::array(result))
    }

    pub(super) fn builtin_cache(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `cache` eagerly evaluates and caches the values into a List
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
            Value::Array(items, kind) if kind.is_itemized() => {
                out.push(Value::Array(std::sync::Arc::clone(items), *kind))
            }
            Value::Array(items, ..) | Value::Slip(items) | Value::Seq(items) => {
                for item in items.iter() {
                    Self::flat_into(item, out);
                }
            }
            Value::LazyList(ll) => {
                if let Some(cached) = ll.cache.lock().unwrap().clone() {
                    for item in &cached {
                        Self::flat_into(item, out);
                    }
                } else {
                    out.push(val.clone());
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
        // If multiple args, reverse the list of args
        if args.len() > 1 {
            let mut items: Vec<Value> = args.to_vec();
            items.reverse();
            return Ok(Value::array(items));
        }
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Array(mut items, ..)) => {
                Arc::make_mut(&mut items).reverse();
                Value::Array(items, ArrayKind::List)
            }
            Some(Value::Str(s)) => {
                // reverse() on a string in list context returns the string as-is
                // (unlike flip() which reverses characters)
                Value::str(s.to_string())
            }
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_sort(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Extract named args (:k, :by) and positional args
        let mut return_k = false;
        let mut by_callable: Option<Value> = None;
        let mut callable: Option<Value> = None;
        let mut positional: Vec<Value> = Vec::new();

        for arg in args {
            match arg {
                Value::Pair(key, val) if key == "k" => {
                    return_k = val.truthy();
                }
                Value::Pair(key, val) if key == "by" => {
                    by_callable = Some(val.as_ref().clone());
                }
                _ => positional.push(arg.clone()),
            }
        }

        // sort(comparator, list, ...) or sort(list, ...) or sort(items...)
        if positional.len() >= 2 {
            let first = &positional[0];
            if matches!(first, Value::Sub(_) | Value::Routine { .. }) {
                callable = Some(first.clone());
                positional.remove(0);
            }
        }

        // Prefer :by over positional callable
        if by_callable.is_some() {
            callable = by_callable;
        }

        // Flatten positional args into items
        let mut items: Vec<Value> = Vec::new();
        for arg in &positional {
            if crate::runtime::utils::is_shaped_array(arg) {
                items.extend(crate::runtime::utils::shaped_array_leaves(arg));
            } else {
                match arg {
                    Value::Array(elems, ..) => items.extend(elems.iter().cloned()),
                    Value::Seq(elems) => items.extend(elems.iter().cloned()),
                    Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. } => {
                        items.extend(Self::value_to_list(arg));
                    }
                    other => items.push(other.clone()),
                }
            }
        }

        // Build sort args for dispatch_sort
        let mut sort_args: Vec<Value> = Vec::new();
        if let Some(c) = callable {
            sort_args.push(c);
        }
        if return_k {
            sort_args.push(Value::Pair("k".to_string(), Box::new(Value::Bool(true))));
        }

        if items.is_empty() && positional.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        self.dispatch_sort(Value::array(items), &sort_args)
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

    pub(super) fn builtin_repeated(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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
        self.call_method_with_values(target, "repeated", method_args)
    }

    pub(super) fn builtin_squish(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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
        self.call_method_with_values(target, "squish", method_args)
    }

    pub(super) fn builtin_map(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let func = args.first().cloned();
        let arg_sources = self.pending_call_arg_sources.clone().unwrap_or_default();
        // Check if the source is a single named array variable for rw writeback
        let source_var = if args.len() == 2 {
            arg_sources
                .get(1)
                .and_then(|entry| entry.as_ref())
                .filter(|name| name.starts_with('@'))
                .cloned()
        } else {
            None
        };
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            if crate::runtime::utils::is_shaped_array(arg) {
                list_items.extend(crate::runtime::utils::shaped_array_leaves(arg));
            } else {
                match arg {
                    Value::Array(items, ..) => list_items.extend(items.iter().cloned()),
                    Value::Hash(map) => {
                        // Hashes in list context flatten to key-value Pairs
                        for (k, v) in map.iter() {
                            list_items.push(Value::Pair(k.clone(), Box::new(v.clone())));
                        }
                    }
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
        if let Some(var_name) = source_var {
            let result = self.eval_map_over_items_rw(func, &mut list_items)?;
            self.env.insert(var_name, Value::real_array(list_items));
            Ok(result)
        } else {
            self.eval_map_over_items(func, list_items)
        }
    }

    pub(super) fn builtin_grep(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Parse named adverbs (:k, :v, :kv, :p) from args
        let mut has_k = false;
        let mut has_kv = false;
        let mut has_p = false;
        let mut positional_args: Vec<Value> = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "k" => has_k = value.truthy(),
                Value::Pair(key, value) if key == "kv" => has_kv = value.truthy(),
                Value::Pair(key, value) if key == "p" => has_p = value.truthy(),
                Value::Pair(key, _) if key == "v" => {} // default
                _ => positional_args.push(arg.clone()),
            }
        }
        let args = &positional_args;
        let func = args.first().cloned();
        // Check if the matcher is a Bool — this is always an error.
        // `grep $_ == 1, 1,2,3` passes a Bool as the matcher.
        if let Some(Value::Bool(_)) = func.as_ref() {
            return Err(RuntimeError::typed_msg(
                "X::Match::Bool",
                "Cannot use Bool as Matcher with '.match'. Did you mean to use $_ ~~ ... instead?",
            ));
        }
        if args.len() == 1 && matches!(args[0], Value::Int(_) | Value::Num(_) | Value::Str(_)) {
            return Err(RuntimeError::typed_msg(
                "X::Match::Bool",
                "Cannot use Bool as Matcher with '.match'. Did you mean to use $_ ~~ ... instead?",
            ));
        }
        if args.len() == 2
            && let Value::GenericRange { end, .. } = &args[1]
        {
            let end_f = end.to_f64();
            if end_f.is_infinite() && end_f.is_sign_positive() {
                let mut method_args: Vec<Value> = func.into_iter().collect();
                if has_k {
                    method_args.push(Value::Pair("k".to_string(), Box::new(Value::Bool(true))));
                } else if has_kv {
                    method_args.push(Value::Pair("kv".to_string(), Box::new(Value::Bool(true))));
                } else if has_p {
                    method_args.push(Value::Pair("p".to_string(), Box::new(Value::Bool(true))));
                }
                return self.call_method_with_values(args[1].clone(), "grep", method_args);
            }
        }
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            match arg {
                // Only flatten List-kind arrays (from @-sigiled variables).
                // Array-kind (from [...] literals) are kept as individual items.
                Value::Array(items, kind)
                    if matches!(
                        kind,
                        crate::value::ArrayKind::List | crate::value::ArrayKind::ItemList
                    ) || (matches!(kind, crate::value::ArrayKind::Array) && args.len() == 2) =>
                {
                    list_items.extend(items.iter().cloned());
                }
                Value::Range(a, b) => list_items.extend((*a..=*b).map(Value::Int)),
                Value::RangeExcl(a, b) => list_items.extend((*a..*b).map(Value::Int)),
                v if v.is_range() => {
                    list_items.extend(crate::runtime::utils::value_to_list(v));
                }
                other => list_items.push(other.clone()),
            }
        }
        if has_k || has_kv || has_p {
            let original_items = list_items.clone();
            let filtered = self.eval_grep_over_items(func, list_items)?;
            let indices = crate::runtime::methods_collection_ops::compute_grep_indices(
                &original_items,
                &filtered,
            );
            if has_k {
                let idx_vals: Vec<Value> = indices.iter().map(|&i| Value::Int(i as i64)).collect();
                Ok(Value::array(idx_vals))
            } else if has_kv {
                let items = if let Value::Array(items, ..) = &filtered {
                    items.to_vec()
                } else {
                    vec![filtered]
                };
                let mut result = Vec::new();
                for (i, item) in indices.iter().zip(items.iter()) {
                    result.push(Value::Int(*i as i64));
                    result.push(item.clone());
                }
                Ok(Value::array(result))
            } else {
                // :p
                let items = if let Value::Array(items, ..) = &filtered {
                    items.to_vec()
                } else {
                    vec![filtered]
                };
                let mut result = Vec::new();
                for (i, item) in indices.iter().zip(items.iter()) {
                    result.push(Value::Pair(i.to_string(), Box::new(item.clone())));
                }
                Ok(Value::array(result))
            }
        } else {
            self.eval_grep_over_items(func, list_items)
        }
    }

    /// `snip(matcher, +values)` — split a list at positions where the matcher stops matching.
    pub(super) fn builtin_snip(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new("Too few positionals passed to 'snip'"));
        }
        let matcher = args[0].clone();
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            match arg {
                Value::Array(items, ..) => list_items.extend(items.iter().cloned()),
                other => list_items.push(other.clone()),
            }
        }
        self.eval_snip(matcher, list_items)
    }

    /// Core snip implementation shared by both sub and method forms.
    pub(super) fn eval_snip(
        &mut self,
        matcher: Value,
        items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Extract the list of matchers: if matcher is a list/array of callables/types,
        // use them in sequence; otherwise treat as a single matcher.
        let matchers: Vec<Value> = match &matcher {
            Value::Array(elems, ..) | Value::Seq(elems) | Value::Slip(elems) => {
                elems.iter().cloned().collect()
            }
            other => vec![other.clone()],
        };

        let mut result_groups: Vec<Value> = Vec::new();
        let mut current_group: Vec<Value> = Vec::new();
        let mut matcher_idx: usize = 0;

        for item in items {
            let current_matcher = matchers.get(matcher_idx);
            let matched = if let Some(m) = current_matcher {
                self.snip_matches(&item, m)?
            } else {
                // No more matchers — everything goes into the last group
                true
            };

            if matched {
                current_group.push(item);
            } else {
                // Snip here: save current group and start a new one
                result_groups.push(Value::array(current_group));
                current_group = vec![item];
                matcher_idx += 1;
            }
        }
        // Push the final group
        if !current_group.is_empty() {
            result_groups.push(Value::array(current_group));
        }

        Ok(Value::array(result_groups))
    }

    /// Check if a value matches a snip matcher (Callable or type object).
    fn snip_matches(&mut self, item: &Value, matcher: &Value) -> Result<bool, RuntimeError> {
        if matches!(matcher, Value::Sub(_)) {
            Ok(self
                .call_sub_value(matcher.clone(), vec![item.clone()], true)?
                .truthy())
        } else {
            Ok(self.smart_match(item, matcher))
        }
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
                Symbol::intern("X::Match::Bool"),
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

    /// Find a variable name in the current environment whose value is identical
    /// (by Arc pointer) to the given value. Used for :into writeback.
    fn find_var_by_identity(&self, value: &Value) -> Option<String> {
        match value {
            Value::Hash(target_arc) => {
                for (name, env_val) in self.env().iter() {
                    if let Value::Hash(env_arc) = env_val
                        && std::sync::Arc::ptr_eq(target_arc, env_arc)
                    {
                        return Some(name.clone());
                    }
                }
                None
            }
            Value::Bag(target_arc) => {
                for (name, env_val) in self.env().iter() {
                    if let Value::Bag(env_arc) = env_val
                        && std::sync::Arc::ptr_eq(target_arc, env_arc)
                    {
                        return Some(name.clone());
                    }
                }
                None
            }
            Value::Mix(target_arc) => {
                for (name, env_val) in self.env().iter() {
                    if let Value::Mix(env_arc) = env_val
                        && std::sync::Arc::ptr_eq(target_arc, env_arc)
                    {
                        return Some(name.clone());
                    }
                }
                None
            }
            _ => None,
        }
    }

    pub(super) fn builtin_classify(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        fn as_items(value: &Value) -> Option<Vec<Value>> {
            match value {
                Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                    Some(items.iter().cloned().collect())
                }
                _ => None,
            }
        }

        fn mapper_path(value: Value) -> Vec<Value> {
            as_items(&value).unwrap_or_else(|| vec![value])
        }

        fn mapper_categories(value: Value) -> Vec<Vec<Value>> {
            if let Some(items) = as_items(&value) {
                if items.is_empty() {
                    return Vec::new();
                }
                let mut out = Vec::new();
                for item in items {
                    out.push(mapper_path(item));
                }
                out
            } else {
                vec![vec![value]]
            }
        }

        fn mixed_level_error(name: &str) -> RuntimeError {
            RuntimeError::new(format!(
                "X::Invalid::ComputedValue: mixed-level {}",
                if name == "categorize" {
                    "categorization"
                } else {
                    "classification"
                }
            ))
        }

        fn insert_nested_bucket(
            buckets: &mut HashMap<String, Value>,
            path: &[Value],
            item: Value,
            name: &str,
        ) -> Result<(), RuntimeError> {
            if path.is_empty() {
                return Ok(());
            }
            let key = path[0].to_string_value();
            if path.len() == 1 {
                let entry = buckets
                    .entry(key)
                    .or_insert_with(|| Value::real_array(Vec::new()));
                match entry {
                    Value::Array(values, ..) => {
                        Arc::make_mut(values).push(item);
                        Ok(())
                    }
                    Value::Hash(_) => Err(mixed_level_error(name)),
                    _ => {
                        *entry = Value::real_array(vec![item]);
                        Ok(())
                    }
                }
            } else {
                let entry = buckets
                    .entry(key)
                    .or_insert_with(|| Value::hash(HashMap::new()));
                match entry {
                    Value::Hash(map) => {
                        let map = Arc::make_mut(map);
                        insert_nested_bucket(map, &path[1..], item, name)
                    }
                    Value::Array(..) => Err(mixed_level_error(name)),
                    _ => {
                        *entry = Value::hash(HashMap::new());
                        if let Value::Hash(map) = entry {
                            let map = Arc::make_mut(map);
                            insert_nested_bucket(map, &path[1..], item, name)
                        } else {
                            Ok(())
                        }
                    }
                }
            }
        }

        // Extract variable names from arg_sources for :into writeback
        let arg_sources = self.take_pending_call_arg_sources();
        let mut mapper: Option<Value> = None;
        let mut as_mapper: Option<Value> = None;
        let mut into_target_raw: Option<Value> = None;
        let mut into_target: Option<Value> = None;
        let mut into_varname: Option<String> = None;
        let mut positional: Vec<Value> = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let fetched_arg = self.auto_fetch_proxy(arg)?;
            match &fetched_arg {
                Value::Pair(key, value) if key == "as" => {
                    as_mapper = Some(self.auto_fetch_proxy(value)?)
                }
                Value::Pair(key, value) if key == "into" => {
                    // Look up the variable name from arg_sources metadata
                    // The compiler encodes FatArrow args as "key=varname"
                    if let Some(ref sources) = arg_sources
                        && let Some(Some(source)) = sources.get(i)
                        && let Some(varname) = source.strip_prefix("into=")
                    {
                        into_varname = Some(varname.to_string());
                    }
                    // Fallback: search env for a variable with matching identity
                    if into_varname.is_none() {
                        into_varname = self.find_var_by_identity(value);
                    }
                    into_target_raw = Some(*value.clone());
                    into_target = Some(self.auto_fetch_proxy(value)?);
                }
                _ => {
                    if mapper.is_none() {
                        mapper = Some(fetched_arg);
                    } else {
                        positional.push(fetched_arg);
                    }
                }
            }
        }
        let Some(mapper) = mapper else {
            return Ok(into_target.unwrap_or_else(|| Value::hash(HashMap::new())));
        };

        // Flatten list/array arguments into individual items and force lazy inputs.
        let mut items = Vec::new();
        for arg in &positional {
            match arg {
                Value::Array(values, ..) | Value::Seq(values) | Value::Slip(values) => {
                    items.extend(values.iter().cloned())
                }
                Value::LazyList(ll) => items.extend(self.force_lazy_list_bridge(ll)?),
                v if v.is_range() => items.extend(crate::runtime::utils::value_to_list(v)),
                other => items.push(other.clone()),
            }
        }

        let mut buckets: HashMap<String, Value> = match into_target.as_ref() {
            Some(Value::Hash(map)) => map.as_ref().clone(),
            _ => HashMap::new(),
        };
        let mut bag_counts: Option<HashMap<String, i64>> = match into_target.as_ref() {
            Some(Value::Bag(b)) => Some(b.as_ref().clone()),
            _ => None,
        };
        let mut mix_counts: Option<HashMap<String, f64>> = match into_target.as_ref() {
            Some(Value::Mix(m)) => Some(m.as_ref().clone()),
            _ => None,
        };

        for item in &items {
            let mapped = match &mapper {
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                    self.call_sub_value(mapper.clone(), vec![item.clone()], true)?
                }
                Value::Hash(map) => map
                    .get(&item.to_string_value())
                    .cloned()
                    .unwrap_or(Value::Nil),
                Value::Array(values, ..) => {
                    let idx = crate::runtime::to_int(item);
                    if idx < 0 {
                        Value::Nil
                    } else {
                        values.get(idx as usize).cloned().unwrap_or(Value::Nil)
                    }
                }
                _ => Value::Nil,
            };
            let mapped = match mapped {
                Value::LazyList(ll) => Value::array(self.force_lazy_list_bridge(&ll)?),
                other => other,
            };

            let mapped_item = if let Some(as_fn) = &as_mapper {
                self.call_sub_value(as_fn.clone(), vec![item.clone()], true)?
            } else {
                item.clone()
            };

            if bag_counts.is_some() || mix_counts.is_some() {
                let paths = if name == "categorize" {
                    mapper_categories(mapped)
                } else {
                    let path = mapper_path(mapped);
                    if path.is_empty() {
                        Vec::new()
                    } else {
                        vec![path]
                    }
                };
                for path in paths {
                    if path.len() != 1 {
                        return Err(RuntimeError::new(format!(
                            "X::Invalid::ComputedValue: multi-level {}",
                            if name == "categorize" {
                                "categorization"
                            } else {
                                "classification"
                            }
                        )));
                    }
                    let key = path[0].to_string_value();
                    if let Some(counts) = bag_counts.as_mut() {
                        *counts.entry(key.clone()).or_insert(0) += 1;
                    }
                    if let Some(counts) = mix_counts.as_mut() {
                        *counts.entry(key).or_insert(0.0) += 1.0;
                    }
                }
                continue;
            }

            let paths = if name == "categorize" {
                mapper_categories(mapped)
            } else {
                let path = mapper_path(mapped);
                if path.is_empty() {
                    Vec::new()
                } else {
                    vec![path]
                }
            };
            for path in paths {
                insert_nested_bucket(&mut buckets, &path, mapped_item.clone(), name)?;
            }
        }

        // Write back result to the caller's variable if :into was specified
        {
            let has_proxy = into_target_raw
                .as_ref()
                .is_some_and(|value| matches!(value, Value::Proxy { .. }));
            let has_varname = into_varname.is_some();
            if has_proxy || has_varname {
                let mut updated: Option<Value> = None;
                if let Some(counts) = bag_counts.clone() {
                    updated = Some(Value::bag(counts));
                } else if let Some(counts) = mix_counts.clone() {
                    updated = Some(Value::mix(counts));
                } else if into_target.is_some() {
                    updated = Some(Value::hash(buckets.clone()));
                }
                if let Some(new_value) = updated {
                    if has_proxy {
                        let _ = self.assign_proxy_lvalue(into_target_raw.unwrap(), new_value)?;
                    } else if let Some(ref vname) = into_varname {
                        self.env_mut().insert(vname.clone(), new_value);
                    }
                }
            }
        }

        if let Some(counts) = bag_counts {
            return Ok(Value::bag(counts));
        }
        if let Some(counts) = mix_counts {
            return Ok(Value::mix(counts));
        }

        Ok(Value::hash(buckets))
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
                    let mut values = super::utils::value_to_list(arg);
                    if values.len() == 1
                        && let Some(single) = values.first()
                    {
                        match single {
                            Value::Array(items, _) => {
                                values = items.as_ref().clone();
                            }
                            Value::Seq(items) | Value::Slip(items) => {
                                values = items.as_ref().clone();
                            }
                            _ => {}
                        }
                    }
                    lists.push(values);
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
            return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
        }

        // Implement Raku's single-arg rule (+@lol): when called with a single
        // iterable arg, iterate it to get the list of streams.
        let effective_args: Vec<Value> = if args.len() == 1 {
            match &args[0] {
                Value::Array(items, kind) if kind.is_itemized() => args.to_vec(),
                Value::Array(items, _) => items.iter().cloned().collect(),
                Value::Seq(items) | Value::Slip(items) => items.iter().cloned().collect(),
                _ => args.to_vec(),
            }
        } else {
            args.to_vec()
        };

        if effective_args.is_empty() {
            return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
        }

        let streams: Vec<Vec<Value>> = effective_args
            .iter()
            .map(|arg| match arg {
                Value::Capture { positional, named }
                    if named.is_empty() && positional.len() == 1 =>
                {
                    vec![arg.clone()]
                }
                Value::Array(items, kind) if kind.is_itemized() => vec![arg.clone()],
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

        Ok(Value::Seq(std::sync::Arc::new(rounds)))
    }

    /// `duckmap(&block, \obj)` — apply block to each element; on type mismatch
    /// descend recursively into iterables, or return the element unchanged.
    pub(super) fn builtin_duckmap(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("duckmap requires a block and an object"));
        }
        let block = args[0].clone();
        let obj = args[1].clone();
        self.duckmap_iterate(&block, &obj)
    }

    /// `deepmap(&block, \obj)` — apply block to every leaf element, preserving structure.
    pub(super) fn builtin_deepmap(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("deepmap requires a block and an object"));
        }
        let block = args[0].clone();
        let obj = args[1].clone();
        self.deepmap_iterate(&block, &obj)
    }

    /// Iterate over the elements of a value, applying duckmap to each.
    /// This is the entry point for both the method and function forms.
    pub(crate) fn duckmap_iterate(
        &mut self,
        block: &Value,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        match target {
            Value::Array(items, kind) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.duckmap_element(block, item) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next => continue,
                        Err(e) if e.is_last => break,
                        Err(e) => return Err(e),
                    }
                }
                if kind.is_real_array() {
                    Ok(Value::real_array(result))
                } else {
                    Ok(Value::array(result))
                }
            }
            Value::Seq(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.duckmap_element(block, item) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next => continue,
                        Err(e) if e.is_last => break,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::Seq(std::sync::Arc::new(result)))
            }
            Value::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    match self.duckmap_element(block, v) {
                        Ok(mapped) => {
                            result.insert(k.clone(), mapped);
                        }
                        Err(e) if e.is_next => continue,
                        Err(e) if e.is_last => break,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::Hash(std::sync::Arc::new(result)))
            }
            // Single non-iterable value: try the block on it directly
            _ => self.duckmap_element(block, target),
        }
    }

    /// Recursively apply a block to every leaf element, preserving structure.
    pub(crate) fn deepmap_iterate(
        &mut self,
        block: &Value,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        self.deepmap_iterate_inner(block, target, false)
    }

    /// Inner recursive helper. `itemize_result` is true for nested calls
    /// so that sublists get wrapped in Scalar containers.
    fn deepmap_iterate_inner(
        &mut self,
        block: &Value,
        target: &Value,
        itemize_result: bool,
    ) -> Result<Value, RuntimeError> {
        match target {
            // Type objects (e.g. Array, Hash) — return as-is to avoid hanging
            Value::Package(_) => Ok(target.clone()),
            Value::Array(items, kind) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.deepmap_iterate_inner(block, item, true) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next => continue,
                        Err(e) => return Err(e),
                    }
                }
                let arr_kind = if kind.is_real_array() {
                    if itemize_result {
                        crate::value::ArrayKind::ItemArray
                    } else {
                        crate::value::ArrayKind::Array
                    }
                } else if itemize_result {
                    crate::value::ArrayKind::ItemList
                } else {
                    crate::value::ArrayKind::List
                };
                Ok(Value::Array(std::sync::Arc::new(result), arr_kind))
            }
            Value::Seq(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.deepmap_iterate_inner(block, item, true) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next => continue,
                        Err(e) => return Err(e),
                    }
                }
                if itemize_result {
                    // Itemize the result as a list
                    Ok(Value::Array(
                        std::sync::Arc::new(result),
                        crate::value::ArrayKind::ItemList,
                    ))
                } else {
                    Ok(Value::Seq(std::sync::Arc::new(result)))
                }
            }
            Value::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    match self.deepmap_iterate_inner(block, v, true) {
                        Ok(val) => {
                            result.insert(k.clone(), val);
                        }
                        Err(e) if e.is_next => continue,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::Hash(std::sync::Arc::new(result)))
            }
            // Leaf value: apply the block
            _ => self.call_sub_value(block.clone(), vec![target.clone()], false),
        }
    }

    /// `nodemap` — apply a block to each element without descending into sublists.
    pub(crate) fn nodemap_iterate(
        &mut self,
        block: &Value,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        match target {
            Value::Array(items, kind) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.call_sub_value(block.clone(), vec![item.clone()], false) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next => continue,
                        Err(e) => return Err(e),
                    }
                }
                if kind.is_real_array() {
                    Ok(Value::real_array(result))
                } else {
                    Ok(Value::array(result))
                }
            }
            Value::Seq(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.call_sub_value(block.clone(), vec![item.clone()], false) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next => continue,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::Seq(std::sync::Arc::new(result)))
            }
            // Single value: apply the block directly
            _ => self.call_sub_value(block.clone(), vec![target.clone()], false),
        }
    }

    /// Apply duckmap to a single element: try the block, on failure descend.
    fn duckmap_element(&mut self, block: &Value, value: &Value) -> Result<Value, RuntimeError> {
        // Try to call the block with this value
        match self.call_sub_value(block.clone(), vec![value.clone()], false) {
            Ok(result) => Ok(result),
            Err(e) if e.is_next || e.is_last || e.is_redo => {
                // Propagate loop control signals (next, last, redo)
                Err(e)
            }
            Err(_) => {
                // Block rejected the value (type mismatch, etc.)
                // Try to descend into iterable structures
                match value {
                    Value::Array(items, kind) => {
                        let mut result = Vec::new();
                        for item in items.iter() {
                            result.push(self.duckmap_element(block, item)?);
                        }
                        if kind.is_real_array() {
                            Ok(Value::real_array(result))
                        } else {
                            Ok(Value::array(result))
                        }
                    }
                    Value::Seq(items) => {
                        let mut result = Vec::new();
                        for item in items.iter() {
                            result.push(self.duckmap_element(block, item)?);
                        }
                        Ok(Value::Seq(std::sync::Arc::new(result)))
                    }
                    Value::Hash(map) => {
                        let mut result = std::collections::HashMap::new();
                        for (k, v) in map.iter() {
                            result.insert(k.clone(), self.duckmap_element(block, v)?);
                        }
                        Ok(Value::Hash(std::sync::Arc::new(result)))
                    }
                    // Not iterable — return unchanged
                    _ => Ok(value.clone()),
                }
            }
        }
    }
}
