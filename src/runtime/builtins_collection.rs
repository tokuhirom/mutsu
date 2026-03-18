use super::*;
use crate::symbol::Symbol;
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

    pub(super) fn builtin_min(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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
        self.extrema_from_values(&positional, false)
    }

    pub(super) fn builtin_max(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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
            _ if value.as_list_items().is_some() => {
                out.extend(value.as_list_items().unwrap().iter().cloned());
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
        self.eval_map_over_items(func, list_items)
    }

    pub(super) fn builtin_grep(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let func = args.first().cloned();
        if args.len() == 1
            && matches!(
                args[0],
                Value::Bool(_) | Value::Int(_) | Value::Num(_) | Value::Str(_)
            )
        {
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
                let method_args: Vec<Value> = func.into_iter().collect();
                return self.call_method_with_values(args[1].clone(), "grep", method_args);
            }
        }
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
        match target {
            Value::Array(items, kind) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    result.push(self.deepmap_iterate(block, item)?);
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
                    result.push(self.deepmap_iterate(block, item)?);
                }
                Ok(Value::Seq(std::sync::Arc::new(result)))
            }
            Value::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    result.insert(k.clone(), self.deepmap_iterate(block, v)?);
                }
                Ok(Value::Hash(std::sync::Arc::new(result)))
            }
            // Leaf value: apply the block
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
