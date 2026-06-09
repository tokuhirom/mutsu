use super::*;

impl Interpreter {
    pub(super) fn dispatch_to_set(&self, target: Value) -> Result<Value, RuntimeError> {
        // Check for lazy/infinite values
        if Self::is_lazy_for_coerce(&target) {
            return Err(RuntimeError::cannot_lazy_what("Set"));
        }
        let mut elems = HashSet::new();
        let mut original_keys: HashMap<String, Value> = HashMap::new();
        let mut has_non_str = false;
        // `flatten` is true when this item sits in a list-context-flattening
        // position (an element of a List `(...)` invocant): nested Lists/Arrays/
        // Seqs/Hashes spill their contents. It is false for elements of an Array
        // `[...]` invocant, where each element is taken whole (matches Raku:
        // `[1,[2,3]].Set` has two elements, but `(1,[2,3]).Set` has three).
        fn add_item(
            elems: &mut HashSet<String>,
            original_keys: &mut HashMap<String, Value>,
            has_non_str: &mut bool,
            item: &Value,
            flatten: bool,
        ) {
            match item {
                Value::Pair(k, v) => {
                    if v.truthy() {
                        elems.insert(k.clone());
                    }
                }
                Value::ValuePair(k, v) => {
                    if v.truthy() {
                        let str_key = k.to_string_value();
                        if !matches!(k.as_ref(), Value::Str(_)) {
                            *has_non_str = true;
                            original_keys
                                .entry(str_key.clone())
                                .or_insert_with(|| k.as_ref().clone());
                        }
                        elems.insert(str_key);
                    }
                }
                Value::Hash(h) if flatten => {
                    for (k, v) in h.iter() {
                        if v.truthy() {
                            elems.insert(k.clone());
                        }
                    }
                }
                Value::Array(inner, kind) if flatten && !kind.is_itemized() => {
                    for inner_item in inner.iter() {
                        add_item(elems, original_keys, has_non_str, inner_item, true);
                    }
                }
                Value::Seq(inner) | Value::Slip(inner) if flatten => {
                    for inner_item in inner.iter() {
                        add_item(elems, original_keys, has_non_str, inner_item, true);
                    }
                }
                Value::Str(_) => {
                    elems.insert(item.to_string_value());
                }
                _ => {
                    let str_key = item.to_string_value();
                    *has_non_str = true;
                    original_keys
                        .entry(str_key.clone())
                        .or_insert_with(|| item.clone());
                    elems.insert(str_key);
                }
            }
        }
        match target {
            Value::Set(_, _) => return Ok(target),
            // A List `(...)` invocant flattens its elements in list context; an
            // Array `[...]` (or itemized) invocant takes each element whole.
            Value::Array(items, crate::value::ArrayKind::List) => {
                for item in items.iter() {
                    add_item(&mut elems, &mut original_keys, &mut has_non_str, item, true);
                }
            }
            Value::Array(items, ..) => {
                for item in items.iter() {
                    add_item(
                        &mut elems,
                        &mut original_keys,
                        &mut has_non_str,
                        item,
                        false,
                    );
                }
            }
            Value::Seq(items) | Value::Slip(items) => {
                for item in items.iter() {
                    add_item(&mut elems, &mut original_keys, &mut has_non_str, item, true);
                }
            }
            Value::Hash(items) => {
                for (k, v) in items.iter() {
                    if v.truthy() {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Bag(b, _) => {
                for k in b.keys() {
                    elems.insert(k.clone());
                }
            }
            Value::Mix(m, _) => {
                for k in m.keys() {
                    elems.insert(k.clone());
                }
            }
            Value::Pair(k, v) => {
                if v.truthy() {
                    elems.insert(k);
                }
            }
            Value::ValuePair(k, v) => {
                if v.truthy() {
                    let str_key = k.to_string_value();
                    if !matches!(k.as_ref(), Value::Str(_)) {
                        has_non_str = true;
                        original_keys
                            .entry(str_key.clone())
                            .or_insert_with(|| k.as_ref().clone());
                    }
                    elems.insert(str_key);
                }
            }
            // Instance types composing Baggy: delegate to internal bag data
            Value::Instance { ref attributes, .. } if attributes.contains_key("__baggy_data__") => {
                let bag_data = attributes.get("__baggy_data__").unwrap().clone();
                return self.dispatch_to_set(bag_data);
            }
            other if other.is_range() => {
                for item in Self::value_to_list(&other) {
                    add_item(
                        &mut elems,
                        &mut original_keys,
                        &mut has_non_str,
                        &item,
                        false,
                    );
                }
            }
            other => {
                let str_key = other.to_string_value();
                if !matches!(&other, Value::Str(_)) {
                    has_non_str = true;
                    original_keys
                        .entry(str_key.clone())
                        .or_insert_with(|| other.clone());
                }
                elems.insert(str_key);
            }
        }
        if has_non_str {
            Ok(Value::set_typed(elems, original_keys))
        } else {
            Ok(Value::set(elems))
        }
    }

    /// Check if a value is lazy/infinite and cannot be coerced to a QuantHash.
    pub(super) fn is_lazy_for_coerce(value: &Value) -> bool {
        match value {
            Value::LazyList(_) => true,
            Value::Array(_, kind) if kind.is_lazy() => true,
            Value::GenericRange { start, end, .. } => {
                Self::is_infinite_endpoint(start) || Self::is_infinite_endpoint(end)
            }
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            _ => false,
        }
    }

    /// Operations that need the full (finite) list cannot run on a lazy or
    /// infinite source. Returns `Some(X::Cannot::Lazy)` when `method` is such an
    /// operation and `target` is lazy/infinite, else `None`. Matches raku, which
    /// throws rather than hanging while materializing the whole sequence.
    /// `.head`/`.first`/`.map`/`[]` handle laziness and are NOT listed here.
    pub(crate) fn lazy_guard_error(method: &str, target: &Value) -> Option<RuntimeError> {
        if !Self::is_lazy_for_coerce(target) {
            return None;
        }
        match method {
            "classify" | "categorize" => Some(RuntimeError::cannot_lazy_with_action(
                method,
                "Hash[Any,Mu]",
            )),
            "sort" | "combinations" | "permutations" => Some(RuntimeError::cannot_lazy(method)),
            _ => None,
        }
    }

    fn is_infinite_endpoint(v: &Value) -> bool {
        match v {
            Value::Num(n) => n.is_infinite(),
            Value::Rat(_, d) | Value::FatRat(_, d) => *d == 0,
            Value::Whatever | Value::HyperWhatever => true,
            _ => false,
        }
    }

    pub(super) fn dispatch_to_bag_with_what(
        &self,
        target: Value,
        what: &str,
    ) -> Result<Value, RuntimeError> {
        // Check for lazy/infinite inputs
        if Self::is_lazy_for_coerce(&target) {
            return Err(RuntimeError::cannot_lazy_what(what));
        }
        let mut counts: HashMap<String, i64> = HashMap::new();
        let mut original_keys: HashMap<String, Value> = HashMap::new();
        let mut has_non_str_keys = false;

        fn pair_weight(v: &Value) -> Result<i64, RuntimeError> {
            match v {
                Value::Int(i) => Ok(*i),
                Value::Num(n) => {
                    if n.is_nan() || n.is_infinite() {
                        return Err(RuntimeError::new(format!(
                            "X::Numeric::CannotConvert: Cannot convert {} to Int",
                            v.to_string_value()
                        )));
                    }
                    Ok(*n as i64)
                }
                Value::Rat(n, d) if *d != 0 => Ok(n / d),
                Value::FatRat(n, d) if *d != 0 => Ok(n / d),
                Value::Bool(b) => Ok(i64::from(*b)),
                Value::Complex(_, _) => Err(RuntimeError::new(
                    "X::Numeric::CannotConvert: Cannot convert Complex to Int".to_string(),
                )),
                Value::Str(s) => {
                    // Strings must be numeric to be valid bag weights
                    match s.parse::<i64>() {
                        Ok(i) => Ok(i),
                        Err(_) => Err(RuntimeError::new(format!(
                            "X::Str::Numeric: Cannot convert string '{}' to a number",
                            s
                        ))),
                    }
                }
                _ => Ok(i64::from(v.truthy())),
            }
        }

        fn add_item(
            counts: &mut HashMap<String, i64>,
            original_keys: &mut HashMap<String, Value>,
            has_non_str_keys: &mut bool,
            item: &Value,
        ) -> Result<(), RuntimeError> {
            match item {
                Value::Pair(k, v) => {
                    let weight = pair_weight(v)?;
                    if weight > 0 {
                        *counts.entry(k.clone()).or_insert(0) += weight;
                    }
                }
                Value::ValuePair(k, v) => {
                    let str_key = k.to_string_value();
                    if !matches!(k.as_ref(), Value::Str(_)) {
                        *has_non_str_keys = true;
                        original_keys
                            .entry(str_key.clone())
                            .or_insert_with(|| k.as_ref().clone());
                    }
                    let weight = pair_weight(v)?;
                    if weight > 0 {
                        *counts.entry(str_key).or_insert(0) += weight;
                    }
                }
                _ => {
                    let str_key = item.to_string_value();
                    if !matches!(item, Value::Str(_)) {
                        *has_non_str_keys = true;
                        original_keys
                            .entry(str_key.clone())
                            .or_insert_with(|| item.clone());
                    }
                    *counts.entry(str_key).or_insert(0) += 1;
                }
            }
            Ok(())
        }

        /// Flatten items from a value into the bag. `flatten` is true when the
        /// value sits in a list-context-flattening position (an element of a
        /// List `(...)` invocant): nested non-itemized arrays, seqs, hashes and
        /// quant-hashes spill. When false (an element of an Array `[...]`
        /// invocant) the value is taken whole as a single key (matches Raku:
        /// `[1,[2,3]].Bag` has two keys, `(1,[2,3]).Bag` has three).
        fn flatten_into(
            counts: &mut HashMap<String, i64>,
            original_keys: &mut HashMap<String, Value>,
            has_non_str_keys: &mut bool,
            value: &Value,
            flatten: bool,
        ) -> Result<(), RuntimeError> {
            match value {
                Value::Array(items, kind) if flatten && !kind.is_itemized() => {
                    for item in items.iter() {
                        flatten_into(counts, original_keys, has_non_str_keys, item, true)?;
                    }
                }
                Value::Seq(items) | Value::Slip(items) if flatten => {
                    for item in items.iter() {
                        flatten_into(counts, original_keys, has_non_str_keys, item, true)?;
                    }
                }
                Value::Hash(h) if flatten => {
                    for (k, v) in h.iter() {
                        let weight = pair_weight(v)?;
                        if weight > 0 {
                            *counts.entry(k.clone()).or_insert(0) += weight;
                        }
                    }
                }
                Value::Set(s, _) if flatten => {
                    for k in s.iter() {
                        counts.insert(k.clone(), 1);
                    }
                }
                Value::Mix(m, _) if flatten => {
                    for (k, v) in m.iter() {
                        counts.insert(k.clone(), *v as i64);
                    }
                }
                Value::Bag(b, _) if flatten => {
                    for (k, v) in b.iter() {
                        *counts.entry(k.clone()).or_insert(0) += *v;
                    }
                }
                other => {
                    add_item(counts, original_keys, has_non_str_keys, other)?;
                }
            }
            Ok(())
        }

        match target {
            Value::Bag(_, _) => return Ok(target),
            Value::Pair(_, _) | Value::ValuePair(_, _) => {
                add_item(
                    &mut counts,
                    &mut original_keys,
                    &mut has_non_str_keys,
                    &target,
                )?;
            }
            ref other if other.is_range() => {
                for item in Self::value_to_list(other) {
                    let str_key = item.to_string_value();
                    if !matches!(item, Value::Str(_)) {
                        has_non_str_keys = true;
                        original_keys
                            .entry(str_key.clone())
                            .or_insert_with(|| item.clone());
                    }
                    *counts.entry(str_key).or_insert(0) += 1;
                }
            }
            // A List `(...)` invocant flattens its elements in list context; an
            // Array `[...]` (or itemized) invocant takes each element whole.
            Value::Array(ref items, crate::value::ArrayKind::List) | Value::Seq(ref items) => {
                for item in items.iter() {
                    flatten_into(
                        &mut counts,
                        &mut original_keys,
                        &mut has_non_str_keys,
                        item,
                        true,
                    )?;
                }
            }
            Value::Array(ref items, _) => {
                for item in items.iter() {
                    flatten_into(
                        &mut counts,
                        &mut original_keys,
                        &mut has_non_str_keys,
                        item,
                        false,
                    )?;
                }
            }
            _ => {
                // Flatten the target into a list and process each item.
                // This handles tuples/lists like (@a, %x).Bag where arrays
                // and hashes need to be expanded.
                let items = Self::value_to_list(&target);
                if items.is_empty() && !matches!(target, Value::Array(_, _) | Value::Hash(_)) {
                    // Single non-collection value
                    add_item(
                        &mut counts,
                        &mut original_keys,
                        &mut has_non_str_keys,
                        &target,
                    )?;
                } else {
                    for item in &items {
                        flatten_into(
                            &mut counts,
                            &mut original_keys,
                            &mut has_non_str_keys,
                            item,
                            true,
                        )?;
                    }
                }
            }
        }
        if has_non_str_keys {
            Ok(Value::bag_typed(counts, original_keys))
        } else {
            Ok(Value::bag(counts))
        }
    }

    pub(super) fn mix_pair_weight(v: &Value) -> Result<f64, RuntimeError> {
        match v {
            Value::Int(i) => Ok(*i as f64),
            Value::Num(n) => {
                if n.is_infinite() {
                    let mut err = RuntimeError::new(format!(
                        "Value out of range. Is: {}, should be in -Inf^..^Inf",
                        if *n > 0.0 { "Inf" } else { "-Inf" }
                    ));
                    err.exception = Some(Box::new(Value::make_instance(
                        crate::symbol::Symbol::intern("X::OutOfRange"),
                        [
                            ("what".to_string(), Value::str_from("Value")),
                            ("got".to_string(), Value::Num(*n)),
                            ("range".to_string(), Value::str_from("-Inf^..^Inf")),
                        ]
                        .into_iter()
                        .collect(),
                    )));
                    Err(err)
                } else if n.is_nan() {
                    let mut err =
                        RuntimeError::new("Value out of range. Is: NaN, should be in -Inf^..^Inf");
                    err.exception = Some(Box::new(Value::make_instance(
                        crate::symbol::Symbol::intern("X::OutOfRange"),
                        [
                            ("what".to_string(), Value::str_from("Value")),
                            ("got".to_string(), Value::Num(*n)),
                            ("range".to_string(), Value::str_from("-Inf^..^Inf")),
                        ]
                        .into_iter()
                        .collect(),
                    )));
                    Err(err)
                } else {
                    Ok(*n)
                }
            }
            Value::Rat(n, d) if *d != 0 => Ok(*n as f64 / *d as f64),
            Value::Bool(b) => Ok(if *b { 1.0 } else { 0.0 }),
            Value::Complex(_, _) => {
                let mut err = RuntimeError::new(
                    "Cannot convert Complex to Real; use .re or .im to extract components",
                );
                err.exception = Some(Box::new(Value::make_instance(
                    crate::symbol::Symbol::intern("X::Numeric::Real"),
                    [
                        ("source".to_string(), v.clone()),
                        (
                            "reason".to_string(),
                            Value::str_from("Complex to Real conversion"),
                        ),
                        ("target".to_string(), Value::str_from("Real")),
                    ]
                    .into_iter()
                    .collect(),
                )));
                Err(err)
            }
            Value::Str(s) => {
                // Try to parse the string as a number
                if let Ok(n) = s.parse::<f64>() {
                    if n.is_infinite() || n.is_nan() {
                        let mut err = RuntimeError::new(format!(
                            "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}' (Str)",
                            s
                        ));
                        err.exception = Some(Box::new(Value::make_instance(
                            crate::symbol::Symbol::intern("X::Str::Numeric"),
                            [
                                ("source".to_string(), Value::str(s.to_string())),
                                (
                                    "reason".to_string(),
                                    Value::str_from(
                                        "base-10 number must begin with valid digits or '.'",
                                    ),
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        )));
                        return Err(err);
                    }
                    Ok(n)
                } else {
                    let mut err = RuntimeError::new(format!(
                        "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}' (Str)",
                        s
                    ));
                    err.exception = Some(Box::new(Value::make_instance(
                        crate::symbol::Symbol::intern("X::Str::Numeric"),
                        [
                            ("source".to_string(), Value::str(s.to_string())),
                            (
                                "reason".to_string(),
                                Value::str_from(
                                    "base-10 number must begin with valid digits or '.'",
                                ),
                            ),
                        ]
                        .into_iter()
                        .collect(),
                    )));
                    Err(err)
                }
            }
            _ => {
                if v.truthy() {
                    Ok(1.0)
                } else {
                    Ok(0.0)
                }
            }
        }
    }

    fn mix_add_item_with_keys(
        weights: &mut HashMap<String, f64>,
        mut original_keys: Option<&mut HashMap<String, Value>>,
        item: &Value,
        flatten: bool,
    ) -> Result<(), RuntimeError> {
        match item {
            Value::Pair(k, v) => {
                let w = Self::mix_pair_weight(v)?;
                *weights.entry(k.clone()).or_insert(0.0) += w;
            }
            Value::ValuePair(k, v) => {
                let w = Self::mix_pair_weight(v)?;
                let str_key = k.to_string_value();
                if let Some(ref mut orig) = original_keys
                    && !matches!(&**k, Value::Str(_))
                {
                    orig.entry(str_key.clone()).or_insert_with(|| (**k).clone());
                }
                *weights.entry(str_key).or_insert(0.0) += w;
            }
            Value::Hash(h) if flatten => {
                for (k, v) in h.iter() {
                    let w = Self::mix_pair_weight(v)?;
                    if w != 0.0 {
                        *weights.entry(k.clone()).or_insert(0.0) += w;
                    }
                }
            }
            // A nested non-itemized array / seq flattens one level in
            // list-context (List `(...)` invocant); an Array `[...]` invocant
            // element (flatten == false) is kept whole.
            Value::Array(items, kind) if flatten && !kind.is_itemized() => {
                for sub_item in items.iter() {
                    Self::mix_add_item_with_keys(
                        weights,
                        original_keys.as_deref_mut(),
                        sub_item,
                        true,
                    )?;
                }
            }
            Value::Seq(items) | Value::Slip(items) if flatten => {
                for sub_item in items.iter() {
                    Self::mix_add_item_with_keys(
                        weights,
                        original_keys.as_deref_mut(),
                        sub_item,
                        true,
                    )?;
                }
            }
            Value::Set(s, _) if flatten => {
                for k in s.iter() {
                    let typed = s.typed_key(k);
                    if let Some(ref mut orig) = original_keys
                        && !matches!(&typed, Value::Str(sv) if sv.as_ref() == k)
                    {
                        orig.entry(k.clone()).or_insert(typed);
                    }
                    *weights.entry(k.clone()).or_insert(0.0) += 1.0;
                }
            }
            Value::Bag(b, _) if flatten => {
                for (k, v) in b.iter() {
                    let typed = b.typed_key(k);
                    if let Some(ref mut orig) = original_keys
                        && !matches!(&typed, Value::Str(sv) if sv.as_ref() == k)
                    {
                        orig.entry(k.clone()).or_insert(typed);
                    }
                    *weights.entry(k.clone()).or_insert(0.0) += *v as f64;
                }
            }
            Value::Mix(m, _) if flatten => {
                for (k, v) in m.iter() {
                    let typed = m.typed_key(k);
                    if let Some(ref mut orig) = original_keys
                        && !matches!(&typed, Value::Str(sv) if sv.as_ref() == k)
                    {
                        orig.entry(k.clone()).or_insert(typed);
                    }
                    *weights.entry(k.clone()).or_insert(0.0) += v;
                }
            }
            _ => {
                let str_key = item.to_string_value();
                if let Some(ref mut orig) = original_keys
                    && !matches!(item, Value::Str(_))
                {
                    orig.entry(str_key.clone()).or_insert_with(|| item.clone());
                }
                *weights.entry(str_key).or_insert(0.0) += 1.0;
            }
        }
        Ok(())
    }

    /// Check if a value is a lazy iterable (infinite range, lazy list, etc.)
    pub(super) fn is_lazy_for_set_ops(v: &Value) -> bool {
        match v {
            Value::LazyList(_) => true,
            Value::Array(_, kind) if kind.is_lazy() => true,
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            Value::GenericRange { end, .. } => {
                let end_f = end.to_f64();
                end_f.is_infinite() && end_f.is_sign_positive()
            }
            _ => false,
        }
    }

    pub(super) fn dispatch_to_mix(&self, target: Value) -> Result<Value, RuntimeError> {
        // Check for lazy iterables
        if Self::is_lazy_for_set_ops(&target) {
            let mut err = RuntimeError::new("Cannot .Mix a lazy list");
            err.exception = Some(Box::new(Value::make_instance(
                crate::symbol::Symbol::intern("X::Cannot::Lazy"),
                [("what".to_string(), Value::str_from("Mix"))]
                    .into_iter()
                    .collect(),
            )));
            return Err(err);
        }
        let mut weights: HashMap<String, f64> = HashMap::new();
        let mut original_keys: HashMap<String, Value> = HashMap::new();
        match target {
            Value::Mix(_, _) => return Ok(target),
            // A List `(...)` invocant flattens its elements in list context; an
            // Array `[...]` (or itemized) invocant takes each element whole.
            Value::Array(items, crate::value::ArrayKind::List)
            | Value::Seq(items)
            | Value::Slip(items) => {
                for item in items.iter() {
                    Self::mix_add_item_with_keys(
                        &mut weights,
                        Some(&mut original_keys),
                        item,
                        true,
                    )?;
                }
            }
            Value::Array(items, ..) => {
                for item in items.iter() {
                    Self::mix_add_item_with_keys(
                        &mut weights,
                        Some(&mut original_keys),
                        item,
                        false,
                    )?;
                }
            }
            ref other @ (Value::Set(_, _)
            | Value::Bag(_, _)
            | Value::Pair(..)
            | Value::ValuePair(..)
            | Value::Hash(_)) => {
                Self::mix_add_item_with_keys(&mut weights, Some(&mut original_keys), other, true)?;
            }
            other if other.is_range() => {
                for item in Self::value_to_list(&other) {
                    *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                }
            }
            other => {
                let str_key = other.to_string_value();
                if !matches!(&other, Value::Str(_)) {
                    original_keys
                        .entry(str_key.clone())
                        .or_insert_with(|| other.clone());
                }
                weights.insert(str_key, 1.0);
            }
        }
        if original_keys.is_empty() {
            Ok(Value::mix(weights))
        } else {
            Ok(Value::mix_with_original_keys(weights, original_keys))
        }
    }

    pub(super) fn dispatch_to_map(&self, target: Value) -> Result<Value, RuntimeError> {
        // Map is stored as Hash internally in mutsu.
        // When converting from Hash to Map, decontainerize all values
        // (Map values are not wrapped in Scalar containers).
        match target {
            Value::Hash(ref map) => {
                // Check if already a Map (same identity)
                if self
                    .container_type_metadata(&target)
                    .and_then(|info| info.declared_type)
                    .is_some_and(|dt| dt == "Map")
                {
                    return Ok(target);
                }
                // Decontainerize values for the new Map
                let deconted: HashMap<String, Value> = map
                    .iter()
                    .map(|(k, v)| {
                        let deconted = match v {
                            Value::Scalar(inner) => (**inner).clone(),
                            other => other.clone(),
                        };
                        (k.clone(), deconted)
                    })
                    .collect();
                Ok(Value::hash(deconted))
            }
            _ => self.dispatch_to_hash_impl(target, true),
        }
    }

    pub(super) fn dispatch_to_hash(&self, target: Value) -> Result<Value, RuntimeError> {
        self.dispatch_to_hash_impl(target, true)
    }

    fn dispatch_to_hash_impl(&self, target: Value, check_odd: bool) -> Result<Value, RuntimeError> {
        match target {
            Value::Hash(_) => Ok(target),
            Value::Array(items, ..) => Self::items_to_hash(items.as_ref(), check_odd),
            Value::Seq(items) | Value::Slip(items) => {
                Self::items_to_hash(items.as_ref(), check_odd)
            }
            Value::Set(s, _) => {
                let mut map = HashMap::new();
                let mut original_keys = HashMap::new();
                let mut has_typed = false;
                for k in s.iter() {
                    map.insert(k.clone(), Value::Bool(true));
                    let typed = s.typed_key(k);
                    if !matches!(&typed, Value::Str(sv) if sv.as_ref() == k) {
                        has_typed = true;
                        original_keys.insert(k.clone(), typed);
                    }
                }
                let result = Value::hash(map);
                if has_typed {
                    original_keys.insert("__mutsu_setty_origin".to_string(), Value::Bool(true));
                    super::utils::register_hash_original_keys(&result, original_keys);
                }
                Ok(result)
            }
            Value::Bag(b, _) => {
                let mut map = HashMap::new();
                let mut original_keys = HashMap::new();
                let mut has_typed = false;
                for (k, v) in b.iter() {
                    map.insert(k.clone(), Value::Int(*v));
                    let typed = b.typed_key(k);
                    if !matches!(&typed, Value::Str(sv) if sv.as_ref() == k) {
                        has_typed = true;
                        original_keys.insert(k.clone(), typed);
                    }
                }
                let result = Value::hash(map);
                if has_typed {
                    original_keys.insert("__mutsu_setty_origin".to_string(), Value::Bool(true));
                    super::utils::register_hash_original_keys(&result, original_keys);
                }
                Ok(result)
            }
            Value::Mix(m, _) => {
                let mut map = HashMap::new();
                let mut original_keys = HashMap::new();
                let mut has_typed = false;
                for (k, v) in m.iter() {
                    map.insert(k.clone(), crate::value::mix_weight_to_value(*v));
                    let typed = m.typed_key(k);
                    if !matches!(&typed, Value::Str(sv) if sv.as_ref() == k) {
                        has_typed = true;
                        original_keys.insert(k.clone(), typed);
                    }
                }
                let result = Value::hash(map);
                if has_typed {
                    original_keys.insert("__mutsu_setty_origin".to_string(), Value::Bool(true));
                    super::utils::register_hash_original_keys(&result, original_keys);
                }
                Ok(result)
            }
            Value::Instance {
                ref class_name,
                ref attributes,
                ..
            } if class_name == "Match" => {
                // %($/) returns the named captures hash
                if let Some(named) = attributes.get("named") {
                    Ok(named.clone())
                } else {
                    Ok(Value::hash(HashMap::new()))
                }
            }
            other => {
                // Single non-Pair scalar: this is 1 element (odd), so throw
                if check_odd {
                    if let Value::Pair(k, v) = other {
                        let mut map = HashMap::new();
                        map.insert(k.to_string(), *v);
                        return Ok(Value::hash(map));
                    }
                    return Err(Self::make_odd_number_error(&[other]));
                }
                let mut map = HashMap::new();
                map.insert(other.to_string_value(), Value::Bool(true));
                Ok(Value::hash(map))
            }
        }
    }

    /// Convert a slice of items to a Hash, optionally checking for odd element count.
    fn items_to_hash(items: &[Value], check_odd: bool) -> Result<Value, RuntimeError> {
        if check_odd {
            // Count non-Pair elements to detect odd count
            let non_pair_count = items
                .iter()
                .filter(|v| !matches!(v, Value::Pair(..) | Value::ValuePair(..)))
                .count();
            if non_pair_count % 2 != 0 {
                return Err(Self::make_odd_number_error(items));
            }
        }
        let mut map = HashMap::new();
        let mut iter = items.iter();
        while let Some(item) = iter.next() {
            match item {
                Value::Pair(k, v) => {
                    map.insert(k.clone(), *v.clone());
                }
                Value::ValuePair(k, v) => {
                    map.insert(k.to_string_value(), *v.clone());
                }
                other => {
                    let key = other.to_string_value();
                    let value = iter.next().cloned().unwrap_or(Value::Nil);
                    map.insert(key, value);
                }
            }
        }
        Ok(Value::hash(map))
    }

    /// Create an X::Hash::Store::OddNumber error.
    fn make_odd_number_error(items: &[Value]) -> RuntimeError {
        let count = items.len();
        let last = items
            .last()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let message = if count == 1 {
            format!(
                "Odd number of elements found where hash initializer expected:\n\
                 Only saw: {}",
                last
            )
        } else {
            format!(
                "Odd number of elements found where hash initializer expected:\n\
                 Found {} (implicit) elements:\nLast element seen: {}",
                count, last
            )
        };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(
            crate::symbol::Symbol::intern("X::Hash::Store::OddNumber"),
            attrs,
        );
        RuntimeError {
            exception: Some(Box::new(ex)),
            ..RuntimeError::new(message)
        }
    }
}
