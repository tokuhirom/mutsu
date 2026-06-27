use super::*;

pub(crate) fn coerce_to_hash(value: Value) -> Value {
    let mix_weight_value = crate::value::mix_weight_to_value;
    let value = value.into_descalarized();
    match value {
        Value::Hash(_) => value,
        Value::Array(items, ..) => {
            // Flatten nested Hashes into pairs before building the hash.
            // This handles `%h = %h1, %h2` where each hash should be merged.
            // Itemized arrays ($[...]) are NOT flattened — they are treated
            // as opaque items, matching Raku's Scalar-container semantics.
            let mut flat: Vec<Value> = Vec::with_capacity(items.len());
            for item in items.iter() {
                if let Value::Hash(h) = item {
                    for (k, v) in h.iter() {
                        flat.push(Value::Pair(k.clone(), Box::new(v.clone())));
                    }
                } else {
                    flat.push(item.clone());
                }
            }
            let mut map = HashMap::new();
            let mut original_keys: HashMap<String, Value> = HashMap::new();
            let mut i = 0;
            while i < flat.len() {
                if let Value::Pair(k, v) = &flat[i] {
                    // A Pair value built by `key => $var` is a write-through
                    // `ContainerRef`; storing into a Hash decontainerizes (copies
                    // the value), matching Raku (`%h = k => $v; $v = 2` leaves
                    // `%h<k>` unchanged).
                    map.insert(k.clone(), v.deref_container());
                    i += 1;
                } else if let Value::ValuePair(k, v) = &flat[i] {
                    let str_key = k.to_string_value();
                    if !matches!(k.as_ref(), Value::Str(_)) {
                        original_keys.insert(str_key.clone(), *k.clone());
                    }
                    map.insert(str_key, v.deref_container());
                    i += 1;
                } else {
                    let key_val = &flat[i];
                    let str_key = key_val.to_string_value();
                    if !matches!(key_val, Value::Str(_)) {
                        original_keys.insert(str_key.clone(), key_val.clone());
                    }
                    let val = if i + 1 < flat.len() {
                        flat[i + 1].clone()
                    } else {
                        Value::Nil
                    };
                    map.insert(str_key, val);
                    i += 2;
                }
            }
            set_hash_original_keys(Value::hash(map), original_keys)
        }
        Value::Seq(items) | Value::HyperSeq(items) | Value::RaceSeq(items) | Value::Slip(items) => {
            let mut map = HashMap::new();
            let mut i = 0;
            while i < items.len() {
                if let Value::Pair(k, v) = &items[i] {
                    map.insert(k.clone(), v.deref_container());
                    i += 1;
                } else if let Value::ValuePair(k, v) = &items[i] {
                    map.insert(k.to_string_value(), v.deref_container());
                    i += 1;
                } else {
                    let key = items[i].to_string_value();
                    let val = if i + 1 < items.len() {
                        items[i + 1].clone()
                    } else {
                        Value::Nil
                    };
                    map.insert(key, val);
                    i += 2;
                }
            }
            Value::hash(map)
        }
        Value::Pair(k, v) => {
            let mut map = HashMap::new();
            map.insert(k, v.deref_container());
            Value::hash(map)
        }
        Value::ValuePair(k, v) => {
            let mut map = HashMap::new();
            map.insert(k.to_string_value(), v.deref_container());
            Value::hash(map)
        }
        Value::Set(items, _) => {
            let mut map = HashMap::new();
            let mut original_keys: HashMap<String, Value> = HashMap::new();
            let mut has_typed = false;
            for key in items.iter() {
                map.insert(key.clone(), Value::Bool(true));
                let typed = items.typed_key(key);
                if !matches!(&typed, Value::Str(sv) if sv.as_ref() == key) {
                    has_typed = true;
                    original_keys.insert(key.clone(), typed);
                }
            }
            let mut result = Value::hash(map);
            if has_typed {
                original_keys.insert("__mutsu_setty_origin".to_string(), Value::Bool(true));
                result = set_hash_original_keys(result, original_keys);
            }
            result
        }
        Value::Bag(items, _) => {
            let mut map = HashMap::new();
            let mut original_keys: HashMap<String, Value> = HashMap::new();
            let mut has_typed = false;
            for (key, count) in items.iter() {
                map.insert(key.clone(), Value::from_bigint(count.clone()));
                let typed = items.typed_key(key);
                if !matches!(&typed, Value::Str(sv) if sv.as_ref() == key) {
                    has_typed = true;
                    original_keys.insert(key.clone(), typed);
                }
            }
            let mut result = Value::hash(map);
            if has_typed {
                original_keys.insert("__mutsu_setty_origin".to_string(), Value::Bool(true));
                result = set_hash_original_keys(result, original_keys);
            }
            result
        }
        Value::Mix(items, _) => {
            let mut map = HashMap::new();
            let mut original_keys: HashMap<String, Value> = HashMap::new();
            let mut has_typed = false;
            for (key, weight) in items.iter() {
                map.insert(key.clone(), mix_weight_value(*weight));
                let typed = items.typed_key(key);
                if !matches!(&typed, Value::Str(sv) if sv.as_ref() == key) {
                    has_typed = true;
                    original_keys.insert(key.clone(), typed);
                }
            }
            let mut result = Value::hash(map);
            if has_typed {
                original_keys.insert("__mutsu_setty_origin".to_string(), Value::Bool(true));
                result = set_hash_original_keys(result, original_keys);
            }
            result
        }
        Value::Range(a, b) => {
            let items: Vec<Value> = (a..=b).map(Value::Int).collect();
            coerce_to_hash(Value::Array(
                crate::value::Value::array_arc(items),
                ArrayKind::List,
            ))
        }
        Value::RangeExcl(a, b) => {
            let items: Vec<Value> = (a..b).map(Value::Int).collect();
            coerce_to_hash(Value::Array(
                crate::value::Value::array_arc(items),
                ArrayKind::List,
            ))
        }
        Value::RangeExclStart(a, b) => {
            let items: Vec<Value> = (a + 1..=b).map(Value::Int).collect();
            coerce_to_hash(Value::Array(
                crate::value::Value::array_arc(items),
                ArrayKind::List,
            ))
        }
        Value::RangeExclBoth(a, b) => {
            let items: Vec<Value> = (a + 1..b).map(Value::Int).collect();
            coerce_to_hash(Value::Array(
                crate::value::Value::array_arc(items),
                ArrayKind::List,
            ))
        }
        Value::Nil => Value::hash(HashMap::new()),
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Match" => {
            // %($/) returns the named captures hash
            if let Some(named) = attributes.as_map().get("named") {
                named.clone()
            } else {
                Value::hash(HashMap::new())
            }
        }
        _ => {
            let mut map = HashMap::new();
            map.insert(value.to_string_value(), Value::Nil);
            Value::hash(map)
        }
    }
}

pub(crate) fn build_hash_from_items(items: Vec<Value>) -> Result<Value, RuntimeError> {
    let total_items = items.len();
    let last_item = items
        .last()
        .map(Value::to_string_value)
        .unwrap_or_else(|| "Nil".to_string());
    let mut map = HashMap::new();
    let mut original_keys: HashMap<String, Value> = HashMap::new();
    let mut iter = items.into_iter();
    while let Some(item) = iter.next() {
        match item {
            Value::Pair(key, boxed_val) => {
                map.insert(key, *boxed_val);
            }
            // A bare (non-itemized) hash in list context flattens into its
            // key=>value pairs (`%m = (%h,)` / `%(%h,)`). A hash sourced from a
            // `$` scalar carries `HashData.itemized` (set by `itemize_value`) and
            // stays an opaque single element — matching Raku, where `%m =
            // ($hashitem,)` dies "Odd number". Keys are the source hash's string
            // keys: flattening an object hash into a plain Hash/Map stringifies
            // them (the target re-tags via its own key-type metadata).
            Value::Hash(ref h) if !h.itemized => {
                for (k, v) in h.iter() {
                    map.insert(k.clone(), v.clone());
                }
            }
            Value::ValuePair(key, boxed_val) => {
                let str_key = Value::hash_key_encode(&key);
                if !matches!(key.as_ref(), Value::Str(_)) {
                    original_keys.insert(str_key.clone(), *key);
                }
                map.insert(str_key, *boxed_val);
            }
            other => {
                let Some(value) = iter.next() else {
                    let message = format!(
                        "Odd number of elements found where hash initializer expected: found {total_items} element(s); last element seen: {last_item}"
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "message".to_string(),
                        crate::value::Value::str(message.clone()),
                    );
                    let ex = crate::value::Value::make_instance(
                        crate::symbol::Symbol::intern("X::Hash::Store::OddNumber"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(message);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                };
                let str_key = Value::hash_key_encode(&other);
                if !matches!(&other, Value::Str(_)) {
                    original_keys.insert(str_key.clone(), other);
                }
                map.insert(str_key, value);
            }
        }
    }
    Ok(set_hash_original_keys(Value::hash(map), original_keys))
}

/// Maximum number of elements when expanding an infinite range into an Array.
/// TODO: Properly implement lazy arrays that reify elements on demand.
const MAX_ARRAY_EXPAND: i64 = 100_000;

pub(crate) fn coerce_to_array(value: Value) -> Value {
    fn metadata_shape_for_items(items: &Arc<crate::value::ArrayData>) -> Option<Vec<usize>> {
        items.shape.clone()
    }

    match value {
        Value::Array(items, kind) => {
            // Assigning an array to an `@` variable snapshots element VALUES
            // (Raku `=` semantics). A `:=`-bound element is a shared
            // `ContainerRef` cell (Phase 2); decontainerize it on copy so a
            // later write through the bound source does not leak into the copy.
            // Only rebuild when a cell is actually present (common path keeps
            // sharing the Arc, so there is no per-assignment cost).
            let items = if items.iter().any(|v| matches!(v, Value::ContainerRef(_))) {
                Arc::new(items.iter().map(|v| v.deref_container()).collect())
            } else {
                items
            };
            if kind.is_itemized() {
                // Itemized arrays (from `$` scalar containers) are treated as
                // a single item when assigned to an `@` variable.
                Value::real_array(vec![Value::Array(items, kind)])
            } else if kind == ArrayKind::Shaped {
                Value::Array(items, kind)
            } else if let Some(shape) = metadata_shape_for_items(&items) {
                let value = Value::Array(items, ArrayKind::Shaped);
                mark_shaped_array(&value, Some(&shape));
                value
            } else {
                Value::Array(items, ArrayKind::Array)
            }
        }
        Value::Nil => Value::real_array(vec![Value::Package(crate::symbol::Symbol::intern("Any"))]),
        Value::Range(a, b) => {
            if b == i64::MAX {
                // Infinite range — mark as lazy
                let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
                Value::Array(
                    Arc::new((a..=end).map(Value::Int).collect()),
                    ArrayKind::Lazy,
                )
            } else {
                let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
                Value::real_array((a..=end).map(Value::Int).collect())
            }
        }
        Value::RangeExcl(a, b) => {
            if b == i64::MAX {
                let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
                Value::Array(
                    Arc::new((a..end).map(Value::Int).collect()),
                    ArrayKind::Lazy,
                )
            } else {
                let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
                Value::real_array((a..end).map(Value::Int).collect())
            }
        }
        Value::RangeExclStart(a, b) => {
            if b == i64::MAX {
                let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
                Value::Array(
                    Arc::new((a + 1..=end).map(Value::Int).collect()),
                    ArrayKind::Lazy,
                )
            } else {
                let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
                Value::real_array((a + 1..=end).map(Value::Int).collect())
            }
        }
        Value::RangeExclBoth(a, b) => {
            if b == i64::MAX {
                let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
                Value::Array(
                    Arc::new((a + 1..end).map(Value::Int).collect()),
                    ArrayKind::Lazy,
                )
            } else {
                let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
                Value::real_array((a + 1..end).map(Value::Int).collect())
            }
        }
        Value::GenericRange {
            ref start, ref end, ..
        } if matches!(start.as_ref(), Value::Str(_)) && matches!(end.as_ref(), Value::Str(_)) => {
            Value::real_array(value_to_list(&value))
        }
        Value::GenericRange { ref end, .. } => {
            let end_f = end.to_f64();
            if end_f.is_infinite() && end_f.is_sign_positive() {
                Value::Array(
                    Arc::new(crate::value::ArrayData::new(value_to_list(&value))),
                    ArrayKind::Lazy,
                )
            } else {
                Value::real_array(value_to_list(&value))
            }
        }
        Value::Slip(items) | Value::Seq(items) | Value::HyperSeq(items) | Value::RaceSeq(items) => {
            Value::Array(
                crate::value::Value::array_arc(items.to_vec()),
                ArrayKind::Array,
            )
        }
        Value::LazyList(_) => value,
        // A bare Hash assigned to an @-var flattens into its pairs
        // (`my @a = %h`). An *itemized* hash (`my $h = %(...); my @a = $h`)
        // stays a single element — `ItemizeVar` now tags it with
        // `HashData.itemized` rather than the old `Scalar(Hash)` wrapper, so it
        // must be handled here too (the `Scalar(inner)` arm below covers the
        // Set/Bag/Mix itemized forms, which still use the wrapper).
        Value::Hash(ref map) if !map.itemized => {
            let pairs: Vec<Value> = map
                .iter()
                .map(|(k, v)| map.typed_pair(k, v.clone()))
                .collect();
            Value::real_array(pairs)
        }
        Value::Hash(map) => Value::real_array(vec![Value::Hash(map)]),
        // A scalar holding a Hash/Set/Bag/Mix (itemized by `ItemizeVar` as
        // `Scalar(container)`) stays a single element, but unwrap the Scalar so
        // `@a[0]` is the bare container (preserving its `.gist`/`.raku`/type),
        // rather than an opaque `$(...)`-wrapped value.
        Value::Scalar(inner)
            if matches!(
                inner.as_ref(),
                Value::Hash(_) | Value::Set(..) | Value::Bag(..) | Value::Mix(..)
            ) =>
        {
            Value::real_array(vec![*inner])
        }
        // Set/Bag/Mix assigned to an @-var flatten into their `key => weight`
        // pairs in list context, exactly like a Hash (Raku: `my @a = set(1,2,3)`
        // yields three `* => True` pairs, so `@a.elems == 3`). This mirrors
        // `value_to_list`. (Note: an array *literal* `[set(...)]` does NOT
        // flatten — that path is handled separately in `exec_make_array_op`.)
        Value::Set(ref items, _) => Value::real_array(
            items
                .iter()
                .map(|s| Value::Pair(s.clone(), Box::new(Value::Bool(true))))
                .collect(),
        ),
        Value::Bag(ref items, _) => Value::real_array(
            items
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::from_bigint(v.clone()))))
                .collect(),
        ),
        Value::Mix(ref items, _) => Value::real_array(
            items
                .iter()
                .map(|(k, v)| {
                    Value::Pair(k.clone(), Box::new(crate::value::mix_weight_to_value(*v)))
                })
                .collect(),
        ),
        // A WalkList assigned to an `@` variable flattens to its candidate
        // closures, so `my @cands = $x.WALK(...)` yields the per-level candidates.
        Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } if class_name.resolve() == "WalkList" => match walk_list_candidates(attributes) {
            Some(cands) => Value::real_array(cands),
            None => Value::real_array(vec![value.clone()]),
        },
        other => Value::real_array(vec![other]),
    }
}

pub(crate) fn coerce_to_str(value: &Value) -> String {
    value.to_str_context()
}
