use super::*;

impl Interpreter {
    pub(super) fn builtin_classify(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        fn as_items(value: &Value) -> Option<Vec<Value>> {
            match value.view() {
                ValueView::Array(items, ..) => Some(items.iter().cloned().collect()),
                ValueView::Seq(items) | ValueView::Slip(items) => {
                    Some(items.iter().cloned().collect())
                }
                _ => None,
            }
        }

        fn mapper_path(value: Value) -> Vec<Value> {
            as_items(&value).unwrap_or_else(|| vec![value])
        }

        /// Returns (paths, is_multi_level).
        /// `is_multi_level` is true when the mapper returned a list whose
        /// elements are themselves lists (multi-level paths), as opposed to
        /// a flat list of single category names.
        fn mapper_categories(value: Value) -> (Vec<Vec<Value>>, bool) {
            if let Some(items) = as_items(&value) {
                if items.is_empty() {
                    return (Vec::new(), false);
                }
                let has_list_element = items.iter().any(|i| as_items(i).is_some());
                let mut out = Vec::new();
                for item in items {
                    out.push(mapper_path(item));
                }
                (out, has_list_element)
            } else {
                (vec![vec![value]], false)
            }
        }

        fn mixed_level_error(name: &str) -> RuntimeError {
            make_mixed_level_err(name)
        }

        fn make_mixed_level_err(name: &str) -> RuntimeError {
            let kind = if name == "categorize" {
                "categorization"
            } else {
                "classification"
            };
            let msg = format!(
                "mapper on {}-list computed to an item with different number of elements in it than previous items, which cannot be used because all values need to have the same number of elements. Mixed-level {} is not supported.",
                name, kind
            );
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let exception =
                Value::make_instance(Symbol::intern("X::Invalid::ComputedValue"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(exception));
            err
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
                let is_array = matches!(entry.view(), ValueView::Array(..));
                let is_hash = matches!(entry.view(), ValueView::Hash(_));
                if is_array {
                    entry.with_array_mut(|values, _| crate::gc::Gc::make_mut(values).push(item));
                    Ok(())
                } else if is_hash {
                    Err(mixed_level_error(name))
                } else {
                    *entry = Value::real_array(vec![item]);
                    Ok(())
                }
            } else {
                let entry = buckets
                    .entry(key)
                    .or_insert_with(|| Value::hash(HashMap::new()));
                let is_hash = matches!(entry.view(), ValueView::Hash(_));
                let is_array = matches!(entry.view(), ValueView::Array(..));
                // Record the next path component's key object in the nested
                // hash's `original_keys` so the multi-level result can be
                // re-keyed by `.WHICH` per level (nested buckets are `Mu`-keyed
                // object hashes in raku, like the top level).
                let recurse = |map: &mut crate::gc::Gc<crate::value::HashData>,
                               item: Value|
                 -> Result<(), RuntimeError> {
                    let data = crate::gc::Gc::make_mut(map);
                    if !matches!(path[1].view(), ValueView::Str(_)) {
                        data.original_keys
                            .get_or_insert_with(HashMap::new)
                            .insert(path[1].to_string_value(), path[1].clone());
                    }
                    insert_nested_bucket(&mut data.map, &path[1..], item, name)
                };
                if is_hash {
                    entry
                        .with_hash_mut(|map| recurse(map, item.clone()))
                        .unwrap()
                } else if is_array {
                    Err(mixed_level_error(name))
                } else {
                    *entry = Value::hash(HashMap::new());
                    entry
                        .with_hash_mut(|map| recurse(map, item.clone()))
                        .unwrap_or(Ok(()))
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
            match fetched_arg.view() {
                ValueView::Pair(key, value) if key == "as" => {
                    as_mapper = Some(self.auto_fetch_proxy(value)?)
                }
                ValueView::Pair(key, value) if key == "into" => {
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
                    into_target_raw = Some(value.clone());
                    into_target = Some(self.auto_fetch_proxy(value)?);
                }
                _ => {
                    if mapper.is_none() {
                        mapper = Some(fetched_arg.clone());
                    } else {
                        positional.push(fetched_arg.clone());
                    }
                }
            }
        }
        let Some(mapper) = mapper else {
            return Ok(into_target.unwrap_or_else(|| Value::hash(HashMap::new())));
        };

        // A list element that is a named-marker `Pair` is data here, not a
        // call-site named argument — decay it to a positional `ValuePair` so
        // the classifier block receives it as its topic.
        fn callable_item(item: &Value) -> Value {
            match item.view() {
                ValueView::Pair(k, v) => Value::value_pair(Value::str_from(k), v.clone()),
                _ => item.clone(),
            }
        }

        // Check for lazy lists — classify/categorize cannot operate on lazy lists
        for arg in &positional {
            let is_lazy = matches!(arg.view(), ValueView::LazyList(_));
            if is_lazy {
                let mut attrs = HashMap::new();
                attrs.insert(
                    "action".to_string(),
                    Value::str_from(if name == "categorize" {
                        ".categorize-list"
                    } else {
                        ".classify-list"
                    }),
                );
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("Cannot {}-list a lazy list", name)),
                );
                let exception = Value::make_instance(Symbol::intern("X::Cannot::Lazy"), attrs);
                let mut err = RuntimeError::new("X::Cannot::Lazy");
                err.exception = Some(Box::new(exception));
                return Err(err);
            }
        }

        // Flatten list/array arguments into individual items and force lazy inputs.
        let mut items = Vec::new();
        for arg in &positional {
            match arg.view() {
                ValueView::Array(values, ..) => items.extend(values.iter().cloned()),
                ValueView::Seq(values) | ValueView::Slip(values) => {
                    items.extend(values.iter().cloned())
                }
                ValueView::LazyList(ll) => items.extend(self.force_lazy_list_bridge(&ll)?),
                // Hash/Set/Bag/Mix classify their pairs (elem => True for Set,
                // elem => count/weight for Bag/Mix), same as for-iteration.
                ValueView::Hash(_)
                | ValueView::Set(..)
                | ValueView::Bag(..)
                | ValueView::Mix(..) => items.extend(crate::runtime::utils::value_to_list(arg)),
                _ if arg.is_range() => items.extend(crate::runtime::utils::value_to_list(arg)),
                _ => items.push(arg.clone()),
            }
        }

        // Object-hash key preservation (§3.3): when the classifier returns a
        // non-`Str` key (e.g. a Junction from `*.contains: any 'a','f'`), the
        // bucket key is stored under its stringification but the result must be
        // an *object hash* so `$result{ any(...) }` is a by-key lookup (not
        // junction autothreading) and `.keys` yields the real key objects.
        // Records each first-level non-Str key object under its encoded string.
        let mut object_keys: HashMap<String, Value> = HashMap::new();
        // An `:into` object hash stores `.WHICH` keys: seed the (raw-string-
        // keyed) working buckets from its original key objects, and remember
        // its key type so the result keeps the object-hash identity.
        let mut into_key_type: Option<String> = None;
        let mut buckets: HashMap<String, Value> = match into_target.as_ref().map(Value::view) {
            Some(ValueView::Hash(map)) => {
                into_key_type = map.key_type.clone();
                if map.has_typed_keys() {
                    let mut seeded = HashMap::with_capacity(map.len());
                    for (k, v) in map.iter() {
                        let obj = map.typed_key(k);
                        let str_key = obj.to_string_value();
                        if !matches!(obj.view(), ValueView::Str(_)) {
                            object_keys.insert(str_key.clone(), obj);
                        }
                        seeded.insert(str_key, v.clone());
                    }
                    seeded
                } else {
                    map.as_ref().map.clone()
                }
            }
            _ => HashMap::new(),
        };
        let mut bag_counts: Option<HashMap<String, i64>> = match into_target
            .as_ref()
            .map(Value::view)
        {
            Some(ValueView::Bag(b, _)) => Some(crate::runtime::utils::bag_counts_as_i64(&b.counts)),
            _ => None,
        };
        let mut mix_counts: Option<HashMap<String, f64>> =
            match into_target.as_ref().map(Value::view) {
                Some(ValueView::Mix(m, _)) => Some(m.weights.clone()),
                _ => None,
            };

        // Track classification level depth across all items for mixed-level detection
        let mut expected_level: Option<usize> = None;
        let mut expected_multi_level: Option<bool> = None;

        for item in &items {
            let mapped = match mapper.view() {
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => {
                    self.call_sub_value(mapper.clone(), vec![callable_item(item)], true)?
                }
                ValueView::Hash(map) => map
                    .get(&item.to_string_value())
                    .cloned()
                    .unwrap_or(Value::NIL),
                ValueView::Array(values, ..) => {
                    let idx = crate::runtime::to_int(item);
                    if idx < 0 {
                        Value::NIL
                    } else {
                        values.get(idx as usize).cloned().unwrap_or(Value::NIL)
                    }
                }
                _ => Value::NIL,
            };
            let mapped = if let ValueView::LazyList(ll) = mapped.view() {
                Value::array(self.force_lazy_list_bridge(&ll)?)
            } else {
                mapped
            };

            let mapped_item = if let Some(as_fn) = &as_mapper {
                self.call_sub_value(as_fn.clone(), vec![callable_item(item)], true)?
            } else {
                item.clone()
            };

            if bag_counts.is_some() || mix_counts.is_some() {
                let paths = if name == "categorize" {
                    let (p, _) = mapper_categories(mapped);
                    p
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

            let (paths, is_multi_level) = if name == "categorize" {
                mapper_categories(mapped)
            } else {
                let path = mapper_path(mapped);
                let ml = path.len() > 1;
                let v = if path.is_empty() {
                    Vec::new()
                } else {
                    vec![path]
                };
                (v, ml)
            };
            if !paths.is_empty() {
                if let Some(prev_ml) = expected_multi_level {
                    if is_multi_level != prev_ml {
                        return Err(make_mixed_level_err(name));
                    }
                } else {
                    expected_multi_level = Some(is_multi_level);
                }
            }
            for path in &paths {
                if let Some(expected) = expected_level {
                    if path.len() != expected {
                        return Err(make_mixed_level_err(name));
                    }
                } else {
                    expected_level = Some(path.len());
                }
                if let Some(first) = path.first()
                    && !matches!(first.view(), ValueView::Str(_))
                {
                    object_keys
                        .entry(first.to_string_value())
                        .or_insert_with(|| first.clone());
                }
                insert_nested_bucket(&mut buckets, path, mapped_item.clone(), name)?;
            }
        }

        // Write back result to the caller's variable if :into was specified
        {
            let has_proxy = into_target_raw
                .as_ref()
                .is_some_and(|value| matches!(value.view(), ValueView::Proxy { .. }));
            let has_varname = into_varname.is_some();
            if has_proxy || has_varname {
                let mut updated: Option<Value> = None;
                if let Some(counts) = bag_counts.clone() {
                    updated = Some(Value::bag(counts));
                } else if let Some(counts) = mix_counts.clone() {
                    updated = Some(Value::mix(counts));
                } else if into_target.is_some() {
                    updated = Some(Self::classify_finish_hash(
                        buckets.clone(),
                        object_keys.clone(),
                        into_key_type
                            .clone()
                            .or_else(|| (!object_keys.is_empty()).then(|| "Any".to_string())),
                    ));
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

        // The standalone `.classify`/`.categorize` (no `:into` target) mints a
        // fresh `Hash[Mu,Mu]` (the `:{...}` shape); `classify-list`/`:into(%h)`
        // classify into an existing container and keep its type.
        let key_type = if into_target.is_none() {
            Some("Mu".to_string())
        } else {
            into_key_type.or_else(|| (!object_keys.is_empty()).then(|| "Any".to_string()))
        };
        Ok(Self::classify_finish_hash(buckets, object_keys, key_type))
    }

    /// Wrap classify's bucket map in a `Hash`, marking it an *object hash*
    /// (typed keys) when any classifier key was non-`Str` (e.g. a Junction). An
    /// object hash makes `$result{ $key }` a by-key lookup rather than junction
    /// autothreading, and `.keys` yield the real key objects.
    /// Itemize each bucket's value list so `%classified<key>` behaves as a
    /// single (non-flattening) array — Raku stores each bucket as `$[...]`, so
    /// `my @a = %c<k>` yields one element and `for %c<k> {}` runs once. Recurses
    /// into nested categorize buckets (multi-level paths become nested hashes).
    fn itemize_bucket_value(mut v: Value, object_hash: bool) -> Value {
        if matches!(v.view(), ValueView::Array(..)) {
            let (items, kind) = v.into_array().unwrap();
            return Value::array_with_kind(items, kind.itemize());
        }
        v.with_hash_mut(|arc| {
            {
                let data = crate::gc::Gc::make_mut(arc);
                let keys: Vec<String> = data.map.keys().cloned().collect();
                for k in keys {
                    if let Some(val) = data.map.remove(&k) {
                        data.map
                            .insert(k, Self::itemize_bucket_value(val, object_hash));
                    }
                }
                // When the top-level result is an object hash (standalone
                // classify/categorize, or `:into` an object hash), the nested
                // multi-level buckets are `Mu`-keyed object hashes too, like
                // raku. Classifying into a PLAIN hash keeps plain nested
                // buckets.
                if object_hash {
                    data.key_type = Some("Mu".to_string());
                }
            }
            if object_hash {
                crate::runtime::utils::ensure_object_hash_which_keys(arc);
            }
        });
        v
    }

    /// Finish a classify/categorize bucket map: itemize the bucket values,
    /// then (when `key_type` is `Some`) mark the result an object hash and
    /// re-key it by `.WHICH` from the recorded key objects. The standalone
    /// `.classify`/`.categorize` pass `Some("Mu")` — raku returns the same
    /// `Hash[Mu,Mu]` shape as a `:{...}` literal; the `:into` forms pass the
    /// target's own key type so the target's identity is preserved.
    fn classify_finish_hash(
        buckets: HashMap<String, Value>,
        object_keys: HashMap<String, Value>,
        key_type: Option<String>,
    ) -> Value {
        let buckets: HashMap<String, Value> = buckets
            .into_iter()
            .map(|(k, v)| (k, Self::itemize_bucket_value(v, key_type.is_some())))
            .collect();
        let mut hash = Value::hash(buckets);
        hash.with_hash_mut(|arc| {
            {
                let data = crate::gc::Gc::make_mut(arc);
                data.key_type = key_type.clone();
                if !object_keys.is_empty() {
                    data.original_keys = Some(object_keys.clone());
                }
            }
            if arc.key_type.is_some() {
                crate::runtime::utils::ensure_object_hash_which_keys(arc);
            }
        });
        hash
    }
}
