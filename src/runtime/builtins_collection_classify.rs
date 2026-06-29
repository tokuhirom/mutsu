use super::*;

impl Interpreter {
    pub(super) fn builtin_classify(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        fn as_items(value: &Value) -> Option<Vec<Value>> {
            match value {
                Value::Array(items, ..) => Some(items.iter().cloned().collect()),
                Value::Seq(items) | Value::Slip(items) => Some(items.iter().cloned().collect()),
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

        // Check for lazy lists — classify/categorize cannot operate on lazy lists
        for arg in &positional {
            let is_lazy = matches!(arg, Value::LazyList(_));
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
            match arg {
                Value::Array(values, ..) => items.extend(values.iter().cloned()),
                Value::Seq(values) | Value::Slip(values) => items.extend(values.iter().cloned()),
                Value::LazyList(ll) => items.extend(self.force_lazy_list_bridge(ll)?),
                v if v.is_range() => items.extend(crate::runtime::utils::value_to_list(v)),
                other => items.push(other.clone()),
            }
        }

        let mut buckets: HashMap<String, Value> = match into_target.as_ref() {
            Some(Value::Hash(map)) => map.as_ref().map.clone(),
            _ => HashMap::new(),
        };
        let mut bag_counts: Option<HashMap<String, i64>> = match into_target.as_ref() {
            Some(Value::Bag(b, _)) => Some(crate::runtime::utils::bag_counts_as_i64(&b.counts)),
            _ => None,
        };
        let mut mix_counts: Option<HashMap<String, f64>> = match into_target.as_ref() {
            Some(Value::Mix(m, _)) => Some(m.weights.clone()),
            _ => None,
        };

        // Track classification level depth across all items for mixed-level detection
        let mut expected_level: Option<usize> = None;
        let mut expected_multi_level: Option<bool> = None;

        // Object-hash key preservation (§3.3): when the classifier returns a
        // non-`Str` key (e.g. a Junction from `*.contains: any 'a','f'`), the
        // bucket key is stored under its stringification but the result must be an
        // *object hash* so `$result{ any(...) }` is a by-key lookup (not junction
        // autothreading) and `.keys` yields the real key objects. Records each
        // first-level non-Str key object under its encoded string.
        let mut object_keys: HashMap<String, Value> = HashMap::new();

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
                    && !matches!(first, Value::Str(_))
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
                .is_some_and(|value| matches!(value, Value::Proxy { .. }));
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

        Ok(Self::classify_finish_hash(buckets, object_keys))
    }

    /// Wrap classify's bucket map in a `Value::Hash`, marking it an *object hash*
    /// (typed keys) when any classifier key was non-`Str` (e.g. a Junction). An
    /// object hash makes `$result{ $key }` a by-key lookup rather than junction
    /// autothreading, and `.keys` yield the real key objects.
    fn classify_finish_hash(
        buckets: HashMap<String, Value>,
        object_keys: HashMap<String, Value>,
    ) -> Value {
        let hash = Value::hash(buckets);
        if object_keys.is_empty() {
            return hash;
        }
        if let Value::Hash(mut arc) = hash {
            let data = std::sync::Arc::make_mut(&mut arc);
            data.key_type = Some("Any".to_string());
            data.original_keys = Some(object_keys);
            return Value::Hash(arc);
        }
        hash
    }
}
