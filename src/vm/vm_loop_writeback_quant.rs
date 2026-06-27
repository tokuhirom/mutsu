use super::*;

impl Interpreter {
    /// Set a single weight on a mutable QuantHash bound to a scalar `source`,
    /// coercing the assigned value the same way an element store (`$b<k> = v`)
    /// does: Int for Bag, Real for Mix, truthiness for Set. A non-numeric `Str`
    /// raises X::Str::Numeric, and a zero weight removes the key (Raku semantics).
    /// No-op (and Ok) if `source` does not hold a mutable QuantHash.
    pub(crate) fn quanthash_set_weight(
        &mut self,
        code: &CompiledCode,
        source: &str,
        key: String,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        let updated = match self.get_env_with_main_alias(source) {
            Some(Value::Bag(bag, true)) => {
                let count = Self::bag_assignment_count(value)?;
                let mut b = (*bag).clone();
                // A Bag holds positive integer counts: a weight of 0 *or below*
                // removes the element (negative counts are not representable),
                // unlike a Mix where a negative weight is retained.
                if !num_traits::Signed::is_positive(&count) {
                    b.counts.remove(&key);
                } else {
                    b.counts.insert(key, count);
                }
                Value::Bag(std::sync::Arc::new(b), true)
            }
            Some(Value::Mix(mix, true)) => {
                let weight = Self::mix_assignment_weight(value)?;
                let mut m = (*mix).clone();
                if weight == 0.0 {
                    m.remove(&key);
                } else {
                    m.insert(key, weight);
                }
                Value::Mix(std::sync::Arc::new(m), true)
            }
            Some(Value::Set(set, true)) => {
                let mut s = (*set).clone();
                if value.truthy() {
                    s.elements.insert(key);
                } else {
                    s.elements.remove(&key);
                }
                Value::Set(std::sync::Arc::new(s), true)
            }
            _ => return Ok(()),
        };
        self.set_env_with_main_alias(source, updated.clone());
        self.update_local_if_exists(code, source, &updated);
        Ok(())
    }

    /// Write the loop variable back to a mutable QuantHash weight during
    /// `for $b.values` (`$_ = X for $b.values` / `for $b.values -> $v { $v = X }`,
    /// `$b` a MixHash/BagHash/SetHash). The key at iteration position `idx` comes
    /// from the pre-captured key order (same unmodified weight map → identical
    /// iteration order as the materialized `.values` list).
    pub(super) fn write_back_quanthash_value_item(
        &mut self,
        code: &CompiledCode,
        source: &str,
        param_name: &Option<String>,
        idx: usize,
        hash_keys: &Option<Vec<String>>,
    ) -> Result<(), RuntimeError> {
        let loop_var = param_name.as_deref().unwrap_or("_");
        let Some(current) = self.env().get(loop_var).cloned() else {
            return Ok(());
        };
        let Some(keys) = hash_keys else {
            return Ok(());
        };
        let Some(key) = keys.get(idx).cloned() else {
            return Ok(());
        };
        self.quanthash_set_weight(code, source, key, &current)
    }

    /// Write a mutable QuantHash weight back from an rw for-loop param: either
    /// `for $b.kv -> \k, \v { v = X }` (key from the `\k` param, value from `\v`)
    /// or `for $b.values -> $v is rw { $v = X }` (key from the captured order at
    /// `idx`, value from the single rw param / `$_`). Coercion may raise
    /// X::Str::Numeric; weight 0 removes the key.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn write_back_quanthash_rw(
        &mut self,
        code: &CompiledCode,
        source: &str,
        rw_param_names: &[String],
        param_name: &Option<String>,
        idx: usize,
        kv_mode: bool,
        hash_keys: &Option<Vec<String>>,
    ) -> Result<(), RuntimeError> {
        if kv_mode {
            // `.kv -> \k, \v`: the key is the first param, the value the second.
            if rw_param_names.len() < 2 {
                return Ok(());
            }
            let Some(key) = self.env().get(&rw_param_names[0]).cloned() else {
                return Ok(());
            };
            let Some(value) = self.env().get(&rw_param_names[1]).cloned() else {
                return Ok(());
            };
            self.quanthash_set_weight(code, source, key.to_string_value(), &value)
        } else {
            // `.values -> $v is rw`: single rw param, key by captured order.
            let var = rw_param_names
                .first()
                .map(String::as_str)
                .or(param_name.as_deref())
                .unwrap_or("_");
            let Some(value) = self.env().get(var).cloned() else {
                return Ok(());
            };
            let Some(keys) = hash_keys else {
                return Ok(());
            };
            let Some(key) = keys.get(idx).cloned() else {
                return Ok(());
            };
            self.quanthash_set_weight(code, source, key, &value)
        }
    }

    /// Write the loop variable back to a hash value during `for %h.values`
    /// (`$_ = X for %h.values` / `for %h.values -> $v { $v = X }`). The hash key
    /// at iteration position `idx` comes from the pre-captured key order, so the
    /// value lands on the same key the `.values` list yielded. Skips the rebuild
    /// when the value is provably unchanged (read-only loops stay O(n)).
    pub(super) fn write_back_hash_value_item(
        &mut self,
        code: &CompiledCode,
        source: &str,
        param_name: &Option<String>,
        idx: usize,
        hash_keys: &Option<Vec<String>>,
    ) {
        let loop_var = param_name.as_deref().unwrap_or("_");
        let Some(current) = self.env().get(loop_var).cloned() else {
            return;
        };
        let Some(keys) = hash_keys else {
            return;
        };
        let Some(key) = keys.get(idx) else {
            return;
        };
        // The source may hold a shared `ContainerRef` cell (a `:=`-bound hash —
        // `my %h := %g`); deref to read the inner Hash and write back THROUGH the
        // cell so the bound source observes the value mutation (Stage 1).
        let raw_source = self.get_env_with_main_alias(source);
        let Some(hash_items) = raw_source.as_ref().and_then(|v| match v.deref_container() {
            Value::Hash(hash_items) => Some(hash_items),
            _ => None,
        }) else {
            return;
        };
        if hash_items
            .get(key)
            .is_some_and(|existing| Self::loop_var_unchanged(&current, existing))
        {
            return;
        }
        let mut updated = hash_items.as_ref().clone();
        updated.insert(key.clone(), current);
        let updated_value = Value::Hash(Value::hash_arc(updated));
        self.write_back_container_source(code, source, &raw_source, updated_value);
    }

    /// Write back modified loop variable to the original scalar variable.
    /// Used when iterating over a list of scalar variables like `for ($a, $b, $c)`.
    pub(super) fn write_back_to_source_var(
        &mut self,
        code: &CompiledCode,
        source_var_names: &[String],
        param_name: &Option<String>,
        idx: usize,
    ) {
        if source_var_names.is_empty() || idx >= source_var_names.len() {
            return;
        }
        let var_name = param_name.as_deref().unwrap_or("_");
        let Some(current_val) = self.env().get(var_name).cloned() else {
            return;
        };
        let target = &source_var_names[idx];
        self.env_mut().insert(target.clone(), current_val.clone());
        self.update_local_if_exists(code, target, &current_val);
    }

    /// Write back the named rw param to the source container at the given index.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn write_back_for_rw_param(
        &mut self,
        code: &CompiledCode,
        source_var: &Option<String>,
        param_name: &Option<String>,
        rw_param_names: &[String],
        idx: usize,
        arity: usize,
        kv_mode: bool,
        hash_keys: &Option<Vec<String>>,
    ) {
        let Some(source) = source_var else {
            return;
        };
        if source.starts_with('@') || source.starts_with('%') {
            // For hash sources, read as array for writeback. The source may hold
            // a shared `ContainerRef` cell (a `:=`-bound array) — deref to read
            // the inner Array and write back THROUGH the cell (Stage 1).
            let source_val = self.get_env_with_main_alias(source);
            let (items, kind) = if source.starts_with('@') {
                match source_val.as_ref().map(|v| v.deref_container()) {
                    Some(Value::Array(items, kind)) => (items, kind),
                    _ => return,
                }
            } else {
                // Hash: can't do positional writeback easily; use hash-specific logic
                // The hash source may be a shared `ContainerRef` cell (a
                // `:=`-bound hash); deref to read the inner Hash and write back
                // THROUGH the cell so the bound source observes the mutation.
                if kv_mode && arity > 1 && rw_param_names.len() >= 2 {
                    // For %hash.kv -> $key, $val is rw: read $key and $val, update hash
                    let key_name = &rw_param_names[0];
                    let val_name = &rw_param_names[1];
                    if let Some(key) = self.env().get(key_name).cloned()
                        && let Some(val) = self.env().get(val_name).cloned()
                        && let Some(Value::Hash(hash_items)) =
                            source_val.as_ref().map(|v| v.deref_container())
                    {
                        let mut updated = hash_items.as_ref().clone();
                        let key_str = key.to_string_value();
                        updated.insert(key_str, val);
                        let updated_value = Value::Hash(Value::hash_arc(updated));
                        self.write_back_container_source(code, source, &source_val, updated_value);
                    }
                } else if !kv_mode {
                    // %hash.values -> $val is rw: positional writeback using pre-captured key order
                    let var_name = param_name.as_deref().unwrap_or("_");
                    if let Some(keys) = hash_keys
                        && let Some(val) = self.env().get(var_name).cloned()
                        && let Some(Value::Hash(hash_items)) =
                            source_val.as_ref().map(|v| v.deref_container())
                        && idx < keys.len()
                    {
                        let mut updated = hash_items.as_ref().clone();
                        updated.insert(keys[idx].clone(), val);
                        let updated_value = Value::Hash(Value::hash_arc(updated));
                        self.write_back_container_source(code, source, &source_val, updated_value);
                    }
                }
                return;
            };
            if kv_mode && arity > 1 && rw_param_names.len() >= 2 {
                // .kv mode: chunk is [key, val]. Write back val at key position.
                let val_name = &rw_param_names[1];
                if let Some(val) = self.env().get(val_name).cloned()
                    && idx < items.len()
                {
                    let mut updated = items.to_vec();
                    updated[idx] = val;
                    let updated_value = Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(updated)),
                        kind,
                    );
                    self.write_back_container_source(code, source, &source_val, updated_value);
                }
            } else if arity > 1 && !rw_param_names.is_empty() {
                // Multi-param rw: read each named param and write back to the array
                let base = idx * arity;
                let mut updated = items.to_vec();
                for (j, pname) in rw_param_names.iter().enumerate() {
                    if base + j < updated.len()
                        && let Some(val) = self.env().get(pname).cloned()
                    {
                        updated[base + j] = val;
                    }
                }
                let updated_value = Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(updated)),
                    kind,
                );
                self.write_back_container_source(code, source, &source_val, updated_value);
            } else {
                // Single-param rw: read the named param (or $_) and write back
                let var_name = param_name.as_deref().unwrap_or("_");
                let Some(current_val) = self.env().get(var_name).cloned() else {
                    return;
                };
                if idx >= items.len() {
                    return;
                }
                // Skip the O(n) rebuild when the rw variable is unchanged — a
                // read-only `<->` loop (`for @big <-> $x { $s += $x }`) would
                // otherwise be O(n^2). See `loop_var_unchanged`.
                if Self::loop_var_unchanged(&current_val, &items[idx]) {
                    return;
                }
                let mut updated = items.to_vec();
                updated[idx] = current_val;
                let updated_value = Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(updated)),
                    kind,
                );
                self.write_back_container_source(code, source, &source_val, updated_value);
            }
        } else if source.starts_with('%') {
            // Hash writeback: not straightforward by index; skip for now
        } else {
            // A mutable QuantHash bound to a scalar (`for $b.values -> $v is rw`,
            // `for $b.kv -> \k, \v`) is written back to its weights by
            // `write_back_quanthash_rw` *before* the body-result match (its
            // coercion can throw). Skip the generic scalar writeback here, which
            // would otherwise clobber `$b` with the bare weight value.
            if matches!(
                self.get_env_with_main_alias(source),
                Some(Value::Bag(_, true) | Value::Mix(_, true) | Value::Set(_, true))
            ) {
                return;
            }
            // Scalar binding: write back directly
            // For kv_mode with arity > 1 on a Pair source (e.g. `for $pair.kv -> $key, $value is rw`),
            // read the value from the second rw param and reconstruct the Pair.
            if kv_mode && arity > 1 && rw_param_names.len() >= 2 {
                if let Some(existing) = self.get_env_with_main_alias(source) {
                    let val_name = &rw_param_names[1];
                    if let Some(val) = self.env().get(val_name).cloned() {
                        let writeback_val = match existing {
                            Value::Pair(key, _) => Value::Pair(key, Box::new(val)),
                            Value::ValuePair(key, _) => Value::ValuePair(key, Box::new(val)),
                            _ => return,
                        };
                        self.set_env_with_main_alias(source, writeback_val.clone());
                        self.update_local_if_exists(code, source, &writeback_val);
                    }
                }
                return;
            }
            let var_name = param_name.as_deref().unwrap_or("_");
            let Some(current_val) = self.env().get(var_name).cloned() else {
                return;
            };
            // If the source variable holds a Pair, update only the pair's value
            // (this handles `for $pair.value -> $v is rw { ... }`). The source may
            // be a `ContainerRef` (a closure-captured / `:=`-bound `$pair`); deref
            // it to inspect the Pair and write back THROUGH the shared container so
            // we don't clobber `$pair` itself with a bare scalar.
            let raw_source = self.get_env_with_main_alias(source);
            let writeback_val = match raw_source.as_ref().map(|v| v.deref_container()) {
                Some(Value::Pair(key, _)) => Value::Pair(key, Box::new(current_val.clone())),
                Some(Value::ValuePair(key, _)) => {
                    Value::ValuePair(key, Box::new(current_val.clone()))
                }
                _ => current_val.clone(),
            };
            if let Some(Value::ContainerRef(arc)) = &raw_source {
                arc.lock().unwrap().clone_from(&writeback_val);
            } else {
                self.set_env_with_main_alias(source, writeback_val.clone());
                self.update_local_if_exists(code, source, &writeback_val);
            }
        }
    }
}
