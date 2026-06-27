use super::*;

impl Interpreter {
    /// Write a mutated whole-container topic back to its source variable after a
    /// `given`/`with` block: `given @a { .push(4) }` aliases `@a`, so a container
    /// mutation through `$_` must propagate. Only for `@`/`%`-sigil sources
    /// (whole-container topicalization); skipped when the topic is unchanged
    /// (same `Arc`) so a read-only `given` stays a no-op.
    pub(super) fn write_back_given_topic(
        &mut self,
        code: &CompiledCode,
        source: &Option<String>,
        pointy_param: &Option<String>,
    ) {
        let Some(source) = source else {
            return;
        };
        if !source.starts_with('@') && !source.starts_with('%') {
            return;
        }
        // For a pointy block, write back the bound parameter's final value
        // (`@p` after `@p.push`); otherwise the topic `$_`.
        let current = match pointy_param {
            Some(p) => self.get_env_with_main_alias(p),
            None => self.env().get("_").cloned(),
        };
        let Some(current) = current else {
            return;
        };
        if let Some(orig) = self.get_env_with_main_alias(source)
            && Self::loop_var_unchanged(&current, &orig)
        {
            return;
        }
        self.set_env_with_main_alias(source, current.clone());
        self.update_local_if_exists(code, source, &current);
    }

    /// Write the final `$_` back to an lvalue container element (`given %h<k>`
    /// / `given @a[i]`). The element source is `(container var, index,
    /// positional)`. This makes both `$_ = ...` (whole reassign) and container
    /// mutations (`.push`) propagate to the original element, matching Raku's
    /// aliasing of an element topic.
    pub(super) fn write_back_element_source(
        &mut self,
        code: &CompiledCode,
        src: &(String, Value, bool),
        pointy_param: &Option<String>,
    ) {
        let (container, index, _positional) = src;
        // For a pointy block, write back the bound parameter's final value;
        // otherwise the topic `$_`.
        let current = match pointy_param {
            Some(p) => self.get_env_with_main_alias(p),
            None => self.env().get("_").cloned(),
        };
        let Some(current) = current else {
            return;
        };
        let key = match index {
            Value::Int(i) => i.to_string(),
            Value::Str(s) => s.as_ref().clone(),
            other => other.to_string_value(),
        };
        let Some(mut cval) = self.get_env_with_main_alias(container) else {
            return;
        };
        // The element index here is an already-resolved existing slot (loop
        // aliasing), so the autoviv reservation cannot realistically fail; this
        // writeback teardown has no error channel, so discard the Result.
        let _ = Self::assign_into_nested_container(&mut cval, &key, current);
        self.set_env_with_main_alias(container, cval.clone());
        self.update_local_if_exists(code, container, &cval);
    }

    /// Whether the loop variable still holds the same binding as the source
    /// element, so writing it back would be a no-op. Conservative: returns
    /// `true` only when provably unchanged (same container `Arc`, so in-place
    /// mutation already reached the source; or an equal immutable scalar).
    /// Anything else returns `false` and falls through to the full writeback.
    pub(super) fn loop_var_unchanged(current: &Value, source_elem: &Value) -> bool {
        use std::sync::Arc;
        match (current, source_elem) {
            (Value::Array(a, _), Value::Array(b, _)) => Arc::ptr_eq(a, b),
            (Value::Hash(a), Value::Hash(b)) => Arc::ptr_eq(a, b),
            (Value::Str(a), Value::Str(b)) => Arc::ptr_eq(a, b) || a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            _ => false,
        }
    }

    /// When iterating an immutable Mix/Set/Bag via `.values`/`.kv`/`.pairs` with
    /// a writable (sigilless / `is rw`) alias, the aliased weight is immutable —
    /// modifying it must raise X::Assignment::RO (matching Raku). Compare each
    /// alias param against the value it was bound to this iteration; return the
    /// error if any was changed. The topic (`$_`) case is handled separately by
    /// `topic_readonly`, so this only needs the named/multi-param forms.
    pub(super) fn immutable_quant_param_mutation(
        &self,
        param_name: &Option<String>,
        multi_param_names: &[String],
        item: &Value,
    ) -> Option<RuntimeError> {
        let changed =
            |name: &str, bound: &Value| self.env().get(name).is_some_and(|cur| cur != bound);
        if !multi_param_names.is_empty() {
            if let Value::Array(chunk, _) = item {
                for (i, name) in multi_param_names.iter().enumerate() {
                    if let Some(bound) = chunk.items.get(i)
                        && changed(name, bound)
                    {
                        return Some(RuntimeError::assignment_ro(Some(name)));
                    }
                }
            }
            return None;
        }
        if let Some(name) = param_name
            && changed(name, item)
        {
            return Some(RuntimeError::assignment_ro(Some(name)));
        }
        None
    }

    /// Write a rebuilt container value back to a for-loop `source` variable.
    /// When the source holds a shared `ContainerRef` cell (a `:=`-bound array /
    /// hash that env↔locals share — Stage 1), write THROUGH the cell so the
    /// bound source observes the mutation without rebinding either variable.
    /// Otherwise fall back to the plain env + local-slot update.
    pub(super) fn write_back_container_source(
        &mut self,
        code: &CompiledCode,
        source: &str,
        raw_source: &Option<Value>,
        updated_value: Value,
    ) {
        if let Some(Value::ContainerRef(arc)) = raw_source {
            arc.lock().unwrap().clone_from(&updated_value);
        } else {
            self.set_env_with_main_alias(source, updated_value.clone());
            self.update_local_if_exists(code, source, &updated_value);
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn write_back_for_topic_item(
        &mut self,
        code: &CompiledCode,
        source_var: &Option<String>,
        param_name: &Option<String>,
        idx: usize,
        reversed: bool,
        total_items: usize,
        values_mode: bool,
        hash_keys: &Option<Vec<String>>,
    ) {
        let Some(source) = source_var else {
            return;
        };
        // `$_ = X for %h.values` / `for %h.values -> $v { $v = X }`: the loop
        // variable aliases a hash *value*, so write it back to the source hash
        // by the pre-captured key order. Bare `for %h` (Pairs) and `.keys` do
        // not reach here (no values_mode tag).
        if source.starts_with('%') {
            if values_mode {
                self.write_back_hash_value_item(code, source, param_name, idx, hash_keys);
            }
            return;
        }
        // A scalar source iterated via `.values` binding a mutable QuantHash is
        // written back by `write_back_quanthash_value_item`, called *before* the
        // body-result match (its coercion can raise X::Str::Numeric, which must
        // flow through the loop's shared error cleanup). So nothing to do here.
        if !source.starts_with('@') {
            return;
        }
        // The loop variable written back to the source element is the named
        // param (`for @m -> @row {...}` / `-> $row {...}` — Raku aliases the
        // element, so `.push`/`@row[0]=v` propagate), or otherwise the implicit
        // topic `$_` (`for @m {...}`).
        let loop_var = param_name.as_deref().unwrap_or("_");
        let Some(current_topic) = self.env().get(loop_var).cloned() else {
            return;
        };
        // The source may hold a shared `ContainerRef` cell rather than a bare
        // Array — a `:=`-bound array (`my @a := @b`) cell-izes both vars so they
        // share one outer cell (Stage 1 / env-locals coherence). Deref to inspect
        // the inner Array; when present, the rebuilt array must be written THROUGH
        // the cell so the bound source (`@b`) observes the topic mutation.
        let raw_source = self.get_env_with_main_alias(source);
        let Some((items, kind)) = raw_source.as_ref().and_then(|v| match v.deref_container() {
            Value::Array(items, kind) => Some((items, kind)),
            _ => None,
        }) else {
            return;
        };
        let actual_idx = if reversed && total_items > 0 {
            total_items - 1 - idx
        } else {
            idx
        };
        if actual_idx >= items.len() {
            return;
        }
        // Skip the O(n) source rebuild when the loop variable is provably
        // unchanged — otherwise even a read-only loop (`for @a { say $_ }`) or
        // an empty body would rebuild the whole backing array every iteration,
        // making the loop O(n^2). When the value is the same (same container
        // Arc, so any in-place mutation is already visible in the source, or an
        // equal immutable scalar), the writeback is a no-op.
        if Self::loop_var_unchanged(&current_topic, &items[actual_idx]) {
            return;
        }
        let mut updated = items.to_vec();
        updated[actual_idx] = current_topic;
        let updated_value = Value::Array(
            std::sync::Arc::new(crate::value::ArrayData::new(updated)),
            kind,
        );
        self.write_back_container_source(code, source, &raw_source, updated_value);
    }
}
