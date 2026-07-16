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
        source_slot: Option<u32>,
        pointy_param: &Option<String>,
    ) {
        let Some(source) = source else {
            return;
        };
        if !source.starts_with('@') && !source.starts_with('%') {
            return;
        }
        // §1.4: with shadow slots active, target the compile-time-baked slot —
        // the by-name `position` search resolves a shadowed source to the OUTER
        // slot. OFF keeps `None` = the by-name path, byte-identical.
        let eff_slot = if crate::compiler::shadow_slots_active() {
            source_slot
        } else {
            None
        };
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
        self.write_local_slot_or_name(code, eff_slot, source, current);
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
        let key = match index.view() {
            ValueView::Int(i) => i.to_string(),
            ValueView::Str(s) => s.as_ref().clone(),
            _ => index.to_string_value(),
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
        match (current.view(), source_elem.view()) {
            (ValueView::Array(a, _), ValueView::Array(b, _)) => crate::gc::Gc::ptr_eq(&a, &b),
            (ValueView::Hash(a), ValueView::Hash(b)) => crate::gc::Gc::ptr_eq(&a, &b),
            (ValueView::Str(a), ValueView::Str(b)) => Arc::ptr_eq(&a, &b) || *a == *b,
            (ValueView::Int(a), ValueView::Int(b)) => a == b,
            (ValueView::Num(a), ValueView::Num(b)) => a == b,
            (ValueView::Bool(a), ValueView::Bool(b)) => a == b,
            // Same instance object (same `Gc<InstanceAttrs>`): attribute
            // mutations write through the shared cell, so the source element
            // already observes them — the O(n) array rebuild would only re-store
            // the identical handle. A rebound loop var (`$_ = Other.new`) holds
            // a different Gc and still falls through to the writeback. Without
            // this arm, a read-only `for @instances { .method }` cloned the
            // whole backing array every iteration (O(n^2) — the dominant cost
            // of bench-class's polymorphism loop).
            (
                ValueView::Instance {
                    class_name: ca,
                    attributes: a,
                    id: ia,
                },
                ValueView::Instance {
                    class_name: cb,
                    attributes: b,
                    id: ib,
                },
            ) => ca == cb && ia == ib && crate::gc::Gc::ptr_eq(&a, &b),
            // Immutable-by-value variants: equal means the writeback is a no-op.
            (ValueView::Package(a), ValueView::Package(b)) => a == b,
            (ValueView::Rat(a, b), ValueView::Rat(c, d)) => a == c && b == d,
            (
                ValueView::Enum {
                    enum_type: ta,
                    key: ka,
                    index: ia,
                    ..
                },
                ValueView::Enum {
                    enum_type: tb,
                    key: kb,
                    index: ib,
                    ..
                },
            ) => ta == tb && ka == kb && ia == ib,
            (ValueView::Nil, ValueView::Nil) => true,
            // A shared `ContainerRef` element (the rw-alias cell `.grep` leaves
            // in its source array, or a `:=`-bound element): mutations through
            // the alias write through the cell itself, so the source element
            // already observes them. The loop var holds either the same cell
            // (Gc identity) or a dereferenced plain value — compare against the
            // cell's current content in that case. Without these arms every
            // iteration of a read-only `for` over a cell-holding array fell
            // through to the O(n) source rebuild (O(n^2) overall; a 20k-element
            // array took seconds instead of milliseconds).
            (ValueView::ContainerRef(a), ValueView::ContainerRef(b)) => {
                crate::gc::Gc::ptr_eq(&a, &b)
            }
            (_, ValueView::ContainerRef(cell)) => {
                let inner = cell.lock().unwrap().clone();
                Self::loop_var_unchanged(current, &inner)
            }
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
            if let ValueView::Array(chunk, _) = item.view() {
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
        source_slot: Option<u32>,
        raw_source: &Option<Value>,
        updated_value: Value,
    ) {
        if let Some(ValueView::ContainerRef(arc)) = raw_source.as_ref().map(Value::view) {
            arc.lock().unwrap().clone_from(&updated_value);
        } else {
            // §1.4: prefer the compile-time-baked slot under the shadow-slot
            // gate (a shadowed source name occupies several slots and the
            // by-name `position` search hits the outer one); OFF passes `None`
            // = the original by-name update, byte-identical.
            let eff_slot = if crate::compiler::shadow_slots_active() {
                source_slot
            } else {
                None
            };
            self.set_env_with_main_alias(source, updated_value.clone());
            self.write_local_slot_or_name(code, eff_slot, source, updated_value);
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn write_back_for_topic_item(
        &mut self,
        code: &CompiledCode,
        source_var: &Option<String>,
        source_slot: Option<u32>,
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
        // §1.4: under the shadow-slot gate, read the baked slot first — the env
        // entry for a shadowed name may hold a stale/other-slot Arc (clobbered
        // by the whole-locals `sync_env_from_locals` broadcast), so rebuilding
        // from it would base the writeback on the wrong array.
        let slot_source = if crate::compiler::shadow_slots_active() {
            source_slot
                .and_then(|s| self.locals.get(s as usize))
                .filter(|v| !v.is_nil())
                .cloned()
        } else {
            None
        };
        let raw_source = slot_source.or_else(|| self.get_env_with_main_alias(source));
        let Some((items, kind)) = raw_source
            .as_ref()
            .and_then(|v| v.deref_container().into_array())
        else {
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
        // Clone the original ArrayData (carrying its element-type / declared-type
        // / shape / default metadata) and replace only the mutated element, so a
        // mutating `for @a { $_++ }` over a native or shaped array preserves
        // `array[int]` typing and the shape (`ArrayData::new` would drop them,
        // turning `array[int]` into a bare `Array` and breaking later `.WHAT`,
        // `.raku`, and shaped `:delete`-dies behaviour).
        let mut new_data = (*items).clone();
        new_data.items[actual_idx] = current_topic;
        let updated_value = Value::array_with_kind(crate::gc::Gc::new(new_data), kind);
        self.write_back_container_source(code, source, source_slot, &raw_source, updated_value);
    }
}
