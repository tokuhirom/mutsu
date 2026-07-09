//! Push/append-through-accessor and subscript-adverb (`:exists`/`:delete`/`:kv`...) ops.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Handle push/unshift/append/prepend through an instance accessor.
    /// Called as __mutsu_push_through_accessor(instance, attr_name, method, vals...).
    /// Mutates the array/hash attribute in-place and propagates the change to all
    /// env bindings that share the same Arc (e.g. after clone).
    pub(super) fn builtin_push_through_accessor(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_push_through_accessor expects instance, attr_name, method, and values",
            ));
        }
        let target = args[0].clone();
        let attr_name = args[1].to_string_value();
        let method = args[2].to_string_value();
        let push_args: Vec<Value> = args[3..].to_vec();

        // Handle Mixin targets (e.g. `&b does R` followed by `push &b.s, val`).
        if let ValueView::Mixin(inner, mixins) = target.view() {
            let attr_key = format!("__mutsu_attr__{}", attr_name);
            if let Some(current) = mixins.get(&attr_key).cloned() {
                let old_array_arc = match current.view() {
                    ValueView::Array(arc, ..) => Some(arc.clone()),
                    _ => None,
                };
                let temp_id: u64 = std::sync::Arc::as_ptr(mixins) as u64;
                let temp_var = format!("__mutsu_push_mixin_tmp_{}", temp_id);
                self.env.insert(temp_var.clone(), current.clone());
                let result = self.call_method_mut_with_values(
                    &temp_var,
                    current.clone(),
                    &method,
                    push_args,
                )?;
                let new_value = self.env.get(&temp_var).cloned().unwrap_or(result.clone());
                self.env.remove(&temp_var);
                let mut updated_mixins = (**mixins).clone();
                updated_mixins.insert(attr_key, new_value.clone());
                let new_mixin =
                    Value::mixin_parts(inner.clone(), std::sync::Arc::new(updated_mixins));
                self.propagate_mixin_update_by_arc(mixins, &new_mixin);
                if let Some(old_arc) = &old_array_arc {
                    self.propagate_shared_array_in_instances(old_arc, &new_value);
                }
                return Ok(result);
            }
            let accessor_val = self.call_method_with_values(target, &attr_name, vec![])?;
            return self.call_method_with_values(accessor_val, &method, push_args);
        }

        let ValueView::Instance {
            attributes,
            id: target_id,
            ..
        } = target.view()
        else {
            // Fallback: just call the method on the accessor result
            let accessor_val = self.call_method_with_values(target, &attr_name, vec![])?;
            return self.call_method_with_values(accessor_val, &method, push_args);
        };

        let attr_key = attr_name.clone();
        let current = attributes
            .as_map()
            .get(&attr_key)
            .cloned()
            .unwrap_or(Value::NIL);
        let old_array_arc = match current.view() {
            ValueView::Array(arc, ..) => Some(arc.clone()),
            _ => None,
        };
        let old_hash_arc = match current.view() {
            ValueView::Hash(arc) => Some(arc.clone()),
            _ => None,
        };

        // Call the mutating method via a temp variable
        let temp_var = format!("__mutsu_push_accessor_tmp_{}", target_id);
        self.env.insert(temp_var.clone(), current.clone());
        let result =
            self.call_method_mut_with_values(&temp_var, current.clone(), &method, push_args)?;
        let new_value = self.env.get(&temp_var).cloned().unwrap_or(result.clone());
        self.env.remove(&temp_var);

        // Update the instance attribute in the live shared cell
        let mut updated = attributes.to_map();
        updated.insert(attr_key, new_value.clone());
        attributes.commit_attrs(updated);

        // Also propagate the array/hash change to all instances sharing
        // the same Arc (handles clone semantics).
        if let Some(old_arc) = &old_array_arc {
            self.propagate_shared_array_in_instances(old_arc, &new_value);
        }
        if let Some(old_arc) = &old_hash_arc {
            self.propagate_shared_hash_in_instances(old_arc, &new_value);
        }

        Ok(result)
    }

    /// Throw X::Adverb for conflicting/unknown subscript adverbs.
    pub(super) fn builtin_subscript_adverb_error(args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "__mutsu_subscript_adverb_error: missing arguments",
            ));
        }
        let mut what = args[0].to_string_value();
        let source = args[1].to_string_value();
        let mut nogo: Vec<String> = Vec::new();
        let mut unexpected: Vec<String> = Vec::new();
        for arg in &args[2..] {
            let s = arg.to_string_value();
            if let Some(name) = s.strip_prefix("__nogo__") {
                nogo.push(name.to_string());
            } else if let Some(name) = s.strip_prefix("__unexpected__") {
                unexpected.push(name.to_string());
            }
        }
        // A conflicting (nogo) combination is reported by the slice candidate as
        // plain "slice"; the bracket-specific "{} slice"/"[] slice" descriptor is
        // only used for a pure unknown-adverb error on a zen slice.
        if !nogo.is_empty() && (what == "{} slice" || what == "[] slice") {
            what = "slice".to_string();
        }
        // Both the conflicting-adverb (`nogo`) and unknown-adverb (`unexpected`)
        // cases must surface as a typed X::Adverb so `throws-like X::Adverb` and
        // attribute matching (.what/.source/.nogo/.unexpected) work.
        Err(RuntimeError::x_adverb(&what, &source, &nogo, &unexpected))
    }

    pub(super) fn builtin_subscript_adverb(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_subscript_adverb expects target, index, and mode",
            ));
        }
        // A positional slice adverb can target any Positional, not just a stored
        // `@`-array: `("a".."z")[3,5]:k` indexes a Range, `(1,2,3)[*]:v` a List.
        // Coerce those to a plain array view up-front so the Array arm below owns
        // the value/key/exists logic (a Hash target keeps its own path).
        let mut target_is_coerced_list = false;
        let target = {
            let t = args[0].clone();
            if !matches!(t.view(), ValueView::Array(..) | ValueView::Hash(_))
                && (t.is_range()
                    || matches!(
                        t.view(),
                        ValueView::Seq(_)
                            | ValueView::HyperSeq(_)
                            | ValueView::RaceSeq(_)
                            | ValueView::LazyList(_)
                    ))
            {
                target_is_coerced_list = true;
                Value::real_array(crate::runtime::utils::value_to_list(&t))
            } else {
                t
            }
        };
        let index = args[1].clone();
        let mode = args[2].to_string_value();
        let var_name = args
            .get(3)
            .map(Value::to_string_value)
            .filter(|s| !s.is_empty());
        let mut delete_after = false;
        let mut adverb_cond: Option<bool> = None;
        let mut skip_next = false;
        for (i, extra) in args.iter().skip(4).enumerate() {
            if skip_next {
                skip_next = false;
                continue;
            }
            match extra.view() {
                ValueView::Pair(key, value) if key == "delete" => {
                    delete_after = value.truthy();
                }
                ValueView::ValuePair(key, value) if key.to_string_value() == "delete" => {
                    delete_after = value.truthy();
                }
                ValueView::Str(s) if s.as_ref() == "__adverb_cond__" => {
                    // The next argument is the dynamic condition expression value.
                    if let Some(cond_val) = args.get(4 + i + 1) {
                        adverb_cond = Some(cond_val.truthy());
                    }
                    skip_next = true;
                }
                _ => {}
            }
        }

        let (kind, keep_missing) = match mode.as_str() {
            "kv" => ("kv", false),
            "not-kv" | "kv0" => ("kv", true),
            "p" => ("p", false),
            "not-p" | "p0" => ("p", true),
            "k" => ("k", false),
            "not-k" | "k0" => ("k", true),
            "v" => ("v", false),
            "not-v" | "v0" => ("v", true),
            _ => return Ok(Value::NIL),
        };
        // When a dynamic condition was provided (e.g. `:k($ok)`),
        // override keep_missing based on the condition's truthiness.
        // If the condition is false, it's like `:!k` (keep_missing = true).
        // If the condition is true, it's like `:k` (keep_missing = false).
        let keep_missing = if let Some(cond) = adverb_cond {
            !cond
        } else {
            keep_missing
        };

        // A lazy subscript (`@a[lazy 11..12]`, materialized to a Seq/LazyList)
        // auto-truncates at the array end: out-of-range indices are dropped rather
        // than reported as missing. An eager Range keeps missing elements.
        let mut truncate_oob = false;
        // A list-valued subscript (`@a[@b]`, `@a[(1,)]`, `@a[1..2]`, a Seq, ...)
        // is ALWAYS a slice and yields a list result, even for a single index —
        // `@a[@b]:!v` is `($T,)`, not the scalar `$T`. Only a bare scalar
        // subscript (`@a[11]:!v`) returns a scalar.
        let mut force_list = true;
        let mut indices = match index.view() {
            ValueView::Array(items, ..) => items.to_vec(),
            // A Range subscript on a hash is a multi-key slice (`%h{"b".."c"}:kv`),
            // so expand it to its element keys.
            _ if index.is_range() => crate::runtime::utils::value_to_list(&index),
            ValueView::Seq(items) => {
                truncate_oob = true;
                items.to_vec()
            }
            ValueView::LazyList(ll) => {
                truncate_oob = true;
                self.force_lazy_list_vm(ll)?
            }
            _ => {
                force_list = false;
                vec![index.clone()]
            }
        };
        // Resolve WhateverCode indices (e.g. `@a[*-1]:k`) by applying them to the
        // target's length, exactly as the plain-value subscript path does.
        if indices
            .iter()
            .any(|i| matches!(i.view(), ValueView::Sub(..)))
        {
            let target_len = match target.view() {
                ValueView::Array(items, ..) => items.len() as i64,
                _ => 0,
            };
            for idx in indices.iter_mut() {
                if matches!(idx.view(), ValueView::Sub(..)) {
                    *idx = self.call_sub_value(idx.clone(), vec![Value::int(target_len)], false)?;
                }
            }
        }
        if matches!(indices.first().map(Value::view), Some(ValueView::Whatever))
            || matches!(indices.first().map(Value::view), Some(ValueView::Num(f)) if f.is_infinite() && f > 0.0)
        {
            indices = match target.view() {
                ValueView::Array(items, ..) => (0..items.len())
                    .map(|i| Value::int(i as i64))
                    .collect::<Vec<_>>(),
                ValueView::Hash(map) => map
                    .keys()
                    .map(|k| Value::str(k.clone()))
                    .collect::<Vec<_>>(),
                _ => Vec::new(),
            };
        }
        // Lazy-subscript truncation: keep only in-range indices of the array.
        if truncate_oob && let ValueView::Array(items, ..) = target.view() {
            let len = items.len() as i64;
            indices.retain(|idx| match idx.view() {
                ValueView::Int(i) => i >= 0 && i < len,
                ValueView::Num(f) => f >= 0.0 && (f as i64) < len,
                _ => idx
                    .to_string_value()
                    .parse::<i64>()
                    .is_ok_and(|i| i >= 0 && i < len),
            });
        }
        let is_multi = indices.len() != 1 || force_list;

        // Positional targets (real arrays, plus Ranges/Seqs/Lists coerced above)
        // own a recursive path so a *nested* sub-list index preserves its nesting
        // in the result — `@a[(3, (30, (5,)))]:k` yields `(3, ((5,),))`, not a
        // flattened list. A Hash target keeps the original flat path below.
        if let ValueView::Array(items, ..) = target.view() {
            // The set of explicitly element-assigned indices travels with the
            // array value (embedded `ArrayData::initialized`), so a slot holding a
            // `Package("Any")` hole is distinguished from a real `Any` value even
            // after the array crosses scopes/closures.
            let bound_map: Option<std::collections::HashSet<usize>> = items.initialized.clone();
            let items_snap: Vec<Value> = items.to_vec();
            // The missing-element default comes from the array's *element type*.
            // Read it from the container value itself (embedded metadata /
            // `is default`) FIRST so it survives rebinding — e.g.
            // `for $@s, Str -> @a { @a[11]:!v }` where the loop variable has no
            // type constraint but the bound array still carries `.of=Str`. Fall
            // back to the declared variable's default/type, then Any.
            let container_default = self.container_default(&target).cloned().or_else(|| {
                var_name
                    .as_ref()
                    .and_then(|name| self.var_default(name).cloned())
            });
            let type_constraint_default = self
                .container_type_metadata(&target)
                .map(|info| info.value_type)
                .filter(|t| !t.is_empty())
                .or_else(|| {
                    var_name
                        .as_ref()
                        .and_then(|name| self.var_type_constraint(name))
                })
                .map(|t| Value::package(Symbol::intern(&t)));
            // A Range/List coerced to an array view has no container element
            // type, so a missing element reads back as `Nil` (matching
            // `("a".."z")[30]:!v`), not the `Any` a real `@`-array reports.
            let missing_value = container_default
                .or(type_constraint_default)
                .unwrap_or_else(|| {
                    if target_is_coerced_list {
                        Value::NIL
                    } else {
                        Value::package(Symbol::intern("Any"))
                    }
                });

            // `:delete` companion (`:delete:kv` etc): remove every *leaf* index of
            // the (possibly nested) index tree from the live binding, then format
            // the pre-delete snapshot. The value/key/exists reported are the
            // pre-deletion state (captured in `items_snap`).
            if delete_after && let Some(var_name) = var_name.as_ref() {
                let hole_type = self
                    .var_type_constraint(var_name)
                    .unwrap_or_else(|| "Any".to_string());
                let mut leaves: Vec<i64> = Vec::new();
                Self::collect_slice_leaf_indices(&indices, &mut leaves);
                let was_array = self
                    .env
                    .get_mut(var_name)
                    .and_then(|entry| {
                        entry.with_array_mut(|live, _| {
                            let hole_value =
                                Value::package(crate::symbol::Symbol::intern(&hole_type));
                            let arr = crate::value::gc_data_mut(live);
                            for i in &leaves {
                                if *i >= 0 && (*i as usize) < arr.len() {
                                    arr[*i as usize] = hole_value.clone();
                                }
                            }
                            // Trim trailing holes.
                            while let Some(last) = arr.last() {
                                let is_hole = match last.view() {
                                    ValueView::Nil => true,
                                    ValueView::Package(name) => {
                                        name == "Any" || name == hole_type.as_str()
                                    }
                                    _ => false,
                                };
                                if is_hole {
                                    arr.pop();
                                } else {
                                    break;
                                }
                            }
                        })
                    })
                    .is_some();
                if was_array {
                    // Sync the env mutation back to the local slot (dual store) so
                    // a later read of the array observes the deletion.
                    self.writeback_multidim_var_to_local(var_name);
                }
            }

            if !is_multi {
                let (key, value, exists) = Self::resolve_positional_scalar(
                    &items_snap,
                    bound_map.as_ref(),
                    &missing_value,
                    &indices[0],
                );
                if !keep_missing && !exists {
                    return Ok(Value::array(Vec::new()));
                }
                return Ok(match kind {
                    "kv" => Value::array(vec![key, value]),
                    "p" => Value::value_pair(key, value),
                    "k" => key,
                    "v" => value,
                    _ => Value::NIL,
                });
            }
            let out = Self::format_positional_slice_level(
                &items_snap,
                bound_map.as_ref(),
                &missing_value,
                &indices,
                kind,
                keep_missing,
            );
            return Ok(Value::array(out));
        }

        let mut rows: Vec<(Value, Value, bool)> = Vec::with_capacity(indices.len());
        match target.view() {
            ValueView::Hash(map) => {
                // The missing-key default comes from the Hash's *value type* — read
                // it from the container value itself (via container metadata /
                // `is default`) so it survives rebinding (e.g. `for %i -> %h {...}`
                // where the loop variable has no type constraint). Fall back to the
                // declared variable's type constraint, then Any.
                let missing_default = self
                    .container_default(&target)
                    .cloned()
                    .or_else(|| {
                        self.container_type_metadata(&target)
                            .map(|info| Value::package(Symbol::intern(&info.value_type)))
                    })
                    .or_else(|| {
                        var_name
                            .as_ref()
                            .and_then(|name| self.var_type_constraint(name))
                            .map(|t| Value::package(Symbol::intern(&t)))
                    })
                    .unwrap_or_else(|| Value::package(Symbol::intern("Any")));
                // Object hashes (`my %h{Int}`) store `.WHICH`-encoded keys, so
                // the subscript value must be encoded the same way to find the
                // entry; the *displayed* key is the original typed subscript.
                let is_object_hash = map.key_type.is_some();
                for idx in &indices {
                    let (key_str, key) = if is_object_hash {
                        (crate::runtime::utils::value_which_key(idx), idx.clone())
                    } else {
                        let s = idx.to_string_value();
                        let k = super::builtins_collection::builtin_val(&[Value::str(s.clone())]);
                        (s, k)
                    };
                    let exists = map.contains_key(&key_str);
                    let value = map
                        .get(&key_str)
                        .cloned()
                        .unwrap_or_else(|| missing_default.clone());
                    rows.push((key, value, exists));
                }
            }
            _ => return Ok(Value::NIL),
        }

        // Hash `:delete` companion (arrays handled in the positional path above).
        if delete_after
            && let Some(var_name) = var_name.as_ref()
            && let Some(entry) = self.env.get_mut(var_name)
        {
            entry.with_hash_mut(|map| {
                let h = crate::value::gc_data_mut(map);
                for idx in &indices {
                    h.remove(&idx.to_string_value());
                }
            });
        }

        if !is_multi {
            let Some((key, value, exists)) = rows.into_iter().next() else {
                return Ok(Value::NIL);
            };
            if !keep_missing && !exists {
                return Ok(Value::array(Vec::new()));
            }
            return Ok(match kind {
                "kv" => Value::array(vec![key, value]),
                "p" => Value::value_pair(key, value),
                "k" => key,
                "v" => value,
                _ => Value::NIL,
            });
        }

        let mut out = Vec::new();
        for (key, value, exists) in rows {
            if !keep_missing && !exists {
                continue;
            }
            match kind {
                "kv" => {
                    out.push(key);
                    out.push(value);
                }
                "p" => out.push(Value::value_pair(key, value)),
                "k" => out.push(key),
                "v" => out.push(value),
                _ => {}
            }
        }
        Ok(Value::array(out))
    }

    /// One level of a positional slice adverb (`:k`/`:v`/`:p`/`:kv`) applied to
    /// `items`. A *nested* index element (a sub-list/Range) recurses and becomes
    /// ONE nested list element in the output, preserving the index tree's shape;
    /// a scalar index contributes its formatted key/value entries in place.
    pub(crate) fn format_positional_slice_level(
        items: &[Value],
        bound_map: Option<&std::collections::HashSet<usize>>,
        missing_value: &Value,
        indices: &[Value],
        kind: &str,
        keep_missing: bool,
    ) -> Vec<Value> {
        let mut out = Vec::new();
        for idx in indices {
            if let Some(sub) = Self::nested_index_elements(idx) {
                let sub_out = Self::format_positional_slice_level(
                    items,
                    bound_map,
                    missing_value,
                    &sub,
                    kind,
                    keep_missing,
                );
                out.push(Value::array(sub_out));
                continue;
            }
            let (key, value, exists) =
                Self::resolve_positional_scalar(items, bound_map, missing_value, idx);
            if !keep_missing && !exists {
                continue;
            }
            match kind {
                "kv" => {
                    out.push(key);
                    out.push(value);
                }
                "p" => out.push(Value::value_pair(key, value)),
                "k" => out.push(key),
                "v" => out.push(value),
                _ => {}
            }
        }
        out
    }

    /// If `idx` is a nested sub-slice (a non-itemized list / Seq / Range used as
    /// an index element), return its direct elements for recursion; else `None`
    /// (it is a scalar leaf index).
    pub(crate) fn nested_index_elements(idx: &Value) -> Option<Vec<Value>> {
        match idx.view() {
            ValueView::Array(items, kind) if !kind.is_itemized() => Some(items.to_vec()),
            ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                Some(items.to_vec())
            }
            _ if idx.is_range() => Some(crate::runtime::utils::value_to_list(idx)),
            _ => None,
        }
    }

    /// Resolve one scalar index against `items`, returning `(key, value, exists)`.
    /// Out-of-range or unfilled slots report the container's `missing_value` with
    /// `exists = false`.
    fn resolve_positional_scalar(
        items: &[Value],
        bound_map: Option<&std::collections::HashSet<usize>>,
        missing_value: &Value,
        idx: &Value,
    ) -> (Value, Value, bool) {
        let i = match idx.view() {
            ValueView::Int(i) => i,
            ValueView::Num(f) => f as i64,
            _ => idx.to_string_value().parse::<i64>().unwrap_or(-1),
        };
        let key = Value::int(i);
        if i < 0 || i as usize >= items.len() {
            return (key, missing_value.clone(), false);
        }
        let ui = i as usize;
        let exists = match bound_map {
            Some(set) => {
                set.contains(&ui)
                    || !matches!(items[ui].view(), ValueView::Package(name) if name == "Any")
            }
            None => true,
        };
        (key, items[ui].clone(), exists)
    }

    /// Collect every *leaf* integer index of a (possibly nested) slice index tree,
    /// for `:delete` to remove each addressed slot.
    pub(crate) fn collect_slice_leaf_indices(indices: &[Value], out: &mut Vec<i64>) {
        for idx in indices {
            if let Some(sub) = Self::nested_index_elements(idx) {
                Self::collect_slice_leaf_indices(&sub, out);
                continue;
            }
            let i = match idx.view() {
                ValueView::Int(i) => i,
                ValueView::Num(f) => f as i64,
                _ => idx.to_string_value().parse::<i64>().unwrap_or(-1),
            };
            out.push(i);
        }
    }
}
