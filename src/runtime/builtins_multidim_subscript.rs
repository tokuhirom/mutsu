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
        if let Value::Mixin(ref inner, ref mixins) = target {
            let attr_key = format!("__mutsu_attr__{}", attr_name);
            if let Some(current) = mixins.get(&attr_key).cloned() {
                let old_array_arc = match &current {
                    Value::Array(arc, ..) => Some(arc.clone()),
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
                let new_mixin = Value::Mixin(inner.clone(), std::sync::Arc::new(updated_mixins));
                self.propagate_mixin_update_by_arc(mixins, &new_mixin);
                if let Some(old_arc) = &old_array_arc {
                    self.propagate_shared_array_in_instances(old_arc, &new_value);
                }
                return Ok(result);
            }
            let accessor_val = self.call_method_with_values(target, &attr_name, vec![])?;
            return self.call_method_with_values(accessor_val, &method, push_args);
        }

        let Value::Instance {
            attributes,
            id: target_id,
            ..
        } = &target
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
            .unwrap_or(Value::Nil);
        let old_array_arc = match &current {
            Value::Array(arc, ..) => Some(arc.clone()),
            _ => None,
        };
        let old_hash_arc = match &current {
            Value::Hash(arc) => Some(arc.clone()),
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
        let target = args[0].clone();
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
            match extra {
                Value::Pair(key, value) if key == "delete" => {
                    delete_after = value.truthy();
                }
                Value::ValuePair(key, value) if key.to_string_value() == "delete" => {
                    delete_after = value.truthy();
                }
                Value::Str(s) if s.as_ref() == "__adverb_cond__" => {
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
            _ => return Ok(Value::Nil),
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
        let mut indices = match index {
            Value::Array(items, ..) => items.to_vec(),
            // A Range subscript on a hash is a multi-key slice (`%h{"b".."c"}:kv`),
            // so expand it to its element keys.
            ref r if r.is_range() => crate::runtime::utils::value_to_list(r),
            Value::Seq(ref items) => {
                truncate_oob = true;
                items.to_vec()
            }
            Value::LazyList(ref ll) => {
                truncate_oob = true;
                self.force_lazy_list_vm(ll)?
            }
            other => vec![other],
        };
        // Resolve WhateverCode indices (e.g. `@a[*-1]:k`) by applying them to the
        // target's length, exactly as the plain-value subscript path does.
        if indices.iter().any(|i| matches!(i, Value::Sub(..))) {
            let target_len = match &target {
                Value::Array(items, ..) => items.len() as i64,
                _ => 0,
            };
            for idx in indices.iter_mut() {
                if matches!(idx, Value::Sub(..)) {
                    *idx = self.call_sub_value(idx.clone(), vec![Value::Int(target_len)], false)?;
                }
            }
        }
        if matches!(indices.first(), Some(Value::Whatever))
            || matches!(indices.first(), Some(Value::Num(f)) if f.is_infinite() && *f > 0.0)
        {
            indices = match &target {
                Value::Array(items, ..) => (0..items.len())
                    .map(|i| Value::Int(i as i64))
                    .collect::<Vec<_>>(),
                Value::Hash(map) => map
                    .keys()
                    .map(|k| Value::str(k.clone()))
                    .collect::<Vec<_>>(),
                _ => Vec::new(),
            };
        }
        // Lazy-subscript truncation: keep only in-range indices of the array.
        if truncate_oob && let Value::Array(items, ..) = &target {
            let len = items.len() as i64;
            indices.retain(|idx| match idx {
                Value::Int(i) => *i >= 0 && *i < len,
                Value::Num(f) => *f >= 0.0 && (*f as i64) < len,
                _ => idx
                    .to_string_value()
                    .parse::<i64>()
                    .is_ok_and(|i| i >= 0 && i < len),
            });
        }
        let is_multi = indices.len() != 1;

        let mut rows: Vec<(Value, Value, bool)> = Vec::with_capacity(indices.len());
        match &target {
            Value::Array(items, ..) => {
                let bound_map = var_name
                    .as_ref()
                    .and_then(|name| self.env.get(&format!("__mutsu_initialized_index::{name}")))
                    .and_then(|v| match v {
                        Value::Hash(map) => Some(map),
                        _ => None,
                    });
                // The missing-element default comes from the array's *element
                // type*. Read it from the container value itself (embedded
                // metadata / `is default`) FIRST so it survives rebinding — e.g.
                // `for $@s, Str -> @a { @a[11]:!v }` where the loop variable has
                // no type constraint but the bound array still carries `.of=Str`.
                // Fall back to the declared variable's default/type, then Any.
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
                    .map(|t| Value::Package(Symbol::intern(&t)));
                let missing_value = container_default
                    .or(type_constraint_default)
                    .unwrap_or_else(|| Value::Package(Symbol::intern("Any")));
                for idx in &indices {
                    let i = match idx {
                        Value::Int(i) => *i,
                        Value::Num(f) => *f as i64,
                        _ => idx.to_string_value().parse::<i64>().unwrap_or(-1),
                    };
                    let key = Value::Int(i);
                    if i < 0 || i as usize >= items.len() {
                        rows.push((key, missing_value.clone(), false));
                        continue;
                    }
                    let ui = i as usize;
                    let exists = if let Some(map) = bound_map {
                        if map.contains_key(&i.to_string()) {
                            true
                        } else {
                            !matches!(&items[ui], Value::Package(name) if name == "Any")
                        }
                    } else {
                        true
                    };
                    rows.push((key, items[ui].clone(), exists));
                }
            }
            Value::Hash(map) => {
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
                            .map(|info| Value::Package(Symbol::intern(&info.value_type)))
                    })
                    .or_else(|| {
                        var_name
                            .as_ref()
                            .and_then(|name| self.var_type_constraint(name))
                            .map(|t| Value::Package(Symbol::intern(&t)))
                    })
                    .unwrap_or_else(|| Value::Package(Symbol::intern("Any")));
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
            _ => return Ok(Value::Nil),
        }

        if delete_after && let Some(var_name) = var_name.as_ref() {
            // Get hole type before mutable borrow
            let hole_type = self
                .var_type_constraint(var_name)
                .unwrap_or_else(|| "Any".to_string());
            if let Some(container) = self.env.get_mut(var_name) {
                match container {
                    Value::Hash(map) => {
                        let h = std::sync::Arc::make_mut(map);
                        for idx in &indices {
                            h.remove(&idx.to_string_value());
                        }
                    }
                    Value::Array(items, ..) => {
                        let hole_value = Value::Package(crate::symbol::Symbol::intern(&hole_type));
                        let arr = std::sync::Arc::make_mut(items);
                        for idx in &indices {
                            let i = match idx {
                                Value::Int(i) => *i,
                                Value::Num(f) => *f as i64,
                                _ => idx.to_string_value().parse::<i64>().unwrap_or(-1),
                            };
                            if i >= 0 && (i as usize) < arr.len() {
                                arr[i as usize] = hole_value.clone();
                            }
                        }
                        // Trim trailing holes
                        while let Some(last) = arr.last() {
                            let is_hole = match last {
                                Value::Nil => true,
                                Value::Package(name) => name == "Any" || name == hole_type.as_str(),
                                _ => false,
                            };
                            if is_hole {
                                arr.pop();
                            } else {
                                break;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        if !is_multi {
            let Some((key, value, exists)) = rows.into_iter().next() else {
                return Ok(Value::Nil);
            };
            if !keep_missing && !exists {
                return Ok(Value::array(Vec::new()));
            }
            return Ok(match kind {
                "kv" => Value::array(vec![key, value]),
                "p" => Value::ValuePair(Box::new(key), Box::new(value)),
                "k" => key,
                "v" => value,
                _ => Value::Nil,
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
                "p" => out.push(Value::ValuePair(Box::new(key), Box::new(value))),
                "k" => out.push(key),
                "v" => out.push(value),
                _ => {}
            }
        }
        Ok(Value::array(out))
    }
}
