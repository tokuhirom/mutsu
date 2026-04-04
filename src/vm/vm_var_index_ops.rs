use super::*;
use std::sync::Arc;

impl VM {
    /// Create a Failure for "Type X does not support associative indexing."
    fn make_assoc_indexing_failure(type_name: &str) -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str_from(&format!(
                "Type {} does not support associative indexing.",
                type_name
            )),
        );
        let ex = Value::make_instance(Symbol::intern("X::AdHoc"), attrs);
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    /// Create a Failure wrapping X::OutOfRange for effective negative indices.
    fn make_out_of_range_failure(effective_index: i64) -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str_from(&format!(
                "Effective index out of range. Is: {}, should be in 0..^Inf",
                effective_index
            )),
        );
        attrs.insert("got".to_string(), Value::Int(effective_index));
        attrs.insert("range".to_string(), Value::str_from("0..^Inf"));
        let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    pub(super) fn exec_index_op(&mut self) -> Result<(), RuntimeError> {
        let index = self.stack.pop().unwrap();
        let mut target = self.stack.pop().unwrap();
        if let Value::Junction { kind, values } = &target {
            let mut results = Vec::with_capacity(values.len());
            for value in values.iter() {
                self.stack.push(value.clone());
                self.stack.push(index.clone());
                self.exec_index_op()?;
                results.push(self.stack.pop().unwrap_or(Value::Nil));
            }
            self.stack.push(Value::junction(kind.clone(), results));
            return Ok(());
        }
        if let Value::Junction { kind, values } = &index {
            let mut results = Vec::with_capacity(values.len());
            for value in values.iter() {
                self.stack.push(target.clone());
                self.stack.push(value.clone());
                self.exec_index_op()?;
                results.push(self.stack.pop().unwrap_or(Value::Nil));
            }
            self.stack.push(Value::junction(kind.clone(), results));
            return Ok(());
        }
        // If target is a HashSlotRef, resolve it to the actual value and re-index.
        if let Value::HashSlotRef { .. } = &target {
            let resolved = target.hash_slot_read();
            // If the resolved value is a Hash, push it as the target for indexing.
            // This supports chained autovivification (e.g. %h<a><b><c>).
            self.stack.push(resolved);
            self.stack.push(index);
            return self.exec_index_op();
        }
        // If target is a Failure, propagate it (// will catch it as undefined)
        if matches!(&target, Value::Instance { class_name, .. } if class_name == "Failure") {
            self.stack.push(target);
            return Ok(());
        }
        if let Value::LazyIoLines { .. } = target {
            target = self.force_if_lazy_io_lines(target)?;
        }
        if let Value::LazyList(ref ll) = target {
            let forced = if matches!(
                ll.env.get("__mutsu_lazylist_from_gather"),
                Some(Value::Bool(true))
            ) {
                match &index {
                    Value::Int(i) if *i >= 0 => self
                        .interpreter
                        .force_lazy_list_prefix_bridge(ll, (*i as usize).saturating_add(1))?,
                    Value::Range(_, end) if *end >= 0 => self
                        .interpreter
                        .force_lazy_list_prefix_bridge(ll, (*end as usize).saturating_add(1))?,
                    Value::RangeExcl(_, end) if *end > 0 => self
                        .interpreter
                        .force_lazy_list_prefix_bridge(ll, *end as usize)?,
                    _ => self.force_lazy_list_vm(ll)?,
                }
            } else {
                self.force_lazy_list_vm(ll)?
            };
            target = Value::array(forced);
        }
        // Normalize Slip target to List for uniform handling
        if let Value::Slip(items) = target {
            target = Value::Array(items, crate::value::ArrayKind::List);
        }
        // Normalize index: convert Seq/LazyList indices to Array for
        // uniform handling in the match below.
        let is_lazy_index = matches!(&index, Value::LazyList(..));
        let index = if let Value::LazyList(ref ll) = index {
            let items = self.force_lazy_list_vm(ll)?;
            Value::array(items)
        } else if let Value::Seq(items) = index {
            Value::Array(items, crate::value::ArrayKind::List)
        } else {
            index
        };
        let result = match (target, index) {
            // Whatever (*) index on Array: @a[*] returns all elements as a List
            (Value::Array(items, _is_arr), Value::Whatever) => {
                Value::Array(items, crate::value::ArrayKind::List)
            }
            (Value::Array(items, is_arr), Value::Int(i)) => {
                if i < 0 {
                    // Return a Failure wrapping X::OutOfRange — `//` treats it as
                    // undefined but any further use (e.g. subscripting) will throw.
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("what".to_string(), Value::str_from("Index"));
                    attrs.insert("got".to_string(), Value::Int(i));
                    attrs.insert("range".to_string(), Value::str_from("0..^Inf"));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Index out of range. Is: {}, should be in 0..^Inf",
                            i
                        )),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                    let mut failure_attrs = std::collections::HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
                } else if is_arr == crate::value::ArrayKind::Shaped && (i as usize) >= items.len() {
                    // Shaped arrays have fixed size; accessing beyond bounds dies
                    return Err(RuntimeError::new(format!(
                        "Index {} for dimension 1 out of range (must be 0..{})",
                        i,
                        items.len() - 1
                    )));
                } else {
                    let default =
                        self.typed_container_default(&Value::Array(items.clone(), is_arr));
                    self.resolve_array_entry(&items, is_arr, i as usize, default)
                }
            }
            (target @ Value::Array(..), Value::Array(indices, ..)) => {
                let depth = Self::array_depth(&target);
                if depth <= 1 && indices.len() > 1 {
                    // Positional slice: @a[0,1,2] returns (@a[0], @a[1], @a[2])
                    let Value::Array(items, kind) = &target else {
                        unreachable!()
                    };
                    let mut out = Vec::with_capacity(indices.len());
                    let len = items.len() as i64;
                    for idx in indices.iter() {
                        // Resolve WhateverCode indices (e.g. *-1, *-3)
                        let resolved_idx = if let Value::Sub(data) = idx {
                            let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                            let mut sub_env = data.env.clone();
                            sub_env.insert(param.to_string(), Value::Int(len));
                            let saved_env = std::mem::take(self.interpreter.env_mut());
                            *self.interpreter.env_mut() = sub_env;
                            let result = self
                                .interpreter
                                .eval_block_value(&data.body)
                                .unwrap_or(Value::Nil);
                            *self.interpreter.env_mut() = saved_env;
                            Some(result)
                        } else {
                            None
                        };
                        let effective_idx = resolved_idx.as_ref().unwrap_or(idx);
                        if let Some(i) = Self::index_to_usize(effective_idx) {
                            if is_lazy_index && i >= items.len() {
                                // Lazy index: stop at array boundary
                                break;
                            }
                            out.push(self.resolve_array_entry(
                                items,
                                *kind,
                                i,
                                self.typed_container_default(&target),
                            ));
                        } else if !is_lazy_index {
                            out.push(self.typed_container_default(&target));
                        }
                    }
                    Value::array(out)
                } else {
                    let strict_oob = indices.len() > 1;
                    let indexed =
                        Self::index_array_multidim(&target, indices.as_ref(), strict_oob)?;
                    if matches!(indexed, Value::Nil) {
                        self.typed_container_default(&target)
                    } else {
                        indexed
                    }
                }
            }
            (Value::Array(items, kind), Value::Range(a, b)) => {
                let start = a.max(0) as usize;
                let end = if Self::range_end_is_unbounded(b) {
                    items.len().saturating_sub(1)
                } else {
                    b.max(-1) as usize
                };
                let default = self.typed_container_default(&Value::Array(items.clone(), kind));
                let mut slice = Vec::new();
                for i in start..=end {
                    slice.push(self.resolve_array_entry(&items, kind, i, default.clone()));
                }
                if kind.is_real_array() {
                    Value::array(slice)
                } else {
                    Value::Seq(Arc::new(slice))
                }
            }
            (Value::Array(items, kind), Value::RangeExcl(a, b)) => {
                let start = a.max(0) as usize;
                let end_excl = if Self::range_end_is_unbounded(b) {
                    items.len()
                } else {
                    b.max(0) as usize
                };
                if start >= end_excl {
                    if kind.is_real_array() {
                        Value::array(Vec::new())
                    } else {
                        Value::Seq(Arc::new(Vec::new()))
                    }
                } else {
                    let default = self.typed_container_default(&Value::Array(items.clone(), kind));
                    let mut slice = Vec::with_capacity(end_excl - start);
                    for i in start..end_excl {
                        slice.push(self.resolve_array_entry(&items, kind, i, default.clone()));
                    }
                    if kind.is_real_array() {
                        Value::array(slice)
                    } else {
                        Value::Seq(Arc::new(slice))
                    }
                }
            }
            (Value::Array(items, is_arr), Value::Num(n)) => {
                let default = self.typed_container_default(&Value::Array(items.clone(), is_arr));
                if n < 0.0 {
                    default
                } else {
                    self.resolve_array_entry(&items, is_arr, n as usize, default)
                }
            }
            (Value::Array(items, is_arr), Value::Rat(n, d)) if d != 0 => {
                let default = self.typed_container_default(&Value::Array(items.clone(), is_arr));
                let i = (n as f64 / d as f64) as usize;
                self.resolve_array_entry(&items, is_arr, i, default)
            }
            (Value::Array(items, is_arr), Value::FatRat(n, d)) if d != 0 => {
                let default = self.typed_container_default(&Value::Array(items.clone(), is_arr));
                let i = (n as f64 / d as f64) as usize;
                self.resolve_array_entry(&items, is_arr, i, default)
            }
            (Value::Array(items, is_arr), Value::BigRat(n, d)) if !d.is_zero() => {
                let default = self.typed_container_default(&Value::Array(items.clone(), is_arr));
                let idx = runtime::to_float_value(&Value::BigRat(n, d)).unwrap_or(0.0);
                if idx < 0.0 {
                    default
                } else {
                    self.resolve_array_entry(&items, is_arr, idx as usize, default)
                }
            }
            (Value::Seq(items), Value::Int(i)) => {
                if i < 0 {
                    Value::Nil
                } else {
                    items.get(i as usize).cloned().unwrap_or(Value::Nil)
                }
            }
            (Value::Seq(items), Value::Range(a, b)) => {
                let start = a.max(0) as usize;
                let end = b.max(-1) as usize;
                let slice = if start >= items.len() {
                    Vec::new()
                } else {
                    let end = end.min(items.len().saturating_sub(1));
                    items[start..=end].to_vec()
                };
                Value::Seq(Arc::new(slice))
            }
            (Value::Seq(items), Value::RangeExcl(a, b)) => {
                let start = a.max(0) as usize;
                let end_excl = b.max(0) as usize;
                let slice = if start >= items.len() {
                    Vec::new()
                } else {
                    let end_excl = end_excl.min(items.len());
                    if start >= end_excl {
                        Vec::new()
                    } else {
                        items[start..end_excl].to_vec()
                    }
                };
                Value::Seq(Arc::new(slice))
            }
            // WhateverCode index on Seq: (1,2,3).Seq[*-1]
            (Value::Seq(items), Value::Sub(ref data)) => {
                let len = items.len() as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    _ => None,
                };
                match i {
                    Some(i) if i >= 0 => items.get(i as usize).cloned().unwrap_or(Value::Nil),
                    Some(i) if i < 0 => Self::make_out_of_range_failure(i),
                    _ => Value::Nil,
                }
            }
            (Value::Hash(items), Value::Whatever) => {
                Value::array(items.values().cloned().collect())
            }
            (Value::Hash(items), Value::Num(f)) if f.is_infinite() && f > 0.0 => {
                Value::array(items.values().cloned().collect())
            }
            (Value::Hash(items), Value::Nil) => Value::Hash(items),
            (Value::Hash(items), Value::Array(keys, ..)) => {
                let default = self.typed_container_default(&Value::Hash(items.clone()));
                Value::array(
                    keys.iter()
                        .map(|k| {
                            let v = self.resolve_hash_entry(&items, &k.to_string_value());
                            if matches!(v, Value::Nil) {
                                default.clone()
                            } else {
                                v
                            }
                        })
                        .collect(),
                )
            }
            (Value::Hash(items), Value::Str(key)) => {
                let v = self.resolve_hash_entry(&items, &key);
                if matches!(v, Value::Nil) {
                    if self.interpreter.hash_autovivify {
                        Value::Hash(items)
                            .hash_autovivify(&key)
                            .unwrap_or(Value::Nil)
                    } else {
                        self.typed_container_default(&Value::Hash(items))
                    }
                } else {
                    v
                }
            }
            (Value::Hash(items), Value::Int(key)) => {
                let key_str = key.to_string();
                let v = self.resolve_hash_entry(&items, &key_str);
                if matches!(v, Value::Nil) {
                    if self.interpreter.hash_autovivify {
                        Value::Hash(items)
                            .hash_autovivify(&key_str)
                            .unwrap_or(Value::Nil)
                    } else {
                        self.typed_container_default(&Value::Hash(items))
                    }
                } else {
                    v
                }
            }
            // WhateverCode positional index on Hash: {}[*-1]
            // Treat hash as a list of pairs with elems = hash.len()
            (Value::Hash(items), Value::Sub(ref data)) => {
                let len = items.len() as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                match &idx {
                    Value::Int(i) if *i < 0 => Self::make_out_of_range_failure(*i),
                    Value::Int(i) => {
                        let pairs: Vec<Value> = items
                            .iter()
                            .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                            .collect();
                        pairs.get(*i as usize).cloned().unwrap_or(Value::Nil)
                    }
                    _ => Value::Nil,
                }
            }
            (Value::Hash(items), key) => {
                let key_str = key.to_string_value();
                let v = self.resolve_hash_entry(&items, &key_str);
                if matches!(v, Value::Nil) {
                    if self.interpreter.hash_autovivify {
                        Value::Hash(items)
                            .hash_autovivify(&key_str)
                            .unwrap_or(Value::Nil)
                    } else {
                        self.typed_container_default(&Value::Hash(items))
                    }
                } else {
                    v
                }
            }
            (
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
                Value::Str(key),
            ) if class_name == "Match" => {
                if let Some(Value::Hash(named)) = attributes.get("named") {
                    named.get(key.as_str()).cloned().unwrap_or(Value::Nil)
                } else {
                    Value::Nil
                }
            }
            (
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
                Value::Int(i),
            ) if class_name == "Match" => {
                if i < 0 {
                    Value::Nil
                } else if let Some(Value::Array(items, ..)) = attributes.get("list") {
                    items.get(i as usize).cloned().unwrap_or(Value::Nil)
                } else {
                    Value::Nil
                }
            }
            (instance @ Value::Instance { .. }, Value::Str(key)) => {
                let default = self.typed_container_default(&instance);
                let result = self
                    .try_compiled_method_or_interpret(
                        instance,
                        "AT-KEY",
                        vec![Value::Str(key.clone())],
                    )
                    .unwrap_or(Value::Nil);
                if matches!(result, Value::Nil) {
                    default
                } else {
                    result
                }
            }
            (instance @ Value::Instance { .. }, Value::Int(i)) => {
                let default = self.typed_container_default(&instance);
                let fallback = instance.clone();
                let result = self
                    .try_compiled_method_or_interpret(instance, "AT-POS", vec![Value::Int(i)])
                    .or_else(|_| {
                        self.try_compiled_method_or_interpret(
                            fallback,
                            "AT-KEY",
                            vec![Value::Int(i)],
                        )
                    })
                    .unwrap_or(Value::Nil);
                if matches!(result, Value::Nil) {
                    default
                } else {
                    result
                }
            }
            (instance @ Value::Instance { .. }, Value::Array(keys, ..)) => {
                let mut results = Vec::with_capacity(keys.len());
                for k in keys.iter().cloned() {
                    let value = match k {
                        Value::Int(_)
                        | Value::Num(_)
                        | Value::Range(_, _)
                        | Value::RangeExcl(_, _)
                        | Value::Whatever
                        | Value::Sub(_)
                        | Value::WeakSub(_) => self
                            .try_compiled_method_or_interpret(
                                instance.clone(),
                                "AT-POS",
                                vec![k.clone()],
                            )
                            .or_else(|_| {
                                self.try_compiled_method_or_interpret(
                                    instance.clone(),
                                    "AT-KEY",
                                    vec![k],
                                )
                            })
                            .unwrap_or(Value::Nil),
                        _ => self
                            .try_compiled_method_or_interpret(instance.clone(), "AT-KEY", vec![k])
                            .unwrap_or(Value::Nil),
                    };
                    results.push(value);
                }
                Value::array(results)
            }
            (instance @ Value::Mixin(..), Value::Str(key)) => {
                let default = self.typed_container_default(&instance);
                let result = self
                    .try_compiled_method_or_interpret(
                        instance,
                        "AT-KEY",
                        vec![Value::Str(key.clone())],
                    )
                    .unwrap_or(Value::Nil);
                if matches!(result, Value::Nil) {
                    default
                } else {
                    result
                }
            }
            (instance @ Value::Mixin(..), Value::Int(i)) => {
                if let Value::Mixin(_, mixins) = &instance
                    && let Some(attr_key) = self.delegated_mixin_attr_key(mixins, "AT-POS")
                    && let Some(attr_value) = mixins.get(&attr_key).cloned()
                {
                    self.stack.push(attr_value);
                    self.stack.push(Value::Int(i));
                    self.exec_index_op()?;
                    let result = self.stack.pop().unwrap_or(Value::Nil);
                    if !matches!(result, Value::Nil) {
                        result
                    } else {
                        self.typed_container_default(&instance)
                    }
                } else {
                    let default = self.typed_container_default(&instance);
                    let fallback = instance.clone();
                    let result = self
                        .try_compiled_method_or_interpret(instance, "AT-POS", vec![Value::Int(i)])
                        .or_else(|_| {
                            self.try_compiled_method_or_interpret(
                                fallback,
                                "AT-KEY",
                                vec![Value::Int(i)],
                            )
                        })
                        .unwrap_or(Value::Nil);
                    if matches!(result, Value::Nil) {
                        default
                    } else {
                        result
                    }
                }
            }
            (instance @ Value::Mixin(..), Value::Array(keys, ..)) => {
                let mut results = Vec::with_capacity(keys.len());
                let delegated_attr: Option<Value> = if let Value::Mixin(_, mixins) = &instance {
                    self.delegated_mixin_attr_key(mixins, "AT-POS")
                        .and_then(|attr_key| mixins.get(&attr_key).cloned())
                } else {
                    None
                };
                for k in keys.iter().cloned() {
                    let value = match (&delegated_attr, &k) {
                        (
                            Some(attr_value),
                            Value::Int(_)
                            | Value::Num(_)
                            | Value::Range(_, _)
                            | Value::RangeExcl(_, _)
                            | Value::Whatever
                            | Value::Sub(_)
                            | Value::WeakSub(_),
                        ) => {
                            self.stack.push(attr_value.clone());
                            self.stack.push(k.clone());
                            self.exec_index_op()?;
                            self.stack.pop().unwrap_or(Value::Nil)
                        }
                        _ => match k {
                            Value::Int(_)
                            | Value::Num(_)
                            | Value::Range(_, _)
                            | Value::RangeExcl(_, _)
                            | Value::Whatever
                            | Value::Sub(_)
                            | Value::WeakSub(_) => self
                                .try_compiled_method_or_interpret(
                                    instance.clone(),
                                    "AT-POS",
                                    vec![k.clone()],
                                )
                                .or_else(|_| {
                                    self.try_compiled_method_or_interpret(
                                        instance.clone(),
                                        "AT-KEY",
                                        vec![k],
                                    )
                                })
                                .unwrap_or(Value::Nil),
                            _ => self
                                .try_compiled_method_or_interpret(
                                    instance.clone(),
                                    "AT-KEY",
                                    vec![k],
                                )
                                .unwrap_or(Value::Nil),
                        },
                    };
                    results.push(value);
                }
                Value::array(results)
            }
            (Value::Str(_), Value::Str(_)) => {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str_from("Type Str does not support associative indexing."),
                );
                let ex = Value::make_instance(Symbol::intern("X::AdHoc"), attrs);
                let mut failure_attrs = std::collections::HashMap::new();
                failure_attrs.insert("exception".to_string(), ex);
                Value::make_instance(Symbol::intern("Failure"), failure_attrs)
            }
            (Value::Set(s, _), Value::Array(keys, ..)) => Value::array(
                keys.iter()
                    .map(|k| Value::Bool(s.contains(&k.to_string_value())))
                    .collect(),
            ),
            (Value::Set(s, _), Value::Str(key)) => Value::Bool(s.contains(key.as_str())),
            (Value::Set(s, _), idx) => Value::Bool(s.contains(&idx.to_string_value())),
            (Value::Bag(b, _), Value::Array(keys, ..)) => Value::array(
                keys.iter()
                    .map(|k| Value::Int(*b.get(&k.to_string_value()).unwrap_or(&0)))
                    .collect(),
            ),
            (Value::Bag(b, _), Value::Str(key)) => Value::Int(*b.get(key.as_str()).unwrap_or(&0)),
            (Value::Bag(b, _), idx) => Value::Int(*b.get(&idx.to_string_value()).unwrap_or(&0)),
            (Value::Mix(m, _), Value::Array(keys, ..)) => Value::array(
                keys.iter()
                    .map(|k| {
                        Self::mix_weight_as_value(*m.get(&k.to_string_value()).unwrap_or(&0.0))
                    })
                    .collect(),
            ),
            (Value::Mix(m, _), Value::Str(key)) => {
                Self::mix_weight_as_value(*m.get(key.as_str()).unwrap_or(&0.0))
            }
            (Value::Mix(m, _), idx) => {
                Self::mix_weight_as_value(*m.get(&idx.to_string_value()).unwrap_or(&0.0))
            }
            // Range indexing (supports infinite ranges)
            (ref range, Value::Int(i)) if range.is_range() => {
                if let Some((start, _end, _excl_start, _excl_end)) = range_params(range) {
                    if i < 0 {
                        Value::Nil
                    } else {
                        Value::Int(start + i)
                    }
                } else {
                    let items = crate::runtime::utils::value_to_list(range);
                    if i < 0 {
                        Value::Nil
                    } else {
                        items.get(i as usize).cloned().unwrap_or(Value::Nil)
                    }
                }
            }
            // WhateverCode index on Range: (1..8)[*-1]
            (ref range, Value::Sub(ref data)) if range.is_range() => {
                let len = crate::runtime::Interpreter::range_elems_f64(range) as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    _ => None,
                };
                match i {
                    Some(i) if i >= 0 => {
                        if let Some((start, end, _excl_start, excl_end)) = range_params(range) {
                            let actual_end = if excl_end { end - 1 } else { end };
                            let val = start + i;
                            if val > actual_end {
                                Value::Nil
                            } else {
                                Value::Int(val)
                            }
                        } else {
                            let items = crate::runtime::utils::value_to_list(range);
                            items.get(i as usize).cloned().unwrap_or(Value::Nil)
                        }
                    }
                    Some(i) if i < 0 => Self::make_out_of_range_failure(i),
                    _ => Value::Nil,
                }
            }
            (ref range, Value::RangeExcl(a, b)) if range.is_range() => {
                if let Some((start, end, _excl_start, excl_end)) = range_params(range) {
                    let actual_end = if excl_end { end - 1 } else { end };
                    let mut result = Vec::new();
                    for i in a..b {
                        let val = match start.checked_add(i) {
                            Some(v) => v,
                            None => break,
                        };
                        if val > actual_end {
                            break;
                        }
                        result.push(Value::Int(val));
                    }
                    Value::array(result)
                } else {
                    let items = crate::runtime::utils::value_to_list(range);
                    let start = a.max(0) as usize;
                    let end_excl = b.max(0) as usize;
                    if start >= items.len() {
                        Value::array(Vec::new())
                    } else {
                        let end_excl = end_excl.min(items.len());
                        if start >= end_excl {
                            Value::array(Vec::new())
                        } else {
                            Value::array(items[start..end_excl].to_vec())
                        }
                    }
                }
            }
            (ref range, Value::Range(a, b)) if range.is_range() => {
                if let Some((start, end, _excl_start, excl_end)) = range_params(range) {
                    let actual_end = if excl_end { end - 1 } else { end };
                    let mut result = Vec::new();
                    for i in a..=b {
                        let val = start + i;
                        if val > actual_end {
                            break;
                        }
                        result.push(Value::Int(val));
                    }
                    Value::array(result)
                } else {
                    let items = crate::runtime::utils::value_to_list(range);
                    let start = a.max(0) as usize;
                    let end = b.max(-1) as usize;
                    if start >= items.len() {
                        Value::array(Vec::new())
                    } else {
                        let end = end.min(items.len().saturating_sub(1));
                        Value::array(items[start..=end].to_vec())
                    }
                }
            }
            (ref range, Value::Array(indices, ..)) if range.is_range() => {
                if let Some((start, _end, _excl_start, _excl_end)) = range_params(range) {
                    let result: Vec<Value> = indices
                        .iter()
                        .map(|idx| match idx {
                            Value::Int(i) => Value::Int(start + i),
                            _ => Value::Nil,
                        })
                        .collect();
                    Value::array(result)
                } else {
                    let items = crate::runtime::utils::value_to_list(range);
                    let result: Vec<Value> = indices
                        .iter()
                        .map(|idx| match idx {
                            Value::Int(i) if *i >= 0 => {
                                items.get(*i as usize).cloned().unwrap_or(Value::Nil)
                            }
                            _ => Value::Nil,
                        })
                        .collect();
                    Value::array(result)
                }
            }
            // WhateverCode index: @a[*-1] → evaluate the lambda with array length
            (Value::Array(ref items, ..), Value::Sub(ref data)) => {
                let len = items.len() as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    Value::Rat(n, d) => {
                        if *d != 0 {
                            Some((*n as f64 / *d as f64).floor() as i64)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                match i {
                    Some(i) if i >= 0 => items.get(i as usize).cloned().unwrap_or(Value::Nil),
                    Some(i) if i < 0 => Self::make_out_of_range_failure(i),
                    _ => Value::Nil,
                }
            }
            // WhateverCode index on Instance (e.g. Buf): $buf[*-1]
            (
                Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                },
                Value::Sub(ref data),
            ) => {
                // Get element count from the instance
                let len = if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) {
                    if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                        bytes.len() as i64
                    } else {
                        0
                    }
                } else {
                    0
                };
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    Value::Rat(n, d) => {
                        if *d != 0 {
                            Some((*n as f64 / *d as f64).floor() as i64)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                match i {
                    Some(i) if i >= 0 => {
                        if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                            bytes.get(i as usize).cloned().unwrap_or(Value::Nil)
                        } else {
                            Value::Nil
                        }
                    }
                    _ => Value::Nil,
                }
            }
            // GenericRange with WhateverCode endpoint: @a[0..*-2]
            (
                Value::Array(ref items, ..),
                Value::GenericRange {
                    ref start,
                    ref end,
                    excl_start,
                    excl_end,
                },
            ) => {
                let len = items.len() as i64;
                let mut resolve_endpoint = |val: &Value| -> i64 {
                    match val {
                        Value::Int(i) => *i,
                        Value::Sub(data) => {
                            let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                            let mut sub_env = data.env.clone();
                            sub_env.insert(param.to_string(), Value::Int(len));
                            let saved_env = std::mem::take(self.interpreter.env_mut());
                            *self.interpreter.env_mut() = sub_env;
                            let result = self
                                .interpreter
                                .eval_block_value(&data.body)
                                .unwrap_or(Value::Nil);
                            *self.interpreter.env_mut() = saved_env;
                            match result {
                                Value::Int(i) => i,
                                _ => 0,
                            }
                        }
                        _ => match val {
                            Value::Num(f) => *f as i64,
                            _ => 0,
                        },
                    }
                };
                let s = resolve_endpoint(start);
                let e = resolve_endpoint(end);
                let actual_start = if excl_start { s + 1 } else { s }.max(0) as usize;
                let actual_end = if excl_end { e } else { e + 1 }.max(0) as usize;
                let actual_end = actual_end.min(items.len());
                let slice = if actual_start >= actual_end {
                    Vec::new()
                } else {
                    items[actual_start..actual_end].to_vec()
                };
                Value::array(slice)
            }
            // Uni/NFC/NFD/NFKC/NFKD indexing: returns integer codepoint values
            (Value::Uni { ref text, .. }, Value::Int(i)) => {
                let chars: Vec<char> = text.chars().collect();
                if i < 0 || (i as usize) >= chars.len() {
                    Value::Nil
                } else {
                    Value::Int(chars[i as usize] as i64)
                }
            }
            (Value::Uni { ref text, .. }, Value::Sub(ref data)) => {
                let chars: Vec<char> = text.chars().collect();
                let len = chars.len() as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    _ => None,
                };
                match i {
                    Some(i) if i >= 0 && (i as usize) < chars.len() => {
                        Value::Int(chars[i as usize] as i64)
                    }
                    _ => Value::Nil,
                }
            }
            (Value::Uni { ref text, .. }, Value::Array(indices, ..)) => {
                let chars: Vec<char> = text.chars().collect();
                Value::array(
                    indices
                        .iter()
                        .map(|idx| {
                            if let Value::Int(i) = idx {
                                if *i >= 0 && (*i as usize) < chars.len() {
                                    Value::Int(chars[*i as usize] as i64)
                                } else {
                                    Value::Nil
                                }
                            } else {
                                Value::Nil
                            }
                        })
                        .collect(),
                )
            }
            // Capture indexing: $capture<key> (named) or $capture[idx] (positional)
            (
                Value::Capture {
                    positional: _,
                    named,
                },
                Value::Str(key),
            ) => named.get(key.as_str()).cloned().unwrap_or(Value::Nil),
            (Value::Capture { positional, .. }, Value::Int(i)) => {
                if i < 0 {
                    Value::Nil
                } else {
                    positional.get(i as usize).cloned().unwrap_or(Value::Nil)
                }
            }
            // Mu type object does not support postcircumfix { }
            (Value::Package(name), _) if name.resolve() == "Mu" => {
                return Err(RuntimeError::typed(
                    "X::Multi::NoMatch",
                    [(
                        "message".to_string(),
                        Value::str("Cannot resolve caller postcircumfix:<{ }>(Mu:U)".to_string()),
                    )]
                    .into_iter()
                    .collect(),
                ));
            }
            // Role parameterization: e.g. R1[C1] → ParametricRole
            (Value::Package(name), idx) if self.interpreter.is_role(&name.resolve()) => {
                let type_args = match idx {
                    Value::Array(items, ..) => items.as_ref().clone(),
                    other => vec![other],
                };
                Value::ParametricRole {
                    base_name: name,
                    type_args,
                }
            }
            // Type parameterization: e.g. Array[Int] or Hash[Int,Str]
            (Value::Package(name), idx) => {
                let type_args = match idx {
                    Value::Array(items, ..) => items.as_ref().clone(),
                    other => vec![other],
                };
                let args = type_args
                    .into_iter()
                    .map(|v| match v {
                        Value::Package(name) => name.resolve(),
                        other => {
                            let s = other.to_string_value();
                            s.trim_start_matches('(').trim_end_matches(')').to_string()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                Value::Package(Symbol::intern(&format!("{}[{}]", name, args)))
            }
            // Pair subscript: $pair<key> returns value if key matches, Nil otherwise
            (Value::Pair(key, value), Value::Str(idx)) => {
                if key.as_str() == idx.as_str() {
                    *value
                } else {
                    Value::Nil
                }
            }
            (Value::ValuePair(key, value), Value::Str(idx)) => {
                if key.to_string_value() == **idx {
                    *value
                } else {
                    Value::Nil
                }
            }
            // Array + Str: coerce numeric string to Int for positional indexing
            (Value::Array(items, is_arr), Value::Str(ref s)) => {
                if let Ok(i) = s.trim().parse::<i64>() {
                    if i < 0 {
                        Self::make_out_of_range_failure(i)
                    } else {
                        let default =
                            self.typed_container_default(&Value::Array(items.clone(), is_arr));
                        self.resolve_array_entry(&items, is_arr, i as usize, default)
                    }
                } else {
                    Self::make_assoc_indexing_failure("Array")
                }
            }
            // Associative indexing on non-hash types returns a Failure
            (ref target, Value::Str(_))
                if matches!(
                    target,
                    Value::Int(_)
                        | Value::BigInt(_)
                        | Value::Num(_)
                        | Value::Rat(..)
                        | Value::Bool(_)
                ) =>
            {
                let type_name = crate::value::types::what_type_name(target);
                Self::make_assoc_indexing_failure(&type_name)
            }
            // Scalar value with integer index: treat as single-element list
            (ref val, Value::Int(0))
                if !matches!(val, Value::Array(..) | Value::Hash(_) | Value::Nil) =>
            {
                val.clone()
            }
            (ref val, Value::Int(i))
                if !matches!(val, Value::Array(..) | Value::Hash(_)) && i > 0 =>
            {
                Value::Nil
            }
            // Scalar value with WhateverCode index: treat as single-element list
            (ref val, Value::Sub(ref data))
                if !matches!(
                    val,
                    Value::Array(..) | Value::Hash(_) | Value::Instance { .. }
                ) =>
            {
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(1)); // elems = 1
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                let i = match &idx {
                    Value::Int(i) => Some(*i),
                    Value::Num(n) => Some(*n as i64),
                    Value::Rat(n, d) => {
                        if *d != 0 {
                            Some((*n as f64 / *d as f64).floor() as i64)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                match i {
                    Some(0) => val.clone(),
                    _ => Value::Nil,
                }
            }
            _ => Value::Nil,
        };
        self.stack.push(result);
        Ok(())
    }
}

/// Extract (start, end, excl_start, excl_end) from a Range value.
fn range_params(v: &Value) -> Option<(i64, i64, bool, bool)> {
    match v {
        Value::Range(a, b) => Some((*a, *b, false, false)),
        Value::RangeExcl(a, b) => Some((*a, *b, false, true)),
        Value::RangeExclStart(a, b) => Some((*a + 1, *b, true, false)),
        Value::RangeExclBoth(a, b) => Some((*a + 1, *b, true, true)),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            if !start.is_numeric() || !end.is_numeric() {
                return None;
            }
            let s = start.to_f64() as i64;
            let e = end.to_f64() as i64;
            if start.to_f64().is_nan() || end.to_f64().is_nan() {
                return None;
            }
            let s = if *excl_start { s + 1 } else { s };
            Some((s, e, *excl_start, *excl_end))
        }
        _ => None,
    }
}
