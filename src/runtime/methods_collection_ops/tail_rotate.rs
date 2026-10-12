use super::*;
use crate::meta_ns::MetaNs;
use crate::value::ValueView;

impl Interpreter {
    /// Look up the PredictiveIterator backing a `Seq.new(iterator)` by seq_id.
    /// Prefers the non-scoped `predictive_seq_iters` map (survives scope), then
    /// falls back to the legacy env key for any in-scope registrations.
    /// `pub(crate)` (not `pub(in crate::runtime)`): also read by
    /// `vm::vm_helpers_lazy`'s ADR-0034 dispatch guard, which must recognize
    /// and skip this placeholder body BEFORE `dispatch_tail`/`.Numeric` get a
    /// chance to resolve it (see that call site's comment).
    pub(crate) fn predictive_seq_iter_for(&self, seq_id: usize) -> Option<Value> {
        if let Some(iter) = self.types.predictive_seq_iters.get(&seq_id) {
            return Some(iter.clone());
        }
        self.env
            .get(&MetaNs::PredictiveSeqIter.key_for_id(seq_id as u64))
            .cloned()
    }

    /// Cost: O(k), k = elements requested (1 for `.tail`), on an Array, a List or a
    /// reified Seq: the last k are sliced off the borrowed items. O(e), e =
    /// elements, on any other list-like (decomposed first) and on a
    /// `Seq.new($iterator)` body (O(e) `skip-one` calls).
    pub(in crate::runtime) fn dispatch_tail(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if crate::builtins::method_table::terminals::is_lazy_tail_target(&target) {
            let mut attrs = HashMap::new();
            attrs.insert("action".to_string(), Value::str_from("tail"));
            attrs.insert(
                "message".to_string(),
                Value::str_from("Cannot .tail a lazy list"),
            );
            let exception = Value::make_instance(Symbol::intern("X::Cannot::Lazy"), attrs);
            let mut err = RuntimeError::new("X::Cannot::Lazy");
            err.exception = Some(Box::new(exception));
            return Err(err);
        }

        if let ValueView::Seq(items) = target.view()
            && items.is_empty()
        {
            let seq_id = items.identity();
            let key = MetaNs::PredictiveSeqIter.key_for_id(seq_id as u64);
            if let Some(iterator) = self.predictive_seq_iter_for(seq_id) {
                let iter_slot = "$mutsu_predictive_tail_iterator";
                let saved_iter = self.env.get(iter_slot).cloned();
                self.env.insert(iter_slot.to_string(), iterator);

                let current_iter = self.env.get(iter_slot).cloned().unwrap_or(Value::NIL);
                let count_only = self
                    .call_method_mut_with_values(
                        iter_slot,
                        current_iter.clone(),
                        "count-only",
                        vec![],
                    )
                    .or_else(|_| {
                        self.call_method_with_values(current_iter, "count-only", vec![])
                    })?;
                let total_len = count_only.to_f64().max(0.0) as usize;
                let tail_count = if args.is_empty() {
                    if total_len == 0 { 0 } else { 1 }
                } else {
                    self.resolve_supply_tail_count(args.first(), total_len)?
                };
                let skip = total_len.saturating_sub(tail_count);
                for _ in 0..skip {
                    let current_iter = self.env.get(iter_slot).cloned().unwrap_or(Value::NIL);
                    let skipped = self
                        .call_method_mut_with_values(
                            iter_slot,
                            current_iter.clone(),
                            "skip-one",
                            vec![],
                        )
                        .or_else(|_| {
                            self.call_method_with_values(current_iter, "skip-one", vec![])
                        })?;
                    if !skipped.truthy() {
                        break;
                    }
                }
                let mut pulled = Vec::with_capacity(tail_count);
                for _ in 0..tail_count {
                    let current_iter = self.env.get(iter_slot).cloned().unwrap_or(Value::NIL);
                    let value = self
                        .call_method_mut_with_values(
                            iter_slot,
                            current_iter.clone(),
                            "pull-one",
                            vec![],
                        )
                        .or_else(|_| {
                            self.call_method_with_values(current_iter, "pull-one", vec![])
                        })?;
                    if value.is_iteration_end() {
                        break;
                    }
                    pulled.push(value);
                }
                if let Some(updated_iter) = self.env.get(iter_slot).cloned() {
                    if let ValueView::Instance { attributes, .. } = updated_iter.view() {
                        for (meta_key, source_name) in attributes.as_map().iter() {
                            let Some(attr_name) =
                                meta_key.as_str().strip_prefix("__mutsu_attr_alias::")
                            else {
                                continue;
                            };
                            let ValueView::Str(source_name) = source_name.view() else {
                                continue;
                            };
                            if let Some(attr_value) = attributes.as_map().get(attr_name).cloned() {
                                self.env.insert(source_name.to_string(), attr_value);
                            }
                        }
                    }
                    // Persist the advanced iterator state in both the non-scoped
                    // map (authoritative, survives scope) and the legacy env key.
                    self.types
                        .predictive_seq_iters
                        .insert(seq_id, updated_iter.clone());
                    self.env.insert(key, updated_iter);
                }
                if let Some(prev) = saved_iter {
                    self.env.insert(iter_slot.to_string(), prev);
                } else {
                    self.env.remove(iter_slot);
                }
                if args.is_empty() {
                    return Ok(pulled.pop().unwrap_or(Value::NIL));
                }
                return Ok(Value::seq(pulled));
            }
        }

        // Borrowed, not copied: only the last `tail_count` items are cloned.
        if args.is_empty() {
            return Ok(crate::runtime::utils::with_list_items(&target, |items| {
                items.last().cloned().unwrap_or(Value::NIL)
            }));
        }

        let len = crate::runtime::utils::list_items_len(&target);
        let tail_count = self.resolve_supply_tail_count(args.first(), len)?;
        Ok(Value::seq(crate::runtime::utils::with_list_items(
            &target,
            |items| items[items.len().saturating_sub(tail_count)..].to_vec(),
        )))
    }

    /// Handle `.head(&callable)` / `.head(*)` where the argument is a
    /// WhateverCode/Callable or a bare Whatever. Raku invokes the callable with
    /// `0` and uses the result `n` as an offset from the end: the number of
    /// leading elements to keep is `len + n` (clamped to `0..=len`). A bare
    /// `*` (Whatever) keeps the whole list. Plain numeric counts are handled by
    /// the native fast path, not here.
    /// Cost: O(e), e = elements of the invocant (copied whole; a `*-n` count needs
    /// the length anyway).
    pub(in crate::runtime) fn dispatch_head(
        &mut self,
        target: Value,
        arg: &Value,
    ) -> Result<Value, RuntimeError> {
        let items = crate::runtime::utils::value_to_list(&target);
        let len = items.len() as i64;
        let count = match arg.view() {
            ValueView::Whatever => len,
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => {
                let computed = self.eval_call_on_value(arg.clone(), vec![Value::int(0)])?;
                let n = match computed.view() {
                    ValueView::Whatever => len,
                    ValueView::Int(i) => i,
                    ValueView::Num(f) => f as i64,
                    ValueView::Rat(num, den) if den != 0 => num / den,
                    _ => computed.to_string_value().parse::<i64>().unwrap_or(0),
                };
                (len + n).clamp(0, len)
            }
            _ => len,
        };
        let count = count.clamp(0, len) as usize;
        Ok(Value::seq(items[..count].to_vec()))
    }

    pub(in crate::runtime) fn split_host_port_literal(input: &str) -> (String, Option<u16>) {
        let s = input.trim();
        if s.is_empty() {
            return (String::new(), None);
        }
        if let Some(stripped) = s.strip_prefix('[')
            && let Some(end_rel) = stripped.find(']')
        {
            let end = end_rel + 1;
            let host = &s[..=end];
            let rest = &s[end + 1..];
            if let Some(port_str) = rest.strip_prefix(':')
                && let Ok(port) = port_str.parse::<u16>()
            {
                return (host.to_string(), Some(port));
            }
            return (s.to_string(), None);
        }
        if let Some((host, port_str)) = s.rsplit_once(':')
            && !host.contains(':')
            && let Ok(port) = port_str.parse::<u16>()
        {
            return (host.to_string(), Some(port));
        }
        (s.to_string(), None)
    }

    /// Cost: see [`rotate_value`](crate::builtins::method_table::collection_rotate::rotate_value).
    pub(in crate::runtime) fn dispatch_rotate(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        crate::builtins::method_table::collection_rotate::rotate_value(&target, args)
    }
}
