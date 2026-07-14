use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn append_slip_item(args: &mut Vec<Value>, item: &Value) {
        match item.view() {
            ValueView::Capture { positional, named } => {
                args.extend(positional.iter().cloned());
                for (k, v) in named.iter() {
                    args.push(Value::pair(k.clone(), v.clone()));
                }
            }
            // Hash values inside a Slip are kept as single positional args.
            // Top-level `|%hash` flattening is handled by MakeSlip, which converts
            // a bare Hash into pairs before wrapping in a Slip. A Hash that is already
            // inside a Slip (e.g. from a Capture's positional list) should stay as-is.
            ValueView::Hash(_) => args.push(item.clone()),
            // A `(:key(val))` positional-pair slipped via `|(...)` flattens back
            // to a *named* argument in Raku (e.g. `.subst(|(:g), ...)` passes `:g`
            // as the named `:g` adverb). Mirror `append_slip_value`'s ValuePair
            // arm so the slipped pair is recognized as a named argument.
            ValueView::ValuePair(key, val) => {
                if let ValueView::Str(name) = key.view() {
                    args.push(Value::pair(name.to_string(), val.clone()));
                } else {
                    args.push(item.clone());
                }
            }
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => {
                args.extend(crate::runtime::utils::value_to_list(item));
            }
            _ => args.push(item.clone()),
        }
    }

    pub(super) fn append_flattened_call_arg(
        args: &mut Vec<Value>,
        arg: Value,
        preserve_empty_slip: bool,
    ) {
        match arg.view() {
            ValueView::Slip(items) => {
                if preserve_empty_slip && items.is_empty() {
                    args.push(Value::slip_arc(items.clone()));
                    return;
                }
                for item in items.iter() {
                    Self::append_slip_item(args, item);
                }
            }
            _ => args.push(arg),
        }
    }

    pub(super) fn preserve_empty_slip_arg(name: &str) -> bool {
        matches!(
            name,
            "infix:<andthen>"
                | "infix:<notandthen>"
                | "andthen"
                | "notandthen"
                | "__mutsu_andthen_finalize"
        )
    }

    pub(super) fn append_slip_value(args: &mut Vec<Value>, slip_val: Value) {
        match slip_val.view() {
            ValueView::Array(elements, ..) => {
                args.extend(elements.iter().cloned());
            }
            ValueView::Seq(elements)
            | ValueView::HyperSeq(elements)
            | ValueView::RaceSeq(elements) => {
                args.extend(elements.iter().cloned());
            }
            ValueView::Capture { positional, named } => {
                args.extend(positional.iter().cloned());
                for (k, v) in named.iter() {
                    args.push(Value::pair(k.clone(), v.clone()));
                }
            }
            ValueView::Slip(items) => {
                for item in items.iter() {
                    Self::append_slip_item(args, item);
                }
            }
            ValueView::Hash(map) => {
                for (k, v) in map.iter() {
                    args.push(Value::pair(k.clone(), v.clone()));
                }
            }
            // When a Pair or ValuePair is slipped via |, it becomes a named
            // argument (regular Pair).  ValuePair is the "positional pair"
            // wrapper produced by (:key(val)), but |$pair always flattens it
            // back to a named argument in Raku.
            ValueView::ValuePair(key, val) => {
                if let ValueView::Str(name) = key.view() {
                    args.push(Value::pair(name.to_string(), val.clone()));
                } else {
                    args.push(Value::value_pair(key.clone(), val.clone()));
                }
            }
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => {
                args.extend(crate::runtime::utils::value_to_list(&slip_val));
            }
            _ => {
                args.push(slip_val);
            }
        }
    }

    /// Auto-FETCH any Proxy values in function call arguments.
    pub(super) fn auto_fetch_proxy_args(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut out = Vec::with_capacity(args.len());
        for arg in args {
            out.push(loan_env!(self, auto_fetch_proxy(&arg))?);
        }
        Ok(out)
    }

    pub(super) fn decode_arg_sources(
        &mut self,
        code: &CompiledCode,
        arg_sources_idx: Option<u32>,
    ) -> Option<Vec<Option<String>>> {
        // §1.4/§1.5: repopulate the companion `name -> slot` map for this call from
        // the `Pair(name, Int(slot))` arg-source entries. Cleared first so a call
        // with no slotted sources leaves it empty (no stale slot from a prior call).
        self.pending_call_arg_source_slots.clear();
        let idx = arg_sources_idx?;
        let ValueView::Array(items, ..) = code.constants[idx as usize].view() else {
            return None;
        };
        let mut slots: Vec<(String, u32)> = Vec::new();
        let names: Vec<Option<String>> = items
            .iter()
            .map(|item| match item.view() {
                ValueView::Str(name) => Some(name.to_string()),
                // A slotted source is `Pair(name, Int(slot))`; extract the name here
                // (byte-identical for name-only consumers) and record the slot.
                ValueView::Pair(name, val) => {
                    if let ValueView::Int(slot) = val.view()
                        && slot >= 0
                    {
                        slots.push((name.clone(), slot as u32));
                    }
                    Some(name.clone())
                }
                _ => None,
            })
            .collect();
        for (name, slot) in slots {
            self.pending_call_arg_source_slots.insert(name, slot);
        }
        Some(names)
    }

    pub(super) fn unwrap_var_ref_value(value: Value) -> Value {
        match value.as_varref() {
            Some((_, inner, _)) => inner.clone(),
            None => value,
        }
    }

    pub(super) fn normalize_call_args_for_target(
        &mut self,
        name: &str,
        raw_args: Vec<Value>,
    ) -> Vec<Value> {
        let plain_args: Vec<Value> = raw_args
            .iter()
            .cloned()
            .map(Self::unwrap_var_ref_value)
            .collect();
        if self.has_declared_function(name) || self.has_multi_function(name) || self.has_proto(name)
        {
            raw_args
        } else {
            plain_args
        }
    }

    /// Check if an error is a "method not found" error (as opposed to a
    /// multi-dispatch failure or other runtime error). Used by .* to
    /// suppress method-not-found but propagate dispatch failures.
    pub(super) fn is_method_not_found_error(e: &RuntimeError) -> bool {
        e.is_method_not_found()
    }

    pub(super) fn rewrite_method_name(method_raw: &str, modifier: Option<&str>) -> String {
        match modifier {
            Some("^") => format!("^{}", method_raw),
            Some("!") => format!("!{}", method_raw),
            _ => method_raw.to_string(),
        }
    }

    pub(super) fn rewrite_method_name_cow<'a>(
        method_raw: &'a str,
        modifier: Option<&str>,
    ) -> std::borrow::Cow<'a, str> {
        match modifier {
            Some("^") => std::borrow::Cow::Owned(format!("^{}", method_raw)),
            Some("!") => std::borrow::Cow::Owned(format!("!{}", method_raw)),
            _ => std::borrow::Cow::Borrowed(method_raw),
        }
    }

    /// How many levels of a *built-in* type's MRO define `method`, for `.+`/`.*`
    /// all-candidates dispatch. mutsu implements a built-in method as one flat
    /// native handler, but Raku models it as a method object at each MRO level
    /// that defines it (e.g. `List.elems` AND `Any.elems`), so `<a b>.+elems`
    /// yields `(2, 2)` — one result per defining level, all from the same handler.
    /// Data-driven from the per-type method tables (`builtin_type_method_names`,
    /// the same lists `.^methods`/`.^can` use) intersected with the type's MRO —
    /// NOT a hard-coded per-method count. Returns 1 for a user `Instance` (its
    /// candidates come from `resolve_all_methods_with_owner`) or when only one
    /// level (or none — caller keeps the single native result) defines it.
    pub(crate) fn builtin_mro_method_candidate_count(
        &mut self,
        target: &Value,
        method: &str,
    ) -> usize {
        if matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Mixin(..)
        ) {
            return 1;
        }
        // Use the *introspective* type name (List vs Array distinguished by
        // ArrayKind), so `<a b>` (List) walks the List MRO, not Array's.
        let type_name = crate::runtime::value_type_name(target);
        let count = Self::builtin_type_mro_chain(type_name)
            .iter()
            .filter(|cn| {
                crate::builtins::builtin_type_methods::builtin_type_method_names(cn)
                    .contains(&method)
            })
            .count();
        count.max(1)
    }

    pub(super) fn call_method_all_with_fallback(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
        skip_native: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        if !skip_native
            && let Some(native_result) =
                self.try_native_method(target, Symbol::intern(method), args)
        {
            let result = native_result?;
            // `.+`/`.*` on a built-in: emit one result per MRO level that defines
            // the method (all identical — same native handler). §2 builtin-MRO
            // all-candidates dispatch (roast S03-metaops/hyper.t 407-408).
            let count = self.builtin_mro_method_candidate_count(target, method);
            return Ok(vec![result; count]);
        }
        loan_env!(
            self,
            call_method_all_with_values(target.clone(), method, args.to_vec())
        )
    }

    pub(super) fn call_method_mut_with_temp_target(
        &mut self,
        item: &Value,
        method: &str,
        args: Vec<Value>,
        slot: usize,
    ) -> Result<(Value, Value), RuntimeError> {
        let temp_name = format!("__mutsu_hyper_target_{slot}");
        self.env_mut().insert(temp_name.clone(), item.clone());
        // TODO: compile to bytecode — hyper method call over a temp-bound item (ledger §1).
        let result = self.vm_call_method_mut_with_values(&temp_name, item.clone(), method, args)?;
        let updated = self
            .env()
            .get(&temp_name)
            .cloned()
            .unwrap_or_else(|| item.clone());
        self.env_mut().remove(&temp_name);
        Ok((result, updated))
    }

    pub(super) fn call_method_all_with_temp_target(
        &mut self,
        item: &Value,
        method: &str,
        args: Vec<Value>,
        slot: usize,
    ) -> Result<(Vec<Value>, Value), RuntimeError> {
        let temp_name = format!("__mutsu_hyper_target_{slot}");
        self.env_mut().insert(temp_name.clone(), item.clone());
        let result = loan_env!(
            self,
            call_method_all_with_values(item.clone(), method, args)
        )?;
        let updated = self
            .env()
            .get(&temp_name)
            .cloned()
            .unwrap_or_else(|| item.clone());
        self.env_mut().remove(&temp_name);
        Ok((result, updated))
    }
}
