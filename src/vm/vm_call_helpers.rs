use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Convert a named-flavour Pair into the positional flavour; a no-op for
    /// everything else (ADR-0021 I1/I4). The same normalization
    /// `OpCode::ContainerizePair` performs at a compiled call boundary,
    /// needed here too because a Slip's elements bypass the compiler
    /// entirely — `exec_make_slip_op` applies this to every element sourced
    /// from a genuinely positional container (Array/Seq/LazyList/Capture's
    /// positional lane).
    pub(super) fn containerize_pair_item(item: Value) -> Value {
        match item.view() {
            ValueView::Pair(k, v) => Value::value_pair(Value::str(k.clone()), v.clone()),
            _ => item,
        }
    }

    /// Convert a positional-flavour Pair (`Str` key only — a named argument
    /// always has a string name) into the named flavour; a no-op for
    /// everything else. The reverse of `containerize_pair_item`, applied at
    /// the two places ADR-0021 I4 mints a named argument from a slip: a bare
    /// `|$pair` and each `|%h` entry.
    pub(super) fn namify_pair_item(item: Value) -> Value {
        match item.view() {
            ValueView::ValuePair(key, val) => match key.view() {
                ValueView::Str(name) => Value::pair(name.to_string(), val.clone()),
                _ => item,
            },
            _ => item,
        }
    }

    pub(super) fn append_slip_item(args: &mut Vec<Value>, item: &Value) {
        match item.view() {
            ValueView::Capture { positional, named } => {
                // I5: containerize on append rather than trusting the stored
                // flavour — the positional lane must stay positional even if
                // an element was minted with the named flavour upstream.
                args.extend(positional.iter().cloned().map(Self::containerize_pair_item));
                for (k, v) in named.iter() {
                    args.push(Value::pair(k.clone(), v.clone()));
                }
            }
            // Hash values inside a Slip are kept as single positional args.
            // Top-level `|%hash` flattening is handled by MakeSlip, which converts
            // a bare Hash into pairs before wrapping in a Slip. A Hash that is already
            // inside a Slip (e.g. from a Capture's positional list) should stay as-is.
            ValueView::Hash(_) => args.push(item.clone()),
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => {
                args.extend(crate::runtime::utils::value_to_list(item));
            }
            // Every other item's flavour was already finalized by
            // `exec_make_slip_op` when this Slip was built (positional
            // sources containerized, a bare Pair/Hash source promoted to
            // named) — trust it rather than reclassifying by value shape.
            _ => args.push(item.clone()),
        }
    }

    /// Does this call site carry a `|EXPR` argument-list interpolation?
    ///
    /// ADR-0054 Slice 4: decided once from the compile-time descriptor
    /// (`decode_arg_slip_positions`), not by probing the stack for
    /// Slip-SHAPED values — a plain argument that merely evaluates to a
    /// Slip (`f(@a.Slip)`) must stay eligible for the light-call / OTF
    /// caches below. The light-call / OTF caches bind the *compiled* arity
    /// directly against the stack, but a `|EXPR` argument spreads into the
    /// argument list and so changes that arity. Spreading lives on the slow
    /// dispatch path (`spread_call_args_by_syntax`), so a call carrying one
    /// must skip those caches.
    pub(super) fn stack_args_have_slip(code: &CompiledCode, arg_sources_idx: Option<u32>) -> bool {
        Self::decode_arg_slip_positions(code, arg_sources_idx).is_some()
    }

    /// Spread a call's raw arguments by call-site syntax (ADR-0054 S1/S2),
    /// not by a value's runtime Slip-shape: only a position the compiler
    /// recorded as `|EXPR` (`decode_arg_slip_positions`) spreads. Every other
    /// argument -- including one that merely evaluated to a Slip
    /// (`f(@a.Slip)`) -- stays exactly one argument, matching Raku (a `Slip`
    /// is an ordinary `List` subtype, not a request to spread). This
    /// replaces the old runtime-value-shape inference (a Slip flattened
    /// unconditionally, regardless of source syntax), which could not
    /// distinguish `f(|@a)` from `f(@a.Slip)`.
    ///
    /// `decoded_sources` (from `decode_arg_sources`, over the SAME
    /// pre-flatten positions as `arg_sources_idx`) is expanded in lockstep
    /// with the returned args, so the two never desync: a spread position
    /// has no single traceable rw source, so every runtime argument it
    /// expands into gets `None` there.
    ///
    /// ADR-0054 Slice 4: this is the ONE mechanism every call op uses --
    /// `ExecCallPairs` (`exec_exec_call_pairs_op`) collapsed its dedicated
    /// `slip_positions_idx` constant into the same `arg_sources_idx`
    /// descriptor and calls this too (passing `None` for `decoded_sources`,
    /// since it never tracked rw-arg sources), which is why the
    /// per-position flattening below now has exactly one call site and is
    /// inlined rather than factored into a separate
    /// `append_flattened_call_arg` helper.
    pub(super) fn spread_call_args_by_syntax(
        code: &CompiledCode,
        raw_args: Vec<Value>,
        arg_sources_idx: Option<u32>,
        decoded_sources: Option<Vec<Option<String>>>,
    ) -> (Vec<Value>, Option<Vec<Option<String>>>) {
        let Some(slip_at) = Self::decode_arg_slip_positions(code, arg_sources_idx) else {
            // No `|` argument recorded at this call site: nothing spreads.
            return (raw_args, decoded_sources);
        };
        let mut args = Vec::with_capacity(raw_args.len());
        let mut sources: Vec<Option<String>> = Vec::with_capacity(raw_args.len());
        let mut has_source = false;
        for (i, arg) in raw_args.into_iter().enumerate() {
            if slip_at.contains(&i) {
                let before = args.len();
                match arg.view() {
                    ValueView::Slip(items) => {
                        for item in items.iter() {
                            Self::append_slip_item(&mut args, item);
                        }
                    }
                    _ => args.push(arg),
                }
                sources.extend(std::iter::repeat_n(None, args.len() - before));
            } else {
                let name = decoded_sources
                    .as_ref()
                    .and_then(|s| s.get(i))
                    .cloned()
                    .flatten();
                has_source |= name.is_some();
                args.push(arg);
                sources.push(name);
            }
        }
        (args, if has_source { Some(sources) } else { None })
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

    /// Decode the per-argument-position rw-source names baked by
    /// `add_arg_sources_constant`. A `|EXPR` position (ADR-0054) is encoded
    /// there as `Value::TRUE` rather than a name -- it falls through the
    /// match below to `None` here, same as `NIL`; use
    /// [`Self::decode_arg_slip_positions`] to recover which positions those
    /// are.
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

    /// Positions this call wrote as `|EXPR` (ADR-0054 S1/S2), decoded from the
    /// same descriptor array `decode_arg_sources` reads (a `|` position is
    /// encoded there as `Value::TRUE`, distinct from `NIL` / `Str(name)` /
    /// `Pair(name, Int(slot))`). Positions are pre-flatten: they index the
    /// compiled argument list, i.e. exactly the arity the call op carries,
    /// before any position's value is spread into zero or more runtime
    /// arguments. `None` when there is no descriptor or no `|` argument.
    pub(super) fn decode_arg_slip_positions(
        code: &CompiledCode,
        arg_sources_idx: Option<u32>,
    ) -> Option<Vec<usize>> {
        let idx = arg_sources_idx?;
        let ValueView::Array(items, ..) = code.constants[idx as usize].view() else {
            return None;
        };
        let positions: Vec<usize> = items
            .iter()
            .enumerate()
            .filter(|(_, item)| matches!(item.view(), ValueView::Bool(true)))
            .map(|(i, _)| i)
            .collect();
        if positions.is_empty() {
            None
        } else {
            Some(positions)
        }
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
        // `fn_base_name_registered` is the cheap negative gate (#5574): when no
        // registry key carries this base name, `has_declared_function` and
        // `has_multi_function` (a full functions-map scan) cannot match, so a
        // builtin like `make` skips both. `has_proto` reads a separate map and
        // stays unguarded.
        if (self.fn_base_name_registered(name)
            && (self.has_declared_function_cached(name) || self.has_multi_function_cached(name)))
            || self.has_proto_cached(name)
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
        // ADR-0019 E1b: authoritative TypeId classifier chain (was
        // `value_type_name` + the `builtin_type_mro_chain` divergent MRO
        // table) — see `Interpreter::dispatch_mro` and
        // `todo/deep/adr0019-e1-typeid-receiver-owner.md`. `target` is
        // guaranteed non-Instance/non-Mixin by the early return above, so the
        // role-skip `dispatch_owner_chain` variant is unnecessary here — the
        // classifier's own chain (which still distinguishes List vs Array by
        // `ArrayKind`, same as the old `value_type_name`) is exactly right.
        let chain = self.dispatch_mro(target);
        let count = chain
            .iter()
            .filter(|t| {
                crate::builtins::builtin_type_methods::builtin_type_method_names(t.as_str())
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
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodallfallback", "native");
            let result = native_result?;
            // `.+`/`.*` on a built-in: emit one result per MRO level that defines
            // the method (all identical — same native handler). §2 builtin-MRO
            // all-candidates dispatch (roast S03-metaops/hyper.t 407-408).
            let count = self.builtin_mro_method_candidate_count(target, method);
            return Ok(vec![result; count]);
        }
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodallfallback", "user");
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
