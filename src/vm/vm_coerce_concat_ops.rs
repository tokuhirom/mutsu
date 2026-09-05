//! Coercion, slip, boolean, string concatenation, and Buf-value ops.
use super::*;
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;

impl Interpreter {
    pub(super) fn exec_decont_op(&mut self) {
        // Strips a SINGLE level of `Scalar` itemization for slurpy flattening.
        // This is intentionally non-recursive and distinct from the recursive
        // Value::descalarize (see the decont family note in value/mod.rs §3).
        let val = self.stack.pop().unwrap();
        let new_val = match val.view() {
            ValueView::Scalar(inner) => inner.clone(),
            _ => val,
        };
        self.stack.push(new_val);
    }

    /// Snapshot a list's elements to plain VALUES (see `OpCode::DecontListElems`).
    /// Reads each element through its `ContainerRef` cell and descalarizes it, so
    /// a list-assignment RHS is fully decontainerized into a value buffer before
    /// any LHS container is written.
    pub(super) fn exec_decont_list_elems_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let items = crate::runtime::value_to_list(&val)
            .into_iter()
            .map(|e| e.into_deref().into_descalarized())
            .collect::<Vec<_>>();
        self.stack.push(Value::real_array(items));
    }

    pub(super) fn exec_make_slip_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // Slipping (`|EXPR`) always flattens through containers/itemization, e.g.
        // `|$_` where the topic is an itemized Seq element must expand the Seq's
        // values (`($seq,).map(|*)`), not wrap the Seq as a single slip item.
        let val = val.into_deref().into_descalarized();
        // A deferred Seq (`Seq.new($iterator)`, `IO::Handle.lines`) must first
        // pull all elements from its source (ADR-0034), else `|$seq` yields
        // nothing. `|EXPR` steals the source like `.iterator`/`.list` (a
        // second `|$seq` on the same Seq must not silently re-slip nothing).
        if let ValueView::Seq(body) = val.view()
            && body.needs_touch()
        {
            let body = Arc::clone(&body);
            let (pulled, _) = self.take_seq_body(&body)?;
            self.stack.push(Value::slip(pulled));
            return Ok(());
        }
        // A `gather`-sourced `LazyList` slipped with `|` must run its body: a
        // plain `gather` is FORCED now (side effects included) and yields its
        // taken values; the `match` arm below only read the still-empty cache, so
        // `|(gather { $x++; take … })` slipped nothing and never ran `$x++`. An
        // explicitly-`lazy` gather (`|(lazy gather …)`) stays lazy so its side
        // effects fire only on later reification (`@a.eager`) — pushing the
        // `LazyList` value itself preserves that deferred tail. Non-gather lazy
        // lists (infinite `scan_spec`/`sequence_spec` reductions like
        // `|[\+] 1..*`) fall through to the `match` arm's bounded force.
        if let ValueView::LazyList(ll) = val.view()
            && ll.is_from_gather()
        {
            if ll.is_genuinely_lazy() {
                self.stack.push(Value::slip(vec![val.clone()]));
            } else {
                let pulled = self.force_lazy_list_vm(&ll)?;
                self.stack.push(Value::slip(pulled));
            }
            return Ok(());
        }
        let items = match val.view() {
            // ADR-0021 I4: `|@l` / `|$list` produce POSITIONAL arguments even
            // when an element happens to be a Pair (e.g. a literal `x => 1`
            // sitting in an array, which mints the named flavour today absent
            // an argument-position boundary to erase it). Containerize each
            // element here, at the one place that knows these came from a
            // positional container, rather than trying to recover that
            // context later in `append_slip_item`.
            ValueView::Array(items, ..) => items
                .iter()
                .cloned()
                .map(Self::containerize_pair_item)
                .collect(),
            // A nested Slip's items were already finalized by the
            // `exec_make_slip_op` call that built it (positional-source
            // elements containerized, a bare-Pair/Hash source promoted to
            // named) — re-processing here would re-flip an already-correct
            // named item back to positional. Pass through unchanged.
            ValueView::Slip(items) => (*items).to_vec(),
            ValueView::Seq(items) => items
                .iter()
                .cloned()
                .map(Self::containerize_pair_item)
                .collect(),
            ValueView::Capture { positional, named } => {
                // I5: a Capture's lanes are already classified by the call
                // site that built it; replay them verbatim by lane rather
                // than reclassifying by value flavour (positional stays
                // positional, named stays named).
                let mut items: Vec<Value> = positional
                    .iter()
                    .cloned()
                    .map(Self::containerize_pair_item)
                    .collect();
                for (k, v) in named.iter() {
                    items.push(Value::pair(k.clone(), v.clone()));
                }
                items
            }
            // typed_pair decodes an object hash's `.WHICH` store keys back to
            // the original key objects (plain hashes get `Pair(str_key, v)`).
            // I4: `|%h` is always named, so promote every entry here rather
            // than leaving it to `append_slip_item` to guess.
            ValueView::Hash(map) => map
                .iter()
                .map(|(k, v)| Self::namify_pair_item(map.typed_pair(k, v.clone())))
                .collect(),
            ValueView::LazyList(ll) => {
                let items = if ll.scan_spec.is_some() {
                    ll.force_scan_to(200_000)
                } else {
                    ll.cache.lock().unwrap().clone().unwrap_or_default()
                };
                items
                    .into_iter()
                    .map(Self::containerize_pair_item)
                    .collect()
            }
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => crate::runtime::utils::value_to_list(&val),
            // A `Buf`/`Blob` is Positional, so `|$buf` slips its ELEMENTS, at the
            // buffer's own width — `|blob32.new(7, 8)` is `slip(7, 8)`, not a
            // one-item slip holding the buffer. `Digest::RIPEMD` ends with
            // `map |*.polymod(256 xx 3), |$reduced_blob32`, which fed the
            // WhateverCode the whole Blob and digested a numified 0.
            // A type object (`|Buf`) carries no element storage and stays one
            // item, as in Rakudo.
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
                && crate::value::value_buf::has_buf_elems(&attributes) =>
            {
                crate::value::value_buf::buf_elems_or_empty(&attributes)
            }
            // Slipping a bare value (`|$pair`, `|Pair.new(...)`) always
            // produces a NAMED argument regardless of the Pair's own stored
            // flavour (I4) — this is the one case where the value itself,
            // not a container it lives in, decides named-ness by being
            // slipped directly.
            _ => vec![Self::namify_pair_item(val)],
        };
        self.stack.push(Value::slip(items));
        Ok(())
    }

    pub(super) fn exec_not_op(&mut self) {
        let val = self.stack.pop().unwrap();
        // Boolifying a Failure marks it as handled
        val.mark_failure_handled();
        let t = self.eval_truthy(&val);
        self.stack.push(Value::truth(!t));
    }

    pub(super) fn exec_bool_coerce_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let out = match val.view() {
            ValueView::Regex(_)
            | ValueView::RegexWithAdverbs { .. }
            | ValueView::Routine { is_regex: true, .. } => {
                let topic = self.env().get("_").cloned().unwrap_or(Value::NIL);
                // The IMPLICIT topic of a bare regex coerces quietly -- see
                // `quiet_topic_for_regex_match`.
                let topic = self.quiet_topic_for_regex_match(topic);
                Value::truth(self.vm_smart_match(&topic, &val))
            }
            _ => {
                // Boolifying a Failure marks it as handled
                val.mark_failure_handled();
                Value::truth(self.eval_truthy(&val))
            }
        };
        self.stack.push(out);
    }

    pub(super) fn exec_concat_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Thread over junctions — concat uses left-first threading
        // (unlike arithmetic/comparison which uses right-first for tighter
        // junctions). When both operands are junctions and the right is
        // tighter, we thread left first and swap the junction kinds.
        if matches!(left.view(), ValueView::Junction { .. })
            || matches!(right.view(), ValueView::Junction { .. })
        {
            let result = self.eval_concat_with_junctions(left, right);
            self.stack.push(result);
            return Ok(());
        }
        // Infix `~` stringifies an operand via `.Stringy` (falling back to `.Str`),
        // so an operand whose class defines a user `Stringy`/`Str` must dispatch it
        // — the pure `concat_values` only knows `.gist` (rendering `Foo()`). This is
        // an internal redispatch with no surrounding CallMethod op, so drain any
        // captured-outer writeback into the caller's slot (Slice 1b render pattern).
        let caller_code = self.current_code;
        let left = self.coerce_stringy_operand(left)?;
        let right = self.coerce_stringy_operand(right)?;
        self.reconcile_caller_after_internal_dispatch(caller_code);
        let result = Self::concat_values(left, right);
        self.stack.push(result);
        Ok(())
    }

    /// Coerce an operand whose class defines a user `Stringy`/`Str` to its
    /// string value (Raku infix `~` uses `.Stringy`, falling back to `.Str`;
    /// the string comparators `eq`/`lt`/… use `.Str`). Plain values and
    /// instances without a user stringifier pass through unchanged (the pure
    /// `concat_values` / `to_str_context` handle those, including built-in
    /// `.gist`/`.Str`). Shared by infix `~` and the string-comparison ops.
    pub(crate) fn coerce_stringy_operand(&mut self, v: Value) -> Result<Value, RuntimeError> {
        // A string context is a READ, so a `Proxy` operand FETCHes: `"x" ~ $p`
        // is `x5`, not `xProxy`. Every other value context already FETCHed
        // (arithmetic via `eval_binary_with_junctions`, `say`/`print`/`note`,
        // method dispatch, coercion); `~` and the string comparators were the
        // hole. Tag-probed: this coercion runs on every `~`/`eq` operand, so the
        // common non-`Proxy` case must not even clone the value.
        //
        // Top-level only, deliberately: a `Proxy` nested inside a rendered
        // container is resolved further down, by the same
        // `list_str_needs_interpreter` scan that already runs here for an
        // `Instance` element (ADR-0040 §9.2), so it costs no second traversal.
        let v = if v.is_proxy_value() {
            self.auto_fetch_proxy(&v)?
        } else {
            v
        };
        // Unhandled Failure throws in string context (infix `~`, `eq`/…),
        // matching Rakudo: `(sub { ... }).() ~ ""` dies with X::StubCode.
        if let Some(err) = self.failure_to_runtime_error_if_unhandled(&v) {
            return Err(err);
        }
        // A Nil operand in a string context (infix `~`, `eq`/`lt`/… string
        // comparisons) warns and resumes with the empty string, matching
        // Rakudo — once per Nil operand (so `Nil ~ Nil` warns twice).
        if v.is_nil() {
            return self.raise_resumable_warning(
                "Use of Nil in string context",
                Value::str(String::new()),
            );
        }
        // A `Seq` whose source has not been pulled yet (an
        // `IO::Handle.lines`/`.words` read, or `Seq.new($iterator)`) reaches a
        // string context through THIS operand coercion, not through method
        // dispatch, so the `.Str` reify guard never ran and the pure
        // stringifier fell back to the opaque `(...)` placeholder
        // (`value/display.rs`) or to an empty join over a still-unfilled body.
        // Route it through the very guard `.Str` itself uses: `"Str"` is not a
        // `seq_method_consumes` entry, so this REIFIES (marking the body
        // retained) without consuming it — matching rakudo's
        // `multi method Str(Seq:D:) { self.cache.Str }`, where `~$s; ~$s` both
        // answer the elements and a later `.List` still works.
        // Tag-probed (`is_seq_value`): this coercion also runs on every `~`/`eq`
        // operand in grammar-action code, where an unconditional `view()` would
        // materialize a lazy Match (see
        // `tests/lazy_match_no_eager_materialization.rs`).
        if v.is_seq_value()
            && let ValueView::Seq(body) = v.view()
            && body.needs_touch()
        {
            return self.reify_or_consume_seq_target(v, "Str");
        }
        // A role-mixed value is NOT an `Instance` view, so it used to fall
        // straight through to the `_` arm below and lose its composed
        // `Stringy`/`Str` (see `mixin_user_stringifier`).
        if let Some(r) = self.mixin_user_stringifier(&v) {
            return Ok(Value::str(r?.to_string_value()));
        }
        // A list element that is an Instance may define its own `Str`, which
        // the pure stringifier the caller falls back to cannot call -- resolve
        // those first, the same way `.Str` / prefix `~` / interpolation do
        // (`runtime/list_element_stringify.rs`). `is @list, 'text'` in the
        // vendored Test module lands here through infix `eq`.
        if Self::list_str_needs_interpreter(&v) {
            return self.resolve_list_element_stringifiers(&v);
        }
        let (cn, is_type_object) = match v.view() {
            ValueView::Instance { class_name, .. } => (class_name.resolve().to_string(), false),
            ValueView::Package(name) => (name.resolve().to_string(), true),
            _ => return Ok(v),
        };
        if self.has_user_method(&cn, "Stringy") {
            let r = self.try_compiled_method_or_interpret(v, "Stringy", Vec::new())?;
            return Ok(Value::str(r.to_string_value()));
        }
        if self.has_user_method(&cn, "Str") {
            let r = self.try_compiled_method_or_interpret(v, "Str", Vec::new())?;
            return Ok(Value::str(r.to_string_value()));
        }
        // A bare type object without a user stringifier warns and resumes with
        // the empty string, matching Rakudo (`"a" ~ Int`, `Int eq "x"`). An
        // Instance without one passes through to the pure stringifier unchanged.
        if is_type_object {
            return self.warn_type_object_string_context(&cn, false);
        }
        Ok(v)
    }

    fn eval_concat_with_junctions(&mut self, left: Value, right: Value) -> Value {
        // Auto-FETCH and decontainerize
        let left = self
            .auto_fetch_proxy(&left)
            .unwrap_or(left)
            .descalarize()
            .clone();
        let right = self
            .auto_fetch_proxy(&right)
            .unwrap_or(right)
            .descalarize()
            .clone();
        // Both junctions: thread left first, swap kinds if right is tighter
        if let (ValueView::Junction { kind: lk, .. }, ValueView::Junction { kind: rk, .. }) =
            (left.view(), right.view())
        {
            let need_swap = Self::thread_right_first(&lk, &rk);
            if let ValueView::Junction { kind, values } = left.view() {
                let values = values.clone();
                let results: Vec<Value> = values
                    .iter()
                    .cloned()
                    .map(|v| self.eval_concat_with_junctions(v, right.clone()))
                    .collect();
                let mut result = Value::junction(kind, results);
                if need_swap {
                    result = Self::swap_junction_kinds(result, &rk, &lk);
                }
                return result;
            }
        }
        if let ValueView::Junction { kind, values } = left.view() {
            let values = values.clone();
            let results: Vec<Value> = values
                .iter()
                .cloned()
                .map(|v| self.eval_concat_with_junctions(v, right.clone()))
                .collect();
            return Value::junction(kind, results);
        }
        if let ValueView::Junction { kind, values } = right.view() {
            let values = values.clone();
            let results: Vec<Value> = values
                .iter()
                .cloned()
                .map(|v| self.eval_concat_with_junctions(left.clone(), v))
                .collect();
            return Value::junction(kind, results);
        }
        Self::concat_values(left, right)
    }

    fn swap_junction_kinds(
        value: Value,
        new_outer: &crate::value::JunctionKind,
        new_inner: &crate::value::JunctionKind,
    ) -> Value {
        if let ValueView::Junction { values, .. } = value.view() {
            let swapped: Vec<Value> = values
                .iter()
                .map(|v| {
                    if let ValueView::Junction { values: inner, .. } = v.view() {
                        Value::junction(*new_inner, inner.to_vec())
                    } else {
                        v.clone()
                    }
                })
                .collect();
            Value::junction(*new_outer, swapped)
        } else {
            value
        }
    }

    /// String/Buf concatenation (`~`). This is the single authoritative impl,
    /// shared by the Interpreter's `~` op and the interpreter's reduction-operator path
    /// (`apply_reduction_op` delegates here). It uses no Interpreter state, so it is a
    /// plain associated function callable as `crate::runtime::Interpreter::concat_values(...)`.
    pub(crate) fn concat_values(left: Value, right: Value) -> Value {
        // Buf ~ Buf → byte concatenation. Rakudo types the result by whether the
        // two operands have the *same* type: `Blob[uint8] ~ Blob[uint8]` stays
        // `Blob[uint8]` and `utf8 ~ utf8` stays `utf8`, but any mismatch widens to
        // the plain mutable `Buf` — so `Blob[uint8] ~ Buf[uint8]` is a `Buf`, which
        // is what lets `my Buf $x = $blob-typed-var` type-check after an append
        // (HTTP::UserAgent accumulates `Blob[uint8] ~= <recv Buf>` and then binds
        // the result to a `Buf`). Preserving the LHS type instead kept it a Blob.
        if Self::is_buf_value(&left) && Self::is_buf_value(&right) {
            let left_class = match left.view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                _ => None,
            };
            let right_class = match right.view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                _ => None,
            };
            let result_class = match (left_class, right_class) {
                (Some(l), Some(r)) if l == r => l,
                _ => crate::symbol::Symbol::intern("Buf"),
            };
            let mut bytes = Self::extract_buf_bytes(&left);
            bytes.extend(Self::extract_buf_bytes(&right));
            return crate::value::value_buf::make_buf_from_bytes(result_class, &bytes);
        }
        // Buf ~ non-Buf or non-Buf ~ Buf: decode the Buf and produce a Str
        if Self::is_buf_value(&left) || Self::is_buf_value(&right) {
            let left_str = if Self::is_buf_value(&left) {
                let bytes = Self::extract_buf_bytes(&left);
                String::from_utf8_lossy(&bytes).into_owned()
            } else {
                crate::runtime::utils::coerce_to_str(&left)
            };
            let right_str = if Self::is_buf_value(&right) {
                let bytes = Self::extract_buf_bytes(&right);
                String::from_utf8_lossy(&bytes).into_owned()
            } else {
                crate::runtime::utils::coerce_to_str(&right)
            };
            let concatenated = format!("{}{}", left_str, right_str);
            if concatenated.is_ascii() {
                return Value::str(concatenated);
            }
            let normalized: String = concatenated.nfc().collect();
            return Value::str(normalized);
        }
        let concatenated = format!(
            "{}{}",
            crate::runtime::utils::coerce_to_str(&left),
            crate::runtime::utils::coerce_to_str(&right)
        );
        if concatenated.is_ascii() {
            Value::str(concatenated)
        } else {
            let normalized: String = concatenated.nfc().collect();
            Value::str(normalized)
        }
    }

    pub fn is_buf_value(val: &Value) -> bool {
        if let ValueView::Instance { class_name, .. } = val.view() {
            crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        } else {
            false
        }
    }

    pub(crate) fn buf_class_name(val: &Value) -> Option<String> {
        match val.view() {
            ValueView::Instance { class_name, .. }
                if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) =>
            {
                Some(class_name.resolve().to_string())
            }
            _ => None,
        }
    }

    pub(crate) fn buf_as_str_error(val: &Value, method: &str) -> RuntimeError {
        let class_name = Self::buf_class_name(val).unwrap_or_else(|| "Blob".to_string());
        let mut err = RuntimeError::new(format!(
            "Cannot use a {class_name} as a Str. You can use .decode to convert to Str."
        ));
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("method".to_string(), Value::str(method.to_string()));
        attrs.insert("payload".to_string(), val.clone());
        err.exception = Some(Box::new(Value::make_instance(
            crate::symbol::Symbol::intern("X::Buf::AsStr"),
            attrs,
        )));
        err
    }

    pub fn extract_buf_bytes(val: &Value) -> Vec<u8> {
        if let ValueView::Instance { attributes, .. } = val.view() {
            return crate::value::value_buf::buf_bytes_or_empty(&attributes);
        }
        Vec::new()
    }
}
