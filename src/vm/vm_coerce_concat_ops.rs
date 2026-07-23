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

    /// Pull every element from a deferred-iterator `Seq` (as stored by
    /// `Seq.new($iterator)`), driving the iterator's `pull-one` until
    /// `IterationEnd`. Works for both built-in and user-defined `Iterator`
    /// classes. The deferred iterator is removed and the Seq is marked consumed.
    pub(crate) fn materialize_deferred_seq(&mut self, items_arc: &Arc<Vec<Value>>) -> Vec<Value> {
        let Some(iterator) = crate::value::seq_take_deferred_iter(items_arc) else {
            return (**items_arc).clone();
        };
        crate::value::seq_consume(items_arc).ok();
        let mut pulled = Vec::new();
        while let Ok(val) = self.call_method_with_values(iterator.clone(), "pull-one", vec![]) {
            if matches!(val.view(), ValueView::Str(s) if s.as_str() == "IterationEnd")
                || matches!(val.view(), ValueView::Package(name) if name == crate::symbol::Symbol::intern("IterationEnd"))
            {
                break;
            }
            pulled.push(val);
        }
        pulled
    }

    pub(super) fn exec_make_slip_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // Slipping (`|EXPR`) always flattens through containers/itemization, e.g.
        // `|$_` where the topic is an itemized Seq element must expand the Seq's
        // values (`($seq,).map(|*)`), not wrap the Seq as a single slip item.
        let val = val.into_deref().into_descalarized();
        // A `Seq.new($iterator)` stores its iterator deferred (empty backing vec).
        // Slipping such a Seq must first pull all elements from the iterator
        // (including user-defined `Iterator` classes), else `|$seq` yields nothing.
        if let ValueView::Seq(items_arc) = val.view()
            && crate::value::seq_has_deferred_iter(&items_arc)
        {
            let pulled = self.materialize_deferred_seq(&items_arc);
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
            ValueView::Array(items, ..) => (*items).to_vec(),
            ValueView::Slip(items) => (*items).to_vec(),
            ValueView::Seq(items) => (*items).to_vec(),
            ValueView::Capture { positional, named } => {
                let mut items = positional.clone();
                for (k, v) in named.iter() {
                    items.push(Value::pair(k.clone(), v.clone()));
                }
                items
            }
            // typed_pair decodes an object hash's `.WHICH` store keys back to
            // the original key objects (plain hashes get `Pair(str_key, v)`).
            ValueView::Hash(map) => map
                .iter()
                .map(|(k, v)| map.typed_pair(k, v.clone()))
                .collect(),
            ValueView::LazyList(ll) => {
                if ll.scan_spec.is_some() {
                    ll.force_scan_to(200_000)
                } else {
                    ll.cache.lock().unwrap().clone().unwrap_or_default()
                }
            }
            ValueView::LazyIoLines { .. } => match self.force_if_lazy_io_lines(val) {
                Ok(forced) => crate::runtime::utils::value_to_list(&forced),
                Err(_) => vec![],
            },
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => crate::runtime::utils::value_to_list(&val),
            _ => vec![val],
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
    pub(super) fn coerce_stringy_operand(&mut self, v: Value) -> Result<Value, RuntimeError> {
        // A Nil operand in a string context (infix `~`, `eq`/`lt`/… string
        // comparisons) warns and resumes with the empty string, matching
        // Rakudo — once per Nil operand (so `Nil ~ Nil` warns twice).
        if v.is_nil() {
            return self.raise_resumable_warning(
                "Use of Nil in string context",
                Value::str(String::new()),
            );
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
        // Buf ~ Buf → Buf (byte concatenation, preserving LHS type)
        if Self::is_buf_value(&left) && Self::is_buf_value(&right) {
            let result_class = if let ValueView::Instance { class_name, .. } = left.view() {
                class_name
            } else {
                crate::symbol::Symbol::intern("Buf")
            };
            let mut bytes = Self::extract_buf_bytes(&left);
            bytes.extend(Self::extract_buf_bytes(&right));
            let byte_vals: Vec<Value> = bytes.into_iter().map(|b| Value::int(b as i64)).collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(byte_vals));
            return Value::make_instance(result_class, attrs);
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
            let cn = class_name.resolve();
            cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
                || cn.starts_with("buf")
                || cn.starts_with("blob")
        } else {
            false
        }
    }

    pub fn extract_buf_bytes(val: &Value) -> Vec<u8> {
        if let ValueView::Instance { attributes, .. } = val.view()
            && let Some(ValueView::Array(items, ..)) =
                attributes.as_map().get("bytes").map(Value::view)
        {
            return items
                .iter()
                .map(|v| match v.view() {
                    ValueView::Int(i) => i as u8,
                    _ => 0,
                })
                .collect();
        }
        Vec::new()
    }
}
