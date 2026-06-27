//! Coercion, slip, boolean, string concatenation, and Buf-value ops.
use super::*;
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;

impl Interpreter {
    pub(super) fn exec_decont_op(&mut self) {
        // Strips a SINGLE level of Value::Scalar for slurpy flattening. This is
        // intentionally non-recursive and distinct from the recursive
        // Value::descalarize (see the decont family note in value/mod.rs §3).
        let val = self.stack.pop().unwrap();
        let new_val = match val {
            Value::Scalar(inner) => *inner,
            other => other,
        };
        self.stack.push(new_val);
    }

    /// Pull every element from a deferred-iterator `Seq` (as stored by
    /// `Seq.new($iterator)`), driving the iterator's `pull-one` until
    /// `IterationEnd`. Works for both built-in and user-defined `Iterator`
    /// classes. The deferred iterator is removed and the Seq is marked consumed.
    pub(super) fn materialize_deferred_seq(&mut self, items_arc: &Arc<Vec<Value>>) -> Vec<Value> {
        let Some(iterator) = crate::value::seq_take_deferred_iter(items_arc) else {
            return (**items_arc).clone();
        };
        crate::value::seq_consume(items_arc).ok();
        let mut pulled = Vec::new();
        while let Ok(val) = self.call_method_with_values(iterator.clone(), "pull-one", vec![]) {
            if matches!(&val, Value::Str(s) if s.as_str() == "IterationEnd")
                || matches!(&val, Value::Package(name) if *name == crate::symbol::Symbol::intern("IterationEnd"))
            {
                break;
            }
            pulled.push(val);
        }
        pulled
    }

    pub(super) fn exec_make_slip_op(&mut self) {
        let val = self.stack.pop().unwrap();
        // Slipping (`|EXPR`) always flattens through containers/itemization, e.g.
        // `|$_` where the topic is an itemized Seq element must expand the Seq's
        // values (`($seq,).map(|*)`), not wrap the Seq as a single slip item.
        let val = val.into_deref().into_descalarized();
        // A `Seq.new($iterator)` stores its iterator deferred (empty backing vec).
        // Slipping such a Seq must first pull all elements from the iterator
        // (including user-defined `Iterator` classes), else `|$seq` yields nothing.
        if let Value::Seq(items_arc) = &val
            && crate::value::seq_has_deferred_iter(items_arc)
        {
            let pulled = self.materialize_deferred_seq(items_arc);
            self.stack.push(Value::slip(pulled));
            return;
        }
        let items = match &val {
            Value::Array(items, ..) => (*items).to_vec(),
            Value::Slip(items) => (*items).to_vec(),
            Value::Seq(items) => (*items).to_vec(),
            Value::Capture { positional, named } => {
                let mut items = (**positional).clone();
                for (k, v) in named.iter() {
                    items.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
                items
            }
            Value::Hash(map) => map
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .collect(),
            Value::LazyList(ll) => {
                if ll.scan_spec.is_some() {
                    ll.force_scan_to(200_000)
                } else {
                    ll.cache.lock().unwrap().clone().unwrap_or_default()
                }
            }
            Value::LazyIoLines { .. } => match self.force_if_lazy_io_lines(val) {
                Ok(forced) => crate::runtime::utils::value_to_list(&forced),
                Err(_) => vec![],
            },
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => crate::runtime::utils::value_to_list(&val),
            _ => vec![val],
        };
        self.stack.push(Value::slip(items));
    }

    pub(super) fn exec_not_op(&mut self) {
        let val = self.stack.pop().unwrap();
        // Boolifying a Failure marks it as handled
        val.mark_failure_handled();
        let t = self.eval_truthy(&val);
        self.stack.push(Value::Bool(!t));
    }

    pub(super) fn exec_bool_coerce_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let out = match &val {
            Value::Regex(_)
            | Value::RegexWithAdverbs { .. }
            | Value::Routine { is_regex: true, .. } => {
                let topic = self.env().get("_").cloned().unwrap_or(Value::Nil);
                Value::Bool(self.vm_smart_match(&topic, &val))
            }
            _ => {
                // Boolifying a Failure marks it as handled
                val.mark_failure_handled();
                Value::Bool(self.eval_truthy(&val))
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
        if matches!(left, Value::Junction { .. }) || matches!(right, Value::Junction { .. }) {
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
        let cn = match &v {
            Value::Instance { class_name, .. } => class_name.resolve().to_string(),
            Value::Package(name) => name.resolve().to_string(),
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
        if let (Value::Junction { kind: lk, .. }, Value::Junction { kind: rk, .. }) =
            (&left, &right)
        {
            let need_swap = Self::thread_right_first(lk, rk);
            let rk = rk.clone();
            let lk = lk.clone();
            if let Value::Junction { kind, values } = left {
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
        if let Value::Junction { kind, values } = left {
            let results: Vec<Value> = values
                .iter()
                .cloned()
                .map(|v| self.eval_concat_with_junctions(v, right.clone()))
                .collect();
            return Value::junction(kind, results);
        }
        if let Value::Junction { kind, values } = right {
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
        if let Value::Junction { values, .. } = value {
            let swapped: Vec<Value> = values
                .iter()
                .map(|v| {
                    if let Value::Junction { values: inner, .. } = v {
                        Value::junction(new_inner.clone(), inner.to_vec())
                    } else {
                        v.clone()
                    }
                })
                .collect();
            Value::junction(new_outer.clone(), swapped)
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
            let result_class = if let Value::Instance { class_name, .. } = &left {
                *class_name
            } else {
                crate::symbol::Symbol::intern("Buf")
            };
            let mut bytes = Self::extract_buf_bytes(&left);
            bytes.extend(Self::extract_buf_bytes(&right));
            let byte_vals: Vec<Value> = bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
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
        if let Value::Instance { class_name, .. } = val {
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
        if let Value::Instance { attributes, .. } = val
            && let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes")
        {
            return items
                .iter()
                .map(|v| match v {
                    Value::Int(i) => *i as u8,
                    _ => 0,
                })
                .collect();
        }
        Vec::new()
    }
}
