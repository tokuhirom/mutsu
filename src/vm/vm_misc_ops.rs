use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ReductionAssoc {
    Left,
    Right,
    Chain,
}

/// Check if a type name is a core Raku type that should always be accepted.
pub(super) fn is_core_raku_type(name: &str) -> bool {
    matches!(
        name,
        "Mu" | "Any"
            | "Cool"
            | "Junction"
            | "Pair"
            | "List"
            | "Seq"
            | "Range"
            | "Map"
            | "Slip"
            | "Set"
            | "Bag"
            | "Mix"
            | "SetHash"
            | "BagHash"
            | "MixHash"
            | "QuantHash"
            | "Capture"
            | "Signature"
            | "Parameter"
            | "Block"
            | "Code"
            | "Sub"
            | "Method"
            | "Routine"
            | "Regex"
            | "Match"
            | "Grammar"
            | "IO"
            | "Proc"
            | "Promise"
            | "Supply"
            | "Channel"
            | "Thread"
            | "ProtocolFamily"
            | "Instant"
            | "Duration"
            | "Version"
            | "Exception"
            | "Failure"
            | "Nil"
            | "Int"
            | "Num"
            | "Rat"
            | "Complex"
            | "Str"
            | "Bool"
            | "Whatever"
            | "HyperWhatever"
            | "WhateverCode"
            | "Stash"
            | "Scalar"
            | "Numeric"
            | "Real"
            | "Stringy"
            | "Callable"
            | "Positional"
            | "Associative"
            | "Array"
            | "Hash"
            | "Iterable"
            | "Iterator"
            | "Dateish"
            | "Date"
            | "DateTime"
            | "Buf"
            | "Blob"
            | "utf8"
    ) || crate::runtime::native_types::is_native_int_type(name)
        || is_parameterized_core_type(name)
}

pub(super) fn is_parameterized_core_type(name: &str) -> bool {
    if let Some(base) = name.split('[').next()
        && name.contains('[')
        && name.ends_with(']')
    {
        return is_core_raku_type(base);
    }
    false
}

/// Compute the (lo, hi) bounds of a value for use in `minmax` reduction.
/// For scalars: returns (v, v).
/// For arrays/lists: returns (min_element, max_element), recursing into elements.
/// For ranges: returns (start, end).
///
/// Single authoritative impl shared by the Interpreter's `minmax` reduction and the
/// interpreter's `apply_reduction_op` `minmax` arm (which delegates here).
pub(crate) fn minmax_bounds_of_value(v: &Value) -> (Value, Value) {
    match v {
        Value::Range(a, b)
        | Value::RangeExcl(a, b)
        | Value::RangeExclStart(a, b)
        | Value::RangeExclBoth(a, b) => (Value::Int(*a), Value::Int(*b)),
        Value::GenericRange { start, end, .. } => ((**start).clone(), (**end).clone()),
        Value::Array(items, _) => {
            if items.is_empty() {
                (Value::Nil, Value::Nil)
            } else {
                let mut lo = items[0].clone();
                let mut hi = items[0].clone();
                for item in items.iter().skip(1) {
                    let (item_lo, item_hi) = minmax_bounds_of_value(item);
                    if crate::runtime::compare_values(&item_lo, &lo) < 0 {
                        lo = item_lo;
                    }
                    if crate::runtime::compare_values(&item_hi, &hi) > 0 {
                        hi = item_hi;
                    }
                }
                (lo, hi)
            }
        }
        Value::Seq(items) => {
            if items.is_empty() {
                (Value::Nil, Value::Nil)
            } else {
                let mut lo = items[0].clone();
                let mut hi = items[0].clone();
                for item in items.iter().skip(1) {
                    let (item_lo, item_hi) = minmax_bounds_of_value(item);
                    if crate::runtime::compare_values(&item_lo, &lo) < 0 {
                        lo = item_lo;
                    }
                    if crate::runtime::compare_values(&item_hi, &hi) > 0 {
                        hi = item_hi;
                    }
                }
                (lo, hi)
            }
        }
        _ => (v.clone(), v.clone()),
    }
}

impl Interpreter {
    pub(super) fn is_builtin_reduction_op(op: &str) -> bool {
        if let Some(inner) = op
            .strip_prefix('R')
            .or_else(|| op.strip_prefix('Z'))
            .or_else(|| op.strip_prefix('X'))
            && !inner.is_empty()
            && Self::is_builtin_reduction_op(inner)
        {
            return true;
        }
        // Hyper operator forms: >>op<<, >>op>>, <<op<<, <<op>>
        if let Some(inner) = Self::strip_hyper_delimiters(op)
            && Self::is_builtin_reduction_op(inner)
        {
            return true;
        }
        matches!(
            op,
            "+" | "-"
                | "*"
                | "/"
                | "%"
                | "~"
                | "||"
                | "&&"
                | "//"
                | "%%"
                | "**"
                | "^^"
                | "+&"
                | "+|"
                | "+^"
                | "+<"
                | "+>"
                | "~&"
                | "~|"
                | "~^"
                | "~<"
                | "~>"
                | "?&"
                | "?|"
                | "?^"
                | "=="
                | "!="
                | "<"
                | ">"
                | "<="
                | ">="
                | "<=>"
                | "==="
                | "=:="
                | "!=:="
                | "=>"
                | "eqv"
                | "eq"
                | "ne"
                | "lt"
                | "gt"
                | "le"
                | "ge"
                | "leg"
                | "cmp"
                | "~~"
                | "min"
                | "max"
                | "div"
                | "mod"
                | "gcd"
                | "lcm"
                | "and"
                | "or"
                | "not"
                | "andthen"
                | "orelse"
                | "xor"
                | "="
                | "minmax"
                | ","
                | "after"
                | "before"
                | "X"
                | "Z"
                | "x"
                | "xx"
                | "&"
                | "|"
                | "^"
                | "o"
                | "∘"
                | "(-)"
                | "∖"
                | "(|)"
                | "∪"
                | "(&)"
                | "∩"
                | "(^)"
                | "⊖"
                | "(.)"
                | "⊍"
                | "(==)"
                | "≡"
                | "≢"
        )
    }

    pub(super) fn reduction_op_associativity(&self, op: &str) -> ReductionAssoc {
        let infix_name = format!("infix:<{}>", op);
        if let Some(assoc) = self.infix_associativity(&infix_name) {
            return match assoc.as_str() {
                "right" => ReductionAssoc::Right,
                "chain" => ReductionAssoc::Chain,
                _ => ReductionAssoc::Left,
            };
        }
        match op {
            "**" => ReductionAssoc::Right,
            "=" | ":=" | "=>" | "x" | "xx" => ReductionAssoc::Right,
            "eqv" | "===" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "eq" | "ne" | "lt" | "gt"
            | "le" | "ge" | "~~" | "=~=" | "=:=" | "!=:=" => ReductionAssoc::Chain,
            _ => ReductionAssoc::Left,
        }
    }

    pub(super) fn reduction_callable_for_op(&mut self, op: &str) -> Option<Value> {
        if let Some(name) = op.strip_prefix('&') {
            let callable = loan_env!(self, resolve_code_var(name));
            if matches!(
                callable,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } | Value::Instance { .. }
            ) {
                return Some(callable);
            }
        }
        if Self::is_builtin_reduction_op(op) {
            return None;
        }
        let infix_name = format!("infix:<{}>", op);
        let callable = loan_env!(self, resolve_code_var(&infix_name));
        if matches!(
            callable,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } | Value::Instance { .. }
        ) {
            return Some(callable);
        }
        if let Some(callable) = self.env().get(&format!("&{}", infix_name)).cloned()
            && matches!(
                callable,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } | Value::Instance { .. }
            )
        {
            return Some(callable);
        }
        if let Some(callable) = self.env().get(&format!("&{}", op)).cloned()
            && matches!(
                callable,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } | Value::Instance { .. }
            )
        {
            return Some(callable);
        }
        None
    }

    /// Strip hyper operator delimiters (>>...<<, >>...>>, <<...<<, <<...>>)
    /// and their Unicode variants, returning the inner operator if found.
    fn strip_hyper_delimiters(s: &str) -> Option<&str> {
        let after_left = s
            .strip_prefix(">>")
            .or_else(|| s.strip_prefix("<<"))
            .or_else(|| s.strip_prefix('\u{00BB}'))
            .or_else(|| s.strip_prefix('\u{00AB}'))?;
        let inner = after_left
            .strip_suffix(">>")
            .or_else(|| after_left.strip_suffix("<<"))
            .or_else(|| after_left.strip_suffix('\u{00BB}'))
            .or_else(|| after_left.strip_suffix('\u{00AB}'))?;
        if inner.is_empty() {
            return None;
        }
        Some(inner)
    }

    pub(super) fn reduction_callable_arity(&self, callable: &Value) -> usize {
        let (params, param_defs) = self.callable_signature(callable);
        if !param_defs.is_empty() {
            let mut total = 0usize;
            let mut required = 0usize;
            for pd in &param_defs {
                if pd.named
                    || pd.slurpy
                    || pd.double_slurpy
                    || pd.onearg
                    || pd.traits.iter().any(|t| t == "invocant")
                {
                    continue;
                }
                total += 1;
                let is_required = pd.required || (!pd.optional_marker && pd.default.is_none());
                if is_required {
                    required += 1;
                }
            }
            if required >= 2 {
                return required;
            }
            if total >= 2 {
                return total;
            }
        }
        params.len().max(2)
    }

    pub(super) fn reduction_step_with_args(
        &mut self,
        base_op: &str,
        callable: Option<&Value>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(callable) = callable {
            if let Value::Routine { name, .. } = callable {
                return loan_env!(self, call_user_routine_direct(&name.resolve(), args));
            }
            return self.vm_call_on_value(callable.clone(), args, None);
        }
        debug_assert!(args.len() == 2);
        self.eval_reduction_operator_values(base_op, &args[0], &args[1])
    }
}
