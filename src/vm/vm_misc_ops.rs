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
    match v.view() {
        ValueView::Range(a, b)
        | ValueView::RangeExcl(a, b)
        | ValueView::RangeExclStart(a, b)
        | ValueView::RangeExclBoth(a, b) => (Value::int(a), Value::int(b)),
        ValueView::GenericRange { start, end, .. } => ((**start).clone(), (**end).clone()),
        ValueView::Array(items, _) => {
            if items.is_empty() {
                (Value::NIL, Value::NIL)
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
        ValueView::Seq(items) => {
            if items.is_empty() {
                (Value::NIL, Value::NIL)
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
                | "notandthen"
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
                | "(+)"
                | "⊎"
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
            // NOTE: `x`/`xx` (list/string repeat) are LEFT-associative in Raku
            // (`raku -e 'say [x] "a", 2, 3'` == "aaaaaa", i.e. `("a" x 2) x 3`),
            // unlike `=`/`:=`/`=>` which genuinely right-associate. They fall
            // through to the `_ => ReductionAssoc::Left` default below.
            "=" | ":=" | "=>" => ReductionAssoc::Right,
            "eqv" | "===" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "eq" | "ne" | "lt" | "gt"
            | "le" | "ge" | "~~" | "=~=" | "=:=" | "!=:=" => ReductionAssoc::Chain,
            _ => ReductionAssoc::Left,
        }
    }

    pub(super) fn reduction_callable_for_op(&mut self, op: &str) -> Option<Value> {
        if let Some(name) = op.strip_prefix('&') {
            let callable = loan_env!(self, resolve_code_var(name));
            if matches!(
                callable.view(),
                ValueView::Sub(_)
                    | ValueView::WeakSub(_)
                    | ValueView::Routine { .. }
                    | ValueView::Instance { .. }
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
            callable.view(),
            ValueView::Sub(_)
                | ValueView::WeakSub(_)
                | ValueView::Routine { .. }
                | ValueView::Instance { .. }
        ) {
            return Some(callable);
        }
        if let Some(callable) = self.env().get(&format!("&{}", infix_name)).cloned()
            && matches!(
                callable.view(),
                ValueView::Sub(_)
                    | ValueView::WeakSub(_)
                    | ValueView::Routine { .. }
                    | ValueView::Instance { .. }
            )
        {
            return Some(callable);
        }
        if let Some(callable) = self.env().get(&format!("&{}", op)).cloned()
            && matches!(
                callable.view(),
                ValueView::Sub(_)
                    | ValueView::WeakSub(_)
                    | ValueView::Routine { .. }
                    | ValueView::Instance { .. }
            )
        {
            return Some(callable);
        }
        None
    }

    /// When a `[[&op]]` reduction's callable is really a BUILTIN infix routine
    /// (`my &op = &[+]`), the reduction still has that operator's identity and
    /// its documented one-element answer: rakudo answers `[[&op]]` with `0` and
    /// `[[&op]] 5` with `5`, exactly as `[+]` / `[+] 5` do. Report the operator
    /// name so the caller can drop the callable and reduce with the builtin,
    /// which is what `reduction_step_with_args` already does per step.
    ///
    /// A user declaration of the same name wins (`has_function`), so a
    /// user-defined `infix:<+>` is still called as a user routine.
    pub(super) fn reduction_builtin_op_for_callable(&self, callable: &Value) -> Option<String> {
        let ValueView::Routine { name, .. } = callable.view() else {
            return None;
        };
        let name = name.resolve();
        if self.has_function(&name) {
            return None;
        }
        let op = name
            .strip_prefix("infix:<")
            .and_then(|rest| rest.strip_suffix('>'))?;
        if Self::is_builtin_reduction_op(op) {
            Some(op.to_string())
        } else {
            None
        }
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
            if let ValueView::Routine { name, .. } = callable.view() {
                let name = name.resolve();
                // `&[+]` / `&infix:<+>` is a `Routine` value naming a BUILTIN
                // operator, which `call_user_routine_direct` cannot find (it
                // looks for a user declaration and then for an `&`-keyed env
                // binding, and a core operator is neither) -- `my &op = &[+];
                // [[&op]] 1, 2, 3` died with "Unknown function: infix:<+>".
                // Evaluate the operator itself in that case, exactly as the
                // no-callable path below does. A user-declared `infix:<+>`
                // still wins: `has_function` sees it and the direct call runs.
                if !self.has_function(&name)
                    && args.len() == 2
                    && let Some(op) = name
                        .strip_prefix("infix:<")
                        .and_then(|rest| rest.strip_suffix('>'))
                    && Self::is_builtin_reduction_op(op)
                {
                    return self.eval_reduction_operator_values(op, &args[0], &args[1]);
                }
                return loan_env!(self, call_user_routine_direct(&name, args));
            }
            return self.vm_call_on_value(callable.clone(), args, None);
        }
        debug_assert!(args.len() == 2);
        self.eval_reduction_operator_values(base_op, &args[0], &args[1])
    }
}
