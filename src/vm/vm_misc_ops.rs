use std::sync::Arc;

use super::*;
use crate::symbol::Symbol;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ReductionAssoc {
    Left,
    Right,
    Chain,
}

/// Check if a type name is a core Raku type that should always be accepted.
fn is_core_raku_type(name: &str) -> bool {
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

fn is_parameterized_core_type(name: &str) -> bool {
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
/// Single authoritative impl shared by the VM's `minmax` reduction and the
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

impl VM {
    fn is_builtin_reduction_op(op: &str) -> bool {
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

    fn reduction_op_associativity(&self, op: &str) -> ReductionAssoc {
        let infix_name = format!("infix:<{}>", op);
        if let Some(assoc) = self.interpreter.infix_associativity(&infix_name) {
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

    pub(super) fn reduction_callable_for_op(&self, op: &str) -> Option<Value> {
        if let Some(name) = op.strip_prefix('&') {
            let callable = self.interpreter.resolve_code_var(name);
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
        let callable = self.interpreter.resolve_code_var(&infix_name);
        if matches!(
            callable,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } | Value::Instance { .. }
        ) {
            return Some(callable);
        }
        if let Some(callable) = self
            .interpreter
            .env()
            .get(&format!("&{}", infix_name))
            .cloned()
            && matches!(
                callable,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } | Value::Instance { .. }
            )
        {
            return Some(callable);
        }
        if let Some(callable) = self.interpreter.env().get(&format!("&{}", op)).cloned()
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

    fn reduction_callable_arity(&self, callable: &Value) -> usize {
        let (params, param_defs) = self.interpreter.callable_signature(callable);
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
                return self
                    .interpreter
                    .call_user_routine_direct(&name.resolve(), args);
            }
            return self.vm_call_on_value(callable.clone(), args, None);
        }
        debug_assert!(args.len() == 2);
        self.eval_reduction_operator_values(base_op, &args[0], &args[1])
    }

    fn array_elements_match_constraint(&mut self, constraint: &str, value: &Value) -> bool {
        // Container element constraints like `Array` or `Array[Int]` match each
        // element as a whole. Scalar constraints like `Int` recurse into nested arrays.
        let constraint_base = constraint
            .split_once('[')
            .map_or(constraint, |(base, _)| base);
        let is_container_constraint = matches!(
            constraint_base,
            "Array" | "Hash" | "List" | "Seq" | "Positional" | "Associative"
        );
        match value {
            Value::Array(items, ..) => {
                if is_container_constraint {
                    // Each element is checked as a whole against the constraint
                    items
                        .iter()
                        .all(|item| self.interpreter.type_matches_value(constraint, item))
                } else {
                    // Recurse into sub-arrays for simple element types
                    items
                        .iter()
                        .all(|item| self.array_elements_match_constraint(constraint, item))
                }
            }
            Value::Seq(items) => {
                if is_container_constraint {
                    // Each element is checked as a whole against the constraint
                    items
                        .iter()
                        .all(|item| self.interpreter.type_matches_value(constraint, item))
                } else {
                    // Recurse into sub-arrays for simple element types
                    items
                        .iter()
                        .all(|item| self.array_elements_match_constraint(constraint, item))
                }
            }
            Value::Nil => true,
            // A type-object hole (e.g. an `Any` gap) is acceptable for a native
            // array: it is coerced to the array's default on store.
            Value::Package(_)
                if crate::runtime::native_types::is_native_array_element_type(constraint) =>
            {
                true
            }
            _ => self.interpreter.type_matches_value(constraint, value),
        }
    }

    /// Build the appropriate error for a typed-array initializer whose elements
    /// fail the element type constraint. Native integer arrays produce an AdHoc
    /// "Cannot unbox" error; boxed types produce X::TypeCheck::Assignment naming
    /// the offending element.
    fn typed_array_element_error(
        &mut self,
        var_name: Option<&str>,
        base_constraint: &str,
        constraint: &str,
        value: &Value,
    ) -> RuntimeError {
        let bad = self
            .first_array_element_mismatch(constraint, value)
            .unwrap_or_else(|| value.clone());
        if crate::runtime::native_types::is_native_array_element_type(base_constraint) {
            return RuntimeError::new(format!(
                "Cannot unbox a {} to {}.",
                crate::runtime::value_type_name(&bad),
                base_constraint,
            ));
        }
        let name = var_name.unwrap_or("@");
        crate::runtime::utils::type_check_element_typed_error(name, constraint, &bad)
    }

    /// Like `array_elements_match_constraint`, but returns the first element that
    /// does not satisfy the constraint (recursing into sub-arrays for scalar
    /// element types), used to build a precise type-check error.
    fn first_array_element_mismatch(&mut self, constraint: &str, value: &Value) -> Option<Value> {
        let constraint_base = constraint
            .split_once('[')
            .map_or(constraint, |(base, _)| base);
        let is_container_constraint = matches!(
            constraint_base,
            "Array" | "Hash" | "List" | "Seq" | "Positional" | "Associative"
        );
        match value {
            Value::Array(items, ..) => {
                for item in items.iter() {
                    if is_container_constraint {
                        if !self.interpreter.type_matches_value(constraint, item) {
                            return Some(item.clone());
                        }
                    } else if let Some(bad) = self.first_array_element_mismatch(constraint, item) {
                        return Some(bad);
                    }
                }
                None
            }
            Value::Seq(items) => {
                for item in items.iter() {
                    if is_container_constraint {
                        if !self.interpreter.type_matches_value(constraint, item) {
                            return Some(item.clone());
                        }
                    } else if let Some(bad) = self.first_array_element_mismatch(constraint, item) {
                        return Some(bad);
                    }
                }
                None
            }
            Value::Nil => None,
            _ => {
                if self.interpreter.type_matches_value(constraint, value) {
                    None
                } else {
                    Some(value.clone())
                }
            }
        }
    }

    /// Check if a range endpoint is invalid (Range, Complex, Seq, etc.) and return
    /// X::Range::InvalidArg if so.
    fn check_range_invalid_arg(left: &Value, right: &Value) -> Option<RuntimeError> {
        fn is_invalid_endpoint(v: &Value) -> bool {
            matches!(
                v,
                Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. }
                    | Value::Complex(..)
                    | Value::Seq(_)
            )
        }
        fn invalid_value(v: &Value) -> Option<&Value> {
            if is_invalid_endpoint(v) {
                Some(v)
            } else {
                None
            }
        }
        let got = invalid_value(left).or_else(|| invalid_value(right));
        got.map(|v| {
            let type_name = crate::runtime::value_type_name(v);
            let mut ex_attrs = std::collections::HashMap::new();
            ex_attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "{} objects are not valid endpoints for Ranges",
                    type_name
                )),
            );
            // For Seq endpoints, store the type object (Raku behavior); for others,
            // store the actual value.
            let got_val = if matches!(v, Value::Seq(_)) {
                Value::Package(Symbol::intern(type_name))
            } else {
                v.clone()
            };
            ex_attrs.insert("got".to_string(), got_val);
            let exception = Value::make_instance(Symbol::intern("X::Range::InvalidArg"), ex_attrs);
            RuntimeError::from_exception_value(exception)
        })
    }

    fn scalarize_range_endpoint(value: Value) -> Value {
        match value {
            Value::Scalar(inner) => Self::scalarize_range_endpoint(inner.as_ref().clone()),
            Value::Instance { class_name, .. } if class_name == "Match" => {
                runtime::coerce_to_numeric(Value::str(value.to_string_value()))
            }
            Value::Array(..)
            | Value::Slip(..)
            | Value::LazyList(..)
            | Value::Hash(..)
            | Value::Set(..)
            | Value::Bag(..)
            | Value::Mix(..) => runtime::coerce_to_numeric(value),
            // Seq is NOT scalarized here — it must be caught by check_range_invalid_arg
            Value::Seq(..) => value,
            other => other,
        }
    }

    pub(super) fn exec_make_range_op(&mut self) -> Result<(), RuntimeError> {
        let right = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        let left = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        if let Some(err) = Self::check_range_invalid_arg(&left, &right) {
            return Err(err);
        }
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::Range(*a, *b),
            (Value::Int(a), Value::Num(b)) if b.is_infinite() && b.is_sign_positive() => {
                Value::Range(*a, i64::MAX)
            }
            (Value::Int(a), Value::Whatever) => Value::Range(*a, i64::MAX),
            (Value::Int(a), Value::HyperWhatever) => Value::Range(*a, i64::MAX),
            (Value::Num(a), Value::Int(b)) if a.is_infinite() && a.is_sign_negative() => {
                Value::Range(i64::MIN, *b)
            }
            (Value::Str(a), Value::Whatever) => Value::GenericRange {
                start: Arc::new(Value::Str(a.clone())),
                end: Arc::new(Value::HyperWhatever),
                excl_start: false,
                excl_end: false,
            },
            (Value::Str(a), Value::HyperWhatever) => Value::GenericRange {
                start: Arc::new(Value::Str(a.clone())),
                end: Arc::new(Value::HyperWhatever),
                excl_start: false,
                excl_end: false,
            },
            (Value::Str(a), Value::Str(b)) => Value::GenericRange {
                start: Arc::new(Value::Str(a.clone())),
                end: Arc::new(Value::Str(b.clone())),
                excl_start: false,
                excl_end: false,
            },
            (l, r) if l.is_numeric() && r.is_numeric() => Value::GenericRange {
                start: Arc::new(l.clone()),
                end: Arc::new(r.clone()),
                excl_start: false,
                excl_end: false,
            },
            (Value::Str(a), r) if r.is_numeric() => Value::GenericRange {
                start: Arc::new(Value::Str(a.clone())),
                end: Arc::new(r.clone()),
                excl_start: false,
                excl_end: false,
            },
            (l, Value::Str(b)) if l.is_numeric() => Value::GenericRange {
                start: Arc::new(l.clone()),
                end: Arc::new(Value::Str(b.clone())),
                excl_start: false,
                excl_end: false,
            },
            // WhateverCode endpoint: e.g. 0..*-2
            (_, Value::Sub(_)) | (Value::Sub(_), _) => Value::GenericRange {
                start: Arc::new(left),
                end: Arc::new(right),
                excl_start: false,
                excl_end: false,
            },
            _ => Value::GenericRange {
                start: Arc::new(left),
                end: Arc::new(right),
                excl_start: false,
                excl_end: false,
            },
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_make_range_excl_op(&mut self) -> Result<(), RuntimeError> {
        let right = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        let left = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        if let Some(err) = Self::check_range_invalid_arg(&left, &right) {
            return Err(err);
        }
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::RangeExcl(*a, *b),
            (l, r) if l.is_numeric() || r.is_numeric() => Value::GenericRange {
                start: Arc::new(left.clone()),
                end: Arc::new(right.clone()),
                excl_start: false,
                excl_end: true,
            },
            _ => Value::GenericRange {
                start: Arc::new(left),
                end: Arc::new(right),
                excl_start: false,
                excl_end: true,
            },
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_make_range_excl_start_op(&mut self) -> Result<(), RuntimeError> {
        let right = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        let left = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        if let Some(err) = Self::check_range_invalid_arg(&left, &right) {
            return Err(err);
        }
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::RangeExclStart(*a, *b),
            (l, r) if l.is_numeric() || r.is_numeric() => Value::GenericRange {
                start: Arc::new(left.clone()),
                end: Arc::new(right.clone()),
                excl_start: true,
                excl_end: false,
            },
            _ => Value::GenericRange {
                start: Arc::new(left),
                end: Arc::new(right),
                excl_start: true,
                excl_end: false,
            },
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_make_range_excl_both_op(&mut self) -> Result<(), RuntimeError> {
        let right = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        let left = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        if let Some(err) = Self::check_range_invalid_arg(&left, &right) {
            return Err(err);
        }
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::RangeExclBoth(*a, *b),
            (l, r) if l.is_numeric() || r.is_numeric() => Value::GenericRange {
                start: Arc::new(left.clone()),
                end: Arc::new(right.clone()),
                excl_start: true,
                excl_end: true,
            },
            _ => Value::GenericRange {
                start: Arc::new(left),
                end: Arc::new(right),
                excl_start: true,
                excl_end: true,
            },
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_num_coerce_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // Auto-FETCH Proxy containers
        let val = self.interpreter.auto_fetch_proxy(&val)?;
        // Junction auto-threading for prefix:<+>
        if let Value::Junction { kind, values } = &val {
            let kind = kind.clone();
            let mut results = Vec::new();
            for v in values.iter() {
                self.stack.push(v.clone());
                self.exec_num_coerce_op()?;
                results.push(self.stack.pop().unwrap_or(Value::Nil));
            }
            self.stack.push(Value::junction(kind, results));
            return Ok(());
        }
        // Type objects (Mu, Any, etc.) cannot be numerically coerced
        if let Value::Package(name) = &val
            && matches!(name.resolve().as_str(), "Mu" | "Any")
        {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller prefix:<+>({}:U); none of these signatures matches:\n    (\\a)",
                name.resolve()
            )));
        }
        if matches!(
            &val,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ) {
            return Err(RuntimeError::new(
                "Cannot resolve caller Numeric(Sub:D: ); none of these signatures matches:\n    (Mu:U \\v: *%_)",
            ));
        }
        // If the value is an Instance, try calling the Numeric method
        if let Value::Instance { .. } = &val
            && let Ok(result) =
                self.try_compiled_method_or_interpret(val.clone(), "Numeric", vec![])
        {
            self.stack.push(result);
            return Ok(());
        }
        // Force a lazy IO words/lines iterator so numeric coercion counts its
        // elements (e.g. `+$fh.words` / `+$fh.lines`) rather than yielding 0.
        let val = if matches!(&val, Value::LazyIoLines { .. }) {
            self.force_if_lazy_io_lines(val)?
        } else {
            val
        };
        // Force LazyList before numeric coercion so we can count elements.
        // If the lazy list has a known element count (e.g. n! for permutations),
        // use that directly without materializing.
        let val = if let Value::LazyList(ll) = &val {
            if let Some(count) = &ll.elems_count {
                self.stack.push(count.clone());
                return Ok(());
            }
            let items = self.force_lazy_list_vm(ll)?;
            Value::Seq(std::sync::Arc::new(items))
        } else {
            val
        };
        if let Value::Str(s) = &val {
            let trimmed = s.trim();
            if trimmed.is_empty() {
                self.stack.push(Value::Int(0));
                return Ok(());
            }
            if let Some(v) = crate::runtime::str_numeric::parse_raku_str_to_numeric(trimmed) {
                self.stack.push(v);
            } else {
                let mut ex_attrs = std::collections::HashMap::new();
                ex_attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}'",
                        s
                    )),
                );
                let ex = Value::make_instance(Symbol::intern("X::Str::Numeric"), ex_attrs);
                let mut failure_attrs = std::collections::HashMap::new();
                failure_attrs.insert("exception".to_string(), ex);
                failure_attrs.insert("handled".to_string(), Value::Bool(false));
                self.stack.push(Value::make_instance(
                    Symbol::intern("Failure"),
                    failure_attrs,
                ));
            }
            return Ok(());
        }
        let result = crate::runtime::utils::coerce_to_numeric(val);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_coerce_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // Auto-FETCH Proxy containers
        let val = self.interpreter.auto_fetch_proxy(&val)?;
        // Mu itself has no Str candidate — stringifying it is a hard
        // error (Rakudo dies with `Cannot resolve caller prefix:<~>(Mu:U)`).
        if let Value::Package(name) = &val
            && name.resolve() == "Mu"
        {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller prefix:<~>({}:U); none of these signatures matches:\n    (\\a)",
                name.resolve()
            )));
        }
        // Any and other Cool-descended type objects stringify to the empty
        // string with a warning (Rakudo emits `Use of uninitialized value
        // of type Any in string context.`).
        if let Value::Package(name) = &val
            && name.resolve() == "Any"
        {
            let msg = format!(
                "Use of uninitialized value of type {} in string context.\nMethods .^name, .raku, .gist, or .say can be used to stringify it to something meaningful.",
                name.resolve()
            );
            return Err(RuntimeError::warn_signal_with_resume(
                msg,
                Value::str(String::new()),
            ));
        }
        // Stringifying an unhandled Failure throws
        if let Some(err) = self.interpreter.failure_to_runtime_error_if_unhandled(&val) {
            return Err(err);
        }
        // Check for user-defined prefix:<~> multi sub first (operator overloading).
        // This must come before .Stringy()/.Str() to avoid infinite recursion when
        // .Stringy() is defined as `{ ~self }` which delegates to prefix:<~>.
        {
            let args = vec![val.clone()];
            if let Some(def) = self
                .interpreter
                .resolve_function_with_types("prefix:<~>", &args)
            {
                let empty_fns = std::collections::HashMap::new();
                let result = self.compile_and_call_function_def(&def, args, &empty_fns)?;
                self.stack.push(result);
                return Ok(());
            }
        }
        // If the value is an Instance, try calling the Stringy method, then Str
        if let Value::Instance { .. } = &val {
            if let Ok(result) =
                self.try_compiled_method_or_interpret(val.clone(), "Stringy", vec![])
            {
                self.stack.push(result);
                return Ok(());
            }
            if let Ok(result) = self.try_compiled_method_or_interpret(val.clone(), "Str", vec![]) {
                self.stack.push(result);
                return Ok(());
            }
        }
        // Force LazyList before stringification
        if let Value::LazyList(_) = &val {
            let result = self.try_compiled_method_or_interpret(val, "Str", vec![])?;
            self.stack.push(result);
            return Ok(());
        }
        // Resolve bound-element sentinels inside arrays before stringification
        let val = self.resolve_bound_array_elements(val);
        self.stack
            .push(Value::str(crate::runtime::utils::coerce_to_str(&val)));
        Ok(())
    }

    pub(super) fn exec_upto_range_op(&mut self) {
        let val = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        let numeric = if val.is_numeric() {
            val
        } else {
            runtime::coerce_to_numeric(val)
        };
        let result = match numeric {
            Value::Int(i) => Value::RangeExcl(0, i),
            Value::Num(_)
            | Value::Rat(_, _)
            | Value::FatRat(_, _)
            | Value::BigRat(_, _)
            | Value::BigInt(_) => Value::GenericRange {
                start: Arc::new(Value::Int(0)),
                end: Arc::new(numeric),
                excl_start: false,
                excl_end: true,
            },
            _ => Value::RangeExcl(0, 0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_pre_increment_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Phase 3 Stage 2: scalar attribute increments read-modify-write the cell.
        let attr_name = Self::const_str(code, name_idx).to_string();
        self.sync_attr_local_from_cell_by_name(code, &attr_name);
        let r = self.exec_pre_increment_op_inner(code, name_idx);
        if r.is_ok() {
            self.mirror_attr_local_to_cell_by_name(code, &attr_name);
        }
        r
    }

    fn exec_pre_increment_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        if name.starts_with('!')
            && let Some(slot) = self.find_local_slot(code, name)
            && !matches!(self.locals[slot], Value::Proxy { .. })
        {
            // ContainerRef: increment through the shared arc (e.g. `$!attr := outer_var`).
            if let Value::ContainerRef(ref arc) = self.locals[slot].clone() {
                if self.atomic_container_incdec(arc, name, true, false) {
                    return Ok(());
                }
                let inner = arc.lock().unwrap().clone();
                let val = self.normalize_incdec_source_with_type(name, inner);
                let new_val = self.increment_value_smart(&val)?;
                arc.lock().unwrap().clone_from(&new_val);
                self.stack.push(new_val);
                return Ok(());
            }
            let raw_val = self.locals[slot].clone();
            let val = self.normalize_incdec_source_with_type(name, raw_val);
            let new_val = self.increment_value_smart(&val)?;
            self.locals[slot] = new_val.clone();
            self.flush_local_to_env(code, slot);
            // Propagate via sigilless alias chain (e.g. `$!attr := outer_var`).
            let alias_key = format!("__mutsu_sigilless_alias::{}", name);
            let mut alias_name = self.interpreter.env().get(&alias_key).and_then(|v| {
                if let Value::Str(n) = v {
                    Some(n.to_string())
                } else {
                    None
                }
            });
            let mut seen_aliases = std::collections::HashSet::new();
            while let Some(current_alias) = alias_name {
                if !seen_aliases.insert(current_alias.clone()) {
                    break;
                }
                self.set_env_with_main_alias(&current_alias, new_val.clone());
                self.update_local_if_exists(code, &current_alias, &new_val);
                let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
                alias_name = self.interpreter.env().get(&next_key).and_then(|v| {
                    if let Value::Str(n) = v {
                        Some(n.to_string())
                    } else {
                        None
                    }
                });
            }
            self.stack.push(new_val);
            return Ok(());
        }
        let val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        // ContainerRef (box-on-capture / `:=`): mutate the shared cell in place so
        // closures over this lexical observe the change and the smart string/Int
        // increment semantics are preserved (the slot holds the same Arc).
        if let Value::ContainerRef(ref arc) = val {
            if self.atomic_container_incdec(arc, name, true, false) {
                return Ok(());
            }
            let inner = arc.lock().unwrap().clone();
            let v = self.normalize_incdec_source_with_type(name, inner);
            let new_val = self.increment_value_smart(&v)?;
            arc.lock().unwrap().clone_from(&new_val);
            self.stack.push(new_val);
            return Ok(());
        }
        let val = self.normalize_incdec_source_with_type(name, val);
        let new_val = self.increment_value_smart(&val)?;
        self.check_incdec_type_constraint(name, &new_val)?;
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(new_val);
        Ok(())
    }

    pub(super) fn exec_pre_decrement_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Phase 3 Stage 2: scalar attribute decrements read-modify-write the cell.
        let attr_name = Self::const_str(code, name_idx).to_string();
        self.sync_attr_local_from_cell_by_name(code, &attr_name);
        let r = self.exec_pre_decrement_op_inner(code, name_idx);
        if r.is_ok() {
            self.mirror_attr_local_to_cell_by_name(code, &attr_name);
        }
        r
    }

    fn exec_pre_decrement_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        if name.starts_with('!')
            && let Some(slot) = self.find_local_slot(code, name)
            && !matches!(self.locals[slot], Value::Proxy { .. })
        {
            // ContainerRef: decrement through the shared arc (e.g. `$!attr := outer_var`).
            if let Value::ContainerRef(ref arc) = self.locals[slot].clone() {
                if self.atomic_container_incdec(arc, name, false, false) {
                    return Ok(());
                }
                let inner = arc.lock().unwrap().clone();
                let val = self.normalize_incdec_source_with_type(name, inner);
                let new_val = self.decrement_value_smart(&val)?;
                arc.lock().unwrap().clone_from(&new_val);
                self.stack.push(new_val);
                return Ok(());
            }
            let raw_val = self.locals[slot].clone();
            let val = self.normalize_incdec_source_with_type(name, raw_val);
            let new_val = self.decrement_value_smart(&val)?;
            self.locals[slot] = new_val.clone();
            self.flush_local_to_env(code, slot);
            // Propagate via sigilless alias chain (e.g. `$!attr := outer_var`).
            let alias_key = format!("__mutsu_sigilless_alias::{}", name);
            let mut alias_name = self.interpreter.env().get(&alias_key).and_then(|v| {
                if let Value::Str(n) = v {
                    Some(n.to_string())
                } else {
                    None
                }
            });
            let mut seen_aliases = std::collections::HashSet::new();
            while let Some(current_alias) = alias_name {
                if !seen_aliases.insert(current_alias.clone()) {
                    break;
                }
                self.set_env_with_main_alias(&current_alias, new_val.clone());
                self.update_local_if_exists(code, &current_alias, &new_val);
                let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
                alias_name = self.interpreter.env().get(&next_key).and_then(|v| {
                    if let Value::Str(n) = v {
                        Some(n.to_string())
                    } else {
                        None
                    }
                });
            }
            self.stack.push(new_val);
            return Ok(());
        }
        let val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        // ContainerRef (box-on-capture / `:=`): mutate the shared cell in place so
        // closures over this lexical observe the change and the smart string/Int
        // decrement semantics are preserved (the slot holds the same Arc).
        if let Value::ContainerRef(ref arc) = val {
            if self.atomic_container_incdec(arc, name, false, false) {
                return Ok(());
            }
            let inner = arc.lock().unwrap().clone();
            let v = self.normalize_incdec_source_with_type(name, inner);
            let new_val = self.decrement_value_smart(&v)?;
            arc.lock().unwrap().clone_from(&new_val);
            self.stack.push(new_val);
            return Ok(());
        }
        let val = self.normalize_incdec_source_with_type(name, val);
        let new_val = self.decrement_value_smart(&val)?;
        self.check_incdec_type_constraint(name, &new_val)?;
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(new_val);
        Ok(())
    }

    pub(super) fn exec_get_capture_var_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = if let Some(v) = self.interpreter.env().get(name).cloned() {
            v
        } else if let Some(key) = name.strip_prefix('<').and_then(|s| s.strip_suffix('>'))
            && let Some(match_val) = self.interpreter.env().get("/").cloned()
        {
            match &match_val {
                Value::Hash(map) => map.get(key).cloned().unwrap_or(Value::Nil),
                _ => self
                    .try_compiled_method_or_interpret(
                        match_val,
                        "AT-KEY",
                        vec![Value::str(key.to_string())],
                    )
                    .unwrap_or(Value::Nil),
            }
        } else {
            Value::Nil
        };
        self.stack.push(val);
    }

    pub(super) fn exec_get_code_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        let mut val = self.interpreter.resolve_code_var(name);
        // Fallback for fast-path method dispatch (skip_env_setup=true):
        // &!attr is not set in env, so read directly from self's instance
        // attributes when available.
        if matches!(val, Value::Nil)
            && let Some(attr_name) = name.strip_prefix('!').filter(|n| !n.is_empty())
            && let Some(Value::Instance { attributes, .. }) =
                self.get_env_with_main_alias("self").as_ref()
            && let Some(attr_val) = attributes.as_map().get(attr_name)
        {
            val = attr_val.clone();
        }
        // In Raku, &foo for an undefined routine is a compile-time error.
        // We approximate this at runtime, but only inside EVAL context
        // to avoid breaking code that relies on &name returning Nil for
        // non-existent routines (e.g. custom EXPORT mechanisms).
        if matches!(val, Value::Nil)
            && !name.contains("::")
            && !name.starts_with('?')
            && !name.starts_with('*')
            && matches!(
                self.interpreter.env().get("__mutsu_in_eval"),
                Some(Value::Bool(true))
            )
        {
            let env_key = format!("&{}", name);
            let is_declared = self.interpreter.env().contains_key(&env_key);
            if !is_declared {
                let suggestions = self.interpreter.suggest_routine_names(name);
                return Err(RuntimeError::undeclared_routine_symbols(
                    name,
                    format!("Undeclared routine:\n    {} used at line 1", name),
                    suggestions,
                ));
            }
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_indirect_code_lookup_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let func_name = Self::const_str(code, name_idx).to_string();
        // Pop the package name from the stack (result of evaluating the package expr)
        let package = self.stack.pop().unwrap_or(Value::Nil);
        // Construct a qualified name: "SETTING::OUTER::...::not"
        // resolve_code_var will strip pseudo-package prefixes and resolve to builtin
        let pkg_str = package.to_string_value();
        let qualified = if pkg_str.is_empty() {
            func_name
        } else {
            format!("{}::{}", pkg_str, func_name)
        };
        let val = self.interpreter.resolve_code_var(&qualified);
        self.stack.push(val);
    }

    /// Execute symbolic variable dereference: $::("name"), @::("name"), %::("name").
    /// Pops the name string from the stack, prepends the sigil, and looks up the variable.
    pub(super) fn exec_symbolic_deref_op(&mut self, code: &CompiledCode, sigil_idx: u32) {
        let sigil = Self::const_str(code, sigil_idx).to_string();
        let name_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = name_val.to_string_value();
        // For $::("x"), look up the bare name "x" (scalars are stored without sigil).
        // For @::("x"), look up "@x" (arrays are stored with sigil).
        // For %::("x"), look up "%x" (hashes are stored with sigil).
        // For &::("x"), resolve as a code variable (function lookup).
        if sigil == "&" {
            let val = self.interpreter.resolve_code_var(&name);
            self.stack.push(val);
            return;
        }
        // Handle CALLER:: prefix(es) for dynamic variable lookup
        let mut remaining = name.as_str();
        let mut caller_depth = 0usize;
        while let Some(rest) = remaining.strip_prefix("CALLER::") {
            caller_depth += 1;
            remaining = rest;
        }
        if caller_depth > 0 {
            let bare_name = match sigil.as_str() {
                "$" => remaining.to_string(),
                "@" => format!("@{}", remaining),
                "%" => format!("%{}", remaining),
                _ => remaining.to_string(),
            };
            let val = self
                .interpreter
                .get_caller_var(&bare_name, caller_depth)
                .unwrap_or(Value::Nil);
            self.stack.push(val);
            return;
        }
        let lookup_name = match sigil.as_str() {
            "$" => name.to_string(),
            "@" => format!("@{}", name),
            "%" => format!("%{}", name),
            _ => name.to_string(),
        };
        let val = self
            .get_env_with_main_alias(&lookup_name)
            .unwrap_or(Value::Nil);
        self.stack.push(val);
    }

    pub(super) fn exec_symbolic_deref_store_op(&mut self, code: &CompiledCode, sigil_idx: u32) {
        let sigil = Self::const_str(code, sigil_idx).to_string();
        let name_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = name_val.to_string_value();
        // Stack: [value] (already below name)
        let raw_value = self.stack.pop().unwrap_or(Value::Nil);
        let store_name = match sigil.as_str() {
            "$" => name.to_string(),
            "@" => format!("@{}", name),
            "%" => format!("%{}", name),
            // Code variables (`&::("infix:<times>")`) must be stored under the
            // `&`-sigilled name so that operator dispatch can find the closure
            // via its `&infix:<...>` env key.
            "&" => format!("&{}", name),
            _ => name.to_string(),
        };
        // For $ sigil (item context), take only first element if value is a list.
        let value = if sigil == "$" {
            match &raw_value {
                Value::Array(items, ..) => items.first().cloned().unwrap_or(Value::Nil),
                _ => raw_value,
            }
        } else {
            raw_value
        };
        self.interpreter
            .env_mut()
            .insert(store_name.clone(), value.clone());
        self.update_local_if_exists(code, &store_name, &value);
        self.stack.push(value);
    }

    pub(super) fn exec_indirect_type_lookup_store_op(&mut self, code: &CompiledCode) {
        let name_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = name_val.to_string_value();
        let value = self.stack.pop().unwrap_or(Value::Nil);
        // ::('$x') stores into the variable named $x.
        // Scalars are stored without the '$' sigil, so strip it.
        // Arrays (@) and hashes (%) are stored with their sigil.
        let store_name = if let Some(bare) = name.strip_prefix('$') {
            bare.to_string()
        } else {
            name.to_string()
        };
        self.interpreter
            .env_mut()
            .insert(store_name.clone(), value.clone());
        self.update_local_if_exists(code, &store_name, &value);
        self.stack.push(value);
    }

    pub(super) fn exec_assign_expr_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // A whole array/hash assign (`@!a = (...)`) always replaces the value, so
        // there is no stale-copy hazard; pass `None` as the pre-snapshot to force
        // the mirror.
        let r = self.exec_assign_expr_op_inner(code, name_idx);
        // Phase 3 Stage 2: mirror name-based attribute writes into the shared cell.
        if r.is_ok()
            && let Value::Str(name) = &code.constants[name_idx as usize]
        {
            if name.starts_with('@') || name.starts_with('%') {
                self.mirror_array_hash_attr_to_cell(code, name_idx, None);
            } else {
                let name = name.to_string();
                self.mirror_attr_value_to_cell_by_name(code, &name);
            }
        }
        r
    }

    fn exec_assign_expr_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = match &code.constants[name_idx as usize] {
            Value::Str(s) => s.to_string(),
            _ => unreachable!("AssignExpr name must be a string constant"),
        };
        self.interpreter.check_readonly_for_modify(&name)?;
        if name.starts_with('%')
            && self
                .interpreter
                .var_type_constraint_fast(&name)
                .and_then(|s| Self::quant_hash_trait_from_constraint(s.as_str()))
                == Some("Mix")
            && self
                .get_env_with_main_alias(&name)
                .is_some_and(|current| !matches!(current, Value::Nil))
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        if name.starts_with('&') && !name.contains("::") {
            let bare = name.trim_start_matches('&');
            let has_variable_slot = self.interpreter.env().contains_key(&name);
            let is_routine_symbol = self.has_function(bare)
                || self.has_multi_function(bare)
                || self.has_proto(bare)
                || self.interpreter.resolve_token_defs(bare).is_some()
                || self.interpreter.has_proto_token(bare);
            if is_routine_symbol && !has_variable_slot {
                return Err(RuntimeError::assignment_ro(None));
            }
        }
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let (raw_val, bind_source) = if let Value::Capture { positional, named } = &raw_val {
            if positional.is_empty() {
                if let (Some(Value::Str(source_name)), Some(inner)) = (
                    named.get("__mutsu_varref_name"),
                    named.get("__mutsu_varref_value"),
                ) {
                    (inner.clone(), Some(source_name.to_string()))
                } else {
                    (raw_val, None)
                }
            } else {
                (raw_val, None)
            }
        } else {
            (raw_val, None)
        };
        // Capture old hash Arc pointer for circular reference fixup.
        let old_hash_arc = if name.starts_with('%') {
            let current = self.get_env_with_main_alias(&name).or_else(|| {
                code.locals.iter().position(|n| n == &name).and_then(|idx| {
                    if idx < self.locals.len() {
                        Some(self.locals[idx].clone())
                    } else {
                        None
                    }
                })
            });
            if let Some(Value::Hash(arc)) = current {
                Some(Arc::as_ptr(&arc) as usize)
            } else {
                None
            }
        } else {
            None
        };
        // Capture old array Arc pointer for circular reference fixup.
        let old_array_arc = if name.starts_with('@') {
            let current = self.get_env_with_main_alias(&name).or_else(|| {
                code.locals.iter().position(|n| n == &name).and_then(|idx| {
                    if idx < self.locals.len() {
                        Some(self.locals[idx].clone())
                    } else {
                        None
                    }
                })
            });
            if let Some(Value::Array(arc, _)) = current {
                Some(Arc::as_ptr(&arc) as usize)
            } else {
                None
            }
        } else {
            None
        };
        let mut val = if name.starts_with('%') {
            let hash_val = runtime::coerce_to_hash(raw_val);
            // Resolve hash sentinel entries (bound variable refs) when assigning
            // to a new hash variable. Assignment creates new containers, so bound
            // refs must be resolved to their current values.
            if let Value::Hash(ref items) = hash_val {
                if Self::hash_has_sentinels(items) {
                    self.resolve_hash_for_iteration(items)
                } else {
                    hash_val
                }
            } else {
                hash_val
            }
        } else if name.starts_with('@') {
            let mut assigned = runtime::coerce_to_array(raw_val);
            // Check for shaped array on current value, and preserve on re-assignment
            let current_val = code
                .locals
                .iter()
                .position(|n| n == &name)
                .and_then(|idx| self.locals.get(idx).cloned())
                .or_else(|| self.get_env_with_main_alias(&name));
            let current_shape = current_val
                .as_ref()
                .and_then(crate::runtime::utils::shaped_array_shape)
                .or_else(|| {
                    // Also check declared shape metadata for multi-dim
                    let key = format!("__mutsu_shaped_array_dims::{}", name);
                    self.interpreter.env().get(&key).and_then(|v| {
                        if let Value::Array(dims, ..) = v {
                            Some(
                                dims.iter()
                                    .filter_map(|d| {
                                        if let Value::Int(n) = d {
                                            Some(*n as usize)
                                        } else {
                                            None
                                        }
                                    })
                                    .collect(),
                            )
                        } else {
                            None
                        }
                    })
                });
            // Only preserve the old shape if the new value is NOT already shaped
            let assigned_has_own_shape = crate::runtime::utils::shaped_array_shape(&assigned)
                .is_some()
                || matches!(&assigned, Value::Array(_, crate::value::ArrayKind::Shaped));
            if let Some(ref shape) = current_shape {
                // For 1D shaped arrays, rebuild with the same shape
                if shape.len() == 1 && !assigned_has_own_shape {
                    let items = runtime::value_to_list(&assigned);
                    let item_count = items.len();
                    let mut shaped_items: Vec<Value> = items.into_iter().take(shape[0]).collect();
                    if item_count < shape[0] {
                        shaped_items.resize(shape[0], Value::Nil);
                    }
                    assigned = Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(shaped_items)),
                        crate::value::ArrayKind::Shaped,
                    );
                    crate::runtime::utils::mark_shaped_array(&assigned, Some(shape));
                    // Preserve container type metadata from old array
                    if let Some(ref cv) = current_val
                        && let Some(info) = self.interpreter.container_type_metadata(cv)
                    {
                        assigned = self.interpreter.tag_container_metadata(assigned, info);
                    }
                }
            }
            if let Some(current) = self.get_env_with_main_alias(&name) {
                let class_name = match &current {
                    Value::Instance { class_name, .. } => Some(*class_name),
                    Value::Package(class_name) => Some(*class_name),
                    _ => None,
                };
                if let Some(class_name) = class_name {
                    let class = class_name.resolve();
                    if class == "Blob" || class.starts_with("blob") {
                        return Err(RuntimeError::assignment_ro(None));
                    }
                    if class == "Buf" || class.starts_with("buf") {
                        let items = runtime::value_to_list(&assigned)
                            .into_iter()
                            .map(|v| Value::Int(runtime::to_int(&v)))
                            .collect::<Vec<_>>();
                        assigned = self.try_compiled_method_or_interpret(
                            Value::Package(class_name),
                            "new",
                            items,
                        )?;
                    }
                }
            }
            assigned
        } else {
            Self::normalize_scalar_assignment_value(raw_val)
        };
        if matches!(val, Value::Nil)
            && let Some(def) = self.interpreter.var_default(&name)
        {
            val = def.clone();
        }
        if self.interpreter.fatal_mode
            && !name.contains("__mutsu_")
            && let Some(err) = self.interpreter.failure_to_runtime_error_if_unhandled(&val)
        {
            return Err(err);
        }
        // When assigning Nil to a typed variable, reset to the type object
        let mut val =
            if matches!(val, Value::Nil) && !name.starts_with('@') && !name.starts_with('%') {
                if let Some(constraint) = self.interpreter.var_type_constraint(&name) {
                    if constraint == "Mu" {
                        val
                    } else {
                        let nominal = self
                            .interpreter
                            .nominal_type_object_name_for_constraint(&constraint);
                        Value::Package(Symbol::intern(&nominal))
                    }
                } else {
                    val
                }
            } else {
                val
            };
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(
            self.interpreter.env().get(&readonly_key),
            Some(Value::Bool(true))
        ) && !matches!(self.interpreter.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        if let Some(source_name) = bind_source {
            let mut resolved_source = source_name;
            let mut seen = std::collections::HashSet::new();
            while seen.insert(resolved_source.clone()) {
                let key = format!("__mutsu_sigilless_alias::{}", resolved_source);
                let Some(Value::Str(next)) = self.interpreter.env().get(&key) else {
                    break;
                };
                resolved_source = next.to_string();
            }
            self.interpreter
                .env_mut()
                .insert(alias_key.clone(), Value::str(resolved_source));
            self.interpreter
                .env_mut()
                .insert(readonly_key, Value::Bool(false));
        }
        // If the current value is a Proxy (in locals or env), invoke STORE instead of overwriting
        {
            let current_proxy = code
                .locals
                .iter()
                .position(|n| n == &name)
                .and_then(|idx| {
                    if idx < self.locals.len() {
                        let v = &self.locals[idx];
                        if matches!(v, Value::Proxy { .. }) {
                            Some(v.clone())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .or_else(|| {
                    self.get_env_with_main_alias(&name).and_then(|v| {
                        if matches!(&v, Value::Proxy { .. }) {
                            Some(v)
                        } else {
                            None
                        }
                    })
                });
            if let Some(Value::Proxy { ref storer, .. }) = current_proxy
                && !matches!(storer.as_ref(), Value::Nil)
            {
                let proxy_val = current_proxy.unwrap();
                self.interpreter
                    .assign_proxy_lvalue(proxy_val, val.clone())?;
                self.stack.push(val);
                return Ok(());
            }
        }
        // Circular hash reference fixup
        if name.starts_with('%') {
            Self::fixup_circular_hash_refs(&mut val, &old_hash_arc);
            // Also update the Dup'd copy on the stack (if any) so that the
            // expression result also has the circular reference.
            if let Some(old_ptr) = old_hash_arc
                && let Some(stack_top) = self.stack.last_mut()
                && let Value::Hash(stack_arc) = stack_top
            {
                let has_old_ref = stack_arc.values().any(|v| {
                    if let Value::Hash(inner_arc) = v {
                        Arc::as_ptr(inner_arc) as usize == old_ptr
                    } else {
                        false
                    }
                });
                if has_old_ref {
                    *stack_top = val.clone();
                }
            }
        }
        // Circular array reference fixup
        if name.starts_with('@') {
            Self::fixup_circular_array_refs(&mut val, &old_array_arc);
            if let Some(old_ptr) = old_array_arc
                && let Some(stack_top) = self.stack.last_mut()
                && let Value::Array(stack_arc, _) = stack_top
            {
                let has_old_ref = stack_arc.iter().any(|v| {
                    if let Value::Array(inner_arc, _) = v {
                        Arc::as_ptr(inner_arc) as usize == old_ptr
                    } else {
                        false
                    }
                });
                if has_old_ref {
                    *stack_top = val.clone();
                }
            }
        }
        self.update_local_if_exists(code, &name, &val);
        self.set_env_with_main_alias(&name, val.clone());
        // Persist anonymous state variable (`$`) across closure calls.
        self.sync_anon_state_value(&name, &val);
        // Track topic mutations for map rw writeback: when `$_` (= "_") is
        // explicitly assigned, record the value so `eval_map_over_items_rw` can
        // read it back even after the block return value overwrites `_`.
        if name == "_" {
            self.interpreter
                .env_mut()
                .insert("__mutsu_rw_map_topic__".to_string(), val.clone());
        }
        let mut alias_name = self.interpreter.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.to_string())
            } else {
                None
            }
        });
        let mut seen_aliases = std::collections::HashSet::new();
        while let Some(current_alias) = alias_name {
            if !seen_aliases.insert(current_alias.clone()) {
                break;
            }
            self.update_local_if_exists(code, &current_alias, &val);
            self.interpreter
                .env_mut()
                .insert(current_alias.clone(), val.clone());
            let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
            alias_name = self.interpreter.env().get(&next_key).and_then(|v| {
                if let Value::Str(name) = v {
                    Some(name.to_string())
                } else {
                    None
                }
            });
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.interpreter
                .env_mut()
                .insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.interpreter
                .env_mut()
                .insert(format!(".{}", attr), val.clone());
        }
        if name == "_"
            && let Some(ref source_var) = self.topic_source_var
            && !source_var.starts_with('@')
            && !source_var.starts_with('%')
        {
            let sv = source_var.clone();
            self.set_env_with_main_alias(&sv, val.clone());
            self.update_local_if_exists(code, &sv, &val);
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_wrap_var_ref_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let value = self.stack.pop().unwrap_or(Value::Nil);
        let name = Self::const_str(code, name_idx).to_string();
        let mut named = std::collections::HashMap::new();
        named.insert("__mutsu_varref_name".to_string(), Value::str(name));
        named.insert("__mutsu_varref_value".to_string(), value);
        self.stack.push(Value::Capture {
            positional: Vec::new(),
            named,
        });
    }

    pub(super) fn exec_get_env_index_op(&mut self, code: &CompiledCode, key_idx: u32) {
        let key = Self::const_str(code, key_idx);
        let val = if let Some(Value::Hash(env_hash)) = self.interpreter.env().get("%*ENV") {
            env_hash.get(key).cloned().unwrap_or_else(|| {
                std::env::var_os(key)
                    .map(|v| {
                        crate::runtime::builtins_collection::builtin_val(&[Value::str(
                            v.to_string_lossy().to_string(),
                        )])
                    })
                    .unwrap_or(Value::Nil)
            })
        } else if let Some(value) = std::env::var_os(key) {
            crate::runtime::builtins_collection::builtin_val(&[Value::str(
                value.to_string_lossy().to_string(),
            )])
        } else {
            Value::Nil
        };
        self.stack.push(val);
    }

    pub(super) fn exec_exists_env_index_op(&mut self, code: &CompiledCode, key_idx: u32) {
        let key = Self::const_str(code, key_idx);
        let exists = if let Some(Value::Hash(env_hash)) = self.interpreter.env().get("%*ENV") {
            env_hash.contains_key(key) || std::env::var_os(key).is_some()
        } else {
            std::env::var_os(key).is_some()
        };
        self.stack.push(Value::Bool(exists));
    }

    pub(super) fn exec_exists_expr_op(&mut self) {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        self.stack.push(Value::Bool(val.truthy()));
    }

    pub(super) fn exec_reduction_op(
        &mut self,
        code: &CompiledCode,
        op_idx: u32,
    ) -> Result<(), RuntimeError> {
        let op = Self::const_str(code, op_idx).to_string();
        // Support scan/meta reduction [\op] and negated forms like [!after].
        let (scan, op_no_scan) = if let Some(stripped) = op.strip_prefix('\\') {
            (true, stripped.to_string())
        } else {
            (false, op.clone())
        };
        // Only treat '!' as negation prefix when the remaining part is a known
        // operator (e.g. [!after], [!==], [!eqv]).  Operators like '!=' are their
        // own base operators and must not be split.
        const KNOWN_BASE_OPS: &[&str] = &[
            "+", "-", "*", "/", "%", "~", "||", "&&", "//", "%%", "**", "^^", "+&", "+|", "+^",
            "+<", "+>", "~&", "~|", "~^", "~<", "~>", "?&", "?|", "?^", "==", "!=", "<", ">", "<=",
            ">=", "<=>", "===", "=:=", "!=:=", "=>", "eqv", "eq", "ne", "lt", "gt", "le", "ge",
            "leg", "cmp", "~~", "min", "max", "gcd", "lcm", "and", "or", "not", "andthen",
            "orelse", "xor", "minmax", ",", "after", "before", "X", "Z", "x", "xx", "&", "|", "^",
            "o", "∘", "(-)", "∖", "(|)", "∪", "(&)", "∩", "(^)", "⊖", "(.)", "⊍", "(==)", "≡", "≢",
        ];
        let (negate, base_op) = if let Some(stripped) = op_no_scan.strip_prefix('!')
            && KNOWN_BASE_OPS.contains(&stripped)
        {
            (true, stripped.to_string())
        } else {
            (false, op_no_scan)
        };
        let base_op = if base_op == "∘" {
            "o".to_string()
        } else {
            base_op
        };
        // Handle lazy-scan short-circuit reduction compiled from ArrayLiteral.
        // The operator has prefix "_sc_" and the operand is an array of thunks.
        if let Some(sc_op) = base_op.strip_prefix("_sc_") {
            let list_value = self.stack.pop().unwrap_or(Value::Nil);
            let thunks: Vec<Value> = runtime::value_to_list(&list_value);
            return self.exec_scan_shortcircuit_reduction(sc_op, negate, scan, thunks);
        }
        let list_value = self.stack.pop().unwrap_or(Value::Nil);
        let input_is_lazy = crate::builtins::methods_0arg::is_value_lazy(&list_value);
        // For scan (triangle reduce) on infinite/lazy inputs, handle lazily
        // to avoid materializing the entire infinite range.
        if scan && input_is_lazy {
            return self.exec_lazy_scan_reduction(&base_op, negate, &list_value);
        }
        let mut list = if let Value::LazyList(ref ll) = list_value {
            self.force_lazy_list_vm(ll)?
        } else {
            runtime::value_to_list(&list_value)
        };
        if list.iter().any(|v| matches!(v, Value::Slip(_))) {
            let mut flattened = Vec::new();
            for item in list {
                if let Value::Slip(items) = item {
                    flattened.extend(items.iter().cloned());
                } else {
                    flattened.push(item);
                }
            }
            list = flattened;
        }
        // Reduction is list-contextual; when the operand itself is a single list-like
        // value, flatten that one value into reduction elements.
        if list.len() == 1 {
            let only = list.remove(0);
            list = match only {
                Value::Array(items, kind) if !kind.is_itemized() => items.iter().cloned().collect(),
                Value::Seq(items) => items.iter().cloned().collect(),
                Value::LazyList(ll) => {
                    if let Ok(items) = self.force_lazy_list_vm(&ll) {
                        items
                    } else {
                        vec![Value::LazyList(ll)]
                    }
                }
                other => vec![other],
            };
        }
        if base_op == "," {
            if scan {
                let mut out = Vec::with_capacity(list.len());
                let mut prefix = Vec::new();
                for item in list {
                    prefix.push(item);
                    out.push(Value::array(prefix.clone()));
                }
                self.stack.push(Value::Seq(std::sync::Arc::new(out)));
            } else {
                self.stack.push(Value::array(list));
            }
            return Ok(());
        }
        let callable = self.reduction_callable_for_op(&base_op);
        let arity = callable
            .as_ref()
            .map(|c| self.reduction_callable_arity(c))
            .unwrap_or(2);
        let step = arity.saturating_sub(1).max(1);
        let assoc = if base_op == "=>" {
            ReductionAssoc::Right
        } else if runtime::is_chain_comparison_op(&base_op) {
            ReductionAssoc::Chain
        } else {
            self.reduction_op_associativity(&base_op)
        };

        if scan {
            if list.is_empty() {
                self.stack.push(Value::Seq(std::sync::Arc::new(Vec::new())));
                return Ok(());
            }
            if list.len() == 1 {
                let is_chain = runtime::is_chain_comparison_op(&base_op);
                let val = if is_chain {
                    Value::Bool(true)
                } else {
                    list[0].clone()
                };
                self.stack.push(Value::Seq(std::sync::Arc::new(vec![val])));
                return Ok(());
            }
            let out = match assoc {
                ReductionAssoc::Right => {
                    let mut out = Vec::new();
                    let mut acc = list.last().cloned().unwrap_or(Value::Nil);
                    out.push(acc.clone());
                    let mut right_edge = list.len().saturating_sub(1);
                    while right_edge >= step {
                        let start = right_edge - step;
                        let mut call_args = list[start..right_edge].to_vec();
                        call_args.push(acc);
                        let v =
                            self.reduction_step_with_args(&base_op, callable.as_ref(), call_args)?;
                        acc = if negate { Value::Bool(!v.truthy()) } else { v };
                        out.push(acc.clone());
                        right_edge = start;
                    }
                    out
                }
                _ => {
                    let is_chain = runtime::is_chain_comparison_op(&base_op);
                    let is_xor = matches!(base_op.as_str(), "^^" | "xor");
                    let mut out = Vec::new();
                    if is_chain {
                        // Chain comparison scan: first element is always True
                        // (vacuously true), then each subsequent element is the
                        // AND of all pairwise comparisons so far.
                        out.push(Value::Bool(true));
                        let mut all_true = true;
                        for i in 0..list.len() - 1 {
                            if all_true {
                                let v = self.eval_reduction_operator_values(
                                    &base_op,
                                    &list[i],
                                    &list[i + 1],
                                )?;
                                let truthy = if negate { !v.truthy() } else { v.truthy() };
                                all_true = truthy;
                            }
                            out.push(Value::Bool(all_true));
                        }
                    } else if is_xor {
                        // [\^^] and [\xor] scan: each element is the xor-reduce
                        // of the prefix up to that point.
                        let mut found: Option<Value> = None;
                        let mut multiple = false;
                        for item in &list {
                            if item.truthy() {
                                if found.is_some() {
                                    multiple = true;
                                }
                                if !multiple {
                                    found = Some(item.clone());
                                }
                            }
                            let result = if multiple {
                                Value::Nil
                            } else if let Some(ref v) = found {
                                v.clone()
                            } else {
                                item.clone()
                            };
                            out.push(result);
                        }
                    } else {
                        // Special case for Z/X meta-operators (e.g. [\Z~], [\X~])
                        // when scan elements are themselves lists. In this case a
                        // simple left-fold loses structure, so we re-apply the
                        // operator to the entire prefix at each step.
                        // E.g. [\Z~](<a b c>, <1 2 3>):
                        //   step 1 = [Z~](<a b c>)       = ("abc",)
                        //   step 2 = [Z~](<a b c>,<1 2 3>) = ("a1","b2","c3")
                        let is_multi_list_zx = callable.is_none()
                            && list
                                .iter()
                                .any(|v| matches!(v, Value::Array(_, _) | Value::Seq(_)))
                            && ((base_op.starts_with('Z') && base_op.len() > 1)
                                || (base_op.starts_with('X') && base_op.len() > 1));
                        if is_multi_list_zx {
                            // Compute prefix reductions from scratch at each step
                            for i in 0..list.len() {
                                let v = if i == 0 {
                                    // Single-element: apply inner_op as a left-fold
                                    // over the first list's elements, wrapped in Seq.
                                    let inner_op = &base_op[1..];
                                    let items = runtime::value_to_list(&list[0]);
                                    if items.is_empty() {
                                        Value::Seq(std::sync::Arc::new(vec![]))
                                    } else {
                                        let mut acc0 = items[0].clone();
                                        for item in items.iter().skip(1) {
                                            acc0 = self.eval_reduction_operator_values(
                                                inner_op, &acc0, item,
                                            )?;
                                        }
                                        Value::Seq(std::sync::Arc::new(vec![acc0]))
                                    }
                                } else {
                                    // Apply the Z/X op to all prefix elements
                                    let mut acc0 = list[0].clone();
                                    for item in list.iter().take(i + 1).skip(1) {
                                        acc0 = self.eval_reduction_operator_values(
                                            &base_op, &acc0, item,
                                        )?;
                                    }
                                    acc0
                                };
                                let result = if negate { Value::Bool(!v.truthy()) } else { v };
                                out.push(result);
                            }
                        } else {
                            let mut acc = list[0].clone();
                            // For certain operators, the first scan element should be
                            // the result of applying [op] to a single element, not the
                            // element itself:
                            //   Z~/X~ meta-operators: [Z~]("a") = ("a",) not "a"
                            //   minmax: [minmax](x) = x..x not x
                            if callable.is_none() {
                                let zx_prefix = (base_op.starts_with('Z') && base_op.len() > 1)
                                    || (base_op.starts_with('X') && base_op.len() > 1);
                                if zx_prefix {
                                    acc = Value::Seq(std::sync::Arc::new(vec![acc]));
                                } else if base_op == "minmax" {
                                    // [minmax](x) = x..x for scalars,
                                    // or min(x)..max(x) for array/list x.
                                    let (lo, hi) = minmax_bounds_of_value(&acc);
                                    acc = match (&lo, &hi) {
                                        (Value::Int(l), Value::Int(h)) => Value::Range(*l, *h),
                                        _ => Value::GenericRange {
                                            start: std::sync::Arc::new(lo),
                                            end: std::sync::Arc::new(hi),
                                            excl_start: false,
                                            excl_end: false,
                                        },
                                    };
                                }
                            }
                            out.push(acc.clone());
                            let mut idx = 1usize;
                            while idx + step <= list.len() {
                                let mut call_args = vec![acc];
                                call_args.extend(list[idx..idx + step].iter().cloned());
                                let v = self.reduction_step_with_args(
                                    &base_op,
                                    callable.as_ref(),
                                    call_args,
                                )?;
                                acc = if negate { Value::Bool(!v.truthy()) } else { v };
                                out.push(acc.clone());
                                idx += step;
                            }
                        }
                    }
                    out
                }
            };
            self.stack.push(Value::Seq(std::sync::Arc::new(out)));
            return Ok(());
        }
        // [^^] and [xor] are list-associative: they check that exactly one element
        // is truthy.  Returns:
        //   - the truthy value if exactly one is truthy
        //   - Nil if more than one is truthy (short-circuits)
        //   - the last element if all are falsy
        if matches!(base_op.as_str(), "^^" | "xor") {
            if list.is_empty() {
                self.stack.push(runtime::reduction_identity(&base_op));
                return Ok(());
            }
            let mut found: Option<Value> = None;
            let mut multiple = false;
            let mut last = Value::Nil;
            for item in &list {
                last = item.clone();
                if item.truthy() {
                    if found.is_some() {
                        multiple = true;
                        break;
                    }
                    found = Some(item.clone());
                }
            }
            let result = if multiple {
                Value::Nil
            } else if let Some(v) = found {
                v
            } else {
                last
            };
            self.stack.push(result);
            return Ok(());
        }
        // For set operators, promote all elements to the highest set type before reducing.
        // In Raku, [(-)] [Set, Set, Mix] first promotes all to Mix, then reduces.
        if matches!(
            base_op.as_str(),
            "(-)" | "∖" | "(|)" | "∪" | "(&)" | "∩" | "(^)" | "⊖" | "(.)" | "⊍" | "(+)" | "⊎"
        ) && list.len() > 2
        {
            let set_level = |v: &Value| -> u8 {
                match v {
                    Value::Mix(_, _) => 2,
                    Value::Bag(_, _) => 1,
                    _ => 0,
                }
            };
            let max_level = list.iter().map(&set_level).max().unwrap_or(0);
            if max_level > 0 {
                for item in &mut list {
                    let level = set_level(item);
                    if level < max_level {
                        let promoted = match max_level {
                            2 => self
                                .try_compiled_method_or_interpret(item.clone(), "Mix", vec![])
                                .unwrap_or_else(|_| item.clone()),
                            1 => self
                                .try_compiled_method_or_interpret(item.clone(), "Bag", vec![])
                                .unwrap_or_else(|_| item.clone()),
                            _ => item.clone(),
                        };
                        *item = promoted;
                    }
                }
            }
        }
        // Multi-arg symmetric difference is NOT a left-fold.
        // For each key, the result weight = max_weight - second_max_weight.
        if matches!(base_op.as_str(), "(^)" | "⊖") && list.len() > 2 {
            self.stack.push(runtime::set_sym_diff_multi(&list));
            return Ok(());
        }
        if list.is_empty() {
            self.stack.push(runtime::reduction_identity(&base_op));
        } else {
            // Chain-associative operators (built-in comparisons and user-defined
            // `is assoc<chain>` infixes) reduce as a conjunction of pairwise
            // applications, not a left-fold.
            let is_comparison =
                runtime::is_chain_comparison_op(&base_op) || matches!(assoc, ReductionAssoc::Chain);
            if is_comparison {
                let mut result = true;
                for i in 0..list.len() - 1 {
                    let v =
                        self.eval_reduction_operator_values(&base_op, &list[i], &list[i + 1])?;
                    let truthy = if negate { !v.truthy() } else { v.truthy() };
                    if !truthy {
                        result = false;
                        break;
                    }
                }
                self.stack.push(Value::Bool(result));
            } else {
                if base_op == "o" {
                    let mut acc = list[0].clone();
                    for item in &list[1..] {
                        acc = self.interpreter.compose_callables(acc, item.clone());
                    }
                    self.stack.push(acc);
                    return Ok(());
                }
                // Single-element reduction with a numeric/coercing operator:
                // apply op(identity, element) so that coercions (e.g. numification
                // for `+`) happen and type errors (e.g. X::Str::Numeric for
                // `[+] "hello"`) are raised.  This matches Raku semantics where
                // `[+] "2"` returns Int 2 (not Str "2").
                if list.len() == 1
                    && callable.is_none()
                    && matches!(
                        base_op.as_str(),
                        "+" | "-" | "*" | "/" | "%" | "**" | "+|" | "+&" | "+^"
                    )
                {
                    // Validate: non-numeric strings must throw X::Str::Numeric
                    if let Value::Str(ref s) = list[0]
                        && crate::runtime::str_numeric::parse_raku_str_to_numeric(s).is_none()
                    {
                        return Err(RuntimeError::str_numeric(
                            s,
                            "base-10 number must begin with valid digits or '.'",
                        ));
                    }
                    let identity = runtime::reduction_identity(&base_op);
                    // Coerce Instance values via Numeric()/Bridge() so that
                    // user-defined numeric types work (e.g. `[*] CustomNumify.new`).
                    let elem = self.coerce_numeric_bridge_value(list[0].clone())?;
                    let v = self.reduction_step_with_args(&base_op, None, vec![identity, elem])?;
                    let result = if negate { Value::Bool(!v.truthy()) } else { v };
                    self.stack.push(result);
                    return Ok(());
                }
                let acc = match assoc {
                    ReductionAssoc::Right => {
                        let mut acc = list.last().cloned().unwrap_or(Value::Nil);
                        let mut right_edge = list.len().saturating_sub(1);
                        while right_edge >= step {
                            let start = right_edge - step;
                            let mut call_args = list[start..right_edge].to_vec();
                            call_args.push(acc);
                            let v = self.reduction_step_with_args(
                                &base_op,
                                callable.as_ref(),
                                call_args,
                            )?;
                            acc = if negate { Value::Bool(!v.truthy()) } else { v };
                            right_edge = start;
                        }
                        acc
                    }
                    _ => {
                        let mut acc = list[0].clone();
                        let mut idx = 1usize;
                        while idx + step <= list.len() {
                            let mut call_args = vec![acc];
                            call_args.extend(list[idx..idx + step].iter().cloned());
                            let v = self.reduction_step_with_args(
                                &base_op,
                                callable.as_ref(),
                                call_args,
                            )?;
                            acc = if negate { Value::Bool(!v.truthy()) } else { v };
                            idx += step;
                        }
                        acc
                    }
                };
                self.stack.push(acc);
            }
        }
        Ok(())
    }

    /// Execute a thunk-based short-circuit reduction where the operand is
    /// an array of thunks (anonymous blocks).  Each thunk is called lazily:
    /// for `&&`/`and`:    evaluation stops on the first false result;
    /// for `||`/`or`:     evaluation stops on the first truthy result;
    /// for `//`/`orelse`: evaluation stops on the first defined result;
    /// for `andthen`:     evaluation stops on the first undefined result;
    /// for `^^`/`xor`:    evaluation stops after two truthy values are found.
    ///
    /// `scan` controls whether intermediate results are collected (triangle
    /// reduction `[\op]`) or only the final value is returned (`[op]`).
    fn exec_scan_shortcircuit_reduction(
        &mut self,
        op: &str,
        _negate: bool,
        scan: bool,
        thunks: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        if thunks.is_empty() {
            if scan {
                self.stack.push(Value::Seq(std::sync::Arc::new(Vec::new())));
            } else {
                // Identity values
                let identity = match op {
                    "&&" | "and" => Value::Bool(true),
                    _ => Value::Bool(false),
                };
                self.stack.push(identity);
            }
            return Ok(());
        }

        let is_xor = matches!(op, "^^" | "xor");

        if is_xor {
            // xor/^^ is list-associative: exactly one element must be truthy.
            // Short-circuit once two truthy values are found.
            let mut found: Option<Value> = None;
            let mut multiple = false;
            let mut last_val = Value::Nil; // track last evaluated value for all-false case
            let mut results: Vec<Value> = Vec::new();
            let mut done = false; // whether we have short-circuited
            for thunk in thunks {
                if done {
                    // Already short-circuited: result is Nil for all remaining
                    if scan {
                        results.push(Value::Nil);
                    }
                    // don't evaluate thunk
                } else {
                    let val = self.vm_call_on_value(thunk, vec![], None)?;
                    last_val = val.clone();
                    if val.truthy() {
                        if found.is_some() {
                            // Second truthy: result is Nil, short-circuit
                            multiple = true;
                            found = None;
                            done = true;
                            if scan {
                                results.push(Value::Nil);
                            }
                        } else {
                            found = Some(val.clone());
                            if scan {
                                results.push(val);
                            }
                        }
                    } else {
                        // Falsy: current xor result is the truthy one found so far,
                        // or val itself if none found yet
                        let cur = if let Some(ref f) = found {
                            f.clone()
                        } else {
                            val
                        };
                        if scan {
                            results.push(cur);
                        }
                    }
                }
            }
            let final_result = if multiple {
                Value::Nil
            } else if let Some(v) = found {
                v
            } else {
                // All false: return last element
                last_val
            };
            if scan {
                self.stack.push(Value::Seq(std::sync::Arc::new(results)));
            } else {
                self.stack.push(final_result);
            }
            return Ok(());
        }

        // Evaluate the first thunk unconditionally.
        let mut acc = self.vm_call_on_value(thunks[0].clone(), vec![], None)?;
        let mut results: Vec<Value> = if scan { vec![acc.clone()] } else { Vec::new() };
        // For the remaining thunks, check acc before evaluating.
        for thunk in thunks.into_iter().skip(1) {
            let should_short_circuit = match op {
                "&&" | "and" => !acc.truthy(),
                "||" | "or" => acc.truthy(),
                "//" | "orelse" => runtime::types::value_is_defined(&acc),
                "andthen" => !runtime::types::value_is_defined(&acc),
                _ => false,
            };
            if should_short_circuit {
                // For `andthen`: once acc is undefined, drop remaining elements from scan.
                // For all other operators (&&/||/and/or/orelse), repeat the accumulated value.
                if scan && op != "andthen" {
                    results.push(acc.clone());
                }
                // don't evaluate thunk
            } else {
                let val = self.vm_call_on_value(thunk, vec![], None)?;
                acc = match op {
                    "&&" | "and" => {
                        if acc.truthy() {
                            val
                        } else {
                            acc
                        }
                    }
                    "||" | "or" => {
                        if acc.truthy() {
                            acc
                        } else {
                            val
                        }
                    }
                    "//" | "orelse" => {
                        if runtime::types::value_is_defined(&acc) {
                            acc
                        } else {
                            val
                        }
                    }
                    "andthen" => {
                        if runtime::types::value_is_defined(&acc) {
                            val
                        } else {
                            Value::Slip(std::sync::Arc::new(vec![]))
                        }
                    }
                    _ => val,
                };
                if scan {
                    results.push(acc.clone());
                }
            }
        }
        if scan {
            self.stack.push(Value::Seq(std::sync::Arc::new(results)));
        } else {
            self.stack.push(acc);
        }
        Ok(())
    }

    /// Lazily evaluate a triangle (scan) reduction on an infinite/lazy range.
    /// Produces a `LazyList` with a `ScanSpec` so that additional elements
    /// can be computed on demand. Pre-computes an initial batch of elements
    /// so that common operations like `.Slip` and `.join` work immediately.
    fn exec_lazy_scan_reduction(
        &mut self,
        base_op: &str,
        negate: bool,
        list_value: &Value,
    ) -> Result<(), RuntimeError> {
        let spec = crate::value::ScanSpec {
            op: base_op.to_string(),
            negate,
            source: list_value.clone(),
            accumulator: None,
            computed_count: 0,
        };
        let ll = crate::value::LazyList::new_scan(spec);
        let ll = std::sync::Arc::new(ll);
        // Pre-compute an initial batch so that eager consumers (e.g. .Slip,
        // .join) that read the cache directly get a useful prefix.
        const INITIAL_BATCH: usize = 1_000;
        self.force_scan_lazy_list(&ll, INITIAL_BATCH)?;
        self.stack.push(Value::LazyList(ll));
        Ok(())
    }

    pub(super) fn exec_routine_magic_op(&mut self) -> Result<(), RuntimeError> {
        // Skip pointy-block entries in the routine stack so that &?ROUTINE
        // inside a pointy block sees the enclosing routine (sub/method).
        let routine_stack = self.interpreter.routine_stack();
        let entry = routine_stack
            .iter()
            .rev()
            .find(|frame| frame.name != "<pointy-block>");
        if let Some(frame) = entry {
            // Anonymous subs are pushed with "<anon>" as the sentinel name.
            // Return the block_stack Sub directly so callers can invoke it.
            if frame.name.is_empty() || frame.name == "<anon>" {
                if let Some(val) = self.interpreter.block_stack_top().cloned()
                    && matches!(val, Value::Sub(_))
                {
                    self.stack.push(val);
                    return Ok(());
                }
                return Err(RuntimeError::undeclared_symbols("Undeclared name"));
            }
            self.stack.push(Value::Routine {
                package: Symbol::intern(&frame.package),
                name: Symbol::intern(&frame.name),
                is_regex: false,
            });
        } else {
            return Err(RuntimeError::undeclared_symbols("Undeclared name"));
        }
        Ok(())
    }

    pub(super) fn exec_block_magic_op(&mut self) -> Result<(), RuntimeError> {
        if let Some(val) = self.interpreter.block_stack_top().cloned() {
            if matches!(val, Value::Sub(_)) {
                self.stack.push(val);
            } else {
                return Err(RuntimeError::undeclared_symbols("Undeclared name"));
            }
        } else {
            return Err(RuntimeError::undeclared_symbols("Undeclared name"));
        }
        Ok(())
    }

    pub(super) fn exec_take_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        if self.interpreter.gather_items_len() > 0 {
            self.interpreter.take_value(val)
        } else {
            // No enclosing gather — raise a CX::Take control exception so a
            // CONTROL block can observe it. If unhandled, the runtime wraps
            // it as X::ControlFlow with illegal=>"take", enclosing=>"gather".
            Err(RuntimeError::take_signal(val))
        }
    }

    pub(super) fn exec_package_scope_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let body_end = body_end as usize;
        let saved = self.current_package().to_string();
        let saved_env = self.interpreter.env().clone();
        let saved_locals = self.locals.clone();
        self.set_current_package(name);
        self.run_range(code, *ip + 1, body_end, compiled_fns)?;
        self.set_current_package(saved);
        let current_env = self.interpreter.env().clone();
        let mut restored_env = saved_env.clone();
        for (k, v) in current_env.iter() {
            if saved_env.contains_key_sym(*k) || k.contains_str("::") {
                restored_env.insert_sym(*k, v.clone());
            }
        }
        self.locals = saved_locals;
        for (idx, local_name) in code.locals.iter().enumerate() {
            if let Some(val) = restored_env.get(local_name).cloned() {
                self.locals[idx] = val;
            }
        }
        *self.interpreter.env_mut() = restored_env;
        *ip = body_end;
        Ok(())
    }

    pub(super) fn exec_phaser_end_op(&mut self, code: &CompiledCode, idx: u32, site_id: u64) {
        // Only register each END phaser once (by site_id), even if the
        // opcode is encountered multiple times inside a repeatedly-called closure.
        if !self.interpreter.register_end_phaser_site(site_id) {
            return;
        }
        let stmt = &code.stmt_pool[idx as usize];
        if let crate::ast::Stmt::Phaser { body, .. } = stmt {
            self.interpreter.push_end_phaser(body.clone());
        }
    }

    pub(super) fn exec_type_check_op(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        self.exec_type_check_op_inner(code, tc_idx, var_name_idx, false)
    }

    /// Type check for `:=` binds to a typed scalar; raises X::TypeCheck::Binding
    /// on mismatch instead of X::TypeCheck::Assignment.
    pub(super) fn exec_type_check_bind_op(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        self.exec_type_check_op_inner(code, tc_idx, var_name_idx, true)
    }

    pub(super) fn exec_type_check_op_inner(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name_idx: Option<u32>,
        bind_mode: bool,
    ) -> Result<(), RuntimeError> {
        let raw_constraint = Self::const_str(code, tc_idx);
        let var_name: Option<&str> = var_name_idx.map(|idx| Self::const_str(code, idx));
        // Apply `use variables :D/:U` pragma to the constraint
        let effective_constraint = self.interpreter.apply_variables_pragma(raw_constraint);
        let constraint: &str = &effective_constraint;
        let (base_constraint, _) = crate::runtime::types::strip_type_smiley(constraint);
        let declared_constraint = base_constraint
            .split_once('(')
            .map_or(base_constraint, |(target, _)| target);
        let value = self.stack.last().expect("TypeCheck: empty stack").clone();
        if var_name.is_some_and(|name| name.starts_with('%')) {
            return Ok(());
        }
        // Lazy values cannot be stored in native typed arrays
        if var_name.is_some_and(|name| name.starts_with('@'))
            && crate::runtime::native_types::is_native_array_element_type(base_constraint)
            && crate::builtins::methods_0arg::is_value_lazy(&value)
        {
            let declared = format!("array[{}]", base_constraint);
            return Err(RuntimeError::typed(
                "X::Cannot::Lazy",
                [
                    (
                        "message".to_string(),
                        Value::str(format!("Cannot store a lazy list onto a {}", declared)),
                    ),
                    ("action".to_string(), Value::str_from("store")),
                ]
                .into_iter()
                .collect(),
            ));
        }
        if var_name.is_some_and(|name| name.starts_with('@'))
            && matches!(&value, Value::Array(..) | Value::Seq(_))
        {
            if !self.array_elements_match_constraint(constraint, &value) {
                return Err(self.typed_array_element_error(
                    var_name,
                    base_constraint,
                    constraint,
                    &value,
                ));
            }
            return Ok(());
        }
        // When the constraint is a container type (List, Array, Positional, Seq, Cool, Any, Mu),
        // an Array value directly satisfies it — do NOT descend into element-level matching.
        // Element-level matching is for declarations like `my Int @x = 1, 2, 3`.
        // Also skip element-level matching for subset types whose base type is a container type
        // (e.g., `subset NumArray of Array where { ... }`).
        let is_container_constraint = matches!(
            declared_constraint,
            "List" | "Array" | "Positional" | "Seq" | "Cool" | "Any" | "Mu" | "Iterable"
        ) || {
            let ultimate_base = self
                .interpreter
                .resolve_subset_base_type(declared_constraint);
            matches!(
                ultimate_base.as_str(),
                "List"
                    | "Array"
                    | "Positional"
                    | "Seq"
                    | "Cool"
                    | "Any"
                    | "Mu"
                    | "Iterable"
                    | "Hash"
                    | "Map"
                    | "Pair"
            )
        };
        if let Value::Array(..) = &value
            && !is_container_constraint
        {
            if !self.array_elements_match_constraint(constraint, &value) {
                return Err(self.typed_array_element_error(
                    var_name,
                    base_constraint,
                    constraint,
                    &value,
                ));
            }
            return Ok(());
        }
        // For finite Range values assigned to typed arrays (my Int @a = ^5),
        // check if the range elements match the element type constraint.
        // Integer ranges always contain Int elements.
        {
            let is_finite_int_range = matches!(
                &value,
                Value::Range(a, b) | Value::RangeExcl(a, b) |
                Value::RangeExclStart(a, b) | Value::RangeExclBoth(a, b)
                if *b != i64::MAX && *a != i64::MIN
            );
            let is_finite_generic_range = matches!(
                &value,
                Value::GenericRange { end, .. }
                if !matches!(end.as_ref(), Value::Num(n) if n.is_infinite())
                    && !matches!(end.as_ref(), Value::Whatever | Value::HyperWhatever)
            );
            let range_ok = if is_finite_int_range {
                self.interpreter
                    .type_matches_value(constraint, &Value::Int(0))
            } else if is_finite_generic_range {
                match &value {
                    Value::GenericRange { start, end, .. } => {
                        self.interpreter.type_matches_value(constraint, start)
                            && self.interpreter.type_matches_value(constraint, end)
                    }
                    _ => false,
                }
            } else {
                false
            };
            if range_ok {
                return Ok(());
            }
            // Infinite ranges and non-matching types fall through to normal check
        }
        if matches!(value, Value::Nil) && self.interpreter.is_definite_constraint(constraint) {
            return Err(RuntimeError::new(format!(
                "X::Syntax::Variable::MissingInitializer: Variable definition of type {} needs to be given an initializer",
                constraint
            )));
        }
        // Native integer type check: validate value is an integer in range.
        // Native types cannot hold Nil/type objects — reject them.
        if crate::runtime::native_types::is_native_int_type(base_constraint) {
            if matches!(value, Value::Nil) {
                return Err(RuntimeError::new(format!(
                    "Cannot unbox a type object (Nil) to {}.",
                    base_constraint
                )));
            }
            self.validate_native_int_assignment(base_constraint, &value)?;
            return Ok(());
        }
        // Native num/str types cannot hold type objects — reject Nil and Package values.
        if matches!(base_constraint, "num" | "num32" | "num64" | "str") {
            if matches!(value, Value::Nil | Value::Package(_)) {
                return Err(RuntimeError::new(format!(
                    "Cannot unbox a type object to {}.",
                    base_constraint
                )));
            }
            return Ok(());
        }
        if runtime::is_known_type_constraint(base_constraint) {
            if !matches!(value, Value::Nil)
                && !self.interpreter.type_matches_value(constraint, &value)
            {
                if base_constraint == "Int"
                    && matches!(value, Value::Num(f) if f.is_nan() || f.is_infinite())
                {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("value".to_string(), value.clone());
                    attrs.insert(
                        "vartype".to_string(),
                        Value::Package(Symbol::intern(base_constraint)),
                    );
                    let desc = if matches!(value, Value::Num(f) if f.is_nan()) {
                        "Cannot convert NaN to Int"
                    } else {
                        "Cannot assign a literal of type Num (Inf) to a variable of type Int"
                    };
                    attrs.insert("message".to_string(), Value::str(desc.to_string()));
                    return Err(RuntimeError::typed("X::Syntax::Number::LiteralType", attrs));
                }
                let coerced = match base_constraint {
                    "Str" => Some(Value::str(crate::runtime::utils::coerce_to_str(&value))),
                    _ => None,
                };
                if let Some(new_val) = coerced {
                    *self.stack.last_mut().unwrap() = new_val;
                } else {
                    // When assigning an unhandled Failure to a typed variable,
                    // explode the Failure first (Raku behavior)
                    if let Some(err) = self
                        .interpreter
                        .failure_to_runtime_error_if_unhandled(&value)
                    {
                        return Err(err);
                    }
                    if bind_mode {
                        return Err(crate::runtime::utils::type_check_binding_typed_error(
                            base_constraint,
                            &value,
                        ));
                    }
                    return Err(RuntimeError::typecheck_assignment(
                        base_constraint,
                        &value,
                        var_name,
                    ));
                }
            }
        } else if !self.interpreter.has_type(declared_constraint)
            && !is_core_raku_type(declared_constraint)
            && !self
                .interpreter
                .has_type_capture_binding(declared_constraint)
        {
            // Check if this is a suppressed nested class name that can be resolved
            if self
                .interpreter
                .resolve_suppressed_type(declared_constraint)
                .is_none()
            {
                // Unknown user-defined type — reject it.
                // In Raku this is a compile-time failure grouped into an
                // X::Comp::Group whose `.sorrows` holds the underlying
                // X::Undeclared (with type "Did you mean" suggestions).
                let suggestions = self.interpreter.suggest_type_names(constraint);
                let mut undecl_msg = format!("Type '{}' is not declared.", constraint);
                if suggestions.len() == 1 {
                    undecl_msg.push_str(&format!(" Did you mean '{}'?", suggestions[0]));
                } else if suggestions.len() > 1 {
                    let quoted: Vec<String> =
                        suggestions.iter().map(|s| format!("'{}'", s)).collect();
                    undecl_msg.push_str(&format!(
                        " Did you mean any of these: {}?",
                        quoted.join(", ")
                    ));
                }
                let mut undecl_attrs = std::collections::HashMap::new();
                undecl_attrs.insert("what".to_string(), Value::str("type".to_string()));
                undecl_attrs.insert("symbol".to_string(), Value::str(constraint.to_string()));
                undecl_attrs.insert(
                    "suggestions".to_string(),
                    Value::array(suggestions.iter().cloned().map(Value::str).collect()),
                );
                undecl_attrs.insert("message".to_string(), Value::str(undecl_msg.clone()));
                let sorrow = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Undeclared"),
                    undecl_attrs,
                );
                // Group message mirrors Raku: the sorrow message plus "Malformed my".
                let group_msg = format!("{}\nMalformed my", undecl_msg);
                let mut group_attrs = std::collections::HashMap::new();
                group_attrs.insert("sorrows".to_string(), Value::array(vec![sorrow]));
                group_attrs.insert("worries".to_string(), Value::array(vec![]));
                group_attrs.insert("panic".to_string(), Value::Nil);
                group_attrs.insert("message".to_string(), Value::str(group_msg));
                return Err(RuntimeError::typed("X::Comp::Group", group_attrs));
            }
        }
        if !matches!(value, Value::Nil)
            && !self.interpreter.type_matches_value(constraint, &value)
            && !self.interpreter.is_container_subclass(constraint)
        {
            if bind_mode {
                return Err(crate::runtime::utils::type_check_binding_typed_error(
                    constraint, &value,
                ));
            }
            return Err(RuntimeError::typecheck_assignment(
                constraint, &value, var_name,
            ));
        }
        if !matches!(value, Value::Nil) {
            let coerced = self
                .interpreter
                .try_coerce_value_for_constraint(constraint, value.clone())?;
            *self.stack.last_mut().unwrap() = coerced;
        }
        Ok(())
    }

    pub(super) fn exec_indirect_type_lookup_op(&mut self) {
        let name_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = name_val.to_string_value();
        self.stack
            .push(self.interpreter.resolve_indirect_type_name(&name));
    }

    pub(super) fn exec_state_var_init_op(&mut self, code: &CompiledCode, slot: u32, key_idx: u32) {
        let init_val = self.stack.pop().unwrap_or(Value::Nil);
        let base_key = Self::const_str(code, key_idx);
        let scoped_key = self.scoped_state_key(base_key);
        let slot_idx = slot as usize;
        let name = &code.locals[slot_idx];
        let val = if let Some(stored) = self.interpreter.get_state_var(&scoped_key) {
            stored.clone()
        } else {
            // Coerce @ variables to Array and % variables to Hash,
            // matching the behavior of SetLocal for these sigils.
            let coerced = if name.starts_with('@') {
                runtime::coerce_to_array(init_val)
            } else if name.starts_with('%') {
                runtime::coerce_to_hash(init_val)
            } else {
                init_val
            };
            self.interpreter
                .set_state_var(scoped_key.clone(), coerced.clone());
            coerced
        };
        self.locals[slot_idx] = val.clone();
        let name = name.to_string();
        // Only insert into env if the value differs from what's already there.
        // This avoids triggering Arc::make_mut deep clone on the CoW env when
        // the state variable is already initialized with the same value (common
        // case in tight loops).
        let needs_env_insert = self.interpreter.env().get(&name) != Some(&val);
        if needs_env_insert {
            self.interpreter.env_mut().insert(name.clone(), val);
        }
        // Store metadata mapping variable name to its state storage key.
        // Closures that capture this variable can use this to update state
        // storage when they modify the variable.
        let meta_key = format!("__mutsu_state_key::{}", name);
        let scoped_key_val = Value::str(scoped_key.clone());
        let needs_meta_insert = self.interpreter.env().get(&meta_key) != Some(&scoped_key_val);
        if needs_meta_insert {
            self.interpreter.env_mut().insert(meta_key, scoped_key_val);
        }
    }

    pub(super) fn exec_block_scope_op(
        &mut self,
        code: &CompiledCode,
        bounds: [u32; 7],
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let [
            pre_end,
            enter_end,
            body_end,
            keep_start,
            undo_start,
            post_start,
            end,
        ] = bounds;
        let pre_start = *ip + 1;
        let enter_start = pre_end as usize;
        let body_start = enter_end as usize;
        let queue_start = body_end as usize;
        let keep_start = keep_start as usize;
        let undo_start = undo_start as usize;
        let post_start = post_start as usize;
        let end = end as usize;
        let routine_snapshot = self.interpreter.snapshot_routine_registry();
        let saved_env = self.interpreter.env().clone();
        let saved_locals = self.locals.clone();
        let once_scope = self.interpreter.next_once_scope_id();
        // Track variables declared within this block scope.
        self.block_declared_vars
            .push(std::collections::HashSet::new());
        // Push saved locals for $OUTER:: variable access.
        self.outer_scope_locals.push(saved_locals.clone());

        // Run PRE phasers first (before ENTER)
        self.run_range(code, pre_start, enter_start, compiled_fns)?;

        let enter_result = self.run_range(code, enter_start, body_start, compiled_fns);
        self.interpreter.push_once_scope(once_scope);
        self.interpreter.push_block_scope_depth();
        self.interpreter.push_lexical_class_scope();
        self.interpreter.push_enum_scope();
        let stack_base = self.stack.len();
        let topic_before = self.last_topic_value.clone();
        let end_phaser_count_before = self.interpreter.end_phaser_count();
        // If ENTER died, skip the body but still run LEAVE phasers
        let mut body_result = if let Err(e) = enter_result {
            Err(e)
        } else {
            self.run_range(code, body_start, queue_start, compiled_fns)
        };
        let body_value = if self.stack.len() > stack_base {
            self.stack.last().cloned()
        } else if self.last_topic_value != topic_before {
            self.last_topic_value.clone()
        } else {
            None
        };
        let ran_undo = !Self::should_run_success_queue(&body_result, body_value);

        // Set $! before LEAVE/UNDO phasers run so they can see the exception
        if let Err(ref e) = body_result
            && Self::is_exceptional_block_exit(e)
        {
            let err_val = if let Some(ex) = e.exception.as_ref() {
                *ex.clone()
            } else {
                let mut exc_attrs = std::collections::HashMap::new();
                exc_attrs.insert("message".to_string(), Value::str(e.message.clone()));
                // Untyped runtime error -> X::AdHoc (see vm_control_ops.rs).
                Value::make_instance(crate::symbol::Symbol::intern("X::AdHoc"), exc_attrs)
            };
            self.interpreter
                .env_mut()
                .insert("!".to_string(), err_val.clone());
            for (i, name) in code.locals.iter().enumerate() {
                if name == "!" {
                    self.locals[i] = err_val;
                    break;
                }
            }
        }

        let queue_res = if !ran_undo {
            self.run_leave_queue_guarded(code, keep_start, undo_start, compiled_fns)
        } else {
            self.run_leave_queue_guarded(code, undo_start, post_start, compiled_fns)
        };

        // When UNDO phasers ran in response to a fail(), mark the error so the
        // resulting Failure value will be created with handled=True (Raku semantics:
        // UNDO acts as a handler for the failure).
        if ran_undo
            && undo_start < post_start
            && let Err(ref mut e) = body_result
            && e.is_fail
        {
            e.fail_handled = true;
        }

        // Run POST phasers after LEAVE (regardless of success/failure path)
        // Set $_ to the return/result value so POST can inspect it
        // Set $! to the exception if the body threw one
        if post_start < end {
            let post_topic = match &body_result {
                Ok(()) => self.last_topic_value.clone().unwrap_or(Value::Nil),
                Err(e) => e.return_value.clone().unwrap_or(Value::Nil),
            };
            self.interpreter
                .env_mut()
                .insert("_".to_string(), post_topic.clone());
            // Also update the local slot for $_ if present
            for (i, name) in code.locals.iter().enumerate() {
                if name == "_" {
                    self.locals[i] = post_topic;
                    break;
                }
            }
            // If the body threw an exception, set $! so POST can see it
            if let Err(ref e) = body_result
                && Self::is_exceptional_block_exit(e)
            {
                let err_val = if let Some(ex) = e.exception.as_ref() {
                    *ex.clone()
                } else {
                    let mut exc_attrs = std::collections::HashMap::new();
                    exc_attrs.insert("message".to_string(), Value::str(e.message.clone()));
                    // Untyped runtime error -> X::AdHoc (see vm_control_ops.rs).
                    Value::make_instance(crate::symbol::Symbol::intern("X::AdHoc"), exc_attrs)
                };
                self.interpreter
                    .env_mut()
                    .insert("!".to_string(), err_val.clone());
                // Also update the local slot for $! if present
                for (i, name) in code.locals.iter().enumerate() {
                    if name == "!" {
                        self.locals[i] = err_val;
                        break;
                    }
                }
            }
        }
        let post_res = self.run_range(code, post_start, end, compiled_fns);

        self.interpreter.restore_routine_registry(routine_snapshot);

        // Pop the block-declared variables set.
        let block_declared = self.block_declared_vars.pop().unwrap_or_default();
        // Pop the outer scope locals snapshot.
        self.outer_scope_locals.pop();

        // Update captured envs of END phasers registered during this block
        // so they see the final values of block-scoped variables (which will
        // be removed from env when the block scope is restored below).
        if self.interpreter.end_phaser_count() > end_phaser_count_before {
            let current = self.interpreter.env().clone();
            self.interpreter
                .update_end_phaser_envs(end_phaser_count_before, &current);
        }
        let current_env = self.interpreter.env().clone();
        let mut restored_env = saved_env.clone();
        for (k, v) in current_env {
            // Package-qualified names (e.g. Test1::ns, Foo::Bar) are package-global
            // and must propagate out of any block scope where they were declared.
            // Sigils may appear before the qualifier (e.g. &Test1::ns, $Foo::var).
            // Skip internal VM metadata keys (which contain `::` but are not
            // user-visible package names, e.g. `__mutsu_var_meta::x`).
            let is_package_qualified = k.with_str(|s| {
                let stripped = s.trim_start_matches(['$', '@', '%', '&']);
                stripped.contains("::") && !stripped.starts_with("__mutsu_")
            });
            if is_package_qualified {
                restored_env.insert_sym(k, v);
                continue;
            }
            // Package type objects declared inside a block (e.g.
            // `{ package Foo { ... } }`) must remain visible outside the
            // block as type objects, just like classes/roles. The
            // `RegisterPackage` opcode stores them in env under the bare
            // package name. Restrict this to keys that look like a real
            // package identifier (uppercase ASCII start, not internal/
            // special variables like `_` or `__mutsu_*`).
            if matches!(&v, Value::Package(_))
                && !saved_env.contains_key_sym(k)
                && k.with_str(|s| s.chars().next().is_some_and(|c| c.is_ascii_uppercase()))
            {
                restored_env.insert_sym(k, v);
                continue;
            }
            if saved_env.contains_key_sym(k) {
                // Lexical topic is block-scoped; don't write inner `$_` back
                // to the outer scope on block exit. Also preserve the alias
                // metadata for `$_` so that `:=` bindings survive block exit.
                if k == "_" || k == "__mutsu_sigilless_alias::_" {
                    continue;
                }
                // Dynamic variables (e.g. $*VAR) are scoped to the block:
                // restore to the saved value rather than propagating the inner value.
                if k.starts_with("*") {
                    continue;
                }
                // Variables declared with `my` inside this block should not
                // propagate their values to the outer scope. Restore the outer
                // scope's original value instead.
                if k.with_str(|s| block_declared.contains(s)) {
                    continue;
                }
                restored_env.insert_sym(k, v);
            }
        }
        self.locals = saved_locals;
        for (idx, name) in code.locals.iter().enumerate() {
            if let Some(val) = restored_env.get(name).cloned() {
                self.locals[idx] = val;
            }
        }
        // For `our`-scoped locals declared inside this block (or in the outer
        // scope but reassigned inside via `our $x = ...`), the persistent
        // package value in `our_vars` may have been updated. Refresh the
        // lexical alias from the package store so the outer scope sees the
        // new value (Raku semantics: `our` is an alias for a package var).
        for (slot, qualified) in &code.our_locals {
            let Some(local_name) = code.locals.get(*slot) else {
                continue;
            };
            // Only refresh slots whose lexical alias existed in the outer
            // scope (i.e., the outer scope also declared `our $x`). For
            // `our` declarations made only inside the block, the lexical
            // alias is block-scoped and must not leak to the outer scope.
            if !saved_env.contains_key(local_name) {
                continue;
            }
            if let Some(val) = self.interpreter.get_our_var(qualified).cloned() {
                if *slot < self.locals.len() {
                    self.locals[*slot] = val.clone();
                }
                restored_env.insert(local_name.clone(), val);
            }
        }
        *self.interpreter.env_mut() = restored_env;
        // After block scope restoration, sync `:=` alias bindings.
        // If a local variable was modified inside the block and has a
        // sigilless alias (e.g. `my $a := $_`), propagate the local's
        // current value to the alias target in env so they stay in sync.
        for (idx, name) in code.locals.iter().enumerate() {
            let alias_key = format!("__mutsu_sigilless_alias::{}", name);
            if let Some(Value::Str(target)) = self.interpreter.env().get(&alias_key).cloned() {
                let local_val = &self.locals[idx];
                if let Some(env_val) = self.interpreter.env().get(target.as_str())
                    && local_val != env_val
                {
                    self.interpreter
                        .env_mut()
                        .insert(target.to_string(), local_val.clone());
                }
            }
        }
        // Drop readonly flags for block-declared variables so a later outer
        // `my` redeclaration of the same name is not marked as readonly by
        // the now-gone binding (`my %h := SetHash.new(...)` inside a block
        // must not leave `%h` readonly in the outer scope).
        for name in &block_declared {
            self.interpreter.readonly_vars_mut().remove(name);
        }
        // Note: `our`-scoped variables persist in our_vars and are accessible
        // via package-qualified names (e.g., $Pkg::var) after block exit.
        self.interpreter.pop_enum_scope();
        self.interpreter.pop_lexical_class_scope();
        self.interpreter.pop_block_scope_depth();
        self.interpreter.pop_once_scope();

        if let Err(e) = post_res {
            // POST failure overrides successful body and return-value body exits
            if body_result.is_ok() && queue_res.is_ok() {
                return Err(e);
            }
            // POST failure also overrides a "return" (non-exceptional exit)
            if let Err(ref be) = body_result
                && be.return_value.is_some()
            {
                return Err(e);
            }
        }
        if let Err(e) = queue_res
            && body_result.is_ok()
        {
            return Err(e);
        }
        body_result?;
        *ip = end;
        Ok(())
    }

    fn is_exceptional_block_exit(err: &RuntimeError) -> bool {
        if err.is_fail {
            return true;
        }
        if err.return_value.is_some() {
            return false;
        }
        !(err.is_last
            || err.is_next
            || err.is_redo
            || err.is_goto
            || err.is_proceed
            || err.is_succeed
            || err.is_leave
            || err.is_resume
            || err.is_react_done)
    }

    fn should_run_success_queue(
        body_result: &Result<(), RuntimeError>,
        current_value: Option<Value>,
    ) -> bool {
        match body_result {
            Ok(()) => current_value.unwrap_or(Value::Nil).truthy(),
            Err(e) if !Self::is_exceptional_block_exit(e) => {
                e.return_value.clone().unwrap_or(Value::Nil).truthy()
            }
            Err(_) => false,
        }
    }

    /// Execute the CheckPhaser opcode: pop TOS, throw X::Phaser::PrePost if falsy.
    /// `condition` is the phaser condition's source text (e.g. `0`) when known.
    pub(super) fn exec_check_phaser_op(
        &mut self,
        is_pre: bool,
        condition: Option<String>,
    ) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        if !val.truthy() {
            let phaser_name = if is_pre { "PRE" } else { "POST" };
            let condition = condition.unwrap_or_default();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "phaser".to_string(),
                Value::Str(Arc::new(phaser_name.to_string())),
            );
            attrs.insert(
                "condition".to_string(),
                Value::Str(Arc::new(condition.clone())),
            );
            let exception =
                Value::make_instance(crate::symbol::Symbol::intern("X::Phaser::PrePost"), attrs);
            // raku: "Precondition '<cond>' failed" / "Postcondition '<cond>' failed".
            let kind = if is_pre {
                "Precondition"
            } else {
                "Postcondition"
            };
            let mut err = RuntimeError::new(format!("{} '{}' failed", kind, condition));
            err.exception = Some(Box::new(exception));
            return Err(err);
        }
        Ok(())
    }

    pub(super) fn exec_do_block_expr_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        label: &Option<String>,
        scope_isolate: bool,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let body_start = *ip + 1;
        let end = body_end as usize;
        let label = label.clone();
        let stack_base = self.stack.len();
        let once_scope = self.interpreter.next_once_scope_id();
        self.interpreter.push_once_scope(once_scope);
        self.interpreter.push_enum_scope();
        let saved_env = if scope_isolate {
            Some((self.interpreter.env().clone(), self.locals.clone()))
        } else {
            None
        };
        // Only catch loop control signals (last/next/redo) when the do block
        // has an explicit label that matches. Unlabeled do blocks should let
        // these signals propagate to the enclosing loop construct.
        let has_label = label.is_some();
        let result = loop {
            match self.run_range(code, body_start, end, compiled_fns) {
                Ok(()) => break Ok(()),
                Err(e) if e.is_redo && has_label && Self::label_matches(&e.label, &label) => {
                    self.stack.truncate(stack_base);
                    continue;
                }
                Err(e) if e.is_next && has_label && Self::label_matches(&e.label, &label) => {
                    self.stack.truncate(stack_base);
                    self.stack.push(Value::Slip(std::sync::Arc::new(vec![])));
                    break Ok(());
                }
                Err(e)
                    if e.is_leave
                        && e.leave_callable_id.is_none()
                        && e.leave_routine.is_none()
                        && Self::label_matches(&e.label, &label) =>
                {
                    self.stack.truncate(stack_base);
                    self.stack.push(
                        e.return_value
                            .unwrap_or(Value::Slip(std::sync::Arc::new(vec![]))),
                    );
                    break Ok(());
                }
                Err(e) if e.is_last && has_label && Self::label_matches(&e.label, &label) => {
                    self.stack.truncate(stack_base);
                    self.stack.push(
                        e.return_value
                            .unwrap_or(Value::Slip(std::sync::Arc::new(vec![]))),
                    );
                    break Ok(());
                }
                Err(e) => break Err(e),
            }
        };
        self.interpreter.pop_enum_scope();
        self.interpreter.pop_once_scope();
        // Restore scope if scope_isolate is true
        if let Some((saved_env, saved_locals)) = saved_env {
            let block_result = self.stack.pop().unwrap_or(Value::Nil);
            // Preserve hash variables declared or re-declared inside the
            // block so that inline `my %h` declarations (e.g. in
            // `:into(my %h := :{})`) remain visible in the outer scope.
            // Only hash variables are preserved to avoid side effects
            // on other container types.
            let current_env = self.interpreter.env().clone();
            let mut new_vars: Vec<(Symbol, Value)> = Vec::new();
            for (name, value) in current_env.iter() {
                if !name.starts_with("%") || name.starts_with("%*") {
                    continue;
                }
                let is_new_or_changed = match saved_env.get_sym(*name) {
                    None => true,
                    Some(saved_val) => match (value, saved_val) {
                        (Value::Hash(a), Value::Hash(b)) => !std::sync::Arc::ptr_eq(a, b),
                        _ => std::mem::discriminant(value) != std::mem::discriminant(saved_val),
                    },
                };
                if is_new_or_changed {
                    new_vars.push((*name, value.clone()));
                }
            }
            let restored_env = saved_env.clone();
            *self.interpreter.env_mut() = restored_env;
            // Re-insert newly declared user variables
            for (name, value) in new_vars {
                self.interpreter.env_mut().insert_sym(name, value);
            }
            self.locals = saved_locals;
            // Re-read locals from env for variables that use env as the primary
            // store (non-simple locals). Simple locals use dirty-tracked SetLocal
            // and their saved_locals state is authoritative; re-reading from env
            // would overwrite them with stale env values (e.g. loop control flags
            // like `__mutsu_loop_first_N` that were updated inside the loop body
            // but not yet flushed to env before this DoBlock snapshot).
            for (idx, name) in code.locals.iter().enumerate() {
                if code.simple_locals[idx] {
                    // Simple locals: env may be stale; saved_locals is correct.
                    continue;
                }
                if let Some(val) = self.interpreter.env().get(name).cloned() {
                    self.locals[idx] = val;
                }
            }
            self.stack.push(block_result);
        }
        *ip = end;
        result
    }

    pub(super) fn exec_once_expr_op(
        &mut self,
        code: &CompiledCode,
        key_idx: u32,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let scope = self
            .interpreter
            .current_once_scope()
            .unwrap_or_else(|| self.interpreter.next_once_scope_id());
        let site_key = Self::const_str(code, key_idx);
        let cache_key = format!("{scope}::{site_key}");
        if let Some(value) = self.interpreter.get_once_value(&cache_key).cloned() {
            self.stack.push(value);
            *ip = body_end as usize;
            return Ok(());
        }

        let body_start = *ip + 1;
        let end = body_end as usize;
        let stack_base = self.stack.len();
        self.run_range(code, body_start, end, compiled_fns)?;
        let value = if self.stack.len() > stack_base {
            self.stack.pop().unwrap_or(Value::Nil)
        } else {
            Value::Nil
        };
        self.interpreter.set_once_value(cache_key, value.clone());
        self.stack.push(value);
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_let_save_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        index_mode: bool,
        is_temp: bool,
    ) {
        let name = Self::const_str(code, name_idx).to_string();
        if index_mode {
            let _idx_val = self.stack.pop().unwrap_or(Value::Int(0));
        }
        let old_val = self
            .get_env_with_main_alias(&name)
            .or_else(|| {
                code.locals
                    .iter()
                    .position(|n| n == &name)
                    .map(|i| self.locals[i].clone())
            })
            .unwrap_or(Value::Nil);
        // For temp, deep-copy Array/Hash so the snapshot is independent of
        // future mutations (Arc is shared, so a shallow clone wouldn't work).
        let save_val = if is_temp {
            Self::deep_copy_value(&old_val)
        } else {
            old_val
        };
        self.interpreter.let_saves_push(name, save_val, is_temp);
    }

    /// Recursively deep-copy a Value so that Array/Hash snapshots are
    /// independent of future in-place mutations (the inner Arc would otherwise
    /// be shared).
    fn deep_copy_value(val: &Value) -> Value {
        match val {
            Value::Array(arc_vec, kind) => {
                let copied: Vec<Value> = arc_vec.iter().map(Self::deep_copy_value).collect();
                Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(copied)),
                    *kind,
                )
            }
            Value::Hash(arc_map) => {
                let copied: std::collections::HashMap<String, Value> = arc_map
                    .iter()
                    .map(|(k, v)| (k.clone(), Self::deep_copy_value(v)))
                    .collect();
                Value::Hash(Value::hash_arc(copied))
            }
            other => other.clone(),
        }
    }

    pub(super) fn exec_let_block_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let mark = self.interpreter.let_saves_len();
        let body_start = *ip + 1;
        let end = body_end as usize;
        match self.run_range(code, body_start, end, compiled_fns) {
            Ok(()) => {
                let topic = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                let success = Self::is_let_success(&topic);
                self.interpreter.resolve_let_saves_on_success(mark, success);
                self.env_dirty = true;
            }
            Err(e) => {
                self.interpreter.restore_let_saves(mark);
                self.env_dirty = true;
                return Err(e);
            }
        }
        *ip = end;
        Ok(())
    }

    /// Validate and coerce a value for native integer type assignment.
    /// Throws on: string values, non-integer numerics (floats), NaN, out-of-range values.
    pub(super) fn validate_native_int_assignment(
        &mut self,
        type_name: &str,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        use crate::runtime::native_types;
        use num_bigint::BigInt as NumBigInt;
        use num_traits::ToPrimitive;

        // Reject strings
        if matches!(value, Value::Str(_)) {
            return Err(RuntimeError::new(format!(
                "Cannot convert string to native integer type '{}'",
                type_name
            )));
        }
        // Reject NaN
        if let Value::Num(n) = value {
            if n.is_nan() {
                return Err(RuntimeError::new(format!(
                    "Cannot convert NaN to native integer type '{}'",
                    type_name
                )));
            }
            // Reject non-integer floats
            if n.fract() != 0.0 {
                return Err(RuntimeError::new(format!(
                    "Cannot convert non-integer value to native integer type '{}'",
                    type_name
                )));
            }
        }
        // Reject Rat with non-integer value
        if let Value::Rat(n, d) = value
            && *d != 0
            && *n % *d != 0
        {
            return Err(RuntimeError::new(format!(
                "Cannot convert non-integer value to native integer type '{}'",
                type_name
            )));
        }

        // Convert value to BigInt for range checking
        let big_val = match value {
            Value::Int(n) => NumBigInt::from(*n),
            Value::BigInt(n) => (**n).clone(),
            Value::Num(n) => NumBigInt::from(*n as i64),
            Value::Rat(n, d) => {
                if *d == 0 {
                    NumBigInt::from(0)
                } else {
                    NumBigInt::from(*n / *d)
                }
            }
            Value::Bool(b) => NumBigInt::from(if *b { 1 } else { 0 }),
            _ => {
                return Err(RuntimeError::new(format!(
                    "Cannot convert value to native integer type '{}'",
                    type_name
                )));
            }
        };

        // Wrap out-of-range values for smaller native types (like C semantics).
        // Full-width signed types (int/int64) throw on overflow.
        // Unsigned types (uint/uint64) wrap negative values to unsigned range.
        let wrapped = if !native_types::is_in_native_range(type_name, &big_val) {
            if matches!(type_name, "int" | "int64") {
                return Err(RuntimeError::new(format!(
                    "Cannot unbox {} bit wide bigint into native integer",
                    big_val.bits()
                )));
            }
            if matches!(type_name, "uint" | "uint64") {
                // Positive values exceeding u64 range should throw
                if big_val > NumBigInt::from(0u64) {
                    return Err(RuntimeError::new(format!(
                        "Cannot unbox {} bit wide bigint into native integer",
                        big_val.bits()
                    )));
                }
            }
            native_types::wrap_native_int(type_name, &big_val)
        } else {
            big_val
        };

        // Coerce to Int on the stack
        let int_val = wrapped.to_i64().map(Value::Int).unwrap_or_else(|| {
            // For uint64 values that don't fit in i64, store as BigInt
            Value::bigint(wrapped)
        });
        *self.stack.last_mut().unwrap() = int_val;
        Ok(())
    }
}
