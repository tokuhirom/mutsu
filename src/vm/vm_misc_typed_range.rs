use super::*;

impl Interpreter {
    pub(super) fn array_elements_match_constraint(
        &mut self,
        constraint: &str,
        value: &Value,
    ) -> bool {
        // Container element constraints like `Array` or `Array[Int]` match each
        // element as a whole. Scalar constraints like `Int` recurse into nested arrays.
        let constraint_base = constraint
            .split_once('[')
            .map_or(constraint, |(base, _)| base);
        let is_container_constraint = matches!(
            constraint_base,
            "Array" | "Hash" | "List" | "Seq" | "Positional" | "Associative"
        );
        match value.view() {
            ValueView::Array(items, ..) => {
                if is_container_constraint {
                    // Each element is checked as a whole against the constraint
                    items
                        .iter()
                        .all(|item| self.type_matches_value(constraint, item))
                } else {
                    // Recurse into sub-arrays for simple element types
                    items
                        .iter()
                        .all(|item| self.array_elements_match_constraint(constraint, item))
                }
            }
            ValueView::Seq(items) => {
                if is_container_constraint {
                    // Each element is checked as a whole against the constraint
                    items
                        .iter()
                        .all(|item| self.type_matches_value(constraint, item))
                } else {
                    // Recurse into sub-arrays for simple element types
                    items
                        .iter()
                        .all(|item| self.array_elements_match_constraint(constraint, item))
                }
            }
            ValueView::Nil => true,
            // A type-object hole (e.g. an `Any` gap) is acceptable for a native
            // array: it is coerced to the array's default on store.
            ValueView::Package(_)
                if crate::runtime::native_types::is_native_array_element_type(constraint) =>
            {
                true
            }
            _ => self.type_matches_value(constraint, value),
        }
    }

    /// Build the appropriate error for a typed-array initializer whose elements
    /// fail the element type constraint. Native integer arrays produce an AdHoc
    /// "Cannot unbox" error; boxed types produce X::TypeCheck::Assignment naming
    /// the offending element.
    pub(super) fn typed_array_element_error(
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
        match value.view() {
            ValueView::Array(items, ..) => {
                for item in items.iter() {
                    if is_container_constraint {
                        if !self.type_matches_value(constraint, item) {
                            return Some(item.clone());
                        }
                    } else if let Some(bad) = self.first_array_element_mismatch(constraint, item) {
                        return Some(bad);
                    }
                }
                None
            }
            ValueView::Seq(items) => {
                for item in items.iter() {
                    if is_container_constraint {
                        if !self.type_matches_value(constraint, item) {
                            return Some(item.clone());
                        }
                    } else if let Some(bad) = self.first_array_element_mismatch(constraint, item) {
                        return Some(bad);
                    }
                }
                None
            }
            ValueView::Nil => None,
            _ => {
                if self.type_matches_value(constraint, value) {
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
                v.view(),
                ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. }
                    | ValueView::Complex(..)
                    | ValueView::Seq(_)
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
            let got_val = if matches!(v.view(), ValueView::Seq(_)) {
                Value::package(Symbol::intern(type_name))
            } else {
                v.clone()
            };
            ex_attrs.insert("got".to_string(), got_val);
            let exception = Value::make_instance(Symbol::intern("X::Range::InvalidArg"), ex_attrs);
            RuntimeError::from_exception_value(exception)
        })
    }

    pub(super) fn scalarize_range_endpoint(value: Value) -> Value {
        let coerce = match value.view() {
            ValueView::Scalar(inner) => {
                return Self::scalarize_range_endpoint(inner.clone());
            }
            ValueView::Instance { class_name, .. } if class_name == "Match" => {
                return runtime::coerce_to_numeric(Value::str(value.to_string_value()));
            }
            ValueView::Array(..)
            | ValueView::Slip(..)
            | ValueView::LazyList(..)
            | ValueView::Hash(..)
            | ValueView::Set(..)
            | ValueView::Bag(..)
            | ValueView::Mix(..) => true,
            // An allomorph (IntStr/RatStr/NumStr/…) as a Range endpoint coerces
            // to its numeric value, so `0..<5>` is `0..5` (not a degenerate
            // single-element range over the un-numified Mixin).
            ValueView::Mixin(inner, mixins)
                if crate::value::types::allomorph_type_name(inner, mixins).is_some() =>
            {
                true
            }
            // Seq is NOT scalarized here — it must be caught by check_range_invalid_arg
            ValueView::Seq(..) => false,
            _ => false,
        };
        if coerce {
            runtime::coerce_to_numeric(value)
        } else {
            value
        }
    }

    pub(super) fn exec_make_range_op(&mut self) -> Result<(), RuntimeError> {
        let right = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        let left = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        if let Some(err) = Self::check_range_invalid_arg(&left, &right) {
            return Err(err);
        }
        let result = match (left.view(), right.view()) {
            (ValueView::Int(a), ValueView::Int(b)) => Value::range(a, b),
            (ValueView::Int(a), ValueView::Num(b)) if b.is_infinite() && b.is_sign_positive() => {
                Value::range(a, i64::MAX)
            }
            (ValueView::Int(a), ValueView::Whatever) => Value::range(a, i64::MAX),
            (ValueView::Int(a), ValueView::HyperWhatever) => Value::range(a, i64::MAX),
            // A `-Inf` start is NOT collapsed to the i64::MIN sentinel: such a
            // range iterates its `-Inf` start ad infinitum (`-Inf+1 == -Inf`),
            // so it must stay a GenericRange preserving the `Num` endpoint. The
            // i64 sentinel would instead count up from i64::MIN, yielding bogus
            // integers (`(-Inf..0).map` would give i64::MIN+n, not -Inf).
            (ValueView::Num(a), ValueView::Int(_)) if a.is_infinite() && a.is_sign_negative() => {
                Value::generic_range(left.clone(), right.clone(), false, false)
            }
            (ValueView::Str(a), ValueView::Whatever) => Value::generic_range(
                Value::str_arc(a.clone()),
                Value::HYPER_WHATEVER,
                false,
                false,
            ),
            (ValueView::Str(a), ValueView::HyperWhatever) => Value::generic_range(
                Value::str_arc(a.clone()),
                Value::HYPER_WHATEVER,
                false,
                false,
            ),
            (ValueView::Str(a), ValueView::Str(b)) => Value::generic_range(
                Value::str_arc(a.clone()),
                Value::str_arc(b.clone()),
                false,
                false,
            ),
            (_, _) if left.is_numeric() && right.is_numeric() => {
                Value::generic_range(left.clone(), right.clone(), false, false)
            }
            (ValueView::Str(a), _) if right.is_numeric() => {
                Value::generic_range(Value::str_arc(a.clone()), right.clone(), false, false)
            }
            (_, ValueView::Str(b)) if left.is_numeric() => {
                Value::generic_range(left.clone(), Value::str_arc(b.clone()), false, false)
            }
            // WhateverCode endpoint: e.g. 0..*-2
            (_, ValueView::Sub(_)) | (ValueView::Sub(_), _) => {
                Value::generic_range(left.clone(), right.clone(), false, false)
            }
            _ => Value::generic_range(left.clone(), right.clone(), false, false),
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
        let result = match (left.view(), right.view()) {
            (ValueView::Int(a), ValueView::Int(b)) => Value::range_excl(a, b),
            (_, _) if left.is_numeric() || right.is_numeric() => {
                Value::generic_range(left.clone(), right.clone(), false, true)
            }
            _ => Value::generic_range(left.clone(), right.clone(), false, true),
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
        let result = match (left.view(), right.view()) {
            (ValueView::Int(a), ValueView::Int(b)) => Value::range_excl_start(a, b),
            (_, _) if left.is_numeric() || right.is_numeric() => {
                Value::generic_range(left.clone(), right.clone(), true, false)
            }
            _ => Value::generic_range(left.clone(), right.clone(), true, false),
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
        let result = match (left.view(), right.view()) {
            (ValueView::Int(a), ValueView::Int(b)) => Value::range_excl_both(a, b),
            (_, _) if left.is_numeric() || right.is_numeric() => {
                Value::generic_range(left.clone(), right.clone(), true, true)
            }
            _ => Value::generic_range(left.clone(), right.clone(), true, true),
        };
        self.stack.push(result);
        Ok(())
    }
}
