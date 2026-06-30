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
        match value {
            Value::Array(items, ..) => {
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
            Value::Seq(items) => {
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
            Value::Nil => true,
            // A type-object hole (e.g. an `Any` gap) is acceptable for a native
            // array: it is coerced to the array's default on store.
            Value::Package(_)
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
        match value {
            Value::Array(items, ..) => {
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
            Value::Seq(items) => {
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
            Value::Nil => None,
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

    pub(super) fn scalarize_range_endpoint(value: Value) -> Value {
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
            // An allomorph (IntStr/RatStr/NumStr/…) as a Range endpoint coerces
            // to its numeric value, so `0..<5>` is `0..5` (not a degenerate
            // single-element range over the un-numified Mixin).
            Value::Mixin(ref inner, ref mixins)
                if crate::value::types::allomorph_type_name(inner, mixins).is_some() =>
            {
                runtime::coerce_to_numeric(value)
            }
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
            // A `-Inf` start is NOT collapsed to the i64::MIN sentinel: such a
            // range iterates its `-Inf` start ad infinitum (`-Inf+1 == -Inf`),
            // so it must stay a GenericRange preserving the `Num` endpoint. The
            // i64 sentinel would instead count up from i64::MIN, yielding bogus
            // integers (`(-Inf..0).map` would give i64::MIN+n, not -Inf).
            (Value::Num(a), Value::Int(_)) if a.is_infinite() && a.is_sign_negative() => {
                Value::GenericRange {
                    start: Arc::new(left.clone()),
                    end: Arc::new(right.clone()),
                    excl_start: false,
                    excl_end: false,
                }
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
}
