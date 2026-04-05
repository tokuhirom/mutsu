use super::*;
use num_traits::ToPrimitive;

/// Check if a Value is a Failure instance.
fn is_failure_value(value: &Value) -> bool {
    matches!(value, Value::Instance { class_name, .. } if class_name == "Failure")
}

/// Parse a coercion type like "Int()" or "Int(Rat)".
/// Returns Some((target_type, optional_source_type)) if it's a coercion type.
pub(crate) fn parse_coercion_type(constraint: &str) -> Option<(&str, Option<&str>)> {
    if let Some(open) = constraint.find('(')
        && constraint.ends_with(')')
    {
        let target = &constraint[..open];
        let source = &constraint[open + 1..constraint.len() - 1];
        if source.is_empty() {
            Some((target, None))
        } else {
            Some((target, Some(source)))
        }
    } else {
        None
    }
}

#[inline]
pub(crate) fn is_coercion_constraint(constraint: &str) -> bool {
    let bytes = constraint.as_bytes();
    bytes.last() == Some(&b')') && bytes.contains(&b'(') && !bytes.contains(&b'[')
}

pub(crate) fn coerce_impossible_error(target: &str, got: &Value) -> RuntimeError {
    let msg = format!(
        "Impossible coercion from '{}' into '{}': no acceptable coercion method found",
        crate::runtime::value_type_name(got),
        target
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("from".to_string(), got.clone());
    attrs.insert("to".to_string(), Value::str(target.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Coerce::Impossible"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Coerce a value to the target type.
pub(in crate::runtime) fn coerce_value(target: &str, value: Value) -> Value {
    let base_target = if target.ends_with(":D") || target.ends_with(":U") || target.ends_with(":_")
    {
        &target[..target.len() - 2]
    } else {
        target
    };
    match base_target {
        "Int" => match &value {
            Value::Int(_) => value,
            Value::Num(n) => Value::Int(*n as i64),
            Value::Rat(_, d) if *d == 0 => {
                RuntimeError::divide_by_zero_failure_for_method("Int", "Rational")
            }
            Value::Rat(n, d) => Value::Int(n / d),
            Value::FatRat(_, d) if *d == 0 => {
                RuntimeError::divide_by_zero_failure_for_method("Int", "Rational")
            }
            Value::FatRat(n, d) => Value::Int(n / d),
            Value::Str(s) => Value::Int(s.parse::<i64>().unwrap_or(0)),
            Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
            _ => value,
        },
        "Num" => match &value {
            Value::Num(_) => value,
            Value::Int(n) => Value::Num(*n as f64),
            Value::Rat(n, d) => Value::Num(*n as f64 / *d as f64),
            Value::Str(s) => Value::Num(s.parse::<f64>().unwrap_or(0.0)),
            _ => value,
        },
        "Str" => Value::str(crate::runtime::utils::coerce_to_str(&value)),
        "Array" | "List" => crate::runtime::utils::coerce_to_array(value),
        "Hash" => crate::runtime::utils::coerce_to_hash(value),
        "Rat" => {
            match &value {
                Value::Rat(_, _) => value,
                Value::Int(n) => Value::Rat(*n, 1),
                Value::Num(n) => {
                    // Simple float to rat conversion
                    let denom = 1_000_000i64;
                    let numer = (*n * denom as f64) as i64;
                    let g = gcd_i64(numer.abs(), denom);
                    Value::Rat(numer / g, denom / g)
                }
                Value::Str(s) => {
                    if s.contains('.') {
                        let trimmed = s.trim();
                        let negative = trimmed.starts_with('-');
                        let abs_str = if negative { &trimmed[1..] } else { trimmed };
                        if let Some((int_part, frac_part)) = abs_str.split_once('.') {
                            let frac_digits = frac_part.len() as u32;
                            let denom = 10i64.pow(frac_digits);
                            let int_val = int_part.parse::<i64>().unwrap_or(0);
                            let frac_val = frac_part.parse::<i64>().unwrap_or(0);
                            let numer = int_val * denom + frac_val;
                            let numer = if negative { -numer } else { numer };
                            let g = gcd_i64(numer.abs(), denom);
                            Value::Rat(numer / g, denom / g)
                        } else {
                            value
                        }
                    } else {
                        let n = s.trim().parse::<i64>().unwrap_or(0);
                        Value::Rat(n, 1)
                    }
                }
                _ => value,
            }
        }
        "Complex" => match &value {
            Value::Complex(_, _) => value,
            Value::Int(n) => Value::Complex(*n as f64, 0.0),
            Value::Num(n) => Value::Complex(*n, 0.0),
            Value::Rat(n, d) if *d != 0 => Value::Complex(*n as f64 / *d as f64, 0.0),
            Value::FatRat(n, d) if *d != 0 => Value::Complex(*n as f64 / *d as f64, 0.0),
            Value::BigInt(n) => Value::Complex(n.to_f64().unwrap_or(0.0), 0.0),
            Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => {
                Value::Complex(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0), 0.0)
            }
            Value::Str(s) => match crate::runtime::str_numeric::parse_raku_str_to_numeric(s) {
                Some(Value::Complex(re, im)) => Value::Complex(re, im),
                Some(Value::Int(n)) => Value::Complex(n as f64, 0.0),
                Some(Value::Num(n)) => Value::Complex(n, 0.0),
                Some(Value::Rat(n, d)) if d != 0 => Value::Complex(n as f64 / d as f64, 0.0),
                Some(Value::FatRat(n, d)) if d != 0 => Value::Complex(n as f64 / d as f64, 0.0),
                _ => value,
            },
            _ => value,
        },
        "Bool" => Value::Bool(value.truthy()),
        _ => value,
    }
}

fn gcd_i64(a: i64, b: i64) -> i64 {
    if b == 0 { a } else { gcd_i64(b, a % b) }
}

impl Interpreter {
    /// Coerce a value to the target type, trying built-in coercions first,
    /// then falling back to target class COERCE when available.
    pub(in crate::runtime) fn try_coerce_value_with_method(
        &mut self,
        target: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let base_target =
            if target.ends_with(":D") || target.ends_with(":U") || target.ends_with(":_") {
                &target[..target.len() - 2]
            } else {
                target
            };
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &value
            && self.class_has_method(&class_name.resolve(), base_target)
        {
            let (coerced, _) = self.run_instance_method(
                &class_name.resolve(),
                (**attributes).clone(),
                base_target,
                vec![],
                Some(value.clone()),
            )?;
            if self.type_matches_value(base_target, &coerced) {
                return Ok(coerced);
            }
            return Err(coerce_impossible_error(target, &value));
        }
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &value
            && self
                .class_mro(&class_name.resolve())
                .iter()
                .any(|c| c == "Str")
            && let Some(inner) = attributes.get("value").cloned()
            && !matches!(inner, Value::Nil)
            && let Ok(coerced) = self.try_coerce_value_with_method(target, inner)
            && self.type_matches_value(base_target, &coerced)
        {
            return Ok(coerced);
        }
        if let Some(variants) = self.enum_types.get(base_target).cloned()
            && let Some(enum_value) =
                self.coerce_to_enum_variant(base_target, &variants, value.clone())
        {
            return Ok(enum_value);
        }
        let result = coerce_value(target, value.clone());
        // If the coercion returned a Failure, propagate it directly
        if is_failure_value(&result) {
            return Ok(result);
        }
        if self.type_matches_value(base_target, &result) {
            return Ok(result);
        }
        if let Ok(coerced) = self.call_method_with_values(value.clone(), base_target, vec![]) {
            // If the method returned a Failure, propagate it directly
            if is_failure_value(&coerced) {
                return Ok(coerced);
            }
            if self.type_matches_value(base_target, &coerced) {
                return Ok(coerced);
            }
            return Err(coerce_impossible_error(target, &value));
        }
        if self.classes.contains_key(base_target) {
            // Wrap Pair values in a Scalar container so they are passed as
            // positional arguments to COERCE/new rather than being flattened
            // into named arguments by the method dispatch logic.
            let coerce_arg = match &value {
                Value::Pair(..) | Value::ValuePair(..) => Value::Scalar(Box::new(value.clone())),
                _ => value.clone(),
            };
            // Try COERCE method first
            if let Ok(coerced) = self.call_method_with_values(
                Value::Package(Symbol::intern(base_target)),
                "COERCE",
                vec![coerce_arg.clone()],
            ) {
                if self.type_matches_value(base_target, &coerced) {
                    return Ok(coerced);
                }
                return Err(coerce_impossible_error(target, &value));
            }
            // Fallback: try calling `new` on the target class with the value,
            // but only if there's an explicit `new` variant that accepts a
            // positional parameter matching the value type.  The default
            // constructor (named-only params) must NOT be used for coercion.
            if self.class_has_new_accepting_positional(base_target, &value)
                && let Ok(coerced) = self.call_method_with_values(
                    Value::Package(Symbol::intern(base_target)),
                    "new",
                    vec![coerce_arg],
                )
                && self.type_matches_value(base_target, &coerced)
            {
                return Ok(coerced);
            }
        }
        Err(coerce_impossible_error(target, &value))
    }

    pub(crate) fn coerce_value_for_constraint(&mut self, constraint: &str, value: Value) -> Value {
        self.try_coerce_value_for_constraint(constraint, value.clone())
            .unwrap_or(value)
    }

    pub(crate) fn try_coerce_value_for_constraint(
        &mut self,
        constraint: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let (constraint, _) = strip_type_smiley(constraint);
        if let Some((target, source)) = parse_coercion_type(constraint) {
            let intermediate = if let Some(src) = source {
                self.try_coerce_value_for_constraint(src, value)?
            } else {
                value
            };
            let resolved_target = self.resolve_constraint_alias(target);
            return self.try_coerce_value_with_method(&resolved_target, intermediate);
        }
        let resolved_constraint = self.resolve_constraint_alias(constraint);
        if resolved_constraint != constraint {
            return self.try_coerce_value_for_constraint(&resolved_constraint, value);
        }
        if let Some(subset) = self.subsets.get(resolved_constraint.as_str()).cloned()
            && subset.base != resolved_constraint
        {
            return self.try_coerce_value_for_constraint(&subset.base, value);
        }
        Ok(value)
    }
}
