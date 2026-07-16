use super::*;
use crate::value::ValueView;
use num_traits::ToPrimitive;

/// Check if a Value is a Failure instance.
fn is_failure_value(value: &Value) -> bool {
    matches!(value.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
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
        "Int" => match value.view() {
            ValueView::Int(_) => value,
            ValueView::Num(n) => Value::int(n as i64),
            ValueView::Rat(_, 0) => {
                RuntimeError::divide_by_zero_failure_for_method("Int", "Rational")
            }
            ValueView::Rat(n, d) => Value::int(n / d),
            ValueView::FatRat(_, 0) => {
                RuntimeError::divide_by_zero_failure_for_method("Int", "Rational")
            }
            ValueView::FatRat(n, d) => Value::int(n / d),
            ValueView::Str(s) => Value::int(s.parse::<i64>().unwrap_or(0)),
            ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
            _ => value,
        },
        "Num" => match value.view() {
            ValueView::Num(_) => value,
            ValueView::Int(n) => Value::num(n as f64),
            ValueView::Rat(n, d) => Value::num(n as f64 / d as f64),
            ValueView::Str(s) => Value::num(s.parse::<f64>().unwrap_or(0.0)),
            _ => value,
        },
        "Str" => Value::str(crate::runtime::utils::coerce_to_str(&value)),
        "Array" | "List" => crate::runtime::utils::coerce_to_array(value),
        "Hash" => crate::runtime::utils::coerce_to_hash(value),
        "Rat" => {
            match value.view() {
                ValueView::Rat(_, _) => value,
                ValueView::Int(n) => Value::rat_raw(n, 1),
                ValueView::Num(n) => {
                    // Simple float to rat conversion
                    let denom = 1_000_000i64;
                    let numer = (n * denom as f64) as i64;
                    let g = gcd_i64(numer.abs(), denom);
                    Value::rat_raw(numer / g, denom / g)
                }
                ValueView::Str(s) => {
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
                            Value::rat_raw(numer / g, denom / g)
                        } else {
                            value
                        }
                    } else {
                        let n = s.trim().parse::<i64>().unwrap_or(0);
                        Value::rat_raw(n, 1)
                    }
                }
                _ => value,
            }
        }
        "Complex" => match value.view() {
            ValueView::Complex(_, _) => value,
            ValueView::Int(n) => Value::complex(n as f64, 0.0),
            ValueView::Num(n) => Value::complex(n, 0.0),
            ValueView::Rat(n, d) if d != 0 => Value::complex(n as f64 / d as f64, 0.0),
            ValueView::FatRat(n, d) if d != 0 => Value::complex(n as f64 / d as f64, 0.0),
            ValueView::BigInt(n) => Value::complex(n.to_f64().unwrap_or(0.0), 0.0),
            ValueView::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => {
                Value::complex(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0), 0.0)
            }
            ValueView::Str(s) => match crate::runtime::str_numeric::parse_raku_str_to_numeric(&s) {
                Some(parsed) => match parsed.view() {
                    ValueView::Complex(re, im) => Value::complex(re, im),
                    ValueView::Int(n) => Value::complex(n as f64, 0.0),
                    ValueView::Num(n) => Value::complex(n, 0.0),
                    ValueView::Rat(n, d) if d != 0 => Value::complex(n as f64 / d as f64, 0.0),
                    ValueView::FatRat(n, d) if d != 0 => Value::complex(n as f64 / d as f64, 0.0),
                    _ => value,
                },
                None => value,
            },
            _ => value,
        },
        "Bool" => Value::truth(value.truthy()),
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
        // An Instance that already satisfies the target type needs no coercion
        // (e.g. re-coercing an IO::Path for an `IO(Cool)` constraint, which the
        // declaration path can trigger on an already-coerced value). Returning it
        // directly avoids re-dispatching a coercion method that may not exist on
        // the native type via the slow path.
        if matches!(value.view(), ValueView::Instance { .. })
            && self.type_matches_value(base_target, &value)
        {
            return Ok(value);
        }
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = value.view()
            && self.class_has_user_method(&class_name.resolve(), base_target)
        {
            let (coerced, _) = self.run_instance_method(
                &class_name.resolve(),
                attributes.to_map(),
                base_target,
                vec![],
                Some(value.clone()),
            )?;
            if self.type_matches_value(base_target, &coerced) {
                return Ok(coerced);
            }
            return Err(coerce_impossible_error(target, &value));
        }
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = value.view()
            && self
                .class_mro(&class_name.resolve())
                .iter()
                .any(|c| c == "Str")
            && let Some(inner) = attributes.as_map().get("value").cloned()
            && !inner.is_nil()
            && let Ok(coerced) = self.try_coerce_value_with_method(target, inner)
            && self.type_matches_value(base_target, &coerced)
        {
            return Ok(coerced);
        }
        let variants = self.registry().enum_types.get(base_target).cloned();
        if let Some(variants) = variants
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
        if self.registry().classes.contains_key(base_target) {
            // Wrap Pair values in a Scalar container so they are passed as
            // positional arguments to COERCE/new rather than being flattened
            // into named arguments by the method dispatch logic.
            let coerce_arg = match value.view() {
                ValueView::Pair(..) | ValueView::ValuePair(..) => Value::scalar(value.clone()),
                _ => value.clone(),
            };
            // Try COERCE method first
            if let Ok(coerced) = self.call_method_with_values(
                Value::package(Symbol::intern(base_target)),
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
                    Value::package(Symbol::intern(base_target)),
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

    /// Try to coerce a string to a specific type (e.g., "42" -> Int(42)).
    /// Returns None if coercion is not possible or not applicable.
    pub(crate) fn try_coerce_str_to_type(&self, s: &str, type_name: &str) -> Option<Value> {
        match type_name {
            "Int" => s.parse::<i64>().ok().map(Value::int),
            "Num" => s.parse::<f64>().ok().map(Value::num),
            "Rat" => {
                if let Ok(n) = s.parse::<i64>() {
                    Some(Value::rat_raw(n, 1))
                } else {
                    None
                }
            }
            "Str" => Some(Value::str(s.to_string())),
            "Bool" => match s {
                "True" => Some(Value::TRUE),
                "False" => Some(Value::FALSE),
                _ => None,
            },
            _ => None,
        }
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
        let subset = self
            .registry()
            .subsets
            .get(resolved_constraint.as_str())
            .cloned();
        if let Some(subset) = subset
            && subset.base != resolved_constraint
        {
            return self.try_coerce_value_for_constraint(&subset.base, value);
        }
        Ok(value)
    }
}
