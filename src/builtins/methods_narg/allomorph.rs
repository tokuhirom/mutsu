use crate::symbol::Symbol;
use crate::value::{Value, ValueView};
use num_traits::{ToPrimitive, Zero};
use std::collections::HashMap;

/// Create a Failure value wrapping an X::OutOfRange exception with the given message.
/// Used for operations that should soft-fail rather than throw.
pub(crate) fn out_of_range_failure(message: &str) -> Value {
    let mut ex_attrs = HashMap::new();
    ex_attrs.insert("message".to_string(), Value::str(message.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), ex_attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// ACCEPTS for allomorphic types (IntStr, RatStr, NumStr, ComplexStr).
/// The allomorph (target) accepts the argument if:
/// - arg is a plain Str: Str parts are equal
/// - arg is numeric (not allomorph, not Str): numeric parts are equal (==)
/// - arg is allomorphic: numeric parts are equal (==)
/// - arg has .Numeric (custom class): numeric parts are equal (==)
pub(crate) fn allomorph_accepts(target: &Value, arg: &Value) -> Option<bool> {
    let ValueView::Mixin(target_inner, target_mixins) = target.view() else {
        return Some(false);
    };
    let target_str = target_mixins
        .get("Str")
        .map(|v| v.to_string_value())
        .unwrap_or_default();

    // Unwrap Scalar
    let arg = match arg.view() {
        ValueView::Scalar(inner) => inner,
        _ => arg,
    };

    match arg.view() {
        // Plain string: compare with the Str part
        ValueView::Str(s) => Some(**s == target_str),
        // Allomorphic argument: compare numeric parts
        ValueView::Mixin(arg_inner, arg_mixins) if arg_mixins.contains_key("Str") => {
            Some(allomorph_values_numerically_equal(target_inner, arg_inner))
        }
        // Numeric types: compare with the numeric part
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(_, _)
        | ValueView::FatRat(_, _)
        | ValueView::BigRat(_, _)
        | ValueView::Complex(_, _)
        | ValueView::Bool(_) => Some(allomorph_values_numerically_equal(target_inner, arg)),
        // Instance (custom class with .Numeric): needs interpreter to call methods
        ValueView::Instance { .. } => None,
        _ => Some(false),
    }
}

/// Check if two values are numerically equal (like Raku's == operator).
fn allomorph_values_numerically_equal(a: &Value, b: &Value) -> bool {
    let a_f = allomorph_value_to_f64(a);
    let b_f = allomorph_value_to_f64(b);
    match (a_f, b_f) {
        (Some(af), Some(bf)) => {
            // Handle Complex: both real and imaginary must match
            if let (ValueView::Complex(ar, ai), ValueView::Complex(br, bi)) = (a.view(), b.view()) {
                return (ar - br).abs() < 1e-15 && (ai - bi).abs() < 1e-15;
            }
            // For Complex vs non-Complex
            if let ValueView::Complex(ar, ai) = a.view() {
                return (ar - bf).abs() < 1e-15 && ai.abs() < 1e-15;
            }
            if let ValueView::Complex(br, bi) = b.view() {
                return (af - br).abs() < 1e-15 && bi.abs() < 1e-15;
            }
            (af - bf).abs() < 1e-15
        }
        _ => false,
    }
}

fn allomorph_value_to_f64(v: &Value) -> Option<f64> {
    match v.view() {
        ValueView::Int(i) => Some(i as f64),
        ValueView::BigInt(n) => n.to_f64(),
        ValueView::Num(f) => Some(f),
        ValueView::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
        ValueView::FatRat(n, d) if d != 0 => Some(n as f64 / d as f64),
        ValueView::BigRat(n, d) if !d.is_zero() => {
            Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
        }
        ValueView::Complex(r, _) => Some(r),
        ValueView::Bool(b) => Some(if b { 1.0 } else { 0.0 }),
        ValueView::Mixin(inner, _) => allomorph_value_to_f64(inner),
        _ => None,
    }
}
