//! Shared numeric formulas for routine and method dispatch.

use crate::value::{RuntimeError, Value};

/// Cost: O(1).
pub(crate) fn log10(x: f64) -> f64 {
    x.ln() / 10.0f64.ln()
}

/// Cost: O(1).
pub(crate) fn atanh(x: f64) -> f64 {
    0.5 * ((1.0 + x) / (1.0 - x)).ln()
}

/// The real inverse-hyperbolic forms that use a reciprocal. Zero produces
/// the same lazy divide-by-zero Failure as `1 / x` in Raku.
// Cost: O(1).
pub(crate) fn inverse_hyperbolic_reciprocal(method: &str, x: f64) -> Value {
    if x == 0.0 {
        return RuntimeError::divide_by_zero_failure(Some(Value::int(1)), Some("/"));
    }
    let reciprocal = 1.0 / x;
    Value::num(match method {
        "asech" => (reciprocal + (reciprocal * reciprocal - 1.0).sqrt()).ln(),
        "acosech" => (reciprocal + (reciprocal * reciprocal + 1.0).sqrt()).ln(),
        "acotanh" => atanh(reciprocal),
        _ => f64::NAN,
    })
}

/// The Failure shared by real and complex inverse-hyperbolic zero paths.
// Cost: O(1).
pub(crate) fn reciprocal_divide_by_zero_failure() -> Value {
    RuntimeError::divide_by_zero_failure(Some(Value::int(1)), Some("/"))
}
