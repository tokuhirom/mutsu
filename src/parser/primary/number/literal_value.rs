//! Pure numeric spelling probes shared with source-node construction.

use super::{Expr, Value, decimal, integer_no_warn};

/// The Rat value of a plain decimal spelling, used for imaginary components.
// Cost: O(n), n = literal length.
pub(crate) fn decimal_literal_value(text: &str) -> Option<Value> {
    match decimal(text) {
        Ok(("", Expr::Literal(value))) => Some(value),
        _ => None,
    }
}

/// Parse a complete numeric literal without executing code or emitting warnings.
// Cost: O(n), n = literal length.
pub(crate) fn numeric_literal_value(text: &str) -> Option<Value> {
    match decimal(text).or_else(|_| integer_no_warn(text)) {
        Ok(("", Expr::Literal(value))) => Some(value),
        _ => None,
    }
}
