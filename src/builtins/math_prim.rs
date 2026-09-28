//! Shared numeric formulas for routine and method dispatch.

/// Cost: O(1).
pub(crate) fn log10(x: f64) -> f64 {
    x.ln() / 10.0f64.ln()
}

/// Cost: O(1).
pub(crate) fn atanh(x: f64) -> f64 {
    0.5 * ((1.0 + x) / (1.0 - x)).ln()
}
