//! `$*TOLERANCE` and the approximate-equality test built on it.
//!
//! One place answers "how close is close enough" for everything that asks:
//! `infix:<=~=>` / `≅` (and its hyper/meta forms) and the `Complex` coercions
//! to a `Real` type, whose imaginary part must be `≅ 0`.

use super::{DEFAULT_TOLERANCE, Interpreter};
use crate::value::{Value, ValueView};

/// The value of a `$*TOLERANCE` binding as a float; `None` when it is not a
/// real number (the caller then uses [`DEFAULT_TOLERANCE`]).
// Cost: O(1).
fn tolerance_as_f64(v: &Value) -> Option<f64> {
    match v.view() {
        ValueView::Num(n) => Some(n),
        ValueView::Rat(n, d) if d != 0 => Some(crate::value::rat_to_f64(n, d)),
        ValueView::Int(n) => Some(n as f64),
        _ => None,
    }
}

/// Rakudo's `a =~= b` on two floats with an explicit `tolerance`.
///
/// Two identical infinities are equal whatever the tolerance (the relative
/// formula would compute `Inf/Inf`); `NaN` equals nothing. Finite equal values
/// are NOT short-circuited: the difference must be strictly LESS than the
/// tolerance, so `$*TOLERANCE = 0` makes every finite comparison false, even
/// `1 ≅ 1`. When either side is zero the difference is absolute, otherwise it
/// is relative to the larger magnitude.
// Cost: O(1).
pub(crate) fn approx_eq_f64(a: f64, b: f64, tolerance: f64) -> bool {
    if a == b && a.is_infinite() {
        return true;
    }
    if a.is_nan() || b.is_nan() {
        return false;
    }
    let diff = (a - b).abs();
    if a == 0.0 || b == 0.0 {
        return diff < tolerance;
    }
    diff < tolerance * a.abs().max(b.abs())
}

impl Interpreter {
    /// The `$*TOLERANCE` in effect, or Rakudo's default when the dynamic
    /// lookup finds no binding (it walks only the caller stack, never the lazy
    /// magic table, so the unset case is the default).
    // Cost: O(d), d = caller-stack depth searched for a dynamic `$*TOLERANCE`.
    pub(crate) fn current_tolerance(&self) -> f64 {
        self.get_dynamic_var("*TOLERANCE")
            .ok()
            .and_then(|v| tolerance_as_f64(&v))
            .unwrap_or(DEFAULT_TOLERANCE)
    }

    /// Whether the imaginary part `im` of a `Complex` counts as zero, so that
    /// the number may be coerced to a `Real` type: `im ≅ 0` under the current
    /// `$*TOLERANCE`. Rakudo's `Complex.Real` (and so `.Int`, `.Num`, `.Rat`,
    /// ...) asks exactly this, which is why `$*TOLERANCE = 0` rejects even
    /// `3+0i`.
    // Cost: O(d), d = caller-stack depth searched for a dynamic `$*TOLERANCE`.
    pub(crate) fn complex_im_is_negligible(&self, im: f64) -> bool {
        approx_eq_f64(im, 0.0, self.current_tolerance())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn approx_eq_f64_is_relative_away_from_zero_and_absolute_at_zero() {
        let tol = 1e-15;
        assert!(approx_eq_f64(1.0, 1.0 + 1e-16, tol));
        assert!(!approx_eq_f64(1.0, 1.0 + 1e-14, tol));
        // Relative: the same absolute gap passes at a large magnitude.
        assert!(approx_eq_f64(1e10, 1e10 + 1e-6, tol));
        // Against zero the gap is absolute, and strictly below the tolerance.
        assert!(approx_eq_f64(0.0, 1e-20, tol));
        assert!(!approx_eq_f64(0.0, 1e-15, tol));
        assert!(!approx_eq_f64(1e-15, 0.0, tol));
    }

    #[test]
    fn approx_eq_f64_zero_tolerance_rejects_every_finite_pair() {
        assert!(!approx_eq_f64(1.0, 1.0, 0.0));
        assert!(!approx_eq_f64(0.0, 0.0, 0.0));
    }

    #[test]
    fn approx_eq_f64_handles_infinities_and_nan() {
        assert!(approx_eq_f64(f64::INFINITY, f64::INFINITY, 0.0));
        assert!(approx_eq_f64(f64::NEG_INFINITY, f64::NEG_INFINITY, 1e-15));
        assert!(!approx_eq_f64(f64::INFINITY, f64::NEG_INFINITY, 1e-15));
        assert!(!approx_eq_f64(f64::NAN, f64::NAN, 1.0));
        assert!(!approx_eq_f64(f64::NAN, 0.0, 1.0));
    }
}
