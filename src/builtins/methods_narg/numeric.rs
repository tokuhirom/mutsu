use crate::runtime;
use crate::value::{Value, ValueView};

// ── 1-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 1-argument method call on a Value.
/// Compute the nth roots of a number. Used by both the `.roots` method and the
/// `roots()` builtin function.
///
/// Rakudo's `Numeric.roots` is `(^$n).map: { Complex.new-from-polar($mag ** (1/$n),
/// ($angle + $_ * 2 * pi) / $n) }`, guarded by three scalar early returns, and
/// this mirrors it exactly:
///
/// - `$n < 1` answers a bare `NaN` (a `Num`, not a one-element list);
/// - `$n == 1` answers the receiver coerced to `Complex` (`4.roots(1)` is
///   `4+0i`, `Inf.roots(1)` is `Inf+0i`);
/// - a non-finite polar magnitude or angle answers a bare `NaN`
///   (`Inf.roots(2)`, `NaN.roots(2)`, `(Inf+1i).roots(2)`).
///
/// Every element of the list form is a `Complex`. The tempting cleanup -- turn
/// a root whose imaginary part is a rounding-error epsilon back into a `Num` --
/// is what made `4.roots(2)` answer `(2e0, -2e0)` where Rakudo answers
/// `(2+0i, -2+2.4492935982947064e-16i)`.
pub(crate) fn compute_roots(target: &Value, n_arg: &Value) -> Value {
    let n_int = match n_arg.view() {
        ValueView::Int(i) => i,
        ValueView::Num(f) => f as i64,
        ValueView::Rat(n, d) if d != 0 => n / d,
        ValueView::BigInt(bi) => {
            use num_traits::ToPrimitive;
            bi.to_i64().unwrap_or(0)
        }
        ValueView::Str(s) => s.parse::<i64>().unwrap_or(0),
        ValueView::Bool(b) => {
            if b {
                1
            } else {
                0
            }
        }
        _ => runtime::to_int(n_arg),
    };

    // n < 1: a bare NaN, not a list.
    if n_int < 1 {
        return Value::num(f64::NAN);
    }

    let n = n_int as usize;

    // Get the complex parts of the target
    let (re, im) = match runtime::to_complex_parts(target) {
        Some(parts) => parts,
        None => {
            // If we can't convert, try as float
            let f = runtime::to_float_value(target).unwrap_or(f64::NAN);
            (f, 0.0)
        }
    };

    // n == 1: the receiver itself, as a Complex.
    if n == 1 {
        return Value::complex(re, im);
    }

    // Polar form. A non-finite magnitude or angle has no meaningful root set;
    // Rakudo answers a bare NaN rather than a list of Inf/NaN components.
    let r = (re * re + im * im).sqrt();
    let theta = im.atan2(re);
    if !r.is_finite() || !theta.is_finite() {
        return Value::num(f64::NAN);
    }

    let mag = r.powf(1.0 / n as f64);
    let mut roots = Vec::with_capacity(n);
    for k in 0..n {
        let angle = (theta + 2.0 * std::f64::consts::PI * k as f64) / n as f64;
        roots.push(Value::complex(mag * angle.cos(), mag * angle.sin()));
    }
    Value::array(roots)
}
