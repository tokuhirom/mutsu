use crate::runtime;
use crate::value::{Value, ValueView};
use num_bigint::BigInt;
use num_traits::Zero;

pub(crate) fn sample_weighted_mix_key(items: &crate::value::MixData) -> Option<Value> {
    let mut total = 0.0;
    for weight in items.values() {
        if weight.is_finite() && *weight > 0.0 {
            total += *weight;
        }
    }
    if total <= 0.0 {
        return None;
    }
    let mut needle = crate::builtins::rng::builtin_rand() * total;
    for (key, weight) in items.iter() {
        if !weight.is_finite() || *weight <= 0.0 {
            continue;
        }
        if needle <= *weight {
            return Some(items.typed_key(key));
        }
        needle -= *weight;
    }
    items
        .iter()
        .find_map(|(key, weight)| (*weight > 0.0).then(|| items.typed_key(key)))
}

pub(crate) fn sample_weighted_bag_key(items: &crate::value::BagData) -> Option<Value> {
    use crate::runtime::utils::bigint_to_i128_sat;
    let mut total: i128 = 0;
    for count in items.values() {
        let count = bigint_to_i128_sat(count);
        if count > 0 {
            total = total.saturating_add(count);
        }
    }
    if total <= 0 {
        return None;
    }
    let needle_f = crate::builtins::rng::builtin_rand() * total as f64;
    let mut needle = needle_f as i128;
    if needle >= total {
        needle = total - 1;
    }
    for (key, count) in items.iter() {
        let count = bigint_to_i128_sat(count);
        if count <= 0 {
            continue;
        }
        if needle < count {
            return Some(items.typed_key(key));
        }
        needle -= count;
    }
    items
        .iter()
        .find_map(|(key, count)| (*count > BigInt::zero()).then(|| items.typed_key(key)))
}

pub(crate) fn int_to_superscript(n: i64) -> String {
    const SUPER_DIGITS: [char; 10] = [
        '\u{2070}', '\u{00B9}', '\u{00B2}', '\u{00B3}', '\u{2074}', '\u{2075}', '\u{2076}',
        '\u{2077}', '\u{2078}', '\u{2079}',
    ];
    let s = n.to_string();
    s.chars()
        .map(|c| match c {
            '-' => '\u{207B}', // superscript minus
            d if d.is_ascii_digit() => SUPER_DIGITS[(d as u8 - b'0') as usize],
            _ => c,
        })
        .collect()
}

pub(crate) fn int_to_subscript(n: i64) -> String {
    const SUB_DIGITS: [char; 10] = [
        '\u{2080}', '\u{2081}', '\u{2082}', '\u{2083}', '\u{2084}', '\u{2085}', '\u{2086}',
        '\u{2087}', '\u{2088}', '\u{2089}',
    ];
    let s = n.to_string();
    s.chars()
        .map(|c| match c {
            '-' => '\u{208B}', // subscript minus
            d if d.is_ascii_digit() => SUB_DIGITS[(d as u8 - b'0') as usize],
            _ => c,
        })
        .collect()
}

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
