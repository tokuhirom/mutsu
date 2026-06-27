#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::value::Value;
use num_bigint::BigInt;
use num_traits::Zero;
use std::collections::HashMap;

pub(crate) fn sample_weighted_mix_key(items: &HashMap<String, f64>) -> Option<Value> {
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
    for (key, weight) in items {
        if !weight.is_finite() || *weight <= 0.0 {
            continue;
        }
        if needle <= *weight {
            return Some(Value::str(key.clone()));
        }
        needle -= *weight;
    }
    items
        .iter()
        .find_map(|(key, weight)| (*weight > 0.0).then(|| Value::str(key.clone())))
}

pub(crate) fn sample_weighted_bag_key(items: &HashMap<String, BigInt>) -> Option<Value> {
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
    for (key, count) in items {
        let count = bigint_to_i128_sat(count);
        if count <= 0 {
            continue;
        }
        if needle < count {
            return Some(Value::str(key.clone()));
        }
        needle -= count;
    }
    items
        .iter()
        .find_map(|(key, count)| (*count > BigInt::zero()).then(|| Value::str(key.clone())))
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
/// `roots()` builtin function. Handles edge cases: n <= 0 returns [NaN],
/// NaN/Inf inputs with n=1 return the input as Complex.
pub(crate) fn compute_roots(target: &Value, n_arg: &Value) -> Value {
    let n_int = match n_arg {
        Value::Int(i) => *i,
        Value::Num(f) => *f as i64,
        Value::Rat(n, d) if *d != 0 => *n / *d,
        Value::BigInt(bi) => {
            use num_traits::ToPrimitive;
            bi.to_i64().unwrap_or(0)
        }
        Value::Str(s) => s.parse::<i64>().unwrap_or(0),
        Value::Bool(b) => {
            if *b {
                1
            } else {
                0
            }
        }
        _ => runtime::to_int(n_arg),
    };

    // n <= 0: return [NaN]
    if n_int <= 0 {
        return Value::array(vec![Value::Num(f64::NAN)]);
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

    // Handle NaN: return [NaN+0i] (Complex NaN)
    if re.is_nan() || im.is_nan() {
        let mut roots = Vec::with_capacity(n);
        for _ in 0..n {
            roots.push(Value::Complex(f64::NAN, 0.0));
        }
        return Value::array(roots);
    }

    // Handle Inf/-Inf: return [Inf+0i] or [-Inf+0i] etc.
    if re.is_infinite() || im.is_infinite() {
        let mut roots = Vec::with_capacity(n);
        // For n=1, return the value itself as Complex
        // For n>1, the roots involve Inf which is complex
        for k in 0..n {
            if n == 1 {
                roots.push(Value::Complex(re, im));
            } else {
                // Infinity roots: magnitude is Inf, angles vary
                let theta = im.atan2(re);
                let angle = (theta + 2.0 * std::f64::consts::PI * k as f64) / n as f64;
                let rr = f64::INFINITY * angle.cos();
                let ii = f64::INFINITY * angle.sin();
                if ii.abs() < 1e-12 {
                    roots.push(Value::Num(rr));
                } else {
                    roots.push(Value::Complex(rr, ii));
                }
            }
        }
        return Value::array(roots);
    }

    // Check if target is zero
    if re == 0.0 && im == 0.0 {
        let mut roots = Vec::with_capacity(n);
        for _ in 0..n {
            roots.push(Value::Complex(0.0, 0.0));
        }
        return Value::array(roots);
    }

    // Normal case: compute nth roots using polar form
    let r = (re * re + im * im).sqrt();
    let theta = im.atan2(re);
    let mag = r.powf(1.0 / n as f64);
    let mut roots = Vec::with_capacity(n);
    for k in 0..n {
        let angle = (theta + 2.0 * std::f64::consts::PI * k as f64) / n as f64;
        let rr = mag * angle.cos();
        let ii = mag * angle.sin();
        if ii.abs() < 1e-12 {
            roots.push(Value::Num(rr));
        } else {
            roots.push(Value::Complex(rr, ii));
        }
    }
    Value::array(roots)
}
