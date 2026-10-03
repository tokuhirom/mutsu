//! Decoding a Mix/MixHash weight (stored as `f64`) back to the Raku value it
//! stands for, and the one rule for printing it beside its key. The weight
//! *arithmetic* lives in `builtins::mix_weight`.

use super::{Value, make_rat};

/// Convert a Mix/MixHash weight (f64) back to a Raku Value.
/// Returns Int for whole numbers, Rat for representable fractions, Num otherwise.
pub fn mix_weight_to_value(w: f64) -> Value {
    if w.is_nan() || w.is_infinite() {
        return Value::Num(w);
    }
    // Check for exact integer
    if w == (w as i64 as f64) && w.abs() < i64::MAX as f64 {
        return Value::Int(w as i64);
    }
    // Try to reconstruct as Rat: use the decimal representation to find
    // a rational number. Multiply by powers of 10 to clear the decimal.
    // This works for values like 42.1, 1.5, 3.14 etc.
    let s = format!("{}", w);
    if let Some(dot_pos) = s.find('.') {
        let decimals = s.len() - dot_pos - 1;
        if decimals <= 15 {
            let denom = 10i64.checked_pow(decimals as u32);
            if let Some(d) = denom {
                // Parse the string without the dot as numerator
                let without_dot: String = s.chars().filter(|c| *c != '.').collect();
                if let Ok(n) = without_dot.parse::<i64>() {
                    // Verify round-trip: n/d as f64 == w
                    if (crate::value::rat_to_f64(n, d) - w).abs() < f64::EPSILON * w.abs().max(1.0)
                    {
                        return make_rat(n, d);
                    }
                }
            }
        }
    }
    Value::Num(w)
}

/// How a weight is shown beside its key by `.Str` and `.gist`: `None` when the
/// weight is exactly 1, which Rakudo prints as the bare key (`Mix(a b(2))`).
///
/// Both renderers call this so a weight is *printed* the way it is *read back*.
/// They each used to carry their own copy of the rule, whose `w as i64`
/// shortcut for whole weights saturated: `(a => 2e300).Mix` printed
/// `Mix(a(9223372036854775807))`.
pub(crate) fn mix_weight_render(w: f64) -> Option<String> {
    if (w - 1.0).abs() < f64::EPSILON {
        None
    } else {
        Some(mix_weight_to_value(w).to_string_value())
    }
}
