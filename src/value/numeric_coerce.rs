//! Numeric coercion of a `Value` to a native `f64` / `i64`. Below the runtime
//! so `Value`'s own methods use the same coercion without naming
//! `runtime::utils` (#10779); the runtime re-exports them.

use crate::value::{Value, ValueView};
use num_traits::{Signed, ToPrimitive, Zero};

/// The `f64` a value numifies to, or `None` when it is not numeric.
// Cost: O(1) for scalars; O(digits) for a `BigRat` that overflows `f64`.
pub(crate) fn to_float_value(val: &Value) -> Option<f64> {
    match val.view() {
        ValueView::ContainerRef(cell) => to_float_value(&cell.lock().unwrap()),
        ValueView::Scalar(inner) => to_float_value(inner),
        ValueView::Mixin(inner, _) => to_float_value(inner),
        ValueView::Num(f) => Some(f),
        ValueView::Int(i) => Some(i as f64),
        ValueView::BigInt(n) => n.to_f64(),
        ValueView::Rat(n, d) => {
            if d != 0 {
                Some(n as f64 / d as f64)
            } else if n > 0 {
                Some(f64::INFINITY)
            } else if n < 0 {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        ValueView::FatRat(n, d) => {
            if d != 0 {
                Some(n as f64 / d as f64)
            } else if n > 0 {
                Some(f64::INFINITY)
            } else if n < 0 {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        ValueView::BigRat(n, d) => {
            if !d.is_zero() {
                if let (Some(nn), Some(dd)) = (n.to_f64(), d.to_f64())
                    && nn.is_finite()
                    && dd.is_finite()
                {
                    Some(nn / dd)
                } else {
                    let scale_pow = 30u32;
                    let scale = num_bigint::BigInt::from(10u8).pow(scale_pow);
                    let scaled = (n * &scale) / d;
                    if let Some(scaled_f) = scaled
                        .to_f64()
                        .or_else(|| scaled.to_string().parse::<f64>().ok())
                    {
                        Some(scaled_f / 10f64.powi(scale_pow as i32))
                    } else if n.is_zero() {
                        Some(0.0)
                    } else if n.is_positive() {
                        Some(f64::INFINITY)
                    } else {
                        Some(f64::NEG_INFINITY)
                    }
                }
            } else if n.is_positive() {
                Some(f64::INFINITY)
            } else if n.is_negative() {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        ValueView::Complex(r, i) => {
            if i == 0.0 {
                Some(r)
            } else {
                None
            }
        }
        ValueView::Enum { value, .. } => Some(value.as_i64() as f64),
        ValueView::Bool(b) => Some(if b { 1.0 } else { 0.0 }),
        ValueView::Str(s) => {
            let t = s.trim();
            // Raku numifies an empty (or whitespace-only) string to 0 — `+""`
            // is `0` and `"" == 0` is True. Without this the string compared
            // as "not a number at all" and `==` answered False.
            if t.is_empty() {
                Some(0.0)
            } else {
                t.parse::<f64>().ok()
            }
        }
        ValueView::Nil => Some(0.0),
        ValueView::Set(items, _) => Some(items.len() as f64),
        ValueView::Bag(items, _) => Some(items.len() as f64),
        ValueView::Mix(items, _) => Some(items.len() as f64),
        ValueView::Hash(items) => Some(items.len() as f64),
        _ if val.as_list_items().is_some() => Some(val.as_list_items().unwrap().len() as f64),
        ValueView::LazyList(ll) => {
            if let Some(cached) = ll.cache.lock().unwrap().as_ref() {
                Some(cached.len() as f64)
            } else {
                Some(0.0)
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => attributes.as_map().get("value").and_then(to_float_value),
        ValueView::Instance { .. } if val.is_match_instance() => {
            val.match_str_value().as_ref().and_then(to_float_value)
        }
        ValueView::Instance { attributes, .. } => {
            super::numeric_payload::numeric_payload_of(&attributes)
                .as_ref()
                .and_then(to_float_value)
        }
        // A type object (e.g. `Any`, `Str`, `Rat`, a user class) numifies to 0 in
        // numeric context (Raku warns "Use of uninitialized value"). This makes
        // `(Any) == 0` / `@a[oob] == 0` true, matching Rakudo. (`Mu` alone has no
        // Numeric and dies in raku, but treating it as 0 is a harmless divergence.)
        ValueView::Package(_) => Some(0.0),
        _ => None,
    }
}

/// The `i64` a value numifies to (0 when it is not numeric); a list-like
/// value answers its element count.
// Cost: O(1), O(len) for a numeric `Str`.
pub(crate) fn to_int(v: &Value) -> i64 {
    match v.view() {
        // Phase 2 element container: a `:=`-bound element cell that leaked into
        // a numeric context reads through to its inner value.
        ValueView::ContainerRef(cell) => to_int(&cell.lock().unwrap()),
        ValueView::Scalar(inner) => to_int(inner),
        ValueView::Mixin(inner, _) => to_int(inner),
        ValueView::Int(i) => i,
        ValueView::BigInt(n) => {
            use num_traits::ToPrimitive;
            n.as_ref()
                .to_i64()
                .unwrap_or(if **n > num_bigint::BigInt::from(0i64) {
                    i64::MAX
                } else {
                    i64::MIN
                })
        }
        ValueView::Num(f) => f as i64,
        ValueView::Range(a, b) => {
            if b >= a {
                b - a + 1
            } else {
                0
            }
        }
        ValueView::RangeExcl(a, b) | ValueView::RangeExclStart(a, b) => {
            if b > a {
                b - a
            } else {
                0
            }
        }
        ValueView::RangeExclBoth(a, b) => {
            if b > a + 1 {
                b - a - 1
            } else {
                0
            }
        }
        ValueView::Rat(n, d) => {
            if d != 0 {
                n / d
            } else {
                0
            }
        }
        ValueView::Complex(r, _) => r as i64,
        // An enum value numifies to its underlying value (`+MYSQL_TYPE_DOUBLE`
        // is 5) — without this a CStruct field write of an enum stored 0.
        ValueView::Enum { value, .. } => value.as_i64(),
        ValueView::Str(s) => s.parse().unwrap_or(0),
        // A regex capture is a Match object, but numeric context uses the
        // captured text.  This is especially important for named captures
        // passed through a slurpy argument list (for example DateTime.new
        // receiving `%/.hash`).
        ValueView::Instance { .. } if v.is_match_instance() => {
            v.match_str_value().as_ref().map_or(0, to_int)
        }
        ValueView::Array(items, ..) => items.len() as i64,
        ValueView::Hash(items) => items.len() as i64,
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            items.len() as i64
        }
        ValueView::Slip(items) => items.len() as i64,
        ValueView::Capture { positional, .. } => positional.len() as i64,
        ValueView::Instance { attributes, .. } => {
            super::numeric_payload::numeric_payload_of(&attributes)
                .as_ref()
                .map_or(0, to_int)
        }
        _ => 0,
    }
}
