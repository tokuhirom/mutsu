/// Math and miscellaneous methods: tclc, wordcase, succ, pred, log, log2, log10,
/// exp, atan2, trig functions, Rat, FatRat, tree, encode, sink, item, race/hyper,
/// NFC/NFD/NFKC/NFKD
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView, make_big_fat_rat, make_rat};
use num_traits::ToPrimitive;

use super::complex_math::complex_trig;

/// Convert a string to Rat, handling integer, decimal, scientific notation, and complex forms.
fn str_to_rat(s: &str) -> Value {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return make_rat(0, 1);
    }
    if let Some((n_str, d_str)) = trimmed.split_once('/') {
        let n = n_str.trim().parse::<i64>().unwrap_or(0);
        let d = d_str.trim().parse::<i64>().unwrap_or(1);
        return make_rat(n, if d == 0 { 1 } else { d });
    }
    let real_str = if let Some(pos) = trimmed
        .find('+')
        .or_else(|| trimmed[1..].find('-').map(|p| p + 1))
    {
        let after = &trimmed[pos + 1..];
        if after.ends_with('i') {
            &trimmed[..pos]
        } else {
            trimmed
        }
    } else if trimmed.ends_with('i') {
        return make_rat(0, 1);
    } else {
        trimmed
    };
    if real_str.contains('.') {
        let negative = real_str.starts_with('-');
        let abs_str = if negative { &real_str[1..] } else { real_str };
        let (decimal_part, exp_part) = if let Some(e_pos) = abs_str.find(['e', 'E']) {
            (&abs_str[..e_pos], abs_str[e_pos + 1..].parse::<i32>().ok())
        } else {
            (abs_str, None)
        };
        if let Some((int_part, frac_part)) = decimal_part.split_once('.') {
            let frac_digits = frac_part.len() as u32;
            if let Some(denom) = 10i64.checked_pow(frac_digits) {
                let int_val = int_part.parse::<i64>().unwrap_or(0);
                let frac_val = frac_part.parse::<i64>().unwrap_or(0);
                let mut numer = int_val * denom + frac_val;
                if negative {
                    numer = -numer;
                }
                if let Some(exp) = exp_part {
                    if exp >= 0 {
                        if let Some(factor) = 10i64.checked_pow(exp as u32) {
                            return make_rat(numer * factor, denom);
                        }
                    } else if let Some(factor) = 10i64.checked_pow((-exp) as u32) {
                        return make_rat(numer, denom * factor);
                    }
                }
                return make_rat(numer, denom);
            }
        }
    }
    if let Some(e_pos) = real_str.find(['e', 'E']) {
        let base = &real_str[..e_pos];
        let exp = real_str[e_pos + 1..].parse::<i32>().unwrap_or(0);
        if let Ok(base_val) = base.parse::<i64>() {
            if exp >= 0 {
                if let Some(factor) = 10i64.checked_pow(exp as u32) {
                    return make_rat(base_val * factor, 1);
                }
            } else if let Some(factor) = 10i64.checked_pow((-exp) as u32) {
                return make_rat(base_val, factor);
            }
        }
    }
    let n = real_str.parse::<i64>().unwrap_or(0);
    make_rat(n, 1)
}

/// Recursively apply `.tree` to nested arrays.
/// `.tree` on an Iterable is `$(self.map(*.tree).Seq)`: every node it descends
/// into becomes an *itemized* Seq, so `(1, (2, 3)).tree.raku` is
/// `$((1, $((2, 3).Seq)).Seq)`. A non-Iterable is its own tree.
///
/// `depth` is how many levels still get treed — `.tree(1)` itemizes only the
/// top node and leaves its children as they are, `.tree(0)` is the identity,
/// and `.tree` / `.tree(*)` are `usize::MAX`. Shared with the `.tree(...)`
/// argument forms in `runtime::…::dispatch_tree`.
pub(crate) fn tree_to_depth(v: &Value, depth: usize) -> Value {
    if depth == 0 {
        return v.clone();
    }
    let children = match v.view() {
        ValueView::Array(inner, ..) => inner.to_vec(),
        ValueView::Seq(inner) | ValueView::Slip(inner) => inner.to_vec(),
        ValueView::Hash(_)
        | ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..) => crate::runtime::utils::value_to_list(v),
        _ => return v.clone(),
    };
    let treed = children
        .iter()
        .map(|c| tree_to_depth(c, depth - 1))
        .collect();
    Value::scalar(Value::seq(treed))
}

/// Levenshtein edit distance between two strings (by Unicode scalar), used for
/// the numeric value of a `StrDistance`.
fn levenshtein(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut cur = vec![0usize; b.len() + 1];
    for (i, ca) in a.iter().enumerate() {
        cur[0] = i + 1;
        for (j, cb) in b.iter().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            cur[j + 1] = (prev[j + 1] + 1).min(cur[j] + 1).min(prev[j] + cost);
        }
        std::mem::swap(&mut prev, &mut cur);
    }
    prev[b.len()]
}

/// Real (numeric) value of a "Cool" builtin instance, used by the `.Rat`/`.FatRat`
/// coercers: an `IO::Path` numifies via its path string, a `Match` via its
/// matched substring, and a `StrDistance` via the edit distance between its
/// `before`/`after` strings.
pub(super) fn cool_instance_numeric(target: &Value) -> Option<f64> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    match class_name.resolve().as_str() {
        "IO::Path" => target.to_string_value().trim().parse::<f64>().ok(),
        "Match" => attributes
            .as_map()
            .get("str")
            .and_then(|v| v.to_string_value().trim().parse::<f64>().ok()),
        "StrDistance" => {
            let before = attributes
                .as_map()
                .get("before")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let after = attributes
                .as_map()
                .get("after")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            Some(levenshtein(&before, &after) as f64)
        }
        _ => None,
    }
}

/// Convert a Value to a string for Unicode normalization.
/// If the value is an Array of Int (Uni-like), convert codepoints to a string.
fn uni_or_str(target: &Value) -> String {
    match target.view() {
        ValueView::Array(items, ..)
            if items.iter().all(|v| matches!(v.view(), ValueView::Int(_))) =>
        {
            items
                .iter()
                .filter_map(|v| match v.view() {
                    ValueView::Int(cp) => char::from_u32(cp as u32),
                    _ => None,
                })
                .collect()
        }
        _ => target.to_string_value(),
    }
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "tclc" => Some(Some(Ok(Value::str(crate::value::tclc_str(
            &target.to_string_value(),
        ))))),
        "wordcase" => Some(Some(Ok(Value::str(crate::value::wordcase_str(
            &target.to_string_value(),
        ))))),
        "succ" => Some(match target.view() {
            ValueView::Enum { .. } | ValueView::Instance { .. } => None,
            ValueView::Int(i) => Some(Ok(Value::int(i + 1))),
            ValueView::Num(f) => Some(Ok(Value::num(f + 1.0))),
            ValueView::Complex(r, i) => Some(Ok(Value::complex(r + 1.0, i))),
            ValueView::Rat(n, d) => Some(Ok(make_rat(n + d, d))),
            ValueView::FatRat(n, d) => Some(Ok(Value::fat_rat_raw(n + d, d))),
            ValueView::BigRat(n, d) => Some(Ok(Value::bigrat(n + d, d.clone()))),
            ValueView::Bool(_) => Some(Ok(Value::TRUE)),
            ValueView::Str(s) => Some(Ok(Value::str(crate::builtins::str_increment::string_succ(
                &s,
            )))),
            _ => Some(Ok(target.clone())),
        }),
        "pred" => Some(match target.view() {
            ValueView::Enum { .. } | ValueView::Instance { .. } => None,
            ValueView::Int(i) => Some(Ok(Value::int(i - 1))),
            ValueView::Num(f) => Some(Ok(Value::num(f - 1.0))),
            ValueView::Complex(r, i) => Some(Ok(Value::complex(r - 1.0, i))),
            ValueView::Rat(n, d) => Some(Ok(make_rat(n - d, d))),
            ValueView::FatRat(n, d) => Some(Ok(Value::fat_rat_raw(n - d, d))),
            ValueView::BigRat(n, d) => Some(Ok(Value::bigrat(n - d, d.clone()))),
            ValueView::Bool(_) => Some(Ok(Value::FALSE)),
            ValueView::Str(s) => Some(Ok(Value::str(crate::builtins::str_increment::string_pred(
                &s,
            )))),
            _ => Some(Ok(target.clone())),
        }),
        "log" => Some(match target.view() {
            ValueView::Int(i) => Some(Ok(Value::num((i as f64).ln()))),
            ValueView::BigInt(i) => Some(Ok(Value::num(i.to_f64().unwrap_or(f64::INFINITY).ln()))),
            ValueView::Num(f) => Some(Ok(Value::num(f.ln()))),
            ValueView::Rat(n, d) if d != 0 => Some(Ok(Value::num((n as f64 / d as f64).ln()))),
            ValueView::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).ln(),
            ))),
            ValueView::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(r);
                Some(Ok(Value::complex(mag, arg)))
            }
            _ => None,
        }),
        "log2" => Some(match target.view() {
            ValueView::Int(i) => Some(Ok(Value::num((i as f64).log2()))),
            ValueView::BigInt(i) => {
                Some(Ok(Value::num(i.to_f64().unwrap_or(f64::INFINITY).log2())))
            }
            ValueView::Num(f) => Some(Ok(Value::num(f.log2()))),
            ValueView::Rat(n, d) if d != 0 => Some(Ok(Value::num((n as f64 / d as f64).log2()))),
            ValueView::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).log2(),
            ))),
            ValueView::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(r);
                let ln2 = 2.0f64.ln();
                Some(Ok(Value::complex(mag / ln2, arg / ln2)))
            }
            _ => None,
        }),
        "log10" => Some(match target.view() {
            ValueView::Int(i) => Some(Ok(Value::num((i as f64).log10()))),
            ValueView::BigInt(i) => {
                Some(Ok(Value::num(i.to_f64().unwrap_or(f64::INFINITY).log10())))
            }
            ValueView::Num(f) => Some(Ok(Value::num(f.log10()))),
            ValueView::Rat(n, d) if d != 0 => Some(Ok(Value::num((n as f64 / d as f64).log10()))),
            ValueView::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).log10(),
            ))),
            ValueView::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(r);
                let ln10 = 10.0f64.ln();
                Some(Ok(Value::complex(mag / ln10, arg / ln10)))
            }
            _ => None,
        }),
        "exp" => Some(match target.view() {
            ValueView::Int(i) => Some(Ok(Value::num((i as f64).exp()))),
            ValueView::BigInt(i) => Some(Ok(Value::num(i.to_f64().unwrap_or(f64::INFINITY).exp()))),
            ValueView::Num(f) => Some(Ok(Value::num(f.exp()))),
            ValueView::Rat(n, d) if d != 0 => Some(Ok(Value::num((n as f64 / d as f64).exp()))),
            ValueView::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).exp(),
            ))),
            ValueView::Complex(r, i) => {
                // exp(a+bi) = exp(a) * (cos(b) + i*sin(b))
                let ea = r.exp();
                Some(Ok(Value::complex(ea * i.cos(), ea * i.sin())))
            }
            _ => None,
        }),
        "atan2" => {
            // .atan2 with no args defaults to x=1
            let y = match target.view() {
                ValueView::Int(i) => i as f64,
                ValueView::BigInt(i) => i.to_f64().unwrap_or(f64::INFINITY),
                ValueView::Num(f) => f,
                ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
                ValueView::FatRat(n, d) if d != 0 => n as f64 / d as f64,
                ValueView::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => {
                    n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)
                }
                ValueView::Str(s) => match s.parse::<f64>() {
                    Ok(f) => f,
                    Err(_) => return Some(Some(Ok(Value::num(f64::NAN)))),
                },
                _ => return Some(None), // fall through to runtime for user types
            };
            Some(Some(Ok(Value::num(y.atan2(1.0)))))
        }
        "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "sec" | "cosec" | "cotan" | "asec"
        | "acosec" | "acotan" | "sinh" | "cosh" | "tanh" | "sech" | "cosech" | "cotanh"
        | "asinh" | "acosh" | "atanh" | "asech" | "acosech" | "acotanh" => {
            // Complex: dispatch to complex trig
            if let ValueView::Complex(re, im) = target.view() {
                let result = complex_trig(method, re, im);
                return Some(Some(Ok(Value::complex(result.0, result.1))));
            }
            let x = match target.view() {
                ValueView::Int(i) => i as f64,
                ValueView::BigInt(i) => i.to_f64().unwrap_or(f64::INFINITY),
                ValueView::Num(f) => f,
                ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
                ValueView::FatRat(n, d) if d != 0 => n as f64 / d as f64,
                ValueView::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => {
                    n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)
                }
                ValueView::Str(s) => match s.parse::<f64>() {
                    Ok(f) => f,
                    Err(_) => return Some(Some(Ok(Value::num(f64::NAN)))),
                },
                _ => return Some(None), // fall through to runtime for user types
            };
            let result = match method {
                "sin" => x.sin(),
                "cos" => x.cos(),
                "tan" => x.tan(),
                "asin" => x.asin(),
                "acos" => x.acos(),
                "atan" => x.atan(),
                "sec" => 1.0 / x.cos(),
                "cosec" => 1.0 / x.sin(),
                "cotan" => 1.0 / x.tan(),
                "asec" => (1.0 / x).acos(),
                "acosec" => (1.0 / x).asin(),
                "acotan" => (1.0 / x).atan(),
                "sinh" => x.sinh(),
                "cosh" => x.cosh(),
                "tanh" => x.tanh(),
                "sech" => 1.0 / x.cosh(),
                "cosech" => 1.0 / x.sinh(),
                "cotanh" => 1.0 / x.tanh(),
                "asinh" => {
                    let sign = x.signum();
                    let ax = x.abs();
                    sign * (ax + (ax * ax + 1.0).sqrt()).ln()
                }
                "acosh" => {
                    if x < 1.0 {
                        f64::NAN
                    } else {
                        (x + (x * x - 1.0).sqrt()).ln()
                    }
                }
                "atanh" => x.atanh(),
                "asech" => {
                    let y = 1.0 / x;
                    (y + (y * y - 1.0).sqrt()).ln()
                }
                "acosech" => {
                    let y = 1.0 / x;
                    (y + (y * y + 1.0).sqrt()).ln()
                }
                "acotanh" => (1.0 / x).atanh(),
                _ => f64::NAN,
            };
            Some(Some(Ok(Value::num(result))))
        }
        "Rat" => Some(match target.view() {
            ValueView::Rat(_, _) => Some(Ok(target.clone())),
            // A big FatRat coerces to a big Rat: drop the FatRat flag.
            ValueView::BigRat(n, d) if target.is_bigfatrat() => {
                Some(Ok(Value::bigrat(n.clone(), d.clone())))
            }
            ValueView::BigRat(_, _) => Some(Ok(target.clone())),
            ValueView::Int(i) => Some(Ok(make_rat(i, 1))),
            ValueView::Num(f) => {
                if f.is_nan() {
                    Some(Ok(Value::rat_raw(0, 0)))
                } else if f.is_infinite() {
                    if f.is_sign_positive() {
                        Some(Ok(Value::rat_raw(1, 0)))
                    } else {
                        Some(Ok(Value::rat_raw(-1, 0)))
                    }
                } else {
                    Some(Ok(crate::builtins::num_to_rat_with_epsilon(f, 1e-6)))
                }
            }
            ValueView::FatRat(n, d) => Some(Ok(make_rat(n, d))),
            ValueView::Str(s) => Some(Ok(str_to_rat(&s))),
            ValueView::Complex(r, im) => {
                if im.abs() <= 1e-15 {
                    Some(Ok(crate::builtins::num_to_rat_with_epsilon(r, 1e-6)))
                } else {
                    Some(Err(RuntimeError::new(
                        "Cannot convert Complex to Real: imaginary part not zero",
                    )))
                }
            }
            ValueView::Array(items, ..) => Some(Ok(make_rat(items.len() as i64, 1))),
            ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                Some(Ok(make_rat(items.len() as i64, 1)))
            }
            ValueView::Hash(map) => Some(Ok(make_rat(map.len() as i64, 1))),
            ValueView::Range(a, b) | ValueView::RangeExclStart(a, b) => {
                let n = if b >= a { b - a + 1 } else { 0 };
                Some(Ok(make_rat(n, 1)))
            }
            ValueView::RangeExcl(a, b) | ValueView::RangeExclBoth(a, b) => {
                let n = if b > a { b - a } else { 0 };
                Some(Ok(make_rat(n, 1)))
            }
            ValueView::GenericRange {
                start,
                end,
                excl_end,
                ..
            } => {
                let s = start.to_f64();
                let e = end.to_f64();
                if s.is_finite() && e.is_finite() {
                    let mut count = (e - s).floor() as i64 + 1;
                    if excl_end {
                        count -= 1;
                    }
                    if count < 0 {
                        count = 0;
                    }
                    Some(Ok(make_rat(count, 1)))
                } else {
                    Some(Ok(make_rat(0, 1)))
                }
            }
            // Duration/Instant store their seconds as a Real `value`; coerce that
            // directly so the exact Rat is preserved (e.g. Duration.new(42).Rat).
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if matches!(class_name.resolve().as_str(), "Duration" | "Instant") => {
                match attributes.as_map().get("value") {
                    Some(inner) => match dispatch(inner, "Rat") {
                        Some(Some(r)) => Some(r),
                        _ => Some(Ok(make_rat(0, 1))),
                    },
                    None => Some(Ok(make_rat(0, 1))),
                }
            }
            ValueView::Instance { .. } => cool_instance_numeric(target).map(|n| {
                if n.fract() == 0.0 && n.is_finite() {
                    Ok(make_rat(n as i64, 1))
                } else if n.is_finite() {
                    Ok(crate::builtins::num_to_rat_with_epsilon(n, 1e-6))
                } else {
                    Ok(make_rat(0, 1))
                }
            }),
            ValueView::Package(_) => Some(Ok(make_rat(0, 1))),
            _ => {
                let n = target.to_f64();
                if n.fract() == 0.0 && n.is_finite() {
                    Some(Ok(make_rat(n as i64, 1)))
                } else if n.is_finite() {
                    Some(Ok(crate::builtins::num_to_rat_with_epsilon(n, 1e-6)))
                } else {
                    Some(Ok(make_rat(0, 1)))
                }
            }
        }),
        "FatRat" => Some(match target.view() {
            ValueView::FatRat(_, _) => Some(Ok(target.clone())),
            ValueView::Rat(n, d) => Some(Ok(Value::fat_rat_raw(n, d))),
            // A big FatRat stays as-is; a big Rat gains the FatRat flag.
            ValueView::BigRat(_, _) if target.is_bigfatrat() => Some(Ok(target.clone())),
            ValueView::BigRat(n, d) => Some(Ok(Value::bigfatrat(n.clone(), d.clone()))),
            ValueView::Int(i) => Some(Ok(Value::fat_rat_raw(i, 1))),
            ValueView::BigInt(i) => Some(Ok(make_big_fat_rat(
                (**i).clone(),
                num_bigint::BigInt::from(1),
            ))),
            ValueView::Num(f) => {
                if f.is_nan() {
                    Some(Ok(Value::fat_rat_raw(0, 0)))
                } else if f.is_infinite() {
                    if f.is_sign_positive() {
                        Some(Ok(Value::fat_rat_raw(1, 0)))
                    } else {
                        Some(Ok(Value::fat_rat_raw(-1, 0)))
                    }
                } else {
                    let rat = crate::builtins::num_to_rat_with_epsilon(f, 1e-6);
                    match rat.view() {
                        ValueView::Rat(n, d) => Some(Ok(Value::fat_rat_raw(n, d))),
                        _ => Some(Ok(Value::fat_rat_raw(0, 1))),
                    }
                }
            }
            // A non-numeric string yields the same lazy `X::Str::Numeric` Failure
            // `.Int`/`.Num` produce (`"foo".FatRat.^name` is `Failure`).
            ValueView::Str(s)
                if crate::runtime::str_numeric::parse_raku_str_to_numeric(s.trim()).is_none() =>
            {
                Some(Ok(
                    crate::builtins::methods_0arg::dispatch_core_coerce::str_numeric_failure(&s),
                ))
            }
            ValueView::Str(s) => {
                let rat = str_to_rat(&s);
                match rat.view() {
                    ValueView::Rat(n, d) => Some(Ok(Value::fat_rat_raw(n, d))),
                    ValueView::BigRat(n, d) => {
                        Some(Ok(crate::value::make_big_fat_rat(n.clone(), d.clone())))
                    }
                    _ => Some(Ok(Value::fat_rat_raw(0, 1))),
                }
            }
            ValueView::Complex(r, im) => {
                if im.abs() <= 1e-15 {
                    let rat = crate::builtins::num_to_rat_with_epsilon(r, 1e-6);
                    match rat.view() {
                        ValueView::Rat(n, d) => Some(Ok(Value::fat_rat_raw(n, d))),
                        _ => Some(Ok(Value::fat_rat_raw(0, 1))),
                    }
                } else {
                    Some(Err(RuntimeError::new(
                        "Cannot convert Complex to Real: imaginary part not zero",
                    )))
                }
            }
            ValueView::Array(items, ..) => Some(Ok(Value::fat_rat_raw(items.len() as i64, 1))),
            ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                Some(Ok(Value::fat_rat_raw(items.len() as i64, 1)))
            }
            ValueView::Hash(map) => Some(Ok(Value::fat_rat_raw(map.len() as i64, 1))),
            ValueView::Range(a, b) | ValueView::RangeExclStart(a, b) => {
                let n = if b >= a { b - a + 1 } else { 0 };
                Some(Ok(Value::fat_rat_raw(n, 1)))
            }
            ValueView::RangeExcl(a, b) | ValueView::RangeExclBoth(a, b) => {
                let n = if b > a { b - a } else { 0 };
                Some(Ok(Value::fat_rat_raw(n, 1)))
            }
            ValueView::GenericRange {
                start,
                end,
                excl_end,
                ..
            } => {
                let s = start.to_f64();
                let e = end.to_f64();
                if s.is_finite() && e.is_finite() {
                    let mut count = (e - s).floor() as i64 + 1;
                    if excl_end {
                        count -= 1;
                    }
                    if count < 0 {
                        count = 0;
                    }
                    Some(Ok(Value::fat_rat_raw(count, 1)))
                } else {
                    Some(Ok(Value::fat_rat_raw(0, 1)))
                }
            }
            ValueView::Package(_) => Some(Ok(Value::fat_rat_raw(0, 1))),
            // IO::Path/Match/StrDistance numify via their Cool string/distance.
            ValueView::Instance { .. } if cool_instance_numeric(target).is_some() => {
                let n = cool_instance_numeric(target).unwrap_or(0.0);
                if n.fract() == 0.0 && n.is_finite() {
                    Some(Ok(Value::fat_rat_raw(n as i64, 1)))
                } else if n.is_finite() {
                    match crate::builtins::num_to_rat_with_epsilon(n, 1e-6).view() {
                        ValueView::Rat(n, d) => Some(Ok(Value::fat_rat_raw(n, d))),
                        _ => Some(Ok(Value::fat_rat_raw(0, 1))),
                    }
                } else {
                    Some(Ok(Value::fat_rat_raw(0, 1)))
                }
            }
            _ => {
                let n = target.to_f64();
                if n.fract() == 0.0 && n.is_finite() {
                    Some(Ok(Value::fat_rat_raw(n as i64, 1)))
                } else if n.is_finite() {
                    let rat = crate::builtins::num_to_rat_with_epsilon(n, 1e-6);
                    match rat.view() {
                        ValueView::Rat(n, d) => Some(Ok(Value::fat_rat_raw(n, d))),
                        _ => Some(Ok(Value::fat_rat_raw(0, 1))),
                    }
                } else {
                    Some(Ok(Value::fat_rat_raw(0, 1)))
                }
            }
        }),
        "tree" => Some(Some(Ok(tree_to_depth(target, usize::MAX)))),
        "encode" => {
            let s = target.to_string_value();
            let bytes: Vec<Value> = s.as_bytes().iter().map(|&b| Value::int(b as i64)).collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(bytes));
            Some(Some(Ok(Value::make_instance(
                Symbol::intern("utf8"),
                attrs,
            ))))
        }
        "sink" => Some(match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => {
                // Sinking a *handled* Failure is a no-op; only an unhandled
                // Failure throws its exception when sunk.
                if target.is_failure_handled() {
                    Some(Ok(Value::NIL))
                } else if let Some(ex) = attributes.as_map().get("exception") {
                    let mut err = RuntimeError::new(ex.to_string_value());
                    err.exception = Some(Box::new(ex.clone()));
                    Some(Err(err))
                } else {
                    Some(Ok(Value::NIL))
                }
            }
            ValueView::Seq(items) => {
                // If there's a deferred iterator and the Seq is not cached, fall through
                // to the runtime which will pull from the iterator.
                if crate::value::seq_has_deferred_iter(&items)
                    && !crate::value::seq_is_cached(&items)
                {
                    return Some(None); // fall through to runtime
                }
                // Sinking a Seq marks it as consumed (unless already cached).
                // Re-sinking a consumed Seq is ok (lives-ok).
                crate::value::seq_sink(&items);
                Some(Ok(Value::NIL))
            }
            ValueView::LazyList(ll) => {
                // Sinking a gather-based LazyList marks it as consumed.
                // Needed for `$s-lazy.sink; $s-lazy.is-lazy` to throw X::Seq::Consumed.
                let is_gather = ll.env.get("__mutsu_lazylist_from_gather").is_some();
                if is_gather {
                    crate::value::lazylist_consume(&ll);
                    Some(Ok(Value::NIL))
                } else {
                    None // fall through to runtime for non-gather lazy lists
                }
            }
            _ => Some(Ok(Value::NIL)),
        }),
        "item" => Some(match target.view() {
            ValueView::Array(items, kind) => {
                Some(Ok(Value::array_with_kind(items.clone(), kind.itemize())))
            }
            ValueView::LazyList(_) => None, // fall through to runtime to force
            // A Hash (and any other aggregate) is wrapped in a `Scalar`
            // container so it behaves as a single non-flattening element in
            // list context (e.g. passed to `map`). `.raku`/`.perl` on the
            // `Scalar` still shows the `$` itemization sigil — see the
            // `Scalar` interception in `call_method_with_values`.
            _ => Some(Ok(Value::scalar(target.clone()))),
        }),
        "race" | "hyper" => {
            if matches!(target.view(), ValueView::LazyList(_)) {
                return Some(None);
            }
            // Single-threaded: materialize and wrap in HyperSeq/RaceSeq
            let items = runtime::value_to_list(target);
            let arc = std::sync::Arc::new(items);
            let result = if method == "hyper" {
                Value::hyper_seq_arc(arc)
            } else {
                Value::race_seq_arc(arc)
            };
            Some(Some(Ok(result)))
        }
        "NFC" | "NFD" | "NFKC" | "NFKD" => {
            use unicode_normalization::UnicodeNormalization;
            let s = uni_or_str(target);
            let normalized: String = match method {
                "NFC" => s.nfc().collect(),
                "NFD" => s.nfd().collect(),
                "NFKC" => s.nfkc().collect(),
                _ => s.nfkd().collect(),
            };
            Some(Some(Ok(Value::uni(method.to_string(), normalized))))
        }
        _ => None,
    }
}
