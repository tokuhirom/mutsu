#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::value::{RuntimeError, Value};
use unicode_segmentation::UnicodeSegmentation;

use super::rng::{builtin_rand, builtin_srand, builtin_srand_auto};
use super::unicode::{titlecase_string, unicode_char_name};

fn is_infinite_range(value: &Value) -> bool {
    match value {
        Value::Range(_, end)
        | Value::RangeExcl(_, end)
        | Value::RangeExclStart(_, end)
        | Value::RangeExclBoth(_, end) => *end == i64::MAX,
        Value::GenericRange { end, .. } => match end.as_ref() {
            Value::HyperWhatever => true,
            Value::Num(n) => n.is_infinite() && n.is_sign_positive(),
            Value::Rat(n, d) => *d == 0 && *n > 0,
            Value::FatRat(n, d) => *d == 0 && *n > 0,
            other => {
                let n = other.to_f64();
                n.is_infinite() && n.is_sign_positive()
            }
        },
        _ => false,
    }
}

fn flat_val_deep(v: &Value, out: &mut Vec<Value>) {
    match v {
        // Lists (not true Arrays), Seqs, and Slips are flattened recursively
        Value::Array(items, false) | Value::Seq(items) | Value::Slip(items) => {
            for item in items.iter() {
                flat_val_deep(item, out);
            }
        }
        // True Arrays (is_array=true) are itemized containers — don't descend
        Value::Array(_, true) => out.push(v.clone()),
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => {
            out.extend(crate::runtime::utils::value_to_list(v));
        }
        other => out.push(other.clone()),
    }
}

// ── Built-in function dispatch ───────────────────────────────────────
/// Try to dispatch a built-in function call.
pub(crate) fn native_function(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    // Always-variadic functions: route regardless of arity.
    // zip:with needs interpreter access (for calling the combiner), so return None
    // when a :with Pair is present to fall through to the interpreter.
    if name == "sum" {
        return native_function_variadic(name, args);
    }
    if name == "zip" {
        if args
            .iter()
            .any(|a| matches!(a, Value::Pair(k, _) if k == "with"))
        {
            return None;
        }
        return native_function_variadic(name, args);
    }
    match args.len() {
        0 => native_function_0arg(name),
        1 => native_function_1arg(name, &args[0]),
        2 => native_function_2arg(name, &args[0], &args[1]),
        3 => native_function_3arg(name, &args[0], &args[1], &args[2]),
        _ => native_function_variadic(name, args),
    }
}

fn native_function_0arg(name: &str) -> Option<Result<Value, RuntimeError>> {
    match name {
        "rand" => Some(Ok(Value::Num(builtin_rand()))),
        "now" => Some(Ok(Value::make_instant_now())),
        "time" => {
            let secs = crate::value::current_time_secs_f64() as i64;
            Some(Ok(Value::Int(secs)))
        }
        "srand" => {
            builtin_srand_auto();
            Some(Ok(Value::Nil))
        }
        _ => None,
    }
}

fn native_function_1arg(name: &str, arg: &Value) -> Option<Result<Value, RuntimeError>> {
    match name {
        "combinations" => {
            // combinations($n) where $n is Int => (^$n).combinations (powerset)
            // combinations($iterable) => $iterable.combinations
            let items = match arg {
                Value::Int(n) => {
                    if *n < 0 {
                        return Some(Ok(Value::Seq(Vec::new().into())));
                    }
                    let items: Vec<Value> = (0..*n).map(Value::Int).collect();
                    return Some(Ok(Value::Seq(
                        super::methods_0arg::collection::combinations_all(&items).into(),
                    )));
                }
                _ => runtime::value_to_list(arg),
            };
            Some(Ok(Value::Seq(
                super::methods_0arg::collection::combinations_all(&items).into(),
            )))
        }
        "srand" => {
            let seed = match arg {
                Value::Int(n) => *n as u64,
                Value::Num(n) => *n as u64,
                _ => arg.to_string_value().parse::<u64>().unwrap_or(0),
            };
            builtin_srand(seed);
            Some(Ok(Value::Nil))
        }
        "uc" => Some(Ok(Value::Str(arg.to_string_value().to_uppercase()))),
        "lc" => Some(Ok(Value::Str(arg.to_string_value().to_lowercase()))),
        "fc" => Some(Ok(Value::Str(super::methods_0arg::unicode_foldcase(
            &arg.to_string_value(),
        )))),
        "tc" => Some(Ok(Value::Str(titlecase_string(&arg.to_string_value())))),
        "tclc" => Some(Ok(Value::Str(crate::value::tclc_str(
            &arg.to_string_value(),
        )))),
        "wordcase" => Some(Ok(Value::Str(crate::value::wordcase_str(
            &arg.to_string_value(),
        )))),
        "chomp" => Some(Ok(Value::Str(
            arg.to_string_value().trim_end_matches('\n').to_string(),
        ))),
        "chop" => {
            // Type objects (Package) should throw
            if let Value::Package(type_name) = arg {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let mut s = arg.to_string_value();
            s.pop();
            Some(Ok(Value::Str(s)))
        }
        "trim" => Some(Ok(Value::Str(arg.to_string_value().trim().to_string()))),
        "trim-leading" => Some(Ok(Value::Str(
            arg.to_string_value().trim_start().to_string(),
        ))),
        "trim-trailing" => Some(Ok(Value::Str(arg.to_string_value().trim_end().to_string()))),
        "flip" => {
            let s = arg.to_string_value();
            let reversed: String = s.graphemes(true).rev().collect();
            Some(Ok(Value::Str(reversed)))
        }
        "words" => {
            let s = arg.to_string_value();
            let parts: Vec<Value> = s
                .split_whitespace()
                .map(|p| Value::Str(p.to_string()))
                .collect();
            Some(Ok(Value::array(parts)))
        }
        "chars" => Some(Ok(Value::Int(
            arg.to_string_value().graphemes(true).count() as i64,
        ))),
        "chr" => {
            let code = match arg {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                _ => arg.to_string_value().parse::<i64>().unwrap_or(-1),
            };
            if !(0..=0x10FFFF).contains(&code) {
                return Some(Err(RuntimeError::new(format!(
                    "chr({}) out of range. bg: Expected 0..1114111",
                    code
                ))));
            }
            if let Some(ch) = std::char::from_u32(code as u32) {
                Some(Ok(Value::Str(ch.to_string())))
            } else {
                Some(Err(RuntimeError::new(format!(
                    "chr({}) does not map to a valid Unicode character",
                    code
                ))))
            }
        }
        "ord" => {
            if let Some(ch) = arg.to_string_value().chars().next() {
                Some(Ok(Value::Int(ch as u32 as i64)))
            } else {
                Some(Ok(Value::Nil))
            }
        }
        "uniname" => {
            let s = arg.to_string_value();
            if s.is_empty() {
                return Some(Ok(Value::Nil));
            }
            if let Some(ch) = s.chars().next() {
                let name = unicode_char_name(ch);
                Some(Ok(Value::Str(name)))
            } else {
                Some(Ok(Value::Nil))
            }
        }
        "is-prime" => Some(super::methods_0arg::coercion::value_is_prime(arg)),
        "abs" => Some(Ok(match arg {
            Value::Int(i) => Value::Int(i.abs()),
            Value::Num(f) => Value::Num(f.abs()),
            Value::Rat(n, d) => Value::Rat(n.abs(), *d),
            Value::Str(s) => {
                if let Ok(i) = s.parse::<i64>() {
                    Value::Int(i.abs())
                } else if let Ok(f) = s.parse::<f64>() {
                    Value::Num(f.abs())
                } else {
                    Value::Int(0)
                }
            }
            _ => Value::Int(0),
        })),
        "sqrt" => Some(Ok(match arg {
            Value::Int(i) => Value::Num((*i as f64).sqrt()),
            Value::Num(f) => Value::Num(f.sqrt()),
            Value::Rat(n, d) if *d != 0 => Value::Num((*n as f64 / *d as f64).sqrt()),
            Value::Complex(r, i) => {
                // sqrt(a+bi) = sqrt((|z|+a)/2) + i*sign(b)*sqrt((|z|-a)/2)
                let mag = (r * r + i * i).sqrt();
                let re = ((mag + r) / 2.0).sqrt();
                let im = i.signum() * ((mag - r) / 2.0).sqrt();
                Value::Complex(re, im)
            }
            _ => Value::Num(f64::NAN),
        })),
        "floor" => Some(Ok(match arg {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
            Value::Num(f) => Value::Int(f.floor() as i64),
            Value::Int(i) => Value::Int(*i),
            Value::Rat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) != (*d < 0) {
                    Value::Int(q - 1)
                } else {
                    Value::Int(q)
                }
            }
            _ => Value::Int(0),
        })),
        "ceiling" | "ceil" => Some(Ok(match arg {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
            Value::Num(f) => Value::Int(f.ceil() as i64),
            Value::Int(i) => Value::Int(*i),
            Value::Rat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) == (*d < 0) {
                    Value::Int(q + 1)
                } else {
                    Value::Int(q)
                }
            }
            _ => Value::Int(0),
        })),
        "round" => Some(Ok(match arg {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
            Value::Num(f) => Value::Int(f.round() as i64),
            Value::Int(i) => Value::Int(*i),
            Value::Rat(n, d) if *d != 0 => {
                let f = *n as f64 / *d as f64;
                Value::Int(f.round() as i64)
            }
            _ => Value::Int(0),
        })),
        "exp" => Some(Ok(match arg {
            Value::Int(i) => Value::Num((*i as f64).exp()),
            Value::Num(f) => Value::Num(f.exp()),
            Value::Rat(n, d) if *d != 0 => Value::Num((*n as f64 / *d as f64).exp()),
            Value::Complex(r, i) => {
                let ea = r.exp();
                Value::Complex(ea * i.cos(), ea * i.sin())
            }
            _ => Value::Num(f64::NAN),
        })),
        "log" => match arg {
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let phase = i.atan2(*r);
                Some(Ok(Value::Complex(mag, phase)))
            }
            _ => {
                let x = runtime::to_float_value(arg).unwrap_or(f64::NAN);
                Some(Ok(Value::Num(x.ln())))
            }
        },
        "log2" => match arg {
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let phase = i.atan2(*r);
                let ln2 = 2.0f64.ln();
                Some(Ok(Value::Complex(mag / ln2, phase / ln2)))
            }
            _ => {
                let x = runtime::to_float_value(arg).unwrap_or(f64::NAN);
                Some(Ok(Value::Num(x.log2())))
            }
        },
        "log10" => match arg {
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let phase = i.atan2(*r);
                let ln10 = 10.0f64.ln();
                Some(Ok(Value::Complex(mag / ln10, phase / ln10)))
            }
            _ => {
                let x = runtime::to_float_value(arg).unwrap_or(f64::NAN);
                Some(Ok(Value::Num(x.log10())))
            }
        },
        "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "sec" | "cosec" | "cotan" | "asec"
        | "acosec" | "acotan" | "sinh" | "cosh" | "tanh" | "sech" | "cosech" | "cotanh"
        | "asinh" | "acosh" | "atanh" | "asech" | "acosech" | "acotanh" => {
            // Complex arguments use complex trig
            if let Value::Complex(re, im) = arg {
                let result = crate::builtins::methods_0arg::complex_trig(name, *re, *im);
                return Some(Ok(Value::Complex(result.0, result.1)));
            }
            // User-defined types need runtime coercion via .Numeric/.Bridge
            if matches!(arg, Value::Instance { .. }) {
                return None;
            }
            let x = runtime::to_float_value(arg).unwrap_or(0.0);
            let result = match name {
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
                "asinh" => x.asinh(),
                "acosh" => x.acosh(),
                "atanh" => x.atanh(),
                "asech" => (1.0 / x).acosh(),
                "acosech" => (1.0 / x).asinh(),
                "acotanh" => (1.0 / x).atanh(),
                _ => 0.0,
            };
            Some(Ok(Value::Num(result)))
        }
        "atan2" => {
            // atan2(y) — defaults to x=1
            if matches!(arg, Value::Instance { .. }) {
                return None;
            }
            let y = runtime::to_float_value(arg).unwrap_or(0.0);
            Some(Ok(Value::Num(y.atan2(1.0))))
        }
        "cis" => {
            let x = runtime::to_float_value(arg).unwrap_or(f64::NAN);
            Some(Ok(Value::Complex(x.cos(), x.sin())))
        }
        "truncate" => {
            if let Some(num) = runtime::to_float_value(arg) {
                if num.is_nan() || num.is_infinite() {
                    Some(Ok(Value::Num(num)))
                } else {
                    Some(Ok(Value::Int(num.trunc() as i64)))
                }
            } else {
                Some(Ok(Value::Int(runtime::to_int(arg))))
            }
        }
        "defined" => Some(Ok(Value::Bool(match arg {
            Value::Nil | Value::Package(_) => false,
            Value::Slip(items) if items.is_empty() => false,
            Value::Instance { class_name, .. } if class_name == "Failure" => false,
            _ => true,
        }))),
        "elems" => match arg {
            Value::Array(items, ..) => Some(Ok(Value::Int(items.len() as i64))),
            Value::Hash(items) => Some(Ok(Value::Int(items.len() as i64))),
            Value::Str(s) => Some(Ok(Value::Int(s.chars().count() as i64))),
            Value::LazyList(_) => None,
            _ => Some(Ok(Value::Int(1))),
        },
        "reverse" => {
            if crate::runtime::utils::is_shaped_array(arg) {
                return Some(Err(RuntimeError::illegal_on_fixed_dimension_array(
                    "reverse",
                )));
            }
            Some(Ok(match arg {
                Value::Array(items, ..) => {
                    let mut reversed = (**items).clone();
                    reversed.reverse();
                    Value::array(reversed)
                }
                Value::Str(s) => Value::Str(s.chars().rev().collect()),
                _ => Value::Nil,
            }))
        }
        "sort" => {
            if crate::runtime::utils::is_shaped_array(arg) {
                let mut leaves = crate::runtime::utils::shaped_array_leaves(arg);
                leaves.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
                return Some(Ok(Value::array(leaves)));
            }
            Some(Ok(match arg {
                Value::Array(items, ..) => {
                    let mut sorted = (**items).clone();
                    sorted.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
                    Value::array(sorted)
                }
                _ => Value::Nil,
            }))
        }
        "rotate" => {
            if crate::runtime::utils::is_shaped_array(arg) {
                return Some(Err(RuntimeError::illegal_on_fixed_dimension_array(
                    "rotate",
                )));
            }
            Some(Ok(match arg {
                Value::Array(items, ..) => {
                    // rotate with no count defaults to 1
                    let n = 1usize;
                    let len = items.len();
                    if len == 0 {
                        Value::array(Vec::new())
                    } else {
                        let n = n % len;
                        let mut rotated = Vec::with_capacity(len);
                        rotated.extend_from_slice(&items[n..]);
                        rotated.extend_from_slice(&items[..n]);
                        Value::array(rotated)
                    }
                }
                _ => Value::Nil,
            }))
        }
        "flat" => {
            if crate::runtime::utils::is_shaped_array(arg) {
                let leaves = crate::runtime::utils::shaped_array_leaves(arg);
                return Some(Ok(Value::Seq(std::sync::Arc::new(leaves))));
            }
            if is_infinite_range(arg) {
                return Some(Ok(arg.clone()));
            }
            let mut flat = Vec::new();
            // Top-level: iterate the arg's elements and flatten each
            let items = crate::runtime::utils::value_to_list(arg);
            for item in &items {
                flat_val_deep(item, &mut flat);
            }
            Some(Ok(Value::Seq(std::sync::Arc::new(flat))))
        }
        "first" => Some(Ok(match arg {
            Value::Array(items, ..) => items.first().cloned().unwrap_or(Value::Nil),
            _ => arg.clone(),
        })),
        "min" => Some(Ok(match arg {
            Value::Array(items, ..) => items
                .iter()
                .cloned()
                .min_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil),
            _ => arg.clone(),
        })),
        "max" => Some(Ok(match arg {
            Value::Array(items, ..) => items
                .iter()
                .cloned()
                .max_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil),
            _ => arg.clone(),
        })),
        "ords" => {
            let s = arg.to_string_value();
            let codes: Vec<Value> = s.chars().map(|ch| Value::Int(ch as u32 as i64)).collect();
            Some(Ok(Value::array(codes)))
        }
        "gist" => Some(Ok(Value::Str(arg.to_string_value()))),
        _ => None,
    }
}

fn native_function_2arg(
    name: &str,
    arg1: &Value,
    arg2: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match name {
        "combinations" => {
            // combinations($n_or_iterable, $k_or_range)
            // If first arg is an iterable, use its elements; otherwise treat as numeric n
            let items = match arg1 {
                Value::Array(..)
                | Value::Seq(..)
                | Value::Slip(..)
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }
                | Value::Hash(..) => runtime::value_to_list(arg1),
                _ => {
                    let n = runtime::to_int(arg1);
                    if n <= 0 {
                        Vec::new()
                    } else {
                        (0..n).map(Value::Int).collect()
                    }
                }
            };
            // Dispatch based on $k type (Int or Range)
            super::native_method_1arg(&Value::array(items), "combinations", arg2)
        }
        "atan2" => {
            // atan2(y, x)
            if matches!(arg1, Value::Instance { .. }) || matches!(arg2, Value::Instance { .. }) {
                return None;
            }
            let y = runtime::to_float_value(arg1).unwrap_or(0.0);
            let x = runtime::to_float_value(arg2).unwrap_or(0.0);
            Some(Ok(Value::Num(y.atan2(x))))
        }
        "chop" => {
            // Type objects (Package) should throw
            if let Value::Package(type_name) = arg1 {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let s = arg1.to_string_value();
            let n = match arg2 {
                Value::Int(i) => (*i).max(0) as usize,
                Value::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    bi.to_usize().unwrap_or(usize::MAX)
                }
                Value::Num(f) => (*f as i64).max(0) as usize,
                _ => arg2.to_string_value().parse::<usize>().unwrap_or(1),
            };
            let char_count = s.chars().count();
            let keep = char_count.saturating_sub(n);
            let result: String = s.chars().take(keep).collect();
            Some(Ok(Value::Str(result)))
        }
        "join" => {
            let sep = arg1.to_string_value();
            if crate::runtime::utils::is_shaped_array(arg2) {
                let leaves = crate::runtime::utils::shaped_array_leaves(arg2);
                let joined = leaves
                    .iter()
                    .map(|v| v.to_string_value())
                    .collect::<Vec<_>>()
                    .join(&sep);
                return Some(Ok(Value::Str(joined)));
            }
            match arg2 {
                Value::Array(items, ..) => {
                    let joined = items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::Str(joined)))
                }
                _ => Some(Ok(Value::Str(String::new()))),
            }
        }
        "rotate" => {
            if crate::runtime::utils::is_shaped_array(arg1) {
                return Some(Err(RuntimeError::illegal_on_fixed_dimension_array(
                    "rotate",
                )));
            }
            match arg1 {
                Value::Array(items, ..) => {
                    let count = runtime::to_int(arg2);
                    let len = items.len() as i64;
                    if len == 0 {
                        return Some(Ok(Value::array(Vec::new())));
                    }
                    let n = ((count % len) + len) % len;
                    let n = n as usize;
                    let mut rotated = Vec::with_capacity(items.len());
                    rotated.extend_from_slice(&items[n..]);
                    rotated.extend_from_slice(&items[..n]);
                    Some(Ok(Value::array(rotated)))
                }
                _ => Some(Ok(Value::Nil)),
            }
        }
        "index" => {
            // Skip native path for junctions — fall through to interpreter for auto-threading
            if matches!(arg1, Value::Junction { .. }) {
                return None;
            }
            let s = arg1.to_string_value();
            let needle = arg2.to_string_value();
            Some(Ok(match s.find(&needle) {
                Some(pos) => Value::Int(s[..pos].chars().count() as i64),
                None => Value::Nil,
            }))
        }
        "rindex" => {
            let s = arg1.to_string_value();
            let needle = arg2.to_string_value();
            Some(Ok(match s.rfind(&needle) {
                Some(pos) => Value::Int(s[..pos].chars().count() as i64),
                None => Value::Nil,
            }))
        }
        "substr" => {
            let s = arg1.to_string_value();
            let start = match arg2 {
                Value::Int(i) => (*i).max(0) as usize,
                _ => return None,
            };
            let chars: Vec<char> = s.chars().collect();
            Some(Ok(Value::Str(
                chars[start.min(chars.len())..].iter().collect(),
            )))
        }
        "samemark" => {
            let target = arg1.to_string_value();
            let source = arg2.to_string_value();
            Some(Ok(Value::Str(crate::builtins::samemark_string(
                &target, &source,
            ))))
        }
        "log" => {
            let x = runtime::to_float_value(arg1).unwrap_or(f64::NAN);
            let base_val = runtime::to_float_value(arg2).unwrap_or(f64::NAN);
            if base_val.is_finite() && base_val > 0.0 && base_val != 1.0 && x > 0.0 {
                Some(Ok(Value::Num(x.ln() / base_val.ln())))
            } else {
                Some(Ok(Value::Num(f64::NAN)))
            }
        }
        "exp" => {
            // exp($x, $base) = $base ** $x
            // Fast path for real args
            if !matches!(arg1, Value::Complex(..)) && !matches!(arg2, Value::Complex(..)) {
                let x = runtime::to_float_value(arg1).unwrap_or(f64::NAN);
                let base = runtime::to_float_value(arg2).unwrap_or(f64::NAN);
                return Some(Ok(Value::Num(base.powf(x))));
            }
            let (base_r, base_i) = match arg2 {
                Value::Int(i) => (*i as f64, 0.0),
                Value::Num(f) => (*f, 0.0),
                Value::Rat(n, d) if *d != 0 => (*n as f64 / *d as f64, 0.0),
                Value::Complex(r, i) => (*r, *i),
                _ => return None,
            };
            let (exp_r, exp_i) = match arg1 {
                Value::Int(i) => (*i as f64, 0.0),
                Value::Num(f) => (*f, 0.0),
                Value::Rat(n, d) if *d != 0 => (*n as f64 / *d as f64, 0.0),
                Value::Complex(r, i) => (*r, *i),
                _ => return None,
            };
            let ln_r = (base_r * base_r + base_i * base_i).sqrt().ln();
            let ln_i = base_i.atan2(base_r);
            let prod_r = exp_r * ln_r - exp_i * ln_i;
            let prod_i = exp_r * ln_i + exp_i * ln_r;
            let ea = prod_r.exp();
            let result_r = ea * prod_i.cos();
            let result_i = ea * prod_i.sin();
            Some(Ok(Value::Complex(result_r, result_i)))
        }
        "round" => {
            let x = runtime::to_float_value(arg1)?;
            let scale = runtime::to_float_value(arg2)?;
            if scale == 0.0 {
                Some(Ok(Value::Int(x.round() as i64)))
            } else {
                let factor = (1.0 / scale).abs();
                Some(Ok(Value::Num((x * factor).round() / factor)))
            }
        }
        "min" => {
            let cmp = match (arg1, arg2) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                _ => arg1.to_string_value().cmp(&arg2.to_string_value()),
            };
            Some(Ok(
                if cmp == std::cmp::Ordering::Less || cmp == std::cmp::Ordering::Equal {
                    arg1.clone()
                } else {
                    arg2.clone()
                },
            ))
        }
        "max" => {
            let cmp = match (arg1, arg2) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                _ => arg1.to_string_value().cmp(&arg2.to_string_value()),
            };
            Some(Ok(
                if cmp == std::cmp::Ordering::Greater || cmp == std::cmp::Ordering::Equal {
                    arg1.clone()
                } else {
                    arg2.clone()
                },
            ))
        }
        _ => None,
    }
}

fn native_function_3arg(
    name: &str,
    arg1: &Value,
    arg2: &Value,
    arg3: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match name {
        "expmod" => Some(crate::builtins::expmod(arg1, arg2, arg3)),
        "substr" => {
            let s = arg1.to_string_value();
            let start = match arg2 {
                Value::Int(i) => (*i).max(0) as usize,
                _ => return None,
            };
            let len = match arg3 {
                Value::Int(i) => (*i).max(0) as usize,
                _ => return None,
            };
            let chars: Vec<char> = s.chars().collect();
            let start = start.min(chars.len());
            let end = (start + len).min(chars.len());
            Some(Ok(Value::Str(chars[start..end].iter().collect())))
        }
        _ => None,
    }
}

fn native_function_variadic(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "min" => {
            if args.is_empty() {
                return Some(Ok(Value::Nil));
            }
            Some(Ok(args
                .iter()
                .cloned()
                .min_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil)))
        }
        "max" => {
            if args.is_empty() {
                return Some(Ok(Value::Nil));
            }
            Some(Ok(args
                .iter()
                .cloned()
                .max_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil)))
        }
        "chrs" => {
            let mut result = String::new();
            let push_chr = |result: &mut String, v: &Value| {
                let code = match v {
                    Value::Int(i) => *i,
                    Value::Num(f) => *f as i64,
                    _ => v.to_string_value().parse::<i64>().unwrap_or(-1),
                };
                if code >= 0
                    && let Some(ch) = std::char::from_u32(code as u32)
                {
                    result.push(ch);
                }
            };
            for arg in args {
                match arg {
                    Value::Array(items, ..) => {
                        for item in items.iter() {
                            push_chr(&mut result, item);
                        }
                    }
                    _ => push_chr(&mut result, arg),
                }
            }
            Some(Ok(Value::Str(result)))
        }
        "zip" => {
            // zip([@a], [@b], ...) — interleave elements from each list
            let lists: Vec<Vec<Value>> = args.iter().map(runtime::value_to_list).collect();
            if lists.is_empty() {
                return Some(Ok(Value::array(vec![])));
            }
            let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);
            let mut result = Vec::with_capacity(min_len);
            for i in 0..min_len {
                let row: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(Value::array(row));
            }
            Some(Ok(Value::array(result)))
        }
        "flat" => {
            if args.len() == 1 && is_infinite_range(&args[0]) {
                return Some(Ok(args[0].clone()));
            }
            let mut result = Vec::new();
            for arg in args {
                flat_val_deep(arg, &mut result);
            }
            Some(Ok(Value::Seq(std::sync::Arc::new(result))))
        }
        "sum" => {
            let mut total: i64 = 0;
            let mut has_num = false;
            let mut total_f: f64 = 0.0;
            for arg in args {
                match arg {
                    Value::Int(i) => {
                        if has_num {
                            total_f += *i as f64;
                        } else {
                            total += i;
                        }
                    }
                    Value::Num(f) => {
                        if !has_num {
                            total_f = total as f64;
                            has_num = true;
                        }
                        total_f += f;
                    }
                    Value::Range(a, b) => {
                        let n = b - a + 1;
                        if n > 0 {
                            let s = n * (a + b) / 2;
                            if has_num {
                                total_f += s as f64;
                            } else {
                                total += s;
                            }
                        }
                    }
                    Value::RangeExcl(a, b) => {
                        let n = b - a;
                        if n > 0 {
                            let b_adj = b - 1;
                            let s = n * (a + b_adj) / 2;
                            if has_num {
                                total_f += s as f64;
                            } else {
                                total += s;
                            }
                        }
                    }
                    Value::Array(items, ..) | Value::Seq(items) => {
                        for item in items.iter() {
                            if has_num {
                                total_f += item.to_f64();
                            } else if let Value::Num(_) = item {
                                total_f = total as f64 + item.to_f64();
                                has_num = true;
                            } else {
                                total += item.to_f64() as i64;
                            }
                        }
                    }
                    _ => {
                        if has_num {
                            total_f += arg.to_f64();
                        } else {
                            total += arg.to_f64() as i64;
                        }
                    }
                }
            }
            if has_num {
                Some(Ok(Value::Num(total_f)))
            } else {
                Some(Ok(Value::Int(total)))
            }
        }
        _ => None,
    }
}
