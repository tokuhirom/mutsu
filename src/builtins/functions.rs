#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, RuntimeError, Value};
use num_bigint::BigInt as NumBigInt;
use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

use super::rng::{builtin_rand, builtin_srand, builtin_srand_auto};
use super::unicode::{titlecase_string, unicode_char_name};

use std::collections::HashMap;

fn bigint_factorial(n: i64) -> NumBigInt {
    let mut acc = NumBigInt::from(1u8);
    for value in 2..=n {
        acc *= value;
    }
    acc
}

/// Implementation of `uniparse` / `parse-names`: takes a comma-separated list
/// of Unicode character names and returns the corresponding string.
/// Returns a Failure wrapping X::Str::InvalidCharName for unknown names.
pub(crate) fn uniparse_impl(input: &str) -> Result<Value, RuntimeError> {
    if input.is_empty() {
        return Ok(Value::str_from(""));
    }
    let mut result = String::new();
    for part in input.split(',') {
        let name = part.trim();
        if name.is_empty() {
            continue;
        }
        if let Some(ch) = crate::token_kind::lookup_unicode_char_by_name(name) {
            result.push(ch);
        } else if let Some(s) = crate::token_kind::lookup_emoji_sequence(name) {
            result.push_str(&s);
        } else {
            // Return a Failure wrapping X::Str::InvalidCharName
            let mut attrs = HashMap::new();
            attrs.insert("name".to_string(), Value::str_from(name));
            let msg = format!("Unrecognized character name [{}]", name);
            attrs.insert("message".to_string(), Value::str_from(&msg));
            let ex = Value::make_instance(Symbol::intern("X::Str::InvalidCharName"), attrs);
            let mut failure_attrs = HashMap::new();
            failure_attrs.insert("exception".to_string(), ex);
            return Ok(Value::make_instance(
                Symbol::intern("Failure"),
                failure_attrs,
            ));
        }
    }
    Ok(Value::str(result))
}

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

/// Recursively flatten a value for `flat()`.
///
/// `flatten_arrays`: when true, `ArrayKind::Array` values are flattened
/// (their elements are exposed). Elements coming from a List/Seq context
/// pass `true`; elements coming from inside an Array pass `false`
/// (because `[...]` itemizes its contents in Raku).
fn flat_val(v: &Value, out: &mut Vec<Value>, flatten_arrays: bool) {
    match v {
        // Lists, Seqs, and Slips are always flattened; their children
        // inherit flatten_arrays=true since Lists don't itemize.
        Value::Array(items, ArrayKind::List) | Value::Seq(items) | Value::Slip(items) => {
            for item in items.iter() {
                flat_val(item, out, true);
            }
        }
        // Real Arrays ([...]): flatten if flag is set. Children get
        // flatten_arrays=false because [...] itemizes its elements.
        Value::Array(items, ArrayKind::Array) if flatten_arrays => {
            for item in items.iter() {
                flat_val(item, out, false);
            }
        }
        // Itemized containers — don't descend
        Value::Array(_, kind) if kind.is_itemized() => out.push(v.clone()),
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
pub(crate) fn native_function(
    name_sym: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let name = name_sym.resolve();
    let name = name.as_str();
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
    if name == "split" {
        return super::split::native_split_function(args);
    }
    if name == "localtime" || name == "gmtime" {
        return Some(builtin_localtime_gmtime(name, args));
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
        "times" => Some(builtin_times()),
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
        "permutations" => {
            // permutations($n) where $n is Int => (0, 1, ..., $n-1).permutations
            // permutations($iterable) => $iterable.permutations
            let items = match arg {
                Value::Int(n) => {
                    if *n <= 0 {
                        return Some(Ok(Value::Seq(vec![Value::array(Vec::new())].into())));
                    }
                    if *n > 20 {
                        let mut attrs = HashMap::new();
                        attrs.insert(
                            "count_only".to_string(),
                            Value::BigInt(std::sync::Arc::new(bigint_factorial(*n))),
                        );
                        attrs.insert("bool_only".to_string(), Value::Bool(true));
                        attrs.insert("permutations_size".to_string(), Value::Int(*n));
                        return Some(Ok(Value::make_instance(Symbol::intern("Iterator"), attrs)));
                    }
                    let items: Vec<Value> = (0..*n).map(Value::Int).collect();
                    return Some(Ok(Value::Seq(
                        super::methods_0arg::collection::all_permutations(&items).into(),
                    )));
                }
                _ => runtime::value_to_list(arg),
            };
            if items.len() > 20 {
                return Some(Err(RuntimeError::new(format!(
                    "Cowardly refusing to permutate more than 20 elements, tried {}",
                    items.len()
                ))));
            }
            Some(Ok(Value::Seq(
                super::methods_0arg::collection::all_permutations(&items).into(),
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
        "uc" => Some(Ok(Value::str(super::unicode::grapheme_uppercase(
            &arg.to_string_value(),
        )))),
        "lc" => Some(Ok(Value::str(super::unicode::grapheme_lowercase(
            &arg.to_string_value(),
        )))),
        "fc" => Some(Ok(Value::str(super::unicode::grapheme_foldcase(
            &arg.to_string_value(),
        )))),
        "tc" => Some(Ok(Value::str(titlecase_string(&arg.to_string_value())))),
        "tclc" => Some(Ok(Value::str(crate::value::tclc_str(
            &arg.to_string_value(),
        )))),
        "wordcase" => Some(Ok(Value::str(crate::value::wordcase_str(
            &arg.to_string_value(),
        )))),
        "chomp" => Some(Ok(Value::str(crate::builtins::chomp_one(
            &arg.to_string_value(),
        )))),
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
            Some(Ok(Value::str(s)))
        }
        "trim" => Some(Ok(Value::str(arg.to_string_value().trim().to_string()))),
        "trim-leading" => Some(Ok(Value::str(
            arg.to_string_value().trim_start().to_string(),
        ))),
        "trim-trailing" => Some(Ok(Value::str(arg.to_string_value().trim_end().to_string()))),
        "flip" => {
            let s = arg.to_string_value();
            let reversed: String = s.graphemes(true).rev().collect::<String>().nfc().collect();
            Some(Ok(Value::str(reversed)))
        }
        "words" => {
            let s = arg.to_string_value();
            let parts: Vec<Value> = s
                .split_whitespace()
                .map(|p| Value::str(p.to_string()))
                .collect();
            Some(Ok(Value::Seq(std::sync::Arc::new(parts))))
        }
        "chars" => Some(Ok(Value::Int(
            arg.to_string_value().graphemes(true).count() as i64,
        ))),
        "chr" => {
            let (code, display) = match arg {
                Value::Int(i) => (*i, format!("{}", i)),
                Value::BigInt(n) => {
                    let hex = format!("{:X}", &**n);
                    return Some(Err(RuntimeError::new(format!(
                        "Codepoint {} (0x{}) is out of bounds in 'chr'",
                        n, hex
                    ))));
                }
                Value::Num(f) => (*f as i64, format!("{}", *f as i64)),
                _ => {
                    let s = arg.to_string_value();
                    let trimmed = s.trim();
                    let i = if let Ok(v) = trimmed.parse::<i64>() {
                        v
                    } else if let Some(hex) = trimmed
                        .strip_prefix("0x")
                        .or_else(|| trimmed.strip_prefix("0X"))
                    {
                        i64::from_str_radix(hex, 16).unwrap_or(-1)
                    } else if let Some(oct) = trimmed
                        .strip_prefix("0o")
                        .or_else(|| trimmed.strip_prefix("0O"))
                    {
                        i64::from_str_radix(oct, 8).unwrap_or(-1)
                    } else if let Some(bin) = trimmed
                        .strip_prefix("0b")
                        .or_else(|| trimmed.strip_prefix("0B"))
                    {
                        i64::from_str_radix(bin, 2).unwrap_or(-1)
                    } else {
                        -1
                    };
                    (i, format!("{}", i))
                }
            };
            if !(0..=0x10FFFF).contains(&code) {
                let hex = format!("{:X}", code);
                return Some(Err(RuntimeError::new(format!(
                    "Codepoint {} (0x{}) is out of bounds in 'chr'",
                    display, hex
                ))));
            }
            if let Some(ch) = std::char::from_u32(code as u32) {
                // NFC-normalize: some codepoints decompose in NFC
                let s: String = ch.to_string().nfc().collect();
                Some(Ok(Value::str(s)))
            } else {
                Some(Err(RuntimeError::new(format!(
                    "Codepoint {} (0x{:X}) is out of bounds in 'chr'",
                    display, code
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
                Some(Ok(Value::str(name)))
            } else {
                Some(Ok(Value::Nil))
            }
        }
        "uniparse" | "parse-names" => {
            let s = arg.to_string_value();
            Some(uniparse_impl(&s))
        }
        "uniprop" => {
            // uniprop(target) — returns General_Category
            match arg {
                Value::Int(i) => {
                    let cp = *i as u32;
                    Some(Ok(super::uniprop::unicode_property_value_for_codepoint(
                        cp, None,
                    )))
                }
                Value::Package(_) => {
                    // uniprop(Str) or uniprop(Int) — type object, not instance
                    let msg = "Cannot resolve caller uniprop".to_string();
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Multi::NoMatch"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(&msg);
                    err.exception = Some(Box::new(ex));
                    Some(Err(err))
                }
                _ => {
                    let s = arg.to_string_value();
                    if s.is_empty() {
                        return Some(Ok(Value::Nil));
                    }
                    let ch = s.chars().next().unwrap();
                    Some(Ok(Value::str(super::unicode::unicode_general_category(ch))))
                }
            }
        }
        "is-prime" => Some(super::methods_0arg::coercion::value_is_prime(arg)),
        "lsb" => super::methods_0arg::native_method_0arg(arg, Symbol::intern("lsb")),
        "msb" => super::methods_0arg::native_method_0arg(arg, Symbol::intern("msb")),
        "sign" => {
            if matches!(arg, Value::Instance { .. }) {
                return None;
            }
            // Delegate to the .sign method implementation
            super::methods_0arg::native_method_0arg(arg, Symbol::intern("sign"))
        }
        "abs" => Some(Ok(match arg {
            Value::Int(i) => Value::Int(i.abs()),
            Value::Num(f) => Value::Num(f.abs()),
            Value::Rat(n, d) => Value::Rat(n.abs(), *d),
            Value::Complex(re, im) => Value::Num((re * re + im * im).sqrt()),
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
        "floor" => {
            if matches!(arg, Value::Instance { .. }) {
                return None;
            }
            Some(Ok(match arg {
                Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
                Value::Num(f) => Value::Num(f.floor()),
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
            }))
        }
        "ceiling" | "ceil" => {
            if matches!(arg, Value::Instance { .. }) {
                return None;
            }
            Some(Ok(match arg {
                Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
                Value::Num(f) => Value::Num(f.ceil()),
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
            }))
        }
        "round" => {
            if matches!(arg, Value::Instance { .. }) {
                return None;
            }
            // Raku-style rounding: (x + 0.5).floor() — round half toward +Inf
            fn raku_round_to_value(f: f64) -> Value {
                let rounded = (f + 0.5).floor();
                if rounded >= i64::MIN as f64 && rounded <= i64::MAX as f64 {
                    Value::Int(rounded as i64)
                } else {
                    // Use BigInt for values outside i64 range
                    use num_bigint::BigInt;
                    use num_traits::ToPrimitive;
                    // Convert via string to avoid precision loss
                    let s = format!("{:.0}", rounded);
                    if let Ok(bi) = s.parse::<BigInt>() {
                        if let Some(i) = bi.to_i64() {
                            Value::Int(i)
                        } else {
                            Value::bigint(bi)
                        }
                    } else {
                        Value::Num(rounded)
                    }
                }
            }
            Some(Ok(match arg {
                Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
                Value::Num(f) => raku_round_to_value(*f),
                Value::Int(i) => Value::Int(*i),
                Value::Rat(n, d) if *d != 0 => {
                    let f = *n as f64 / *d as f64;
                    raku_round_to_value(f)
                }
                _ => Value::Int(0),
            }))
        }
        "exp" => Some(Ok(match arg {
            Value::Instance { .. } => return None,
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
            Value::Instance { .. } => None,
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
            Value::Instance { .. } => None,
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
            Value::Instance { .. } => None,
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
                // Use formula-based computation for asinh to match Raku's
                // behavior: for very large inputs (e.g. 1e200), the intermediate
                // x^2 overflows to Inf, producing Inf as the result (matching Raku).
                // Handle negative values via sign to avoid cancellation error.
                "asinh" => {
                    let sign = x.signum();
                    let ax = x.abs();
                    sign * (ax + (ax * ax + 1.0).sqrt()).ln()
                }
                // acosh: use native for correctness (NaN for x < 1), but match
                // Raku's Inf for very large positive inputs via formula fallback.
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
            if matches!(arg, Value::Instance { .. }) {
                return None;
            }
            let x = runtime::to_float_value(arg).unwrap_or(f64::NAN);
            Some(Ok(Value::Complex(x.cos(), x.sin())))
        }
        "truncate" => {
            if matches!(arg, Value::Instance { .. }) {
                return None;
            }
            // Bool.truncate preserves Bool type
            if let Value::Bool(b) = arg {
                return Some(Ok(Value::Bool(*b)));
            }
            // Delegate to the .truncate method for proper type handling
            super::methods_0arg::native_method_0arg(arg, Symbol::intern("truncate")).or_else(|| {
                if let Some(num) = runtime::to_float_value(arg) {
                    if num.is_nan() || num.is_infinite() {
                        Some(Ok(Value::Num(num)))
                    } else {
                        Some(Ok(Value::Int(num.trunc() as i64)))
                    }
                } else {
                    Some(Ok(Value::Int(runtime::to_int(arg))))
                }
            })
        }
        "defined" => {
            // For junctions, autothread .defined and collapse to Bool
            if let Value::Junction { .. } = arg {
                if let Some(Ok(result)) = crate::builtins::methods_0arg::native_method_0arg(
                    arg,
                    crate::symbol::Symbol::intern("defined"),
                ) {
                    Some(Ok(result))
                } else {
                    Some(Ok(Value::Bool(true)))
                }
            } else {
                Some(Ok(Value::Bool(match arg {
                    Value::Nil | Value::Package(_) => false,
                    Value::Slip(items) if items.is_empty() => false,
                    Value::Instance { class_name, .. } if class_name == "Failure" => false,
                    _ => true,
                })))
            }
        }
        "elems" => match arg {
            Value::Array(items, ..) => Some(Ok(Value::Int(items.len() as i64))),
            Value::Hash(items) => Some(Ok(Value::Int(items.len() as i64))),
            Value::Str(s) => Some(Ok(Value::Int(s.chars().count() as i64))),
            Value::LazyList(_) => None,
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Stash" => match attributes.get("symbols") {
                Some(Value::Hash(map)) => Some(Ok(Value::Int(map.len() as i64))),
                _ => Some(Ok(Value::Int(0))),
            },
            _ => Some(Ok(Value::Int(1))),
        },
        "reverse" => {
            if let Some(shape) = crate::runtime::utils::shaped_array_shape(arg) {
                if shape.len() > 1 {
                    return Some(Err(RuntimeError::illegal_on_fixed_dimension_array(
                        "reverse",
                    )));
                }
                // 1D shaped array: reverse the leaves
                let mut leaves = crate::runtime::utils::shaped_array_leaves(arg);
                leaves.reverse();
                return Some(Ok(Value::array(leaves)));
            }
            Some(Ok(match arg {
                Value::Array(items, ..) => {
                    let mut reversed = (**items).clone();
                    reversed.reverse();
                    Value::array(reversed)
                }
                // reverse() on a string returns a single-element list (not a flip)
                other => Value::array(vec![other.clone()]),
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
            if let Some(shape) = crate::runtime::utils::shaped_array_shape(arg) {
                if shape.len() > 1 {
                    return Some(Err(RuntimeError::illegal_on_fixed_dimension_array(
                        "rotate",
                    )));
                }
                // 1D shaped array: rotate the leaves
                let leaves = crate::runtime::utils::shaped_array_leaves(arg);
                let len = leaves.len();
                if len == 0 {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let n = 1usize % len;
                let mut rotated = Vec::with_capacity(len);
                rotated.extend_from_slice(&leaves[n..]);
                rotated.extend_from_slice(&leaves[..n]);
                return Some(Ok(Value::array(rotated)));
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
            // Flatten the argument with flatten_arrays=true so that
            // top-level arrays (including @a in `flat (6, @a)`) are
            // flattened, while nested arrays inside [...] are preserved.
            flat_val(arg, &mut flat, true);
            Some(Ok(Value::Seq(std::sync::Arc::new(flat))))
        }
        "first" => Some(Ok(match arg {
            Value::Array(items, ..) => items.first().cloned().unwrap_or(Value::Nil),
            _ => arg.clone(),
        })),
        "min" => Some(Ok(match arg {
            Value::Hash(items) => items
                .iter()
                .min_by(|(ak, _), (bk, _)| ak.cmp(bk))
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .unwrap_or(Value::Nil),
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
            Value::Hash(items) => items
                .iter()
                .max_by(|(ak, _), (bk, _)| ak.cmp(bk))
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .unwrap_or(Value::Nil),
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
        "gist" => Some(Ok(Value::str(arg.to_string_value()))),
        _ => None,
    }
}

fn native_function_2arg(
    name: &str,
    arg1: &Value,
    arg2: &Value,
) -> Option<Result<Value, RuntimeError>> {
    fn failure_exception(value: &Value) -> Option<Value> {
        match value {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => attributes.get("exception").cloned(),
            Value::Mixin(inner, mixins) => {
                if let Some(mixed) = mixins.get("Failure")
                    && let Some(ex) = failure_exception(mixed)
                {
                    return Some(ex);
                }
                failure_exception(inner)
            }
            _ => None,
        }
    }

    fn minmax_two(arg1: &Value, arg2: &Value, want_max: bool) -> Result<Value, RuntimeError> {
        let ex1 = failure_exception(arg1);
        let ex2 = failure_exception(arg2);
        if ex1.is_some() && ex2.is_some() {
            let ex = ex1.unwrap_or(Value::Nil);
            let mut err = RuntimeError::new(ex.to_string_value());
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        if ex1.is_some() {
            return Ok(arg1.clone());
        }
        if ex2.is_some() {
            return Ok(arg2.clone());
        }
        if matches!(arg1, Value::Package(name) if name == "Any") {
            return Ok(arg2.clone());
        }
        if matches!(arg2, Value::Package(name) if name == "Any") {
            return Ok(arg1.clone());
        }

        let cmp = crate::runtime::compare_values(arg1, arg2);
        Ok(if (want_max && cmp >= 0) || (!want_max && cmp <= 0) {
            arg1.clone()
        } else {
            arg2.clone()
        })
    }

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
            super::native_method_1arg(&Value::array(items), Symbol::intern("combinations"), arg2)
        }
        "roll" => {
            let count = match arg1 {
                Value::Int(i) if *i > 0 => Some(*i as usize),
                Value::Int(_) => Some(0),
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                Value::Whatever => None,
                Value::Str(s) => {
                    let parsed = s.trim().parse::<i64>().ok()?;
                    Some(parsed.max(0) as usize)
                }
                _ => return None,
            };
            let items = crate::runtime::utils::value_to_list(arg2);
            if count.is_none() {
                if items.is_empty() {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let generated = 1024usize;
                let mut out = Vec::with_capacity(generated);
                for _ in 0..generated {
                    let mut idx = (builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    out.push(items[idx].clone());
                }
                return Some(Ok(Value::LazyList(std::sync::Arc::new(
                    crate::value::LazyList::new_cached(out),
                ))));
            }
            let count = count.unwrap_or(0);
            if items.is_empty() || count == 0 {
                return Some(Ok(Value::array(Vec::new())));
            }
            let mut result = Vec::with_capacity(count);
            for _ in 0..count {
                let mut idx = (builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                result.push(items[idx].clone());
            }
            Some(Ok(Value::array(result)))
        }
        "pick" => {
            // pick($count, @list) — sub form delegates to method .pick($count)
            let list = Value::array(runtime::value_to_list(arg2));
            super::native_method_1arg(&list, Symbol::intern("pick"), arg1)
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
        "roots" => {
            // roots($number, $n) — compute nth roots of $number
            Some(Ok(super::methods_narg::compute_roots(arg1, arg2)))
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
            Some(Ok(Value::str(result)))
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
                return Some(Ok(Value::str(joined)));
            }
            match arg2 {
                Value::Array(items, kind) if !kind.is_itemized() => {
                    let joined = items
                        .iter()
                        .map(|v| v.to_str_context())
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                Value::Seq(items) | Value::Slip(items) => {
                    let joined = items
                        .iter()
                        .map(|v| v.to_str_context())
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                Value::LazyList(_) => {
                    // Fall through to runtime to force the lazy list
                    None
                }
                _ => {
                    // Treat as single-element list (includes itemized arrays)
                    Some(Ok(Value::str(arg2.to_str_context())))
                }
            }
        }
        "rotate" => {
            if let Some(shape) = crate::runtime::utils::shaped_array_shape(arg1) {
                if shape.len() > 1 {
                    return Some(Err(RuntimeError::illegal_on_fixed_dimension_array(
                        "rotate",
                    )));
                }
                // 1D shaped array: rotate the leaves
                let leaves = crate::runtime::utils::shaped_array_leaves(arg1);
                let len = leaves.len() as i64;
                if len == 0 {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let count = runtime::to_int(arg2);
                let n = ((count % len) + len) % len;
                let n = n as usize;
                let mut rotated = Vec::with_capacity(leaves.len());
                rotated.extend_from_slice(&leaves[n..]);
                rotated.extend_from_slice(&leaves[..n]);
                return Some(Ok(Value::array(rotated)));
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
            if matches!(arg1, Value::Junction { .. }) || matches!(arg2, Value::Junction { .. }) {
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
            // Fall through to runtime for arrays (list of needles)
            if matches!(arg2, Value::Array(..)) {
                return None;
            }
            let s = arg1.to_string_value();
            let needle = arg2.to_string_value();
            Some(Ok(match s.rfind(&needle) {
                Some(pos) => Value::Int(s[..pos].chars().count() as i64),
                None => Value::Nil,
            }))
        }
        "substr" => {
            if matches!(arg1, Value::Junction { .. }) || matches!(arg2, Value::Junction { .. }) {
                return None;
            }
            let start = match arg2 {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Int(_) => return None, // negative: let runtime handle
                _ => return None,
            };
            let s = arg1.to_string_value();
            let chars: Vec<char> = s.chars().collect();
            if start > chars.len() {
                return None; // out-of-range: let runtime handle (returns Failure)
            }
            Some(Ok(Value::str(chars[start..].iter().collect())))
        }
        "samemark" => {
            let target = arg1.to_string_value();
            let source = arg2.to_string_value();
            Some(Ok(Value::str(crate::builtins::samemark_string(
                &target, &source,
            ))))
        }
        "samecase" => {
            let source = arg1.to_string_value();
            let pattern = arg2.to_string_value();
            Some(Ok(Value::str(crate::builtins::samecase_string(
                &source, &pattern,
            ))))
        }
        "log" => {
            if matches!(arg1, Value::Instance { .. }) || matches!(arg2, Value::Instance { .. }) {
                return None;
            }
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
            // Raku-style rounding: (x + 0.5).floor()
            fn raku_round_f64(x: f64) -> f64 {
                (x + 0.5).floor()
            }
            if scale == 0.0 {
                Some(Ok(Value::Int(raku_round_f64(x) as i64)))
            } else {
                let factor = (1.0 / scale).abs();
                Some(Ok(Value::Num(raku_round_f64(x * factor) / factor)))
            }
        }
        "min" => {
            if matches!(arg1, Value::Pair(name, _) if name == "by")
                || matches!(arg2, Value::Pair(name, _) if name == "by")
                || matches!(arg1, Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by"))
                || matches!(arg2, Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by"))
            {
                return None;
            }
            Some(minmax_two(arg1, arg2, false))
        }
        "max" => {
            if matches!(arg1, Value::Pair(name, _) if name == "by")
                || matches!(arg2, Value::Pair(name, _) if name == "by")
                || matches!(arg1, Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by"))
                || matches!(arg2, Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by"))
            {
                return None;
            }
            Some(minmax_two(arg1, arg2, true))
        }
        "words" => {
            let s = arg1.to_string_value();
            let limit = match arg2 {
                Value::Int(i) => Some((*i).max(0) as usize),
                Value::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    Some(bi.to_usize().unwrap_or(usize::MAX))
                }
                Value::Whatever => None,
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                Value::Rat(n, d) if *d == 0 && *n > 0 => None,
                _ => return None,
            };
            let mut words: Vec<Value> = s
                .split_whitespace()
                .map(|w| Value::str(w.to_string()))
                .collect();
            if let Some(n) = limit {
                words.truncate(n);
            }
            Some(Ok(Value::Seq(std::sync::Arc::new(words))))
        }
        "uniprop" => {
            // uniprop(target, property_name)
            let prop_name = arg2.to_string_value();
            match arg1 {
                Value::Int(i) => {
                    let cp = *i as u32;
                    Some(Ok(super::uniprop::unicode_property_value_for_codepoint(
                        cp,
                        Some(&prop_name),
                    )))
                }
                _ => {
                    let s = arg1.to_string_value();
                    if s.is_empty() {
                        return Some(Ok(Value::Nil));
                    }
                    let ch = s.chars().next().unwrap();
                    Some(Ok(super::uniprop::unicode_property_value(ch, &prop_name)))
                }
            }
        }
        "unimatch" => {
            // unimatch(target, property_value)
            let prop_value = arg2.to_string_value();
            match arg1 {
                Value::Package(_) => {
                    let msg = "Cannot resolve caller unimatch".to_string();
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Multi::NoMatch"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(&msg);
                    err.exception = Some(Box::new(ex));
                    Some(Err(err))
                }
                Value::Int(i) => {
                    let cp = *i as u32;
                    Some(Ok(super::uniprop::unimatch_for_codepoint(
                        cp,
                        &prop_value,
                        None,
                    )))
                }
                _ => {
                    let s = arg1.to_string_value();
                    if s.is_empty() {
                        return Some(Ok(Value::Nil));
                    }
                    let ch = s.chars().next().unwrap();
                    Some(Ok(Value::Bool(super::uniprop::unimatch(
                        ch,
                        &prop_value,
                        None,
                    ))))
                }
            }
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
        "unimatch" => {
            // unimatch(target, property_value, property_name)
            let prop_value = arg2.to_string_value();
            let prop_name = arg3.to_string_value();
            match arg1 {
                Value::Int(i) => {
                    let cp = *i as u32;
                    Some(Ok(super::uniprop::unimatch_for_codepoint(
                        cp,
                        &prop_value,
                        Some(&prop_name),
                    )))
                }
                _ => {
                    let s = arg1.to_string_value();
                    if s.is_empty() {
                        return Some(Ok(Value::Nil));
                    }
                    let ch = s.chars().next().unwrap();
                    Some(Ok(Value::Bool(super::uniprop::unimatch(
                        ch,
                        &prop_value,
                        Some(&prop_name),
                    ))))
                }
            }
        }
        "substr" => {
            if matches!(arg1, Value::Junction { .. })
                || matches!(arg2, Value::Junction { .. })
                || matches!(arg3, Value::Junction { .. })
            {
                return None;
            }
            let start = match arg2 {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Int(_) => return None, // negative: let runtime handle
                _ => return None,
            };
            let len = match arg3 {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Int(_) => return None,
                _ => return None,
            };
            let s = arg1.to_string_value();
            let chars: Vec<char> = s.chars().collect();
            if start > chars.len() {
                return None; // out-of-range: let runtime handle (returns Failure)
            }
            let end = (start + len).min(chars.len());
            Some(Ok(Value::str(chars[start..end].iter().collect())))
        }
        _ => None,
    }
}

fn gcd_u64(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// If the value represents an integer (even as Num, Rat, Str, or BigInt), return as BigInt.
fn generic_range_as_bigint(v: &Value) -> Option<NumBigInt> {
    match v {
        Value::Int(i) => Some(NumBigInt::from(*i)),
        Value::BigInt(n) => Some((**n).clone()),
        Value::Num(f) => {
            if f.is_finite() && *f == f.trunc() && f.abs() < i64::MAX as f64 {
                Some(NumBigInt::from(*f as i64))
            } else {
                None
            }
        }
        Value::Rat(n, d) => {
            if *d != 0 && *n % *d == 0 {
                Some(NumBigInt::from(*n / *d))
            } else {
                None
            }
        }
        Value::Str(s) => s.trim().parse::<i64>().ok().map(NumBigInt::from),
        Value::Bool(b) => Some(NumBigInt::from(if *b { 1 } else { 0 })),
        _ => None,
    }
}

fn native_function_variadic(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "min" => {
            if args.is_empty() {
                return Some(Ok(Value::Nil));
            }
            if args.iter().any(|arg| {
                matches!(arg, Value::Pair(name, _) if name == "by")
                    || matches!(arg, Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by"))
            }) {
                return None;
            }
            let mut acc = args[0].clone();
            for rhs in &args[1..] {
                let next = native_function_2arg("min", &acc, rhs)?;
                match next {
                    Ok(v) => acc = v,
                    Err(e) => return Some(Err(e)),
                }
            }
            Some(Ok(acc))
        }
        "max" => {
            if args.is_empty() {
                return Some(Ok(Value::Nil));
            }
            if args.iter().any(|arg| {
                matches!(arg, Value::Pair(name, _) if name == "by")
                    || matches!(arg, Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(name) if name.as_str() == "by"))
            }) {
                return None;
            }
            let mut acc = args[0].clone();
            for rhs in &args[1..] {
                let next = native_function_2arg("max", &acc, rhs)?;
                match next {
                    Ok(v) => acc = v,
                    Err(e) => return Some(Err(e)),
                }
            }
            Some(Ok(acc))
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
            Some(Ok(Value::str(result)))
        }
        "zip" => {
            // zip([@a], [@b], ...) — interleave elements from each list
            // zip takes a single list-of-lists argument; each sub-list is a
            // "column" and zip transposes them into rows.
            let is_lazy_input =
                |v: &Value| -> bool { matches!(v, Value::LazyList(_)) || is_infinite_range(v) };
            let (raw_inputs, single_arg) = if args.len() == 1 {
                (runtime::value_to_list(&args[0]), true)
            } else {
                (args.to_vec(), false)
            };
            let all_lazy = if single_arg {
                // For single-arg zip, check if all sub-lists are lazy
                raw_inputs.iter().all(is_lazy_input)
            } else {
                args.iter().all(is_lazy_input)
            };
            let lists: Vec<Vec<Value>> = raw_inputs.iter().map(runtime::value_to_list).collect();
            if lists.is_empty() {
                return Some(Ok(Value::Seq(std::sync::Arc::new(vec![]))));
            }
            let max_expand: usize = 1_000;
            let min_len = lists
                .iter()
                .map(|l| l.len())
                .min()
                .unwrap_or(0)
                .min(max_expand);
            let mut result = Vec::with_capacity(min_len);
            for i in 0..min_len {
                let row: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(Value::array(row));
            }
            if all_lazy {
                Some(Ok(Value::LazyList(std::sync::Arc::new(
                    crate::value::LazyList::new_cached(result),
                ))))
            } else {
                Some(Ok(Value::array(result)))
            }
        }
        "flat" => {
            if args.len() == 1 && is_infinite_range(&args[0]) {
                return Some(Ok(args[0].clone()));
            }
            let mut result = Vec::new();
            for arg in args {
                flat_val(arg, &mut result, true);
            }
            Some(Ok(Value::Seq(std::sync::Arc::new(result))))
        }
        "sum" => {
            // If any argument (or element inside an array arg) is a Junction,
            // fold with junction-aware addition
            let has_junction = args.iter().any(|a| match a {
                Value::Junction { .. } => true,
                Value::Array(items, ..) | Value::Seq(items) => {
                    items.iter().any(|v| matches!(v, Value::Junction { .. }))
                }
                _ => false,
            });
            if has_junction {
                let items: Vec<Value> = args
                    .iter()
                    .flat_map(|a| match a {
                        Value::Array(items, ..) | Value::Seq(items) => {
                            items.iter().cloned().collect::<Vec<_>>()
                        }
                        other => vec![other.clone()],
                    })
                    .collect();
                let result = items.into_iter().try_fold(
                    Value::Int(0),
                    |acc, item| -> Result<Value, RuntimeError> {
                        crate::builtins::methods_0arg::collection::add_with_junction_threading(
                            acc, item,
                        )
                    },
                );
                return Some(result);
            }
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
                    Value::RangeExclStart(a, b) => {
                        let start = a + 1;
                        if start <= *b {
                            let n = b - start + 1;
                            let s = n * (start + b) / 2;
                            if has_num {
                                total_f += s as f64;
                            } else {
                                total += s;
                            }
                        }
                    }
                    Value::RangeExclBoth(a, b) => {
                        let start = a + 1;
                        let end = b - 1;
                        if start <= end {
                            let n = end - start + 1;
                            let s = n * (start + end) / 2;
                            if has_num {
                                total_f += s as f64;
                            } else {
                                total += s;
                            }
                        }
                    }
                    Value::GenericRange {
                        start,
                        end,
                        excl_start,
                        excl_end,
                    } => {
                        // Try to detect integer-valued ranges for Gauss formula
                        let start_bi = generic_range_as_bigint(start);
                        let end_bi = generic_range_as_bigint(end);
                        if let (Some(a), Some(b)) = (start_bi, end_bi) {
                            let one = NumBigInt::from(1);
                            let two = NumBigInt::from(2);
                            let zero = NumBigInt::from(0);
                            let eff_start = if *excl_start { &a + &one } else { a };
                            let eff_end = if *excl_end { &b - &one } else { b };
                            if eff_start <= eff_end {
                                let n = &eff_end - &eff_start + &one;
                                let s_plus = &eff_start + &eff_end;
                                let s = if &s_plus % &two == zero {
                                    (&s_plus / &two) * &n
                                } else {
                                    &s_plus * (&n / &two)
                                };
                                if let Ok(val) = i64::try_from(&s) {
                                    if has_num {
                                        total_f += val as f64;
                                    } else {
                                        total += val;
                                    }
                                } else {
                                    // Result is too large for i64, return BigInt directly
                                    return Some(Ok(Value::BigInt(std::sync::Arc::new(s))));
                                }
                            }
                        } else {
                            // Non-integer range: sum via list with Rat support
                            let items = crate::runtime::utils::value_to_list(arg);
                            let items_have_rat =
                                items.iter().any(|v| matches!(v, Value::Rat(_, _)));
                            if items_have_rat {
                                // Use rational arithmetic for the entire result
                                let mut rat_num: i64 = total;
                                let mut rat_den: i64 = 1;
                                for item in &items {
                                    let (in_num, in_den) = match item {
                                        Value::Rat(n, d) => (*n, *d),
                                        Value::Int(n) => (*n, 1),
                                        _ => (crate::runtime::to_int(item), 1),
                                    };
                                    rat_num = rat_num * in_den + in_num * rat_den;
                                    rat_den *= in_den;
                                    let g = gcd_u64(rat_num.unsigned_abs(), rat_den.unsigned_abs())
                                        as i64;
                                    if g > 1 {
                                        rat_num /= g;
                                        rat_den /= g;
                                    }
                                }
                                // Return Rat result directly
                                return if rat_den == 1 {
                                    Some(Ok(Value::Int(rat_num)))
                                } else {
                                    Some(Ok(Value::Rat(rat_num, rat_den)))
                                };
                            }
                            for item in &items {
                                match item {
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
                                    _ => {
                                        if has_num {
                                            total_f += item.to_f64();
                                        } else {
                                            total += item.to_f64() as i64;
                                        }
                                    }
                                }
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

/// Perl 5-compatible `times` builtin: returns `($user, $system)` CPU times in seconds.
fn builtin_times() -> Result<Value, RuntimeError> {
    #[cfg(unix)]
    {
        let mut usage = std::mem::MaybeUninit::<libc::rusage>::uninit();
        let ret = unsafe { libc::getrusage(libc::RUSAGE_SELF, usage.as_mut_ptr()) };
        if ret == 0 {
            let usage = unsafe { usage.assume_init() };
            let user = usage.ru_utime.tv_sec as f64 + usage.ru_utime.tv_usec as f64 / 1_000_000.0;
            let sys = usage.ru_stime.tv_sec as f64 + usage.ru_stime.tv_usec as f64 / 1_000_000.0;
            Ok(Value::Array(
                vec![Value::Num(user), Value::Num(sys)].into(),
                ArrayKind::List,
            ))
        } else {
            Ok(Value::Array(
                vec![Value::Num(0.0), Value::Num(0.0)].into(),
                ArrayKind::List,
            ))
        }
    }
    #[cfg(not(unix))]
    {
        Ok(Value::Array(
            vec![Value::Num(0.0), Value::Num(0.0)].into(),
            ArrayKind::List,
        ))
    }
}

/// Perl 5-compatible `localtime`/`gmtime` builtins.
/// With args: returns a 9-element list `($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)`
/// Without args: returns a ctime-style formatted string.
fn builtin_localtime_gmtime(name: &str, args: &[Value]) -> Result<Value, RuntimeError> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let epoch_secs: i64 = if args.is_empty() {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i64
    } else {
        args[0].to_f64() as i64
    };

    #[cfg(unix)]
    {
        let time_t = epoch_secs as libc::time_t;
        let mut tm = std::mem::MaybeUninit::<libc::tm>::uninit();
        let result = if name == "gmtime" {
            unsafe { libc::gmtime_r(&time_t, tm.as_mut_ptr()) }
        } else {
            unsafe { libc::localtime_r(&time_t, tm.as_mut_ptr()) }
        };
        if result.is_null() {
            return Err(RuntimeError::new(format!(
                "{name}: invalid time value {epoch_secs}"
            )));
        }
        let tm = unsafe { tm.assume_init() };

        let sec = tm.tm_sec;
        let min = tm.tm_min;
        let hour = tm.tm_hour;
        let mday = tm.tm_mday;
        let mon = tm.tm_mon; // 0-based
        let year = tm.tm_year + 1900;
        let wday = tm.tm_wday;
        let yday = tm.tm_yday;
        let isdst = tm.tm_isdst;

        if args.is_empty() {
            // Scalar-like context: return the ctime-style formatted string
            let dow_names = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
            let mon_names = [
                "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
            ];
            let dow_str = dow_names.get(wday as usize).unwrap_or(&"???");
            let mon_str = mon_names.get(mon as usize).unwrap_or(&"???");
            let scalar_str = format!(
                "{} {} {:2} {:02}:{:02}:{:02} {}",
                dow_str, mon_str, mday, hour, min, sec, year
            );
            Ok(Value::str(scalar_str))
        } else {
            // List context: return the 9-element list
            Ok(Value::Array(
                vec![
                    Value::Int(sec as i64),
                    Value::Int(min as i64),
                    Value::Int(hour as i64),
                    Value::Int(mday as i64),
                    Value::Int(mon as i64),
                    Value::Int(year as i64),
                    Value::Int(wday as i64),
                    Value::Int(yday as i64),
                    Value::Int(isdst as i64),
                ]
                .into(),
                ArrayKind::List,
            ))
        }
    }
    #[cfg(not(unix))]
    {
        let _ = (name, epoch_secs);
        Err(RuntimeError::new(
            "localtime/gmtime not supported on this platform".to_string(),
        ))
    }
}
