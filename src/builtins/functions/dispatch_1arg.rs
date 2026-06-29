#![allow(clippy::result_large_err)]
use super::flat::{deitemize_flat_operand, flat_val, is_infinite_range};
use super::math::factorial_bigint;
use super::uniparse::uniparse_impl;
use crate::builtins::rng::builtin_srand;
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
use std::collections::HashMap;
use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

pub(crate) fn native_function_1arg(name: &str, arg: &Value) -> Option<Result<Value, RuntimeError>> {
    // Eager list operations (combinations/permutations/...) cannot run on a
    // lazy/infinite source: throw X::Cannot::Lazy instead of hanging while
    // generating an unbounded result. Shared with the method-dispatch paths.
    if let Some(err) = crate::runtime::Interpreter::lazy_guard_error(name, arg) {
        return Some(Err(err));
    }
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
                        crate::builtins::methods_0arg::collection::combinations_all(&items).into(),
                    )));
                }
                _ => runtime::value_to_list(arg),
            };
            Some(Ok(Value::Seq(
                crate::builtins::methods_0arg::collection::combinations_all(&items).into(),
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
                        // For large n, return a lazy list that knows its count (n!)
                        // without actually generating all permutations.
                        let factorial = factorial_bigint(*n as u64);
                        let ll = crate::value::LazyList {
                            body: Vec::new(),
                            env: crate::env::Env::new(),
                            cache: std::sync::Mutex::new(None),
                            compiled_code: None,
                            compiled_fns: None,
                            elems_count: Some(Value::BigInt(factorial)),
                            scan_spec: None,
                            sequence_spec: None,
                            coroutine: None,
                            lazy_pipe: None,
                            closure_seq: None,
                            walk_pending: None,
                        };
                        return Some(Ok(Value::LazyList(std::sync::Arc::new(ll))));
                    }
                    let items: Vec<Value> = (0..*n).map(Value::Int).collect();
                    return Some(Ok(Value::Seq(
                        crate::builtins::methods_0arg::collection::all_permutations(&items).into(),
                    )));
                }
                _ => runtime::value_to_list(arg),
            };
            if items.len() > 20 {
                // For large iterables, return a lazy list that knows its count
                let factorial = factorial_bigint(items.len() as u64);
                let ll = crate::value::LazyList {
                    body: Vec::new(),
                    env: crate::env::Env::new(),
                    cache: std::sync::Mutex::new(None),
                    compiled_code: None,
                    compiled_fns: None,
                    elems_count: Some(Value::BigInt(factorial)),
                    scan_spec: None,
                    sequence_spec: None,
                    coroutine: None,
                    lazy_pipe: None,
                    closure_seq: None,
                    walk_pending: None,
                };
                return Some(Ok(Value::LazyList(std::sync::Arc::new(ll))));
            }
            Some(Ok(Value::Seq(
                crate::builtins::methods_0arg::collection::all_permutations(&items).into(),
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
        "uc" => Some(Ok(Value::str(
            crate::builtins::unicode::grapheme_uppercase(&arg.to_string_value()),
        ))),
        "lc" => Some(Ok(Value::str(
            crate::builtins::unicode::grapheme_lowercase(&arg.to_string_value()),
        ))),
        "fc" => Some(Ok(Value::str(crate::builtins::unicode::grapheme_foldcase(
            &arg.to_string_value(),
        )))),
        "tc" => Some(Ok(Value::str(crate::builtins::unicode::titlecase_string(
            &arg.to_string_value(),
        )))),
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
            match arg {
                Value::Int(i) => match crate::builtins::unicode::uniname_from_int(*i) {
                    Ok(name) => Some(Ok(Value::str(name))),
                    Err(e) => Some(Err(e)),
                },
                Value::Package(name) => {
                    // Type objects like Str, Int should throw X::Multi::NoMatch
                    let msg = format!(
                        "Cannot resolve caller uniname({}); none of these signatures matches:\n    (\\what)",
                        name
                    );
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    let ex = Value::make_instance(Symbol::intern("X::Multi::NoMatch"), attrs);
                    let mut err = RuntimeError::new(&msg);
                    err.exception = Some(Box::new(ex));
                    Some(Err(err))
                }
                _ => {
                    let s = arg.to_string_value();
                    if s.is_empty() {
                        return Some(Ok(Value::Nil));
                    }
                    if let Some(ch) = s.chars().next() {
                        let name = crate::builtins::unicode::unicode_char_name(ch);
                        Some(Ok(Value::str(name)))
                    } else {
                        Some(Ok(Value::Nil))
                    }
                }
            }
        }
        "uninames" => {
            let s = arg.to_string_value();
            let names: Vec<Value> = s
                .chars()
                .map(|ch| Value::str(crate::builtins::unicode::unicode_char_name(ch)))
                .collect();
            Some(Ok(Value::array(names)))
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
                    Some(Ok(
                        crate::builtins::uniprop::unicode_property_value_for_codepoint(cp, None),
                    ))
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
                    Some(Ok(Value::str(
                        crate::builtins::unicode::unicode_general_category(ch),
                    )))
                }
            }
        }
        "is-prime" => Some(crate::builtins::methods_0arg::coercion::value_is_prime(arg)),
        // unival/univals delegate to the .unival/.univals method implementation
        // (dispatch_core_unicode) — single source of truth, no separate copy.
        "unival" => {
            crate::builtins::methods_0arg::native_method_0arg(arg, Symbol::intern("unival"))
        }
        "univals" => {
            crate::builtins::methods_0arg::native_method_0arg(arg, Symbol::intern("univals"))
        }
        "lsb" => crate::builtins::methods_0arg::native_method_0arg(arg, Symbol::intern("lsb")),
        "msb" => crate::builtins::methods_0arg::native_method_0arg(arg, Symbol::intern("msb")),
        "sign" => {
            if matches!(arg, Value::Instance { .. }) {
                return None;
            }
            // Delegate to the .sign method implementation
            crate::builtins::methods_0arg::native_method_0arg(arg, Symbol::intern("sign"))
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
            crate::builtins::methods_0arg::native_method_0arg(arg, Symbol::intern("truncate"))
                .or_else(|| {
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
        // `elems($x)` is defined as `$x.elems`; delegate to the single `.elems`
        // method impl instead of a drifting second copy. The inline version here
        // wrongly counted Str chars (raku: a Str is 1 element) and missed Seq.
        // `native_method_0arg` returns `None` for the cases the method leaves to
        // the interpreter (e.g. gather-sourced lazy lists), which fall through.
        "elems" => crate::builtins::methods_0arg::native_method_0arg(arg, Symbol::intern("elems")),
        "reverse" => {
            // LazyIoLines needs the interpreter to materialize — fall through
            if matches!(arg, Value::LazyIoLines { .. }) {
                return None;
            }
            if let Some(shape) = crate::runtime::utils::shaped_array_shape(arg) {
                if shape.len() > 1 {
                    return Some(Err(RuntimeError::illegal_on_fixed_dimension_array(
                        "reverse",
                    )));
                }
                // 1D shaped array: reverse the leaves
                let mut leaves = crate::runtime::utils::shaped_array_leaves(arg);
                leaves.reverse();
                return Some(Ok(Value::Seq(std::sync::Arc::new(leaves))));
            }
            Some(Ok(match arg {
                Value::Array(items, ..) => {
                    let mut reversed = (**items).clone();
                    reversed.reverse();
                    Value::Seq(std::sync::Arc::new(reversed.items))
                }
                Value::Seq(items) | Value::Slip(items) => {
                    let mut reversed = (**items).clone();
                    reversed.reverse();
                    Value::Seq(std::sync::Arc::new(reversed))
                }
                Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. } => {
                    let mut items = crate::runtime::Interpreter::value_to_list(arg);
                    items.reverse();
                    Value::Seq(std::sync::Arc::new(items))
                }
                // reverse() on a string returns a single-element Seq (not a flip)
                other => Value::Seq(std::sync::Arc::new(vec![other.clone()])),
            }))
        }
        "sort" => {
            if crate::runtime::utils::is_shaped_array(arg) {
                let mut leaves = crate::runtime::utils::shaped_array_leaves(arg);
                leaves.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
                return Some(Ok(Value::Seq(std::sync::Arc::new(leaves))));
            }
            Some(Ok(match arg {
                Value::Array(items, ..) => {
                    let mut sorted = (**items).clone();
                    sorted.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
                    Value::Seq(std::sync::Arc::new(sorted.items))
                }
                Value::Seq(items) | Value::Slip(items) => {
                    let mut sorted = (**items).clone();
                    sorted.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
                    Value::Seq(std::sync::Arc::new(sorted))
                }
                Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. } => {
                    let mut sorted = crate::runtime::Interpreter::value_to_list(arg);
                    sorted.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
                    Value::Seq(std::sync::Arc::new(sorted))
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
                    return Some(Ok(Value::Seq(std::sync::Arc::new(Vec::new()))));
                }
                let n = 1usize % len;
                let mut rotated = Vec::with_capacity(len);
                rotated.extend_from_slice(&leaves[n..]);
                rotated.extend_from_slice(&leaves[..n]);
                return Some(Ok(Value::Seq(std::sync::Arc::new(rotated))));
            }
            // rotate with no count defaults to 1; returns a Seq (raku semantics).
            let items: Option<Vec<Value>> = match arg {
                Value::Array(items, ..) => Some(items.iter().cloned().collect()),
                Value::Seq(items) | Value::Slip(items) => Some((**items).clone()),
                Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. } => {
                    Some(crate::runtime::Interpreter::value_to_list(arg))
                }
                _ => None,
            };
            Some(Ok(match items {
                Some(items) if !items.is_empty() => {
                    let n = 1usize % items.len();
                    let mut rotated = Vec::with_capacity(items.len());
                    rotated.extend_from_slice(&items[n..]);
                    rotated.extend_from_slice(&items[..n]);
                    Value::Seq(std::sync::Arc::new(rotated))
                }
                Some(_) => Value::Seq(std::sync::Arc::new(Vec::new())),
                None => Value::Nil,
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
            // The top-level operand is de-itemized first so `$(1,2,3).flat`
            // descends (Raku un-itemizes the receiver before flattening).
            flat_val(&deitemize_flat_operand(arg), &mut flat, true);
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
                .map(|(k, v)| items.typed_pair(k, v.clone()))
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
                .map(|(k, v)| items.typed_pair(k, v.clone()))
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
            // `ords` returns a Seq (like `comb`), not a List.
            Some(Ok(Value::Seq(std::sync::Arc::new(codes))))
        }
        "gist" => Some(Ok(Value::str(arg.to_string_value()))),
        _ => None,
    }
}
