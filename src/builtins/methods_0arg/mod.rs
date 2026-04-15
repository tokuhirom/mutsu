#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, EnumValue, RuntimeError, Value};
use num_traits::{Signed, ToPrimitive, Zero};
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;

use super::rng::builtin_rand;

pub(crate) mod coercion;
pub(crate) mod collection;
pub(crate) mod complex_math;
mod dispatch_core_coerce;
mod dispatch_core_list;
mod dispatch_core_math;
mod dispatch_core_numeric;
pub(crate) mod dispatch_core_range;
mod dispatch_core_repr;
mod dispatch_core_str;
mod dispatch_core_unicode;
pub(crate) mod match_helpers;
pub(crate) mod raku_repr;
pub(crate) mod temporal;
pub(crate) mod temporal_dispatch;

use std::collections::HashMap;

/// Create an X::Multi::NoMatch error for a method called on a type object.
fn make_no_match_error(method_name: &str) -> RuntimeError {
    let msg = format!("Cannot resolve caller {}", method_name);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::NoMatch"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

///// Raku-style rounding: round half toward positive infinity (ceiling).
fn raku_round(x: f64) -> f64 {
    (x + 0.5).floor()
}

/// Raku-style rounding that returns a Value, using BigInt for large values.
fn raku_round_to_value(f: f64) -> Value {
    let rounded = raku_round(f);
    if rounded >= i64::MIN as f64 && rounded <= i64::MAX as f64 {
        Value::Int(rounded as i64)
    } else {
        use num_bigint::BigInt;
        use num_traits::ToPrimitive;
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

fn sample_weighted_mix_key(items: &HashMap<String, f64>) -> Option<Value> {
    let mut total = 0.0;
    for weight in items.values() {
        if weight.is_finite() && *weight > 0.0 {
            total += *weight;
        }
    }
    if total <= 0.0 {
        return None;
    }
    let mut needle = builtin_rand() * total;
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

fn sample_weighted_bag_key(items: &HashMap<String, i64>) -> Option<Value> {
    let mut total: i128 = 0;
    for count in items.values() {
        if *count > 0 {
            total += *count as i128;
        }
    }
    if total <= 0 {
        return None;
    }
    let needle_f = builtin_rand() * total as f64;
    let mut needle = needle_f as i128;
    if needle >= total {
        needle = total - 1;
    }
    for (key, count) in items {
        if *count <= 0 {
            continue;
        }
        if needle < *count as i128 {
            return Some(Value::str(key.clone()));
        }
        needle -= *count as i128;
    }
    items
        .iter()
        .find_map(|(key, count)| (*count > 0).then(|| Value::str(key.clone())))
}

/// Normalize Unicode Nd (decimal digit) characters to their ASCII equivalents.
/// Returns `None` if any non-sign, non-digit character is found.
fn normalize_unicode_digits(s: &str) -> Option<String> {
    let mut result = String::with_capacity(s.len());
    let mut has_unicode = false;
    for ch in s.chars() {
        if ch.is_ascii_digit() || ch == '-' || ch == '+' || ch == '_' || ch == '.' {
            result.push(ch);
        } else if ch == '\u{2212}' {
            result.push('-');
            has_unicode = true;
        } else if let Some(d) = crate::builtins::unicode::unicode_decimal_digit_value(ch) {
            result.push(char::from_digit(d, 10).unwrap());
            has_unicode = true;
        } else {
            return None;
        }
    }
    if has_unicode { Some(result) } else { None }
}

fn parse_raku_int_from_str(s: &str) -> Option<Value> {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return None;
    }
    // Try normalizing Unicode digits to ASCII before parsing
    if let Some(ascii) = normalize_unicode_digits(trimmed) {
        return parse_raku_int_from_str(&ascii);
    }
    let normalized = trimmed.replace('\u{2212}', "-");
    let (sign, body) = if let Some(rest) = normalized.strip_prefix('-') {
        (-1_i32, rest)
    } else if let Some(rest) = normalized.strip_prefix('+') {
        (1_i32, rest)
    } else {
        (1_i32, normalized.as_str())
    };
    let body_no_underscores = body.replace('_', "");
    if body_no_underscores.is_empty() {
        return None;
    }
    if let Some((radix, digits)) = body_no_underscores
        .strip_prefix("0x")
        .or_else(|| body_no_underscores.strip_prefix("0X"))
        .map(|digits| (16_u32, digits))
        .or_else(|| {
            body_no_underscores
                .strip_prefix("0o")
                .or_else(|| body_no_underscores.strip_prefix("0O"))
                .map(|digits| (8_u32, digits))
        })
        .or_else(|| {
            body_no_underscores
                .strip_prefix("0b")
                .or_else(|| body_no_underscores.strip_prefix("0B"))
                .map(|digits| (2_u32, digits))
        })
        .or_else(|| {
            body_no_underscores
                .strip_prefix("0d")
                .or_else(|| body_no_underscores.strip_prefix("0D"))
                .map(|digits| (10_u32, digits))
        })
    {
        if digits.is_empty() {
            return None;
        }
        let mut n = num_bigint::BigInt::parse_bytes(digits.as_bytes(), radix)?;
        if sign < 0 {
            n = -n;
        }
        return Some(Value::from_bigint(n));
    }

    let signed_no_underscores = if sign < 0 {
        format!("-{}", body_no_underscores)
    } else {
        body_no_underscores
    };
    if let Ok(n) = signed_no_underscores.parse::<num_bigint::BigInt>() {
        return Some(Value::from_bigint(n));
    }

    if let Ok(f) = signed_no_underscores.parse::<f64>()
        && f.is_finite()
    {
        let truncated = f.trunc();
        let digits = format!("{:.0}", truncated);
        if let Ok(n) = digits.parse::<num_bigint::BigInt>() {
            return Some(Value::from_bigint(n));
        }
    }
    None
}

fn int_lsb_value(target: &Value) -> Option<Value> {
    match target {
        Value::Int(i) => {
            if *i == 0 {
                Some(Value::Nil)
            } else {
                Some(Value::Int(i.unsigned_abs().trailing_zeros() as i64))
            }
        }
        Value::BigInt(n) => {
            if n.is_zero() {
                return Some(Value::Nil);
            }
            let one = num_bigint::BigInt::from(1u8);
            let mut x = n.as_ref().abs();
            let mut pos = 0_i64;
            while (&x & &one).is_zero() {
                x >>= 1;
                pos += 1;
            }
            Some(Value::Int(pos))
        }
        _ => None,
    }
}

fn int_msb_value(target: &Value) -> Option<Value> {
    match target {
        Value::Int(i) => {
            if *i == 0 {
                return Some(Value::Nil);
            }
            if *i > 0 {
                return Some(Value::Int((63 - i.leading_zeros()) as i64));
            }
            if *i == -1 {
                return Some(Value::Int(0));
            }
            let m = i.unsigned_abs().saturating_sub(1);
            let bitlen = (64 - m.leading_zeros()) as i64;
            Some(Value::Int(bitlen))
        }
        Value::BigInt(n) => {
            if n.is_zero() {
                return Some(Value::Nil);
            }
            if n.sign() == num_bigint::Sign::Minus {
                if **n == num_bigint::BigInt::from(-1i8) {
                    return Some(Value::Int(0));
                }
                let mut x = n.as_ref().abs() - num_bigint::BigInt::from(1u8);
                let mut bitlen = 0_i64;
                while !x.is_zero() {
                    x >>= 1;
                    bitlen += 1;
                }
                return Some(Value::Int(bitlen));
            }
            let mut x = n.as_ref().clone();
            let mut msb = -1_i64;
            while !x.is_zero() {
                x >>= 1;
                msb += 1;
            }
            Some(Value::Int(msb))
        }
        _ => None,
    }
}

/// Format a single item for 0-arg `.fmt()` on lists.
/// Pairs format as "%s\t%s", other values as "%s".
fn fmt_0arg_item(item: &Value) -> String {
    match item {
        Value::Pair(k, v) => {
            runtime::format_sprintf_args("%s\t%s", &[Value::str(k.to_string()), *v.clone()])
        }
        Value::ValuePair(k, v) => runtime::format_sprintf_args("%s\t%s", &[*k.clone(), *v.clone()]),
        _ => runtime::format_sprintf("%s", Some(item)),
    }
}

// ── 0-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 0-argument method call on a Value.
/// Returns `Some(Ok(..))` / `Some(Err(..))` when handled, `None` to fall through.
pub(crate) fn native_method_0arg(
    target: &Value,
    method_sym: Symbol,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    // Scalar containers are transparent for method dispatch.
    if let Value::Scalar(inner) = target {
        return native_method_0arg(inner, method_sym);
    }

    // $!.pending returns a list of all tracked Failure values (S04 spec).
    if method == "pending" {
        let failures = crate::value::get_pending_failures();
        return Some(Ok(Value::Array(
            std::sync::Arc::new(failures),
            crate::value::ArrayKind::List,
        )));
    }

    // Nil absorber for common methods: Nil.message, Nil.payload, etc.
    // In Raku, calling most methods on Nil returns Nil.
    if matches!(target, Value::Nil)
        && matches!(
            method,
            "message" | "payload" | "backtrace" | "exception" | "handled"
        )
    {
        return Some(Ok(Value::Nil));
    }

    // For Mixin values, handle Bool/WHICH method specially, then delegate to inner.
    if let Value::Mixin(inner, mixins) = target {
        if method == "Bool"
            && let Some(bool_val) = mixins.get("Bool")
        {
            return Some(Ok(bool_val.clone()));
        }
        if (method == "Str" || method == "~")
            && let Some(str_val) = mixins.get("Str")
        {
            return Some(Ok(str_val.clone()));
        }
        if method == "WHICH"
            && let Some(allo_name) = crate::value::types::allomorph_type_name(inner, mixins)
        {
            let inner_which = match inner.as_ref() {
                Value::Int(n) => format!("Int|{}", n),
                Value::BigInt(n) => format!("Int|{}", n),
                Value::Num(n) => format!("Num|{}", n),
                Value::Rat(n, d) => format!("Rat|{}/{}", n, d),
                Value::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
                Value::Complex(r, i) => format!("Complex|{}+{}i", r, i),
                _ => format!("{:?}", inner),
            };
            let str_part = mixins
                .get("Str")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let which_str = format!("{}|{}|Str|{}", allo_name, inner_which, str_part);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("WHICH".to_string(), Value::str(which_str));
            return Some(Ok(Value::make_instance(
                crate::symbol::Symbol::intern("ValueObjAt"),
                attrs,
            )));
        }
        // For allomorphic types, string-oriented methods should use the Str part.
        if let Some(str_val) = mixins.get("Str") {
            match method {
                "comb" | "chars" | "codes" | "words" | "lines" | "chomp" | "chop" | "trim"
                | "trim-leading" | "trim-trailing" | "uc" | "lc" | "tc" | "tclc" | "fc"
                | "flip" | "samemark" | "samespace" | "uniname" | "uninames" | "unival"
                | "univals" | "uniprop" | "uniprops" | "uniparse" | "parse-names" | "NFC"
                | "NFD" | "NFKC" | "NFKD" | "encode" => {
                    return native_method_0arg(str_val, method_sym);
                }
                _ => {}
            }
        }
        return native_method_0arg(inner, method_sym);
    }
    // Cool numeric coercion: when a Str calls a numeric method, coerce to numeric first.
    // In Raku, Cool types (including Str) coerce to Numeric for numeric operations.
    if let Value::Str(s) = target {
        match method {
            "abs" | "sign" | "exp" | "log" | "log2" | "log10" | "sqrt" | "ceiling" | "floor"
            | "truncate" | "round" | "conj" | "cis" | "rand" | "sin" | "cos" | "tan" | "asin"
            | "acos" | "atan" | "sinh" | "cosh" | "tanh" | "sec" | "cosec" | "cotan" | "asec"
            | "acosec" | "acotan" | "sech" | "cosech" | "cotanh" | "asech" | "acosech"
            | "acotanh" | "atan2" | "narrow" | "polymod" | "base" | "chr" | "expmod" | "lsb"
            | "msb" | "is-int" | "i" | "Complex-i" => {
                let coerced = if let Ok(i) = s.parse::<i64>() {
                    Value::Int(i)
                } else if let Ok(f) = s.parse::<f64>() {
                    Value::Num(f)
                } else if let Some(v) = parse_raku_int_from_str(s) {
                    v
                } else {
                    return None;
                };
                return native_method_0arg(&coerced, method_sym);
            }
            _ => {}
        }
    }
    // Any.nl-out returns the default newline separator "\n"
    if method == "nl-out" {
        return Some(Ok(Value::str_from("\n")));
    }
    // Native int coercer methods (.byte(), .int8(), .uint16(), etc.)
    if runtime::native_types::is_native_int_type(method) {
        return Some(raku_repr::native_int_coerce_method(target, method));
    }
    // Uni types: override .chars, .codes, .comb to work on codepoints
    if let Value::Uni { text, .. } = target {
        match method {
            "chars" | "codes" => {
                return Some(Ok(Value::Int(text.chars().count() as i64)));
            }
            "comb" => {
                let parts: Vec<Value> = text.chars().map(|c| Value::str(c.to_string())).collect();
                return Some(Ok(Value::Seq(std::sync::Arc::new(parts))));
            }
            "Str" => {
                use unicode_normalization::UnicodeNormalization;
                return Some(Ok(Value::str(text.nfc().collect::<String>())));
            }
            "Int" | "Numeric" => {
                return Some(Ok(Value::Int(text.chars().count() as i64)));
            }
            "list" => {
                let codepoints: Vec<Value> = text.chars().map(|c| Value::Int(c as i64)).collect();
                return Some(Ok(Value::array(codepoints)));
            }
            "elems" => {
                return Some(Ok(Value::Int(text.chars().count() as i64)));
            }
            "raku" | "perl" => {
                let codepoints: Vec<String> = text
                    .chars()
                    .map(|c| format!("0x{:04X}", c as u32))
                    .collect();
                return Some(Ok(Value::str(format!(
                    "Uni.new({})",
                    codepoints.join(", ")
                ))));
            }
            "gist" => {
                let codepoints: Vec<String> =
                    text.chars().map(|c| format!("{:04X}", c as u32)).collect();
                return Some(Ok(Value::str(format!("Uni:0x<{}>", codepoints.join(" ")))));
            }
            "NFC" | "NFD" | "NFKC" | "NFKD" => {
                let normalized: String = match method {
                    "NFC" => text.nfc().collect(),
                    "NFD" => text.nfd().collect(),
                    "NFKC" => text.nfkc().collect(),
                    _ => text.nfkd().collect(),
                };
                return Some(Ok(Value::Uni {
                    form: method.to_string(),
                    text: normalized,
                }));
            }
            _ => {}
        }
    }
    // CompUnit::DependencySpecification methods
    if let Value::CompUnitDepSpec { short_name } = target {
        return match method {
            "short-name" => Some(Ok(Value::str(short_name.resolve()))),
            "version-matcher" => Some(Ok(Value::Bool(true))),
            "auth-matcher" => Some(Ok(Value::Bool(true))),
            "api-matcher" => Some(Ok(Value::Bool(true))),
            "Str" | "gist" => Some(Ok(Value::str(short_name.resolve()))),
            _ => None,
        };
    }
    // Capture methods
    if let Value::Capture { positional, named } = target
        && let result @ Some(_) = dispatch_capture(positional, named, method)
    {
        return result;
    }
    // Try core string/numeric/array methods first
    if let result @ Some(_) = dispatch_core(target, method) {
        return result;
    }
    // Then collection methods (keys, values, kv, pairs, etc.)
    if let result @ Some(_) = collection::dispatch(target, method) {
        return result;
    }
    // Then type coercion and specialized methods
    coercion::dispatch(target, method)
}

fn dispatch_capture(
    positional: &[Value],
    named: &std::collections::HashMap<String, Value>,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    match method {
        "hash" | "Hash" => {
            let mut map = std::collections::HashMap::new();
            for (k, v) in named {
                map.insert(k.clone(), v.clone());
            }
            Some(Ok(Value::hash(map)))
        }
        "list" => Some(Ok(Value::array(positional.to_vec()))),
        "elems" => Some(Ok(Value::Int(positional.len() as i64))),
        "is-lazy" => Some(Ok(Value::Bool(false))),
        "keys" => Some(Ok(Value::array(
            named.keys().map(|k| Value::str(k.clone())).collect(),
        ))),
        "values" => Some(Ok(Value::array(named.values().cloned().collect()))),
        "pairs" => Some(Ok(Value::array(
            named
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .collect(),
        ))),
        "raku" | "perl" => {
            let mut parts = Vec::new();
            for v in positional {
                match v {
                    Value::Pair(k, val) => {
                        parts.push(format!(
                            "{} => {}",
                            raku_repr::raku_value(&Value::str(k.clone())),
                            raku_repr::raku_value(val)
                        ));
                    }
                    Value::ValuePair(k, val) => {
                        parts.push(format!(
                            "{} => {}",
                            raku_repr::raku_value(k),
                            raku_repr::raku_value(val)
                        ));
                    }
                    _ => parts.push(raku_repr::raku_value(v)),
                }
            }
            let mut named_keys: Vec<&String> = named.keys().collect();
            named_keys.sort();
            for k in named_keys {
                let v = &named[k];
                if let Value::Bool(true) = v {
                    parts.push(format!(":{}", k));
                } else if let Value::Bool(false) = v {
                    parts.push(format!(":!{}", k));
                } else {
                    parts.push(format!(":{}({})", k, raku_repr::raku_value(v)));
                }
            }
            Some(Ok(Value::str(format!("\\({})", parts.join(", ")))))
        }
        "gist" | "Str" => {
            let target = Value::Capture {
                positional: positional.to_vec(),
                named: named.clone(),
            };
            Some(Ok(Value::str(target.to_string_value())))
        }
        "Bool" => Some(Ok(Value::Bool(!positional.is_empty() || !named.is_empty()))),
        "WHAT" => Some(Ok(Value::Package(Symbol::intern("Capture")))),
        "flat" => {
            let cap = Value::Capture {
                positional: positional.to_vec(),
                named: named.clone(),
            };
            Some(Ok(Value::Seq(Arc::new(vec![cap]))))
        }
        "Seq" | "List" => {
            let cap = Value::Capture {
                positional: positional.to_vec(),
                named: named.clone(),
            };
            Some(Ok(Value::Seq(Arc::new(vec![cap]))))
        }
        _ => None,
    }
}

/// Create a X::Cannot::Lazy Failure for .elems on an infinite range.
fn range_elems_lazy_failure(action: &str) -> Option<Result<Value, RuntimeError>> {
    let mut ex_attrs = std::collections::HashMap::new();
    ex_attrs.insert(
        "message".to_string(),
        Value::str(format!("Cannot .{} a lazy list", action)),
    );
    ex_attrs.insert("action".to_string(), Value::str(format!(".{}", action)));
    let exception = Value::make_instance(Symbol::intern("X::Cannot::Lazy"), ex_attrs);
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), exception);
    failure_attrs.insert("handled".to_string(), Value::Bool(false));
    Some(Ok(Value::make_instance(
        Symbol::intern("Failure"),
        failure_attrs,
    )))
}

fn is_infinite_endpoint(v: &Value) -> bool {
    match v {
        Value::Whatever | Value::HyperWhatever => true,
        Value::Num(n) => n.is_infinite(),
        Value::Rat(n, d) => *d == 0 && *n != 0,
        Value::FatRat(n, d) => *d == 0 && *n != 0,
        other => {
            let n = other.to_f64();
            n.is_infinite()
        }
    }
}

fn is_infinite_range(value: &Value) -> bool {
    match value {
        Value::Range(start, end)
        | Value::RangeExcl(start, end)
        | Value::RangeExclStart(start, end)
        | Value::RangeExclBoth(start, end) => *end == i64::MAX || *start == i64::MIN,
        Value::GenericRange { start, end, .. } => {
            is_infinite_endpoint(start) || is_infinite_endpoint(end)
        }
        _ => false,
    }
}

pub(crate) fn is_value_lazy(value: &Value) -> bool {
    matches!(value, Value::LazyList(_))
        || matches!(value, Value::Array(_, kind) if kind.is_lazy())
        || is_infinite_range(value)
}

fn flatten_deep_value(value: &Value, out: &mut Vec<Value>, flatten_arrays: bool) {
    match value {
        Value::Array(items, kind) if *kind == ArrayKind::List || flatten_arrays => {
            for item in items.iter() {
                flatten_deep_value(item, out, flatten_arrays);
            }
        }
        Value::Seq(items) | Value::Slip(items) => {
            for item in items.iter() {
                flatten_deep_value(item, out, flatten_arrays);
            }
        }
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => out.extend(crate::runtime::utils::value_to_list(value)),
        other => out.push(other.clone()),
    }
}

/// Format a range endpoint for display, converting i64::MAX to Inf and i64::MIN to -Inf.
fn range_endpoint_display(v: i64) -> String {
    if v == i64::MAX {
        "Inf".to_string()
    } else if v == i64::MIN {
        "-Inf".to_string()
    } else {
        v.to_string()
    }
}

/// Return the gist (compact display) representation of a Range value.
fn range_gist_string(value: &Value) -> String {
    match value {
        Value::Range(a, b) => {
            format!(
                "{}..{}",
                range_endpoint_display(*a),
                range_endpoint_display(*b)
            )
        }
        Value::RangeExcl(a, b) => {
            if *a == 0 {
                format!("^{}", range_endpoint_display(*b))
            } else {
                format!(
                    "{}..^{}",
                    range_endpoint_display(*a),
                    range_endpoint_display(*b)
                )
            }
        }
        Value::RangeExclStart(a, b) => {
            format!(
                "{}^..{}",
                range_endpoint_display(*a),
                range_endpoint_display(*b)
            )
        }
        Value::RangeExclBoth(a, b) => {
            format!(
                "{}^..^{}",
                range_endpoint_display(*a),
                range_endpoint_display(*b)
            )
        }
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let start_sep = if *excl_start { "^.." } else { ".." };
            let end_sep = if *excl_end { "^" } else { "" };
            let endpoint_str = |v: &Value, is_end: bool| -> String {
                match v {
                    Value::Whatever | Value::HyperWhatever => {
                        if is_end {
                            "Inf".to_string()
                        } else {
                            "-Inf".to_string()
                        }
                    }
                    Value::Rat(n, d) => {
                        if *d == 0 {
                            if *n == 0 {
                                "NaN".to_string()
                            } else if *n > 0 {
                                "Inf".to_string()
                            } else {
                                "-Inf".to_string()
                            }
                        } else {
                            let val = *n as f64 / *d as f64;
                            if val.fract() == 0.0 {
                                format!("{:.1}", val)
                            } else {
                                format!("{}", val)
                            }
                        }
                    }
                    _ => v.to_string_value(),
                }
            };
            let start_str = endpoint_str(start.as_ref(), false);
            let end_str = endpoint_str(end.as_ref(), true);
            // Special case: 0..^N displays as ^N
            if !*excl_start && *excl_end {
                let is_zero = match start.as_ref() {
                    Value::Int(0) => true,
                    Value::Num(f) if *f == 0.0 => true,
                    Value::Rat(0, _) => true,
                    _ => false,
                };
                if is_zero {
                    return format!("^{}", end_str);
                }
            }
            format!("{}{}{}{}", start_str, start_sep, end_sep, end_str)
        }
        _ => value.to_string_value(),
    }
}

fn gist_array_wrap(inner: &str, kind: ArrayKind) -> String {
    match kind {
        ArrayKind::Array | ArrayKind::Shaped | ArrayKind::Lazy => format!("[{}]", inner),
        ArrayKind::List => format!("({})", inner),
        ArrayKind::ItemArray => format!("$[{}]", inner),
        ArrayKind::ItemList => format!("$({})", inner),
    }
}

/// Format a numeric value for Instant/Duration .raku output.
/// Always includes a decimal point (e.g. "42.0", "-400.2").
fn format_temporal_num(f: f64) -> String {
    if f.is_nan() {
        return "NaN".to_string();
    }
    if f.is_infinite() {
        return if f > 0.0 {
            "Inf".to_string()
        } else {
            "-Inf".to_string()
        };
    }
    // For values outside the safe i64 Rat range, emit scientific notation
    // so that Str -> Num literal round-trips through the parser instead of
    // overflowing the Rat literal parser.
    if f.is_finite() && f.abs() >= 1e18 {
        return format!("{:e}", f);
    }
    let s = format!("{}", f);
    if s.contains('.') {
        s
    } else {
        format!("{}.0", s)
    }
}

/// Re-export raku_value for backward compatibility.
pub use raku_repr::raku_value;

/// Re-export complex_trig for external use.
pub(crate) use complex_math::complex_trig;

/// Unicode case folding for `.fc` and `fc()`.
pub(crate) fn unicode_foldcase(s: &str) -> String {
    let mut lowered = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\u{00DF}' | '\u{1E9E}' => lowered.push_str("ss"),
            _ => {
                for lc in c.to_lowercase() {
                    lowered.push(lc);
                }
            }
        }
    }

    let mut expanded = String::with_capacity(lowered.len());
    for c in lowered.nfkd() {
        if c == '\u{0345}' {
            expanded.push('\u{03B9}');
        } else {
            expanded.push(c);
        }
    }
    expanded.nfc().collect()
}

fn dispatch_core(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    fn has_date_attrs(attributes: &std::sync::Arc<crate::value::InstanceAttrs>) -> bool {
        attributes.contains_key("year")
            && attributes.contains_key("month")
            && attributes.contains_key("day")
    }
    fn has_datetime_attrs(attributes: &std::sync::Arc<crate::value::InstanceAttrs>) -> bool {
        has_date_attrs(attributes)
            && attributes.contains_key("hour")
            && attributes.contains_key("minute")
            && attributes.contains_key("second")
            && attributes.contains_key("timezone")
    }

    // Date/DateTime 0-arg methods
    match target {
        Value::Instance { attributes, .. } if has_datetime_attrs(attributes) => {
            if let Some(result) = temporal_dispatch::datetime_method_0arg(attributes, method) {
                return Some(result);
            }
        }
        Value::Instance { attributes, .. } if has_date_attrs(attributes) => {
            if let Some(result) = temporal_dispatch::date_method_0arg(attributes, method) {
                return Some(result);
            }
        }
        _ => {}
    }

    // Instant.Instant returns self (identity coercion)
    if method == "Instant" {
        match target {
            Value::Instance { class_name, .. } if class_name == "Instant" => {
                return Some(Ok(target.clone()));
            }
            Value::Package(name) if name == "Instant" => {
                return Some(Ok(target.clone()));
            }
            _ => {}
        }
    }

    // Buf/Blob.Str throws X::Buf::AsStr
    if (method == "Str" || method == "Stringy")
        && let Value::Instance { class_name, .. } = target
        && crate::vm::VM::is_buf_value(target)
    {
        let cn = class_name.resolve();
        let mut err = RuntimeError::new(format!(
            "Cannot use a {cn} as a Str. You can use .decode to convert to Str.",
        ));
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("method".to_string(), Value::str(method.to_string()));
        attrs.insert("payload".to_string(), target.clone());
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Buf::AsStr"),
            attrs,
        )));
        return Some(Err(err));
    }

    // Buf/Blob .values and .list return the byte values as integers
    if (method == "values" || method == "list")
        && let Value::Instance { attributes, .. } = target
        && crate::vm::VM::is_buf_value(target)
    {
        if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
            return Some(Ok(Value::array(bytes.to_vec())));
        }
        return Some(Ok(Value::array(Vec::new())));
    }

    // CX::Warn methods: message, resume
    if let Value::Instance {
        class_name,
        attributes,
        ..
    } = target
        && class_name == "CX::Warn"
    {
        match method {
            "message" => {
                return Some(Ok(attributes
                    .get("message")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            "resume" => return Some(Err(RuntimeError::resume_signal())),
            "gist" | "Str" => {
                return Some(Ok(attributes
                    .get("message")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            _ => {}
        }
    }

    // Exception/X:: methods: gist, Str, message
    if let Value::Instance {
        class_name,
        attributes,
        ..
    } = target
    {
        let cn = class_name.resolve();
        if cn == "Exception" || cn.starts_with("X::") || cn.starts_with("CX::") {
            match method {
                "gist" => {
                    let bt = attributes
                        .get("backtrace")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let append_bt = |msg: String| -> String {
                        if bt.is_empty() {
                            msg
                        } else {
                            format!("{}\n{}", msg, bt)
                        }
                    };
                    if let Some(msg) = attributes.get("message") {
                        let msg_str = msg.to_string_value();
                        if !msg_str.is_empty() {
                            return Some(Ok(Value::str(append_bt(msg_str))));
                        }
                    }
                    if cn == "Exception" {
                        return Some(Ok(Value::str(append_bt(
                            "Unthrown Exception with no message".to_string(),
                        ))));
                    }
                    if cn == "X::AdHoc" {
                        if let Some(payload) = attributes.get("payload") {
                            let payload_str = payload.to_string_value();
                            if !payload_str.is_empty() {
                                return Some(Ok(Value::str(append_bt(payload_str))));
                            }
                        }
                        return Some(Ok(Value::str(append_bt("Unexplained error".to_string()))));
                    }
                    return Some(Ok(Value::str(append_bt(format!("{} with no message", cn)))));
                }
                "Str" => {
                    if let Some(msg) = attributes.get("message") {
                        let msg_str = msg.to_string_value();
                        if !msg_str.is_empty() {
                            return Some(Ok(Value::str(msg_str)));
                        }
                    }
                    if cn == "Exception" {
                        return Some(Ok(Value::str(format!("Something went wrong in ({})", cn))));
                    }
                    return Some(Ok(Value::str(format!("{} with no message", cn))));
                }
                "message" => {
                    if let Some(msg) = attributes.get("message") {
                        return Some(Ok(msg.clone()));
                    }
                    if cn == "X::AdHoc"
                        && let Some(payload) = attributes.get("payload")
                    {
                        return Some(Ok(payload.clone()));
                    }
                    return Some(Ok(Value::str(String::new())));
                }
                _ => {}
            }
        }
    }

    // .resume on exception objects
    if method == "resume"
        && let Value::Instance { class_name, .. } = target
    {
        let cn = class_name.resolve();
        if cn == "Exception" || cn.starts_with("X::") || cn == "Failure" || cn == "CX::Warn" {
            return Some(Err(RuntimeError::resume_signal()));
        }
    }

    // .throw on exception objects
    if method == "throw"
        && let Value::Instance {
            class_name,
            attributes,
            ..
        } = target
    {
        let cn = class_name.resolve();
        // Only the core exception classes use the fast path. For CX::* we
        // fall through to the slow path, which can inspect the class's
        // composed roles (e.g. X::Control) to decide whether the throw
        // should raise a control exception.
        if cn == "Exception" || cn.starts_with("X::") || cn == "Failure" {
            let msg = attributes
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| target.to_string_value());
            let mut err = RuntimeError::new(&msg);
            err.exception = Some(Box::new(target.clone()));
            return Some(Err(err));
        }
    }

    // Array of Match objects: .to/.from/.ast
    if let Value::Array(arr, _) = target {
        match method {
            "to" | "pos" => {
                if let Some(last) = arr.last() {
                    return native_method_0arg(last, Symbol::intern(method));
                }
                return Some(Ok(Value::Int(0)));
            }
            "from" => {
                if let Some(first) = arr.first() {
                    return native_method_0arg(first, Symbol::intern(method));
                }
                return Some(Ok(Value::Int(0)));
            }
            "ast" => {
                if let Some(last) = arr.last() {
                    return native_method_0arg(last, Symbol::intern("ast"));
                }
                return Some(Ok(Value::Nil));
            }
            _ => {}
        }
    }

    // Match object methods
    if let Value::Instance {
        class_name,
        attributes,
        ..
    } = target
        && class_name == "Match"
    {
        match method {
            "from" => {
                return Some(Ok(attributes.get("from").cloned().unwrap_or(Value::Int(0))));
            }
            "to" | "pos" => {
                return Some(Ok(attributes.get("to").cloned().unwrap_or(Value::Int(0))));
            }
            "gist" => {
                let matched = target.to_string_value();
                let mut gist = format!("\u{FF62}{}\u{FF63}", matched);
                if let Some(Value::Hash(named)) = attributes.get("named")
                    && !named.is_empty()
                {
                    let mut keys: Vec<&String> = named.keys().collect();
                    keys.sort();
                    for key in keys {
                        if let Some(value) = named.get(key) {
                            gist.push_str(&format!(
                                "\n {} => \u{FF62}{}\u{FF63}",
                                key,
                                value.to_string_value()
                            ));
                        }
                    }
                }
                return Some(Ok(Value::str(gist)));
            }
            "Str" => {
                return Some(Ok(attributes
                    .get("str")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            "Bool" => return Some(Ok(Value::Bool(true))),
            "orig" => {
                return Some(Ok(attributes
                    .get("orig")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            "raku" | "perl" => {
                return Some(Ok(Value::str(match_helpers::match_raku_repr(attributes))));
            }
            "list" | "Array" => {
                return Some(Ok(attributes
                    .get("list")
                    .cloned()
                    .unwrap_or_else(|| Value::array(Vec::new()))));
            }
            "hash" | "Hash" => {
                return Some(Ok(attributes
                    .get("named")
                    .cloned()
                    .unwrap_or_else(|| Value::hash(HashMap::new()))));
            }
            "keys" => {
                let mut keys = Vec::new();
                if let Some(Value::Array(list, _)) = attributes.get("list") {
                    for i in 0..list.len() {
                        keys.push(Value::Int(i as i64));
                    }
                }
                if let Some(Value::Hash(named)) = attributes.get("named") {
                    let mut sorted: Vec<&String> = named.keys().collect();
                    sorted.sort();
                    for k in sorted {
                        keys.push(Value::str(k.clone()));
                    }
                }
                return Some(Ok(Value::array(keys)));
            }
            "values" => {
                let mut vals = Vec::new();
                if let Some(Value::Array(list, _)) = attributes.get("list") {
                    vals.extend(list.iter().cloned());
                }
                if let Some(Value::Hash(named)) = attributes.get("named") {
                    let mut sorted: Vec<(&String, &Value)> = named.iter().collect();
                    sorted.sort_by_key(|(k, _)| (*k).clone());
                    for (_, v) in sorted {
                        vals.push(v.clone());
                    }
                }
                return Some(Ok(Value::array(vals)));
            }
            "pairs" => {
                let mut pairs = Vec::new();
                if let Some(Value::Array(list, _)) = attributes.get("list") {
                    for (i, v) in list.iter().enumerate() {
                        pairs.push(Value::Pair(i.to_string(), Box::new(v.clone())));
                    }
                }
                if let Some(Value::Hash(named)) = attributes.get("named") {
                    let mut sorted: Vec<(&String, &Value)> = named.iter().collect();
                    sorted.sort_by_key(|(k, _)| (*k).clone());
                    for (k, v) in sorted {
                        pairs.push(Value::Pair(k.clone(), Box::new(v.clone())));
                    }
                }
                return Some(Ok(Value::array(pairs)));
            }
            "kv" => {
                let mut kv = Vec::new();
                if let Some(Value::Array(list, _)) = attributes.get("list") {
                    for (i, v) in list.iter().enumerate() {
                        kv.push(Value::Int(i as i64));
                        kv.push(v.clone());
                    }
                }
                if let Some(Value::Hash(named)) = attributes.get("named") {
                    let mut sorted: Vec<(&String, &Value)> = named.iter().collect();
                    sorted.sort_by_key(|(k, _)| (*k).clone());
                    for (k, v) in sorted {
                        kv.push(Value::str(k.clone()));
                        kv.push(v.clone());
                    }
                }
                return Some(Ok(Value::array(kv)));
            }
            "elems" => {
                let count = match attributes.get("list") {
                    Some(Value::Array(list, _)) => list.len(),
                    _ => 0,
                };
                return Some(Ok(Value::Int(count as i64)));
            }
            "ast" | "made" => {
                return Some(Ok(attributes.get("ast").cloned().unwrap_or(Value::Nil)));
            }
            "prematch" => {
                if let Some(orig_val) = attributes.get("orig") {
                    let orig = orig_val.to_string_value();
                    let from = match attributes.get("from") {
                        Some(Value::Int(n)) => *n as usize,
                        _ => 0,
                    };
                    let chars: Vec<char> = orig.chars().collect();
                    let pre: String = chars[..from.min(chars.len())].iter().collect();
                    return Some(Ok(Value::str(pre)));
                }
                return Some(Ok(Value::str(String::new())));
            }
            "postmatch" => {
                if let Some(orig_val) = attributes.get("orig") {
                    let orig = orig_val.to_string_value();
                    let to = match attributes.get("to") {
                        Some(Value::Int(n)) => *n as usize,
                        _ => 0,
                    };
                    let chars: Vec<char> = orig.chars().collect();
                    let post: String = chars[to.min(chars.len())..].iter().collect();
                    return Some(Ok(Value::str(post)));
                }
                return Some(Ok(Value::str(String::new())));
            }
            "actions" => {
                return Some(Ok(attributes.get("actions").cloned().unwrap_or(Value::Nil)));
            }
            "caps" => {
                return Some(Ok(match_helpers::match_caps(attributes)));
            }
            "chunks" => {
                return Some(Ok(match_helpers::match_chunks(attributes)));
            }
            _ => {
                let str_val = Value::str(target.to_string_value());
                return native_method_0arg(&str_val, Symbol::intern(method));
            }
        }
    }

    // Numeric type object .Range methods
    if let Value::Package(name) = target
        && method == "Range"
        && matches!(
            name.resolve().as_str(),
            "Real" | "Num" | "Rational" | "Rat" | "FatRat" | "BigRat"
        )
    {
        return Some(Ok(Value::GenericRange {
            start: Arc::new(Value::Num(f64::NEG_INFINITY)),
            end: Arc::new(Value::Num(f64::INFINITY)),
            excl_start: false,
            excl_end: false,
        }));
    }
    if let Value::Package(name) = target
        && name.resolve() == "Int"
        && method == "Range"
    {
        return Some(Ok(Value::GenericRange {
            start: Arc::new(Value::Num(f64::NEG_INFINITY)),
            end: Arc::new(Value::Num(f64::INFINITY)),
            excl_start: true,
            excl_end: true,
        }));
    }
    if let Value::Package(name) = target
        && crate::runtime::native_types::is_native_int_type(&name.resolve())
        && method == "Range"
        && let Some((min_big, max_big)) =
            crate::runtime::native_types::native_int_bounds(&name.resolve())
    {
        let min_i64 = min_big.to_i64();
        let max_i64 = max_big.to_i64();
        if let (Some(min_v), Some(max_v)) = (min_i64, max_i64) {
            return Some(Ok(Value::Range(min_v, max_v)));
        } else {
            let min_val = min_i64
                .map(Value::Int)
                .unwrap_or_else(|| Value::bigint(min_big));
            let max_val = max_i64
                .map(Value::Int)
                .unwrap_or_else(|| Value::bigint(max_big));
            return Some(Ok(Value::GenericRange {
                start: Arc::new(min_val),
                end: Arc::new(max_val),
                excl_start: false,
                excl_end: false,
            }));
        }
    }
    // .int-bounds on Range values
    // Note: i64 Range variants always have concrete integer bounds (even when
    // i64::MIN/MAX are used as sentinel for -Inf/Inf), so we always return them.
    // Only GenericRange can have true Inf/NaN endpoints that need to throw.
    if method == "int-bounds" {
        match target {
            Value::Range(start, end) => {
                return Some(Ok(Value::array(vec![Value::Int(*start), Value::Int(*end)])));
            }
            Value::RangeExcl(start, end) => {
                return Some(Ok(Value::array(vec![
                    Value::Int(*start),
                    Value::Int(*end - 1),
                ])));
            }
            Value::RangeExclStart(start, end) => {
                return Some(Ok(Value::array(vec![
                    Value::Int(*start + 1),
                    Value::Int(*end),
                ])));
            }
            Value::RangeExclBoth(start, end) => {
                return Some(Ok(Value::array(vec![
                    Value::Int(*start + 1),
                    Value::Int(*end - 1),
                ])));
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // Check if endpoints contain Inf, -Inf, or NaN — these cannot
                // have integer bounds.
                let has_non_int_endpoint = |v: &Value| -> bool {
                    match v {
                        Value::Num(f) => f.is_infinite() || f.is_nan(),
                        Value::Str(_) => true,
                        _ => false,
                    }
                };
                if has_non_int_endpoint(start.as_ref()) || has_non_int_endpoint(end.as_ref()) {
                    let range_repr = crate::runtime::utils::gist_value(target);
                    return Some(Err(crate::value::RuntimeError::new(format!(
                        "Cannot determine integer bounds of {range_repr}"
                    ))));
                }
                let s = if *excl_start {
                    match start.as_ref() {
                        Value::Int(n) => Value::Int(n + 1),
                        Value::BigInt(n) => Value::bigint(n.as_ref() + 1),
                        Value::Rat(n, d) => {
                            Value::Int(((*n as f64 / *d as f64).floor() as i64) + 1)
                        }
                        other => Value::Int(other.to_f64() as i64 + 1),
                    }
                } else {
                    match start.as_ref() {
                        Value::Int(_) | Value::BigInt(_) => start.as_ref().clone(),
                        Value::Rat(n, d) => {
                            let f = *n as f64 / *d as f64;
                            Value::Int(f.ceil() as i64)
                        }
                        other => Value::Int(other.to_f64().ceil() as i64),
                    }
                };
                let e = if *excl_end {
                    match end.as_ref() {
                        Value::Int(n) => Value::Int(n - 1),
                        Value::BigInt(n) => Value::bigint(n.as_ref() - 1),
                        Value::Rat(n, d) => Value::Int(((*n as f64 / *d as f64).ceil() as i64) - 1),
                        other => Value::Int(other.to_f64() as i64 - 1),
                    }
                } else {
                    match end.as_ref() {
                        Value::Int(_) | Value::BigInt(_) => end.as_ref().clone(),
                        Value::Rat(n, d) => {
                            let f = *n as f64 / *d as f64;
                            Value::Int(f.floor() as i64)
                        }
                        other => Value::Int(other.to_f64().floor() as i64),
                    }
                };
                return Some(Ok(Value::array(vec![s, e])));
            }
            _ => {}
        }
    }
    // Kernel type object methods
    if let Value::Package(name) = target
        && name == "Kernel"
        && method == "endian"
    {
        return Some(Ok(Value::Enum {
            enum_type: Symbol::intern("Endian"),
            key: Symbol::intern(if cfg!(target_endian = "little") {
                "LittleEndian"
            } else {
                "BigEndian"
            }),
            value: EnumValue::Int(if cfg!(target_endian = "little") { 1 } else { 2 }),
            index: if cfg!(target_endian = "little") { 1 } else { 2 },
        }));
    }

    // Delegate to sub-dispatch functions.
    // Each returns Option<Option<Result<..>>> where:
    //   None = method not handled, try next
    //   Some(inner) = method matched, return inner
    macro_rules! try_dispatch {
        ($module:ident) => {
            if let Some(result) = $module::dispatch(target, method) {
                return result;
            }
        };
    }

    try_dispatch!(dispatch_core_coerce);
    try_dispatch!(dispatch_core_unicode);
    try_dispatch!(dispatch_core_numeric);
    try_dispatch!(dispatch_core_list);
    try_dispatch!(dispatch_core_str);
    try_dispatch!(dispatch_core_repr);
    try_dispatch!(dispatch_core_range);
    try_dispatch!(dispatch_core_math);

    None
}
