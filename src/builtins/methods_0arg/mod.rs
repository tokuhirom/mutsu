#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, RuntimeError, Value, make_big_rat, make_rat};
use num_traits::{Signed, ToPrimitive, Zero};
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

use super::rng::builtin_rand;

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
use super::unicode::titlecase_string;

pub(crate) mod coercion;
pub(crate) mod collection;
pub(crate) mod temporal;
pub(crate) mod temporal_dispatch;

use std::collections::HashMap;

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

fn parse_raku_int_from_str(s: &str) -> Option<Value> {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return None;
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

/// Produce Raku-compatible `.raku` representation for a Match object.
fn match_raku_repr(attributes: &HashMap<String, Value>) -> String {
    let orig = attributes
        .get("orig")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let from = match attributes.get("from") {
        Some(Value::Int(n)) => *n,
        _ => 0,
    };
    let to = match attributes.get("to") {
        Some(Value::Int(n)) => *n,
        _ => 0,
    };

    let escaped_orig = orig
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\t', "\\t")
        .replace('\r', "\\r")
        .replace('\0', "\\0");

    let mut parts = vec![
        format!(":orig(\"{}\")", escaped_orig),
        format!(":from({})", from),
        format!(":pos({})", to),
    ];

    // Positional captures (:list)
    if let Some(Value::Array(items, ..)) = attributes.get("list")
        && !items.is_empty()
    {
        let items_raku: Vec<String> = items.iter().map(value_raku_repr).collect();
        let trailing = if items.len() == 1 { "," } else { "" };
        parts.push(format!(":list(({}{})", items_raku.join(", "), trailing));
    }

    // Named captures (:hash)
    if let Some(Value::Hash(map, ..)) = attributes.get("named")
        && !map.is_empty()
    {
        let mut pairs: Vec<String> = map
            .iter()
            .map(|(k, v)| format!("(:{}({}))", k, value_raku_repr(v)))
            .collect();
        pairs.sort(); // Deterministic output
        parts.push(format!(":hash(Map.new({}))", pairs.join(", ")));
    }

    format!("Match.new({})", parts.join(", "))
}

/// Produce `.raku` representation for a value, recursing into Match objects.
fn value_raku_repr(val: &Value) -> String {
    match val {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Match" => match_raku_repr(attributes),
        Value::Array(items, ..) => {
            let items_raku: Vec<String> = items.iter().map(value_raku_repr).collect();
            format!("[{}]", items_raku.join(", "))
        }
        other => {
            // Use the existing raku_value helper for non-Match values
            raku_value(other)
        }
    }
}

/// Extract the `from` position from a Match object value.
fn match_value_from(val: &Value) -> i64 {
    if let Value::Instance { attributes, .. } = val
        && let Some(Value::Int(n)) = attributes.get("from")
    {
        return *n;
    }
    0
}

/// Collect all captures from a Match object sorted by position.
/// Returns a list of Pair(key, match_value) where key is Int for positional, Str for named.
fn match_caps(attributes: &HashMap<String, Value>) -> Value {
    let mut pairs: Vec<(i64, Value)> = Vec::new();

    // Collect named capture positions to filter out shadowed positional captures
    let mut named_positions: Vec<(i64, i64)> = Vec::new();
    if let Some(Value::Hash(named, ..)) = attributes.get("named") {
        for (_key, val) in named.iter() {
            named_positions.push((match_value_from(val), match_value_to(val)));
        }
    }

    // Collect positional captures, skipping those shadowed by named captures
    if let Some(Value::Array(items, ..)) = attributes.get("list") {
        for (i, val) in items.iter().enumerate() {
            let from = match_value_from(val);
            let to = match_value_to(val);
            let shadowed = named_positions
                .iter()
                .any(|(nf, nt)| *nf == from && *nt == to);
            if !shadowed {
                pairs.push((from, Value::Pair(i.to_string(), Box::new(val.clone()))));
            }
        }
    }

    // Collect named captures
    if let Some(Value::Hash(named, ..)) = attributes.get("named") {
        for (key, val) in named.iter() {
            let from = match_value_from(val);
            pairs.push((from, Value::Pair(key.clone(), Box::new(val.clone()))));
        }
    }

    // Sort by position
    pairs.sort_by_key(|(from, _)| *from);

    Value::array(pairs.into_iter().map(|(_, pair)| pair).collect())
}

/// Like `.caps` but also includes non-captured text between captures as `~ => text` pairs.
fn match_chunks(attributes: &HashMap<String, Value>) -> Value {
    // Collect named capture positions to filter out shadowed positional captures
    let mut named_positions: Vec<(i64, i64)> = Vec::new();
    if let Some(Value::Hash(named, ..)) = attributes.get("named") {
        for (_key, val) in named.iter() {
            named_positions.push((match_value_from(val), match_value_to(val)));
        }
    }

    // Collect all captures with their from/to positions
    let mut captures: Vec<(i64, i64, Value)> = Vec::new();

    if let Some(Value::Array(items, ..)) = attributes.get("list") {
        for (i, val) in items.iter().enumerate() {
            let from = match_value_from(val);
            let to = match_value_to(val);
            let shadowed = named_positions
                .iter()
                .any(|(nf, nt)| *nf == from && *nt == to);
            if !shadowed {
                captures.push((from, to, Value::Pair(i.to_string(), Box::new(val.clone()))));
            }
        }
    }

    if let Some(Value::Hash(named, ..)) = attributes.get("named") {
        for (key, val) in named.iter() {
            let from = match_value_from(val);
            let to = match_value_to(val);
            captures.push((from, to, Value::Pair(key.clone(), Box::new(val.clone()))));
        }
    }

    // Sort by position
    captures.sort_by_key(|(from, _, _)| *from);

    let match_from = match attributes.get("from") {
        Some(Value::Int(n)) => *n,
        _ => 0,
    };
    let match_to = match attributes.get("to") {
        Some(Value::Int(n)) => *n,
        _ => 0,
    };
    let orig = attributes
        .get("orig")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let orig_chars: Vec<char> = orig.chars().collect();

    let mut result: Vec<Value> = Vec::new();
    let mut pos = match_from;

    for (from, to, pair) in captures {
        if from > pos {
            // Insert non-captured text
            let start = pos as usize;
            let end = from as usize;
            let text: String = orig_chars
                .get(start..end)
                .map(|s| s.iter().collect())
                .unwrap_or_default();
            result.push(Value::Pair("~".to_string(), Box::new(Value::str(text))));
        }
        result.push(pair);
        pos = to;
    }

    // Trailing non-captured text
    if pos < match_to {
        let start = pos as usize;
        let end = match_to as usize;
        let text: String = orig_chars
            .get(start..end)
            .map(|s| s.iter().collect())
            .unwrap_or_default();
        result.push(Value::Pair("~".to_string(), Box::new(Value::str(text))));
    }

    Value::array(result)
}

/// Extract the `to` position from a Match object value.
fn match_value_to(val: &Value) -> i64 {
    if let Value::Instance { attributes, .. } = val
        && let Some(Value::Int(n)) = attributes.get("to")
    {
        return *n;
    }
    0
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
                crate::symbol::Symbol::intern("ObjAt"),
                attrs,
            )));
        }
        return native_method_0arg(inner, method_sym);
    }
    // Native int coercer methods (.byte(), .int8(), .uint16(), etc.)
    if runtime::native_types::is_native_int_type(method) {
        return Some(native_int_coerce_method(target, method));
    }
    // Uni types: override .chars, .codes, .comb to work on codepoints
    if let Value::Uni { text, .. } = target {
        match method {
            "chars" | "codes" => {
                return Some(Ok(Value::Int(text.chars().count() as i64)));
            }
            "comb" => {
                let parts: Vec<Value> = text.chars().map(|c| Value::str(c.to_string())).collect();
                return Some(Ok(Value::array(parts)));
            }
            "Str" => return Some(Ok(Value::str(text.clone()))),
            "list" => {
                let codepoints: Vec<Value> = text.chars().map(|c| Value::Int(c as i64)).collect();
                return Some(Ok(Value::array(codepoints)));
            }
            "elems" => {
                return Some(Ok(Value::Int(text.chars().count() as i64)));
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
    if let Value::Capture { positional, named } = target {
        return dispatch_capture(positional, named, method);
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
        "hash" => {
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
                parts.push(raku_value(v));
            }
            for (k, v) in named {
                if let Value::Bool(true) = v {
                    parts.push(format!(":{}", k));
                } else if let Value::Bool(false) = v {
                    parts.push(format!(":!{}", k));
                } else {
                    parts.push(format!(":{}({})", k, raku_value(v)));
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
            // $(expr) produces a Capture representing an itemized container.
            // .flat on it should return itself as opaque (don't descend).
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

fn is_value_lazy(value: &Value) -> bool {
    matches!(value, Value::LazyList(_)) || is_infinite_range(value)
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

/// Helper for .raku representation of a value
fn is_self_array_ref_marker(v: &Value) -> bool {
    matches!(v, Value::Pair(name, _) if name == "__mutsu_self_array_ref")
}

/// Return the gist (compact display) representation of a Range value.
/// Unlike .Str which iterates and joins with spaces, .gist shows range notation (e.g. "1..5").
fn range_gist_string(value: &Value) -> String {
    match value {
        Value::Range(a, b) => format!("{}..{}", a, b),
        Value::RangeExcl(a, b) => format!("{}..^{}", a, b),
        Value::RangeExclStart(a, b) => format!("^{}..{}", a, b),
        Value::RangeExclBoth(a, b) => format!("^{}..^{}", a, b),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let prefix = if *excl_start { "^" } else { "" };
            let sep = if *excl_end { "..^" } else { ".." };
            format!(
                "{}{}{}{}",
                prefix,
                start.to_string_value(),
                sep,
                end.to_string_value()
            )
        }
        _ => value.to_string_value(),
    }
}

fn gist_array_wrap(inner: &str, kind: ArrayKind) -> String {
    match kind {
        ArrayKind::Array | ArrayKind::Shaped => format!("[{}]", inner),
        ArrayKind::List => format!("({})", inner),
        ArrayKind::ItemArray => format!("$[{}]", inner),
        ArrayKind::ItemList => format!("$({})", inner),
    }
}

fn raku_array_wrap(inner: &str, kind: ArrayKind) -> String {
    match kind {
        ArrayKind::Array | ArrayKind::Shaped => format!("[{}]", inner),
        ArrayKind::List => format!("({})", inner),
        ArrayKind::ItemArray => format!("$[{}]", inner),
        ArrayKind::ItemList => {
            if inner.is_empty() {
                "$( )".to_string()
            } else {
                format!("$({})", inner)
            }
        }
    }
}

fn raku_value(v: &Value) -> String {
    match v {
        Value::Array(items, kind) => {
            let snapshot = |k: ArrayKind| {
                let inner = items
                    .iter()
                    .filter(|item| !is_self_array_ref_marker(item))
                    .map(raku_value)
                    .collect::<Vec<_>>()
                    .join(", ");
                raku_array_wrap(&inner, k)
            };
            let inner = items
                .iter()
                .map(|item| {
                    if is_self_array_ref_marker(item) {
                        snapshot(*kind)
                    } else {
                        raku_value(item)
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            raku_array_wrap(&inner, *kind)
        }
        Value::Seq(items) => {
            let inner = items.iter().map(raku_value).collect::<Vec<_>>().join(", ");
            format!("({})", inner)
        }
        Value::Slip(items) => {
            let inner = items.iter().map(raku_value).collect::<Vec<_>>().join(", ");
            format!("slip({})", inner)
        }
        Value::Str(s) => format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"")),
        Value::Int(i) => i.to_string(),
        Value::Rat(n, d) => {
            if *d == 0 {
                if *n == 0 {
                    "NaN".to_string()
                } else if *n > 0 {
                    "Inf".to_string()
                } else {
                    "-Inf".to_string()
                }
            } else if *n % *d == 0 {
                format!("{}.0", *n / *d)
            } else {
                // Non-integer rat: check if it's a simple decimal
                let whole = *n as f64 / *d as f64;
                let mut dd = d.abs();
                while dd % 2 == 0 {
                    dd /= 2;
                }
                while dd % 5 == 0 {
                    dd /= 5;
                }
                if dd == 1 {
                    let s = format!("{}", whole);
                    if s.contains('.') {
                        s
                    } else {
                        format!("{}.0", whole)
                    }
                } else {
                    format!("<{}/{}>", n, d)
                }
            }
        }
        Value::BigRat(n, d) => {
            if d == &num_bigint::BigInt::from(0) {
                if n == &num_bigint::BigInt::from(0) {
                    "NaN".to_string()
                } else if n > &num_bigint::BigInt::from(0) {
                    "Inf".to_string()
                } else {
                    "-Inf".to_string()
                }
            } else if (n % d) == num_bigint::BigInt::from(0) {
                format!("{}.0", n / d)
            } else {
                let mut dd = d.abs();
                while (&dd % 2u8) == num_bigint::BigInt::from(0) {
                    dd /= 2u8;
                }
                while (&dd % 5u8) == num_bigint::BigInt::from(0) {
                    dd /= 5u8;
                }
                if dd == num_bigint::BigInt::from(1u8) {
                    v.to_string_value()
                } else {
                    format!("<{}/{}>", n, d)
                }
            }
        }
        Value::FatRat(n, d) => format!("<{}/{}>", n, d),
        Value::Bool(b) => if *b { "True" } else { "False" }.to_string(),
        Value::Num(f) => {
            if f.is_nan() {
                "NaN".to_string()
            } else if f.is_infinite() {
                if *f > 0.0 {
                    "Inf".to_string()
                } else {
                    "-Inf".to_string()
                }
            } else {
                format!("{}", f)
            }
        }
        Value::Complex(r, i) => format!("<{}>", crate::value::format_complex(*r, *i)),
        Value::Pair(key, value) => format!("{} => {}", key, raku_value(value)),
        Value::ValuePair(key, value) => {
            let key_repr = match key.as_ref() {
                Value::Pair(_, _) | Value::ValuePair(_, _) => format!("({})", raku_value(key)),
                _ => raku_value(key),
            };
            format!("{} => {}", key_repr, raku_value(value))
        }
        Value::Nil => "Nil".to_string(),
        Value::Package(name) => name.resolve().to_string(),
        other => other.to_string_value(),
    }
}

fn hash_pick_item(key: &str, value: &Value) -> Value {
    match key {
        "True" => Value::ValuePair(Box::new(Value::Bool(true)), Box::new(value.clone())),
        "False" => Value::ValuePair(Box::new(Value::Bool(false)), Box::new(value.clone())),
        _ => {
            if let Ok(n) = key.parse::<f64>() {
                return Value::ValuePair(Box::new(Value::Num(n)), Box::new(value.clone()));
            }
            Value::Pair(key.to_string(), Box::new(value.clone()))
        }
    }
}

fn dispatch_core(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    // Date/DateTime 0-arg methods
    match target {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Date" => {
            if let Some(result) = temporal_dispatch::date_method_0arg(attributes, method) {
                return Some(result);
            }
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "DateTime" => {
            if let Some(result) = temporal_dispatch::datetime_method_0arg(attributes, method) {
                return Some(result);
            }
        }
        _ => {}
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
            "resume" => return Some(Ok(Value::Nil)),
            "gist" | "Str" => {
                return Some(Ok(attributes
                    .get("message")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            _ => {}
        }
    }

    // Array of Match objects: .to returns last element's .to, .from returns first element's .from
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

    // Match object methods: from, to, Str, pos; delegates unknown to string
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
                let mut gist = format!("｢{}｣", matched);
                if let Some(Value::Hash(named)) = attributes.get("named")
                    && !named.is_empty()
                {
                    let mut keys: Vec<&String> = named.keys().collect();
                    keys.sort();
                    for key in keys {
                        if let Some(value) = named.get(key) {
                            gist.push_str(&format!("\n {} => ｢{}｣", key, value.to_string_value()));
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
                return Some(Ok(Value::str(match_raku_repr(attributes))));
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
                // Combine positional indices and named capture keys
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
            "ast" | "made" => {
                return Some(Ok(attributes.get("ast").cloned().unwrap_or(Value::Nil)));
            }
            "prematch" => {
                if let Some(Value::Str(orig)) = attributes.get("orig") {
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
                if let Some(Value::Str(orig)) = attributes.get("orig") {
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
                return Some(Ok(match_caps(attributes)));
            }
            "chunks" => {
                return Some(Ok(match_chunks(attributes)));
            }
            _ => {
                // Delegate unknown methods to string representation
                let str_val = Value::str(target.to_string_value());
                return native_method_0arg(&str_val, Symbol::intern(method));
            }
        }
    }
    // Native int type .Range method
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
            // For types where bounds don't fit in i64 (e.g. uint64)
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
    if method == "int-bounds" {
        match target {
            Value::Range(start, end) => {
                return Some(Ok(Value::array(vec![Value::Int(*start), Value::Int(*end)])));
            }
            Value::RangeExcl(start, end) => {
                return Some(Ok(Value::array(vec![
                    Value::Int(*start + 1),
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
                let s = if *excl_start {
                    match start.as_ref() {
                        Value::Int(n) => Value::Int(n + 1),
                        Value::BigInt(n) => Value::bigint(n.as_ref() + 1),
                        other => Value::Int(other.to_f64() as i64 + 1),
                    }
                } else {
                    start.as_ref().clone()
                };
                let e = if *excl_end {
                    match end.as_ref() {
                        Value::Int(n) => Value::Int(n - 1),
                        Value::BigInt(n) => Value::bigint(n.as_ref() - 1),
                        other => Value::Int(other.to_f64() as i64 - 1),
                    }
                } else {
                    end.as_ref().clone()
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
            value: if cfg!(target_endian = "little") { 1 } else { 2 },
            index: if cfg!(target_endian = "little") { 1 } else { 2 },
        }));
    }
    match method {
        "self" => Some(Ok(target.clone())),
        "clone" => {
            match target {
                Value::Package(_) | Value::Nil => Some(Ok(target.clone())),
                Value::Array(items, kind) => {
                    Some(Ok(Value::Array(Arc::new(items.to_vec()), *kind)))
                }
                Value::Hash(map) => Some(Ok(Value::Hash(Arc::new((**map).clone())))),
                _ => None, // fall through to slow path for instances etc.
            }
        }
        "defined" => Some(Ok(Value::Bool(match target {
            Value::Nil | Value::Package(_) => false,
            Value::Slip(items) if items.is_empty() => false,
            Value::Instance { class_name, .. } if class_name == "Failure" => false,
            _ => true,
        }))),
        "DEFINITE" => Some(Ok(Value::Bool(match target {
            Value::Nil | Value::Package(_) | Value::CustomType { .. } => false,
            Value::Slip(items) if items.is_empty() => false,
            Value::Instance { class_name, .. } if class_name == "Failure" => false,
            _ => true,
        }))),
        "WHICH" => {
            let which_str = match target {
                Value::Package(name) => name.resolve(),
                Value::Int(n) => format!("Int|{}", n),
                Value::BigInt(n) => format!("Int|{}", n),
                Value::Num(n) => format!("Num|{}", n),
                Value::Str(s) => format!("Str|{}", s),
                Value::Bool(b) => format!("Bool|{}", if *b { 1 } else { 0 }),
                Value::Rat(n, d) => format!("Rat|{}/{}", n, d),
                Value::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
                Value::Complex(r, i) => format!("Complex|{}+{}i", r, i),
                Value::Nil => "Any|U140803128".to_string(),
                _ => format!(
                    "{:?}|0x{:p}",
                    runtime::utils::value_type_name(target),
                    target as *const Value
                ),
            };
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("WHICH".to_string(), Value::str(which_str));
            Some(Ok(Value::make_instance(Symbol::intern("ObjAt"), attrs)))
        }
        "Bool" => {
            if matches!(target, Value::Instance { .. })
                && (target.does_check("Real") || target.does_check("Numeric"))
            {
                None
            } else {
                Some(Ok(Value::Bool(target.truthy())))
            }
        }
        "Str" | "Stringy" => match target {
            Value::Package(_) | Value::Instance { .. } => None,
            Value::Str(s) if s.as_str() == "IO::Special" => Some(Ok(Value::str_from(""))),
            Value::Array(items, ArrayKind::Array)
                if items.iter().all(|v| matches!(v, Value::Int(_))) =>
            {
                // Uni array: convert codepoints to a string
                let s: String = items
                    .iter()
                    .filter_map(|v| match v {
                        Value::Int(cp) => char::from_u32(*cp as u32),
                        _ => None,
                    })
                    .collect();
                Some(Ok(Value::str(s)))
            }
            _ => Some(Ok(Value::str(target.to_string_value()))),
        },
        "Int" => {
            let result = match target {
                Value::Int(i) => Value::Int(*i),
                Value::BigInt(_) => target.clone(),
                Value::Num(f) => Value::Int(*f as i64),
                Value::Rat(n, d) if *d != 0 => Value::Int(*n / *d),
                Value::Str(s) => {
                    if let Some(v) = parse_raku_int_from_str(s) {
                        v
                    } else {
                        return Some(Err(RuntimeError::new(format!(
                            "X::Str::Numeric: Cannot convert string '{}' to a number",
                            s
                        ))));
                    }
                }
                Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                Value::Complex(r, _) => Value::Int(*r as i64),
                Value::Hash(h) => Value::Int(h.len() as i64),
                Value::Array(items, ..) => Value::Int(items.len() as i64),
                _ => return None,
            };
            Some(Ok(result))
        }
        "Num" => {
            let result = match target {
                Value::Int(i) => Value::Num(*i as f64),
                Value::BigInt(n) => {
                    use num_traits::ToPrimitive;
                    Value::Num(n.to_f64().unwrap_or(f64::INFINITY))
                }
                Value::Num(f) => Value::Num(*f),
                Value::Rat(n, d) if *d != 0 => Value::Num(*n as f64 / *d as f64),
                Value::Str(s) => {
                    let trimmed = s.trim();
                    // Normalize U+2212 MINUS SIGN to ASCII hyphen-minus
                    let normalized = trimmed.replace('\u{2212}', "-");
                    if let Ok(f) = normalized.parse::<f64>() {
                        Value::Num(f)
                    } else {
                        return Some(Err(RuntimeError::new(format!(
                            "X::Str::Numeric: Cannot convert string '{}' to a number",
                            s
                        ))));
                    }
                }
                Value::Bool(b) => Value::Num(if *b { 1.0 } else { 0.0 }),
                Value::Complex(r, _) => Value::Num(*r),
                _ => return None,
            };
            Some(Ok(result))
        }
        "Real" => {
            let result = match target {
                Value::Int(i) => Value::Int(*i),
                Value::BigInt(_) => target.clone(),
                Value::Num(f) => Value::Num(*f),
                Value::Rat(n, d) => Value::Rat(*n, *d),
                Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                Value::Complex(r, im) => {
                    if im.abs() <= 1e-15 {
                        Value::Num(*r)
                    } else {
                        return Some(Err(RuntimeError::new(
                            "Cannot convert Complex to Real: imaginary part not zero",
                        )));
                    }
                }
                Value::Str(s) => {
                    if let Ok(i) = s.trim().parse::<i64>() {
                        Value::Int(i)
                    } else if let Ok(f) = s.trim().parse::<f64>() {
                        Value::Num(f)
                    } else {
                        return Some(Err(RuntimeError::new(format!(
                            "X::Str::Numeric: Cannot convert string '{}' to a number",
                            s
                        ))));
                    }
                }
                _ => return None,
            };
            Some(Ok(result))
        }
        "Numeric" => {
            let result = match target {
                Value::Int(i) => Value::Int(*i),
                Value::BigInt(_) => target.clone(),
                Value::Num(f) => Value::Num(*f),
                Value::Rat(n, d) if *d != 0 => Value::Num(*n as f64 / *d as f64),
                Value::Str(s) => {
                    if let Ok(i) = s.trim().parse::<i64>() {
                        Value::Int(i)
                    } else if let Ok(f) = s.trim().parse::<f64>() {
                        Value::Num(f)
                    } else {
                        return Some(Err(RuntimeError::new(format!(
                            "X::Str::Numeric: Cannot convert string '{}' to a number",
                            s
                        ))));
                    }
                }
                Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                Value::Complex(r, _) => Value::Num(*r),
                Value::Array(items, ..) => Value::Int(items.len() as i64),
                Value::Hash(h) => Value::Int(h.len() as i64),
                _ => return None,
            };
            Some(Ok(result))
        }
        "Bridge" => {
            let result = match target {
                Value::Int(i) => Value::Num(*i as f64),
                Value::BigInt(n) => Value::Num(n.to_f64().unwrap_or(f64::INFINITY)),
                Value::Num(f) => Value::Num(*f),
                Value::Rat(n, d) if *d != 0 => Value::Num(*n as f64 / *d as f64),
                Value::FatRat(n, d) if *d != 0 => {
                    Value::Num(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" || class_name == "Duration" => {
                    let bridged = attributes
                        .get("value")
                        .and_then(|v| match v {
                            Value::Int(i) => Some(*i as f64),
                            Value::BigInt(n) => Some(n.to_f64().unwrap_or(f64::INFINITY)),
                            Value::Num(f) => Some(*f),
                            Value::Rat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
                            Value::FatRat(n, d) if *d != 0 => {
                                Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                            }
                            _ => None,
                        })
                        .unwrap_or(0.0);
                    Value::Num(bridged)
                }
                _ => return None,
            };
            Some(Ok(result))
        }
        "bytes" => match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Buf"
                || class_name == "Blob"
                || class_name.resolve().starts_with("Buf[")
                || class_name.resolve().starts_with("Blob[") =>
            {
                if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                    Some(Ok(Value::Int(bytes.len() as i64)))
                } else {
                    Some(Ok(Value::Int(0)))
                }
            }
            Value::Str(s) => Some(Ok(Value::Int(s.len() as i64))),
            _ => Some(Ok(Value::Int(target.to_string_value().len() as i64))),
        },
        "chars" => Some(Ok(Value::Int(
            target.to_string_value().graphemes(true).count() as i64,
        ))),
        "ord" => {
            let s = target.to_string_value();
            if let Some(ch) = s.chars().next() {
                Some(Ok(Value::Int(ch as u32 as i64)))
            } else {
                Some(Ok(Value::Nil))
            }
        }
        "ords" => {
            let s = target.to_string_value();
            let normalized: String = s.nfc().collect();
            let ords: Vec<Value> = normalized
                .chars()
                .map(|c| Value::Int(c as u32 as i64))
                .collect();
            Some(Ok(Value::array(ords)))
        }
        "chr" => {
            let code = match target {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                _ => {
                    let s = target.to_string_value();
                    s.parse::<i64>().unwrap_or(0)
                }
            };
            if let Some(ch) = char::from_u32(code as u32) {
                Some(Ok(Value::str(ch.to_string())))
            } else {
                Some(Err(RuntimeError::new(format!(
                    "chr({}) does not map to a valid Unicode character",
                    code
                ))))
            }
        }
        "chrs" => {
            // .chrs on a list/array of ints or a range
            let val_to_i64 = |v: &Value| -> i64 {
                match v {
                    Value::Int(i) => *i,
                    Value::Num(f) => *f as i64,
                    _ => v.to_string_value().parse::<i64>().unwrap_or(0),
                }
            };
            let items: Vec<i64> = match target {
                Value::Array(items, ..) => items.iter().map(&val_to_i64).collect(),
                Value::Range(a, b) => (*a..=*b).collect(),
                Value::RangeExcl(a, b) => (*a..*b).collect(),
                _ => vec![val_to_i64(target)],
            };
            let s: String = items
                .iter()
                .filter_map(|&code| char::from_u32(code as u32))
                .collect();
            Some(Ok(Value::str(s)))
        }
        "elems" => {
            if let Some(items) = target.as_list_items() {
                return Some(Ok(Value::Int(items.len() as i64)));
            }
            let result = match target {
                Value::Hash(items) => Value::Int(items.len() as i64),
                Value::Set(items) => Value::Int(items.len() as i64),
                Value::Bag(items) => Value::Int(items.len() as i64),
                Value::Mix(items) => Value::Int(items.len() as i64),
                Value::Junction { values, .. } => Value::Int(values.len() as i64),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Buf" || class_name == "Blob" => {
                    if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                        Value::Int(bytes.len() as i64)
                    } else {
                        Value::Int(0)
                    }
                }
                // LazyList is lazy — .elems returns a Failure
                Value::LazyList(_) => {
                    let mut ex_attrs = std::collections::HashMap::new();
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str("Cannot .elems a lazy list".to_string()),
                    );
                    let exception =
                        Value::make_instance(Symbol::intern("X::Cannot::Lazy"), ex_attrs);
                    let mut failure_attrs = std::collections::HashMap::new();
                    failure_attrs.insert("exception".to_string(), exception);
                    failure_attrs.insert("handled".to_string(), Value::Bool(false));
                    return Some(Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    )));
                }
                _ => Value::Int(1),
            };
            Some(Ok(result))
        }
        "default" => {
            let result = match target {
                Value::Array(..) | Value::Hash(..) => Value::Package(Symbol::intern("Any")),
                Value::Set(..) => Value::Bool(false),
                Value::Bag(..) | Value::Mix(..) => Value::Int(0),
                _ => return None,
            };
            Some(Ok(result))
        }
        "Complex-i" | "i" => {
            let imag = match target {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::BigInt(n) => n.to_f64().unwrap_or(0.0),
                Value::BigRat(n, d) if !d.is_zero() => {
                    n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)
                }
                Value::Complex(r, i) => return Some(Ok(Value::Complex(-*i, *r))),
                _ => 0.0,
            };
            Some(Ok(Value::Complex(0.0, imag)))
        }
        "abs" => {
            let result = match target {
                Value::Int(i) => Value::Int(i.abs()),
                Value::BigInt(n) => Value::bigint(n.as_ref().abs()),
                Value::Num(f) => Value::Num(f.abs()),
                Value::Rat(n, d) => Value::Rat(n.abs(), *d),
                Value::Complex(r, i) => Value::Num((r * r + i * i).sqrt()),
                _ => return None,
            };
            Some(Ok(result))
        }
        "lsb" => int_lsb_value(target).map(Ok),
        "msb" => int_msb_value(target).map(Ok),
        "rand" => {
            let max = match target {
                Value::Int(n) => *n as f64,
                Value::Num(n) => *n,
                Value::Rat(n, d) => *n as f64 / *d as f64,
                Value::Range(start, end) => {
                    let span = (*end - *start + 1).max(1) as f64;
                    return Some(Ok(Value::Int(
                        *start + (builtin_rand() * span).floor() as i64,
                    )));
                }
                Value::RangeExcl(start, end) => {
                    if *start >= *end {
                        return Some(Ok(Value::Nil));
                    }
                    let span = (*end - *start).max(1) as f64;
                    return Some(Ok(Value::Int(
                        *start + (builtin_rand() * span).floor() as i64,
                    )));
                }
                Value::RangeExclStart(start, end) => {
                    if *start >= *end {
                        return Some(Ok(Value::Nil));
                    }
                    let from = *start + 1;
                    let span = (*end - from + 1).max(1) as f64;
                    return Some(Ok(Value::Int(
                        from + (builtin_rand() * span).floor() as i64,
                    )));
                }
                Value::RangeExclBoth(start, end) => {
                    let from = *start + 1;
                    let to = *end - 1;
                    if from > to {
                        return Some(Ok(Value::Nil));
                    }
                    let span = (to - from + 1).max(1) as f64;
                    return Some(Ok(Value::Int(
                        from + (builtin_rand() * span).floor() as i64,
                    )));
                }
                Value::GenericRange {
                    start,
                    end,
                    excl_start,
                    excl_end,
                } => {
                    let Some(mut from) = runtime::to_float_value(start) else {
                        return Some(Ok(Value::Nil));
                    };
                    let Some(mut to) = runtime::to_float_value(end) else {
                        return Some(Ok(Value::Nil));
                    };
                    if *excl_start {
                        from = f64::from_bits(from.to_bits().saturating_add(1));
                    }
                    if *excl_end {
                        to = f64::from_bits(to.to_bits().saturating_sub(1));
                    }
                    if !from.is_finite() || !to.is_finite() || from > to {
                        return Some(Ok(Value::Nil));
                    }
                    let v = from + builtin_rand() * (to - from);
                    return Some(Ok(Value::Num(v)));
                }
                _ => return None,
            };
            Some(Ok(Value::Num(builtin_rand() * max)))
        }
        "uc" => Some(Ok(Value::str(target.to_string_value().to_uppercase()))),
        "lc" => Some(Ok(Value::str(target.to_string_value().to_lowercase()))),
        "fc" => Some(Ok(Value::str(unicode_foldcase(&target.to_string_value())))),
        "tc" => Some(Ok(Value::str(titlecase_string(&target.to_string_value())))),
        "sign" => {
            let result = match target {
                Value::Int(i) => Value::Int(i.signum()),
                Value::Num(f) => {
                    if f.is_nan() {
                        Value::Num(f64::NAN)
                    } else {
                        Value::Int(if *f > 0.0 {
                            1
                        } else if *f < 0.0 {
                            -1
                        } else {
                            0
                        })
                    }
                }
                Value::Rat(n, d) => {
                    if *d == 0 {
                        if *n > 0 {
                            Value::Int(1)
                        } else if *n < 0 {
                            Value::Int(-1)
                        } else {
                            Value::Num(f64::NAN)
                        }
                    } else {
                        // sign is determined by n/d
                        let sign = n.signum() * d.signum();
                        Value::Int(sign)
                    }
                }
                Value::FatRat(n, d) => {
                    if *d == 0 {
                        if *n > 0 {
                            Value::Int(1)
                        } else if *n < 0 {
                            Value::Int(-1)
                        } else {
                            Value::Num(f64::NAN)
                        }
                    } else {
                        let sign = n.signum() * d.signum();
                        Value::Int(sign)
                    }
                }
                Value::BigInt(n) => {
                    use num_bigint::Sign;
                    Value::Int(match n.sign() {
                        Sign::Plus => 1,
                        Sign::Minus => -1,
                        Sign::NoSign => 0,
                    })
                }
                Value::Complex(re, im) => {
                    // sign of a Complex number is the number divided by its absolute value
                    let abs = (re * re + im * im).sqrt();
                    if abs == 0.0 {
                        Value::Complex(0.0, 0.0)
                    } else {
                        Value::Complex(re / abs, im / abs)
                    }
                }
                Value::Enum { value, .. } => Value::Int(value.signum()),
                _ => return None,
            };
            Some(Ok(result))
        }
        "end" => {
            if let Some(items) = target.as_list_items() {
                return Some(Ok(Value::Int(items.len() as i64 - 1)));
            }
            match target {
                Value::Hash(items) => Some(Ok(Value::Int(items.len() as i64 - 1))),
                Value::Set(items) => Some(Ok(Value::Int(items.len() as i64 - 1))),
                Value::Bag(items) => Some(Ok(Value::Int(items.len() as i64 - 1))),
                Value::Mix(items) => Some(Ok(Value::Int(items.len() as i64 - 1))),
                Value::Junction { values, .. } => Some(Ok(Value::Int(values.len() as i64 - 1))),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Buf" || class_name == "Blob" => {
                    if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                        Some(Ok(Value::Int(bytes.len() as i64 - 1)))
                    } else {
                        Some(Ok(Value::Int(-1)))
                    }
                }
                Value::LazyList(_) => None,
                _ => Some(Ok(Value::Int(0))),
            }
        }
        "flat" => match target {
            Value::Array(items, ..) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    flatten_deep_value(item, &mut result, false);
                }
                Some(Ok(Value::Seq(Arc::new(result))))
            }
            Value::Seq(items) | Value::Slip(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    flatten_deep_value(item, &mut result, false);
                }
                Some(Ok(Value::Seq(Arc::new(result))))
            }
            other if is_infinite_range(other) => Some(Ok(other.clone())),
            _ => {
                let mut result = Vec::new();
                flatten_deep_value(target, &mut result, false);
                Some(Ok(Value::Seq(Arc::new(result))))
            }
        },
        "sort" => match target {
            Value::Array(items, ..) => {
                let mut sorted = (**items).clone();
                sorted.sort_by(|a, b| crate::runtime::compare_values(a, b).cmp(&0));
                Some(Ok(Value::array(sorted)))
            }
            _ => None,
        },
        "reverse" => match target {
            Value::Array(items, kind) => {
                let mut reversed = (**items).clone();
                reversed.reverse();
                Some(Ok(Value::Array(std::sync::Arc::new(reversed), *kind)))
            }
            Value::Range(a, b)
            | Value::RangeExcl(a, b)
            | Value::RangeExclStart(a, b)
            | Value::RangeExclBoth(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    None
                } else {
                    let mut reversed = crate::runtime::utils::value_to_list(target);
                    reversed.reverse();
                    Some(Ok(Value::array(reversed)))
                }
            }
            Value::Str(s) => Some(Ok(Value::str(s.chars().rev().collect()))),
            _ => None,
        },
        "unique" => match target {
            Value::Array(items, ..) => {
                let mut seen: Vec<Value> = Vec::new();
                let mut result = Vec::new();
                for item in items.iter() {
                    if !seen
                        .iter()
                        .any(|existing| crate::runtime::values_identical(existing, item))
                    {
                        seen.push(item.clone());
                        result.push(item.clone());
                    }
                }
                Some(Ok(Value::array(result)))
            }
            Value::Seq(items) | Value::Slip(items) => {
                let mut seen: Vec<Value> = Vec::new();
                let mut result = Vec::new();
                for item in items.iter() {
                    if !seen
                        .iter()
                        .any(|existing| crate::runtime::values_identical(existing, item))
                    {
                        seen.push(item.clone());
                        result.push(item.clone());
                    }
                }
                Some(Ok(Value::array(result)))
            }
            Value::LazyList(_) => None,
            // Supply.unique is handled by native_supply
            Value::Instance { class_name, .. } if class_name == "Supply" => None,
            _ => Some(Ok(target.clone())),
        },
        "floor" => match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(Value::Int(f.floor() as i64))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::BigInt(_) => Some(Ok(target.clone())),
            Value::Rat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) != (*d < 0) {
                    Some(Ok(Value::Int(q - 1)))
                } else {
                    Some(Ok(Value::Int(q)))
                }
            }
            Value::BigRat(n, d) if !d.is_zero() => {
                use num_integer::Integer;
                let (q, r) = n.div_rem(d);
                if !r.is_zero() && n.is_negative() != d.is_negative() {
                    Some(Ok(Value::bigint(q - 1)))
                } else {
                    Some(Ok(Value::bigint(q)))
                }
            }
            Value::FatRat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) != (*d < 0) {
                    Some(Ok(Value::Int(q - 1)))
                } else {
                    Some(Ok(Value::Int(q)))
                }
            }
            Value::Complex(re, im) => Some(Ok(Value::Complex(re.floor(), im.floor()))),
            _ => None,
        },
        "ceiling" | "ceil" => match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(Value::Int(f.ceil() as i64))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::BigInt(_) => Some(Ok(target.clone())),
            Value::Rat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) == (*d < 0) {
                    Some(Ok(Value::Int(q + 1)))
                } else {
                    Some(Ok(Value::Int(q)))
                }
            }
            Value::BigRat(n, d) if !d.is_zero() => {
                use num_integer::Integer;
                let (q, r) = n.div_rem(d);
                if !r.is_zero() && n.is_negative() == d.is_negative() {
                    Some(Ok(Value::bigint(q + 1)))
                } else {
                    Some(Ok(Value::bigint(q)))
                }
            }
            Value::FatRat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) == (*d < 0) {
                    Some(Ok(Value::Int(q + 1)))
                } else {
                    Some(Ok(Value::Int(q)))
                }
            }
            Value::Complex(re, im) => Some(Ok(Value::Complex(re.ceil(), im.ceil()))),
            _ => None,
        },
        "round" => match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(raku_round_to_value(*f))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::BigInt(_) => Some(Ok(target.clone())),
            Value::Rat(n, d) if *d != 0 => {
                let f = *n as f64 / *d as f64;
                Some(Ok(raku_round_to_value(f)))
            }
            Value::BigRat(n, d) if !d.is_zero() => {
                use num_bigint::BigInt;
                use num_integer::Integer;
                // round = floor(x + 0.5) for Raku semantics
                // For BigRat: floor((2n + d) / 2d)
                let two_n: BigInt = n * 2;
                let two_d: BigInt = d * 2;
                let sum: BigInt = &two_n + d;
                let (q, r) = sum.div_rem(&two_d);
                if !r.is_zero() && sum.is_negative() != two_d.is_negative() {
                    Some(Ok(Value::bigint(q - 1)))
                } else {
                    Some(Ok(Value::bigint(q)))
                }
            }
            Value::FatRat(n, d) if *d != 0 => {
                let f = *n as f64 / *d as f64;
                Some(Ok(raku_round_to_value(f)))
            }
            Value::Complex(re, im) => Some(Ok(Value::Complex(raku_round(*re), raku_round(*im)))),
            _ => None,
        },
        "truncate" => match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(Value::Int(f.trunc() as i64))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::BigInt(_) => Some(Ok(target.clone())),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Int(*n / *d))),
            Value::BigRat(n, d) if !d.is_zero() => Some(Ok(Value::bigint(n / d))),
            Value::FatRat(n, d) if *d != 0 => Some(Ok(Value::Int(*n / *d))),
            Value::Complex(re, im) => Some(Ok(Value::Complex(re.trunc(), im.trunc()))),
            _ => None,
        },
        "narrow" => match target {
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::Rat(n, d) if *d != 0 && *n % *d == 0 => Some(Ok(Value::Int(*n / *d))),
            Value::Rat(n, d) => Some(Ok(Value::Rat(*n, *d))),
            Value::Num(f) if f.is_finite() => {
                // Use tolerance (1e-15) to check if approximately an integer
                let rounded = f.round();
                let tol = 1e-15;
                let diff = (*f - rounded).abs();
                let max = f.abs().max(rounded.abs());
                let approx_int = if max == 0.0 { true } else { diff / max <= tol };
                if approx_int {
                    Some(Ok(Value::Int(rounded as i64)))
                } else {
                    Some(Ok(Value::Num(*f)))
                }
            }
            Value::Num(f) => Some(Ok(Value::Num(*f))),
            Value::Complex(re, im) => {
                // Check if imaginary part is approximately zero
                let tol = 1e-15;
                let im_approx_zero = if *im == 0.0 {
                    true
                } else {
                    let max_mag = re.abs().max(im.abs());
                    if max_mag == 0.0 {
                        true
                    } else {
                        im.abs() / max_mag <= tol
                    }
                };
                if im_approx_zero {
                    // Narrow to real part, then try narrowing that to Int
                    let rounded = re.round();
                    let re_approx_int = if re.is_finite() {
                        let diff = (*re - rounded).abs();
                        let max = re.abs().max(rounded.abs());
                        if max == 0.0 { true } else { diff / max <= tol }
                    } else {
                        false
                    };
                    if re_approx_int {
                        Some(Ok(Value::Int(rounded as i64)))
                    } else {
                        Some(Ok(Value::Num(*re)))
                    }
                } else {
                    // Check if real part is approximately zero
                    let re_approx_zero = if *re == 0.0 {
                        true
                    } else {
                        let max_mag = re.abs().max(im.abs());
                        if max_mag == 0.0 {
                            true
                        } else {
                            re.abs() / max_mag <= tol
                        }
                    };
                    let new_re = if re_approx_zero { 0.0 } else { *re };
                    let new_im = *im;
                    Some(Ok(Value::Complex(new_re, new_im)))
                }
            }
            _ => Some(Ok(target.clone())),
        },
        "sqrt" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).sqrt()))),
            Value::Num(f) => Some(Ok(Value::Num(f.sqrt()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).sqrt()))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt();
                let re = ((mag + r) / 2.0).sqrt();
                let im = i.signum() * ((mag - r) / 2.0).sqrt();
                Some(Ok(Value::Complex(re, im)))
            }
            _ => None,
        },
        "words" => {
            let s = target.to_string_value();
            let words: Vec<Value> = s
                .split_whitespace()
                .map(|w| Value::str(w.to_string()))
                .collect();
            Some(Ok(Value::array(words)))
        }
        "codes" => {
            let s = target.to_string_value();
            Some(Ok(Value::Int(s.chars().count() as i64)))
        }
        "lines" => {
            // Skip for Supply instances — handled by native Supply.lines
            if let Value::Instance { class_name, .. } = target
                && (class_name == "Supply"
                    || class_name == "IO::Handle"
                    || class_name == "IO::Path"
                    || class_name == "IO::Socket::INET")
            {
                return None;
            }
            let s = target.to_string_value();
            let lines: Vec<Value> = crate::builtins::split_lines_chomped(&s)
                .into_iter()
                .map(Value::str)
                .collect();
            Some(Ok(Value::array(lines)))
        }
        "trim" => Some(Ok(Value::str(target.to_string_value().trim().to_string()))),
        "trim-leading" => Some(Ok(Value::str(
            target.to_string_value().trim_start().to_string(),
        ))),
        "trim-trailing" => Some(Ok(Value::str(
            target.to_string_value().trim_end().to_string(),
        ))),
        "flip" => {
            let s = target.to_string_value();
            let reversed: String = s.graphemes(true).rev().collect();
            Some(Ok(Value::str(reversed)))
        }
        "so" => Some(Ok(Value::Bool(target.truthy()))),
        "not" => Some(Ok(Value::Bool(!target.truthy()))),
        "is-lazy" => Some(Ok(Value::Bool(is_value_lazy(target)))),
        "lazy" => {
            if is_value_lazy(target) {
                if let Value::LazyList(list) = target {
                    let mut env = list.env.clone();
                    env.insert(
                        "__mutsu_preserve_lazy_on_array_assign".to_string(),
                        Value::Bool(true),
                    );
                    let cache = list.cache.lock().unwrap().clone();
                    return Some(Ok(Value::LazyList(std::sync::Arc::new(
                        crate::value::LazyList {
                            body: list.body.clone(),
                            env,
                            cache: std::sync::Mutex::new(cache),
                        },
                    ))));
                }
                return Some(Ok(target.clone()));
            }
            let items = if let Some(items) = target.as_list_items() {
                items.as_ref().clone()
            } else {
                return Some(Ok(target.clone()));
            };
            let mut env = crate::env::Env::new();
            env.insert(
                "__mutsu_preserve_lazy_on_array_assign".to_string(),
                Value::Bool(true),
            );
            Some(Ok(Value::LazyList(std::sync::Arc::new(
                crate::value::LazyList {
                    body: vec![],
                    env,
                    cache: std::sync::Mutex::new(Some(items)),
                },
            ))))
        }
        "chomp" => Some(Ok(Value::str(crate::builtins::chomp_one(
            &target.to_string_value(),
        )))),
        "chop" => {
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let mut s = target.to_string_value();
            s.pop();
            Some(Ok(Value::str(s)))
        }
        "comb" => {
            let s = target.to_string_value();
            let parts: Vec<Value> = s.chars().map(|c| Value::str(c.to_string())).collect();
            Some(Ok(Value::array(parts)))
        }
        "join" => {
            if let Some(items) = target.as_list_items() {
                let joined = items
                    .iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join("");
                Some(Ok(Value::str(joined)))
            } else {
                Some(Ok(Value::str(target.to_string_value())))
            }
        }
        "gist" | "raku" | "perl" => match target {
            Value::Bool(b) => {
                if method == "gist" {
                    Some(Ok(Value::str(
                        if *b { "True" } else { "False" }.to_string(),
                    )))
                } else {
                    Some(Ok(Value::str(
                        if *b { "Bool::True" } else { "Bool::False" }.to_string(),
                    )))
                }
            }
            Value::Nil => Some(Ok(Value::str_from("(Any)"))),
            Value::Rat(n, d) => {
                if *d == 0 {
                    if *n == 0 {
                        Some(Ok(Value::str_from("NaN")))
                    } else if *n > 0 {
                        Some(Ok(Value::str_from("Inf")))
                    } else {
                        Some(Ok(Value::str_from("-Inf")))
                    }
                } else if *n % *d == 0 {
                    if method == "raku" || method == "perl" {
                        // .raku on Rat always shows decimal: 27.0, not 27
                        Some(Ok(Value::str(format!("{}.0", *n / *d))))
                    } else {
                        Some(Ok(Value::str(format!("{}", *n / *d))))
                    }
                } else {
                    if method == "gist" {
                        return Some(Ok(Value::str(target.to_string_value())));
                    }
                    let mut dd = *d;
                    while dd % 2 == 0 {
                        dd /= 2;
                    }
                    while dd % 5 == 0 {
                        dd /= 5;
                    }
                    if dd == 1 {
                        let val = *n as f64 / *d as f64;
                        Some(Ok(Value::str(format!("{}", val))))
                    } else {
                        Some(Ok(Value::str(format!("<{}/{}>", n, d))))
                    }
                }
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Signature" => {
                let attr_key = if method == "gist" { "gist" } else { "raku" };
                Some(Ok(attributes
                    .get(attr_key)
                    .cloned()
                    .unwrap_or_else(|| Value::str(format!("{}()", class_name)))))
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "CallFrame" => {
                if method == "gist" {
                    let file = attributes
                        .get("file")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let line = attributes
                        .get("line")
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "0".to_string());
                    Some(Ok(Value::str(format!("{} at line {}", file, line))))
                } else {
                    // .raku: CallFrame.new(...)
                    let file = attributes
                        .get("file")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let line = attributes
                        .get("line")
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "0".to_string());
                    Some(Ok(Value::str(format!(
                        "CallFrame.new(annotations => {{:file(\"{}\"), :line(\"{}\")}}, my => {{}})",
                        file, line
                    ))))
                }
            }
            Value::Package(_) | Value::Instance { .. } | Value::Enum { .. } => None,
            Value::Version {
                parts, plus, minus, ..
            } => {
                let s = Value::version_parts_to_string(parts);
                let suffix = if *plus {
                    "+"
                } else if *minus {
                    "-"
                } else {
                    ""
                };
                Some(Ok(Value::str(format!("v{}{}", s, suffix))))
            }
            Value::Str(s) => {
                if method == "raku" || method == "perl" {
                    // .raku wraps strings in quotes and escapes special chars
                    let escaped = s
                        .replace('\\', "\\\\")
                        .replace('"', "\\\"")
                        .replace('\n', "\\n")
                        .replace('\t', "\\t")
                        .replace('\r', "\\r")
                        .replace('\0', "\\0");
                    Some(Ok(Value::str(format!("\"{}\"", escaped))))
                } else {
                    Some(Ok(Value::Str(s.clone())))
                }
            }
            Value::Array(_, _) if method == "raku" || method == "perl" => {
                Some(Ok(Value::str(raku_value(target))))
            }
            Value::Seq(_) if method == "raku" || method == "perl" => {
                Some(Ok(Value::str(raku_value(target))))
            }
            Value::Array(items, kind) if method == "gist" => {
                fn gist_item(v: &Value) -> String {
                    match v {
                        Value::Nil => "Nil".to_string(),
                        Value::Array(inner, kind) => {
                            let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                            gist_array_wrap(&elems, *kind)
                        }
                        Value::Seq(inner) | Value::Slip(inner) => {
                            let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                            format!("({})", elems)
                        }
                        Value::Hash(map) => {
                            let mut sorted_keys: Vec<&String> = map.keys().collect();
                            sorted_keys.sort();
                            let parts: Vec<String> = sorted_keys
                                .iter()
                                .map(|k| format!("{} => {}", k, gist_item(&map[*k])))
                                .collect();
                            format!("{{{}}}", parts.join(" "))
                        }
                        other if other.is_range() => range_gist_string(other),
                        other => other.to_string_value(),
                    }
                }
                let inner = items.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                Some(Ok(Value::str(gist_array_wrap(&inner, *kind))))
            }
            Value::Seq(items) | Value::Slip(items) if method == "gist" => {
                fn gist_item(v: &Value) -> String {
                    match v {
                        Value::Nil => "Nil".to_string(),
                        Value::Array(inner, kind) => {
                            let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                            gist_array_wrap(&elems, *kind)
                        }
                        Value::Seq(inner) | Value::Slip(inner) => {
                            let elems = inner.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                            format!("({})", elems)
                        }
                        Value::Hash(map) => {
                            let mut sorted_keys: Vec<&String> = map.keys().collect();
                            sorted_keys.sort();
                            let parts: Vec<String> = sorted_keys
                                .iter()
                                .map(|k| format!("{} => {}", k, gist_item(&map[*k])))
                                .collect();
                            format!("{{{}}}", parts.join(" "))
                        }
                        other if other.is_range() => range_gist_string(other),
                        other => other.to_string_value(),
                    }
                }
                let inner = items.iter().map(gist_item).collect::<Vec<_>>().join(" ");
                Some(Ok(Value::str(format!("({})", inner))))
            }
            Value::Slip(items) if method == "raku" || method == "perl" => {
                let inner = items.iter().map(raku_value).collect::<Vec<_>>().join(", ");
                Some(Ok(Value::str(format!("slip({})", inner))))
            }
            Value::Junction { .. } if method == "raku" || method == "perl" => None,
            Value::Pair(k, v) => {
                if method == "raku" || method == "perl" {
                    Some(Ok(Value::str(raku_value(target))))
                } else {
                    // gist: use " => " separator
                    Some(Ok(Value::str(format!(
                        "{} => {}",
                        k,
                        runtime::gist_value(v)
                    ))))
                }
            }
            Value::ValuePair(k, v) => {
                if method == "raku" || method == "perl" {
                    Some(Ok(Value::str(raku_value(target))))
                } else {
                    // gist: use " => " separator
                    Some(Ok(Value::str(format!(
                        "{} => {}",
                        runtime::gist_value(k),
                        runtime::gist_value(v)
                    ))))
                }
            }
            Value::BigInt(i) => {
                if method == "raku" || method == "perl" {
                    Some(Ok(Value::str(format!("{i}.0"))))
                } else {
                    Some(Ok(Value::str(i.to_string())))
                }
            }
            Value::Int(i) => Some(Ok(Value::str(format!("{}", i)))),
            Value::Complex(r, i) => {
                if method == "raku" || method == "perl" {
                    Some(Ok(Value::str(format!(
                        "<{}>",
                        crate::value::format_complex(*r, *i)
                    ))))
                } else {
                    Some(Ok(Value::str(crate::value::format_complex(*r, *i))))
                }
            }
            Value::Hash(map) => {
                if method == "raku" || method == "perl" {
                    let mut sorted_keys: Vec<&String> = map.keys().collect();
                    sorted_keys.sort();
                    let parts: Vec<String> = sorted_keys
                        .iter()
                        .map(|k| {
                            let v = &map[*k];
                            if let Value::Bool(true) = v {
                                format!(":{}", k)
                            } else if let Value::Bool(false) = v {
                                format!(":!{}", k)
                            } else {
                                format!(":{}({})", k, raku_value(v))
                            }
                        })
                        .collect();
                    Some(Ok(Value::str(format!("{{{}}}", parts.join(", ")))))
                } else {
                    // gist
                    let mut sorted_keys: Vec<&String> = map.keys().collect();
                    sorted_keys.sort();
                    let parts: Vec<String> = sorted_keys
                        .iter()
                        .map(|k| format!("{} => {}", k, map[*k].to_string_value()))
                        .collect();
                    Some(Ok(Value::str(format!("{{{}}}", parts.join(", ")))))
                }
            }
            _ if target.is_range() && method == "gist" => {
                Some(Ok(Value::str(range_gist_string(target))))
            }
            _ => Some(Ok(Value::str(target.to_string_value()))),
        },
        "head" => match target {
            Value::Array(items, ..) => Some(Ok(items.first().cloned().unwrap_or(Value::Nil))),
            _ => {
                let items = runtime::value_to_list(target);
                Some(Ok(items.first().cloned().unwrap_or(Value::Nil)))
            }
        },
        "tail" => match target {
            Value::Array(items, ..) => Some(Ok(items.last().cloned().unwrap_or(Value::Nil))),
            Value::Instance { class_name, .. } if class_name == "Supply" => None,
            _ => {
                let items = runtime::value_to_list(target);
                Some(Ok(items.last().cloned().unwrap_or(Value::Nil)))
            }
        },
        "pick" => match target {
            Value::Mix(_) => Some(Err(RuntimeError::new(
                "Cannot call .pick on a Mix (immutable)",
            ))),
            Value::Hash(items) => {
                if items.is_empty() {
                    Some(Ok(Value::Nil))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    let (key, value) = items.iter().nth(idx).expect("index in range");
                    Some(Ok(hash_pick_item(key, value)))
                }
            }
            _ => {
                let items = runtime::value_to_list(target);
                if items.is_empty() {
                    Some(Ok(Value::Nil))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    Some(Ok(items[idx].clone()))
                }
            }
        },
        "roll" => {
            if let Value::Mix(items) = target {
                return Some(Ok(sample_weighted_mix_key(items).unwrap_or(Value::Nil)));
            }
            let items = runtime::value_to_list(target);
            if items.is_empty() {
                Some(Ok(Value::Nil))
            } else {
                let mut idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                Some(Ok(items[idx].clone()))
            }
        }
        "first" => match target {
            Value::Array(items, ..) => Some(Ok(items.first().cloned().unwrap_or(Value::Nil))),
            _ => None,
        },
        "min" => match target {
            Value::Array(items, ..) => Some(Ok(items
                .iter()
                .cloned()
                .min_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil))),
            Value::Range(a, b) => Some(Ok(if a <= b { Value::Int(*a) } else { Value::Nil })),
            Value::RangeExcl(a, b) => Some(Ok(if a < b { Value::Int(*a) } else { Value::Nil })),
            Value::RangeExclStart(a, b) => Some(Ok(if a < b {
                Value::Int(*a + 1)
            } else {
                Value::Nil
            })),
            Value::RangeExclBoth(a, b) => Some(Ok(if a + 1 < *b {
                Value::Int(*a + 1)
            } else {
                Value::Nil
            })),
            Value::GenericRange { start, .. } => {
                let items = crate::runtime::utils::value_to_list(target);
                if items.len() == 1 && matches!(items.first(), Some(Value::GenericRange { .. })) {
                    Some(Ok(start.as_ref().clone()))
                } else {
                    Some(Ok(items.first().cloned().unwrap_or(Value::Nil)))
                }
            }
            Value::Hash(_) => None,
            Value::Package(_) | Value::Instance { .. } => None,
            _ => Some(Ok(target.clone())),
        },
        "max" => match target {
            Value::Array(items, ..) => Some(Ok(items
                .iter()
                .cloned()
                .max_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil))),
            Value::Range(a, b) => Some(Ok(if a <= b { Value::Int(*b) } else { Value::Nil })),
            Value::RangeExcl(a, b) => Some(Ok(if a < b {
                Value::Int(*b - 1)
            } else {
                Value::Nil
            })),
            Value::RangeExclStart(a, b) => {
                Some(Ok(if a < b { Value::Int(*b) } else { Value::Nil }))
            }
            Value::RangeExclBoth(a, b) => Some(Ok(if a + 1 < *b {
                Value::Int(*b - 1)
            } else {
                Value::Nil
            })),
            Value::GenericRange { end, .. } => {
                let items = crate::runtime::utils::value_to_list(target);
                if items.len() == 1 && matches!(items.first(), Some(Value::GenericRange { .. })) {
                    Some(Ok(end.as_ref().clone()))
                } else {
                    Some(Ok(items.last().cloned().unwrap_or(Value::Nil)))
                }
            }
            Value::Hash(_) => None,
            Value::Package(_) | Value::Instance { .. } => None,
            _ => Some(Ok(target.clone())),
        },
        "of" => match target {
            Value::Hash(_) => Some(Ok(Value::Package(Symbol::intern("Mu")))),
            Value::Package(name) if name.resolve() == "Hash" || name.resolve() == "Array" => {
                Some(Ok(Value::Package(Symbol::intern("Mu"))))
            }
            Value::Array(..) => Some(Ok(Value::Package(Symbol::intern("Mu")))),
            _ => None,
        },
        "tclc" => Some(Ok(Value::str(crate::value::tclc_str(
            &target.to_string_value(),
        )))),
        "wordcase" => Some(Ok(Value::str(crate::value::wordcase_str(
            &target.to_string_value(),
        )))),
        "succ" => match target {
            Value::Enum { .. } | Value::Instance { .. } => None,
            Value::Int(i) => Some(Ok(Value::Int(i + 1))),
            Value::Num(f) => Some(Ok(Value::Num(f + 1.0))),
            Value::Rat(n, d) => Some(Ok(make_rat(n + d, *d))),
            Value::FatRat(n, d) => Some(Ok(Value::FatRat(n + d, *d))),
            Value::BigRat(n, d) => Some(Ok(Value::BigRat(n + d, d.clone()))),
            Value::Bool(_) => Some(Ok(Value::Bool(true))),
            Value::Str(s) => {
                if s.is_empty() {
                    Some(Ok(Value::str(String::new())))
                } else {
                    let mut chars: Vec<char> = s.chars().collect();
                    if let Some(last) = chars.last_mut() {
                        *last = char::from_u32(*last as u32 + 1).unwrap_or(*last);
                    }
                    Some(Ok(Value::str(chars.into_iter().collect())))
                }
            }
            _ => Some(Ok(target.clone())),
        },
        "pred" => match target {
            Value::Enum { .. } | Value::Instance { .. } => None,
            Value::Int(i) => Some(Ok(Value::Int(i - 1))),
            Value::Num(f) => Some(Ok(Value::Num(f - 1.0))),
            Value::Rat(n, d) => Some(Ok(make_rat(n - d, *d))),
            Value::FatRat(n, d) => Some(Ok(Value::FatRat(n - d, *d))),
            Value::BigRat(n, d) => Some(Ok(Value::BigRat(n - d, d.clone()))),
            Value::Bool(_) => Some(Ok(Value::Bool(false))),
            _ => Some(Ok(target.clone())),
        },
        "log" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).ln()))),
            Value::BigInt(i) => Some(Ok(Value::Num(i.to_f64().unwrap_or(f64::INFINITY).ln()))),
            Value::Num(f) => Some(Ok(Value::Num(f.ln()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).ln()))),
            Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::Num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).ln(),
            ))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(*r);
                Some(Ok(Value::Complex(mag, arg)))
            }
            _ => Some(Ok(Value::Num(f64::NAN))),
        },
        "log2" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).log2()))),
            Value::BigInt(i) => Some(Ok(Value::Num(i.to_f64().unwrap_or(f64::INFINITY).log2()))),
            Value::Num(f) => Some(Ok(Value::Num(f.log2()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).log2()))),
            Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::Num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).log2(),
            ))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(*r);
                let ln2 = 2.0f64.ln();
                Some(Ok(Value::Complex(mag / ln2, arg / ln2)))
            }
            _ => Some(Ok(Value::Num(f64::NAN))),
        },
        "log10" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).log10()))),
            Value::BigInt(i) => Some(Ok(Value::Num(i.to_f64().unwrap_or(f64::INFINITY).log10()))),
            Value::Num(f) => Some(Ok(Value::Num(f.log10()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).log10()))),
            Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::Num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).log10(),
            ))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(*r);
                let ln10 = 10.0f64.ln();
                Some(Ok(Value::Complex(mag / ln10, arg / ln10)))
            }
            _ => Some(Ok(Value::Num(f64::NAN))),
        },
        "exp" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).exp()))),
            Value::BigInt(i) => Some(Ok(Value::Num(i.to_f64().unwrap_or(f64::INFINITY).exp()))),
            Value::Num(f) => Some(Ok(Value::Num(f.exp()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).exp()))),
            Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::Num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).exp(),
            ))),
            Value::Complex(r, i) => {
                // exp(a+bi) = exp(a) * (cos(b) + i*sin(b))
                let ea = r.exp();
                Some(Ok(Value::Complex(ea * i.cos(), ea * i.sin())))
            }
            _ => Some(Ok(Value::Num(f64::NAN))),
        },
        "atan2" => {
            // .atan2 with no args defaults to x=1
            let y = match target {
                Value::Int(i) => *i as f64,
                Value::BigInt(i) => i.to_f64().unwrap_or(f64::INFINITY),
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => {
                    n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)
                }
                Value::Str(s) => match s.parse::<f64>() {
                    Ok(f) => f,
                    Err(_) => return Some(Ok(Value::Num(f64::NAN))),
                },
                _ => return None, // fall through to runtime for user types
            };
            Some(Ok(Value::Num(y.atan2(1.0))))
        }
        "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "sec" | "cosec" | "cotan" | "asec"
        | "acosec" | "acotan" | "sinh" | "cosh" | "tanh" | "sech" | "cosech" | "cotanh"
        | "asinh" | "acosh" | "atanh" | "asech" | "acosech" | "acotanh" => {
            // Complex: dispatch to complex trig
            if let Value::Complex(re, im) = target {
                let result = complex_trig(method, *re, *im);
                return Some(Ok(Value::Complex(result.0, result.1)));
            }
            let x = match target {
                Value::Int(i) => *i as f64,
                Value::BigInt(i) => i.to_f64().unwrap_or(f64::INFINITY),
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => {
                    n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)
                }
                Value::Str(s) => match s.parse::<f64>() {
                    Ok(f) => f,
                    Err(_) => return Some(Ok(Value::Num(f64::NAN))),
                },
                _ => return None, // fall through to runtime for user types
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
                "asinh" => x.asinh(),
                "acosh" => x.acosh(),
                "atanh" => x.atanh(),
                "asech" => (1.0 / x).acosh(),
                "acosech" => (1.0 / x).asinh(),
                "acotanh" => (1.0 / x).atanh(),
                _ => f64::NAN,
            };
            Some(Ok(Value::Num(result)))
        }
        "Rat" => match target {
            Value::Rat(_, _) => Some(Ok(target.clone())),
            Value::BigRat(_, _) => Some(Ok(target.clone())),
            Value::Int(i) => Some(Ok(make_rat(*i, 1))),
            Value::Num(f) => {
                let denom = 1_000_000i64;
                let numer = (f * denom as f64).round() as i64;
                Some(Ok(make_rat(numer, denom)))
            }
            Value::FatRat(n, d) => Some(Ok(make_rat(*n, *d))),
            Value::Str(s) => {
                // Try parsing as rational: "3/5", "0.2", or plain integer
                if let Some((n_str, d_str)) = s.split_once('/') {
                    let n = n_str.trim().parse::<i64>().unwrap_or(0);
                    let d = d_str.trim().parse::<i64>().unwrap_or(1);
                    Some(Ok(make_rat(n, if d == 0 { 1 } else { d })))
                } else if s.contains('.') {
                    // Decimal string: count decimal places for exact rational
                    let trimmed = s.trim();
                    let negative = trimmed.starts_with('-');
                    let abs_str = if negative { &trimmed[1..] } else { trimmed };
                    if let Some((int_part, frac_part)) = abs_str.split_once('.') {
                        let frac_digits = frac_part.len() as u32;
                        let denom = 10i64.pow(frac_digits);
                        let int_val = int_part.parse::<i64>().unwrap_or(0);
                        let frac_val = frac_part.parse::<i64>().unwrap_or(0);
                        let numer = int_val * denom + frac_val;
                        let numer = if negative { -numer } else { numer };
                        Some(Ok(make_rat(numer, denom)))
                    } else {
                        Some(Ok(make_rat(0, 1)))
                    }
                } else {
                    let n = s.trim().parse::<i64>().unwrap_or(0);
                    Some(Ok(make_rat(n, 1)))
                }
            }
            Value::Complex(r, im) => {
                if im.abs() <= 1e-15 {
                    let denom = 1_000_000i64;
                    let numer = (r * denom as f64).round() as i64;
                    Some(Ok(make_rat(numer, denom)))
                } else {
                    Some(Err(RuntimeError::new(
                        "Cannot convert Complex to Real: imaginary part not zero",
                    )))
                }
            }
            Value::Instance { .. } => None,
            Value::Package(_) => Some(Ok(make_rat(0, 1))),
            _ => Some(Ok(make_rat(0, 1))),
        },
        "FatRat" => match target {
            Value::FatRat(_, _) => Some(Ok(target.clone())),
            Value::Rat(n, d) => Some(Ok(Value::FatRat(*n, *d))),
            Value::BigRat(_, _) => Some(Ok(target.clone())),
            Value::Int(i) => Some(Ok(Value::FatRat(*i, 1))),
            Value::BigInt(i) => Some(Ok(make_big_rat((**i).clone(), num_bigint::BigInt::from(1)))),
            Value::Num(f) => {
                let denom = 1_000_000i64;
                let numer = (f * denom as f64).round() as i64;
                Some(Ok(Value::FatRat(numer, denom)))
            }
            Value::Str(s) => {
                if let Ok(f) = s.parse::<f64>() {
                    let denom = 1_000_000i64;
                    let numer = (f * denom as f64).round() as i64;
                    Some(Ok(Value::FatRat(numer, denom)))
                } else {
                    Some(Ok(Value::FatRat(0, 1)))
                }
            }
            Value::Package(_) => Some(Ok(Value::FatRat(0, 1))),
            _ => Some(Ok(Value::FatRat(0, 1))),
        },
        "tree" => match target {
            Value::Array(items, ..) => Some(Ok(Value::array(tree_recursive(items)))),
            _ => Some(Ok(target.clone())),
        },
        "encode" => {
            let s = target.to_string_value();
            let bytes: Vec<Value> = s.as_bytes().iter().map(|&b| Value::Int(b as i64)).collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(bytes));
            Some(Ok(Value::make_instance(Symbol::intern("Buf"), attrs)))
        }
        "sink" => match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => {
                if let Some(ex) = attributes.get("exception") {
                    let mut err = RuntimeError::new(ex.to_string_value());
                    err.exception = Some(Box::new(ex.clone()));
                    Some(Err(err))
                } else {
                    Some(Ok(Value::Nil))
                }
            }
            _ => Some(Ok(Value::Nil)),
        },
        "item" => match target {
            Value::Array(items, kind) => Some(Ok(Value::Array(items.clone(), kind.itemize()))),
            other => Some(Ok(Value::Scalar(Box::new(other.clone())))),
        },
        "race" | "hyper" => {
            // Single-threaded: just materialize into an array
            let items = runtime::value_to_list(target);
            Some(Ok(Value::array(items)))
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
            Some(Ok(Value::Uni {
                form: method.to_string(),
                text: normalized,
            }))
        }
        _ => None,
    }
}

/// Recursively apply `.tree` to nested arrays.
fn tree_recursive(items: &[Value]) -> Vec<Value> {
    items
        .iter()
        .map(|v| match v {
            Value::Array(inner, ..) => Value::array(tree_recursive(inner)),
            other => other.clone(),
        })
        .collect()
}

/// Convert a Value to a string for Unicode normalization.
/// If the value is an Array of Int (Uni-like), convert codepoints to a string.
fn uni_or_str(target: &Value) -> String {
    match target {
        Value::Array(items, ..) if items.iter().all(|v| matches!(v, Value::Int(_))) => items
            .iter()
            .filter_map(|v| match v {
                Value::Int(cp) => char::from_u32(*cp as u32),
                _ => None,
            })
            .collect(),
        _ => target.to_string_value(),
    }
}

/// Unicode case folding for `.fc` and `fc()`.
/// This applies full fold behavior by combining lowercase conversion with
/// compatibility decomposition, Greek ypogegrammeni expansion, and recomposition.
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

/// Complex trig function dispatch: returns (real, imag)
pub(crate) fn complex_trig(method: &str, a: f64, b: f64) -> (f64, f64) {
    match method {
        "sin" => (a.sin() * b.cosh(), a.cos() * b.sinh()),
        "cos" => (a.cos() * b.cosh(), -(a.sin() * b.sinh())),
        "tan" => complex_div(complex_trig("sin", a, b), complex_trig("cos", a, b)),
        "asin" => {
            // asin(z) = -i * ln(iz + sqrt(1 - z^2))
            let (re, im) = complex_asin(a, b);
            (re, im)
        }
        "acos" => {
            // acos(z) = pi/2 - asin(z)
            let (re, im) = complex_asin(a, b);
            (std::f64::consts::FRAC_PI_2 - re, -im)
        }
        "atan" => {
            // atan(z) = i/2 * ln((1-iz)/(1+iz))
            let (re, im) = complex_atan(a, b);
            (re, im)
        }
        "sec" => complex_recip(complex_trig("cos", a, b)),
        "cosec" => complex_recip(complex_trig("sin", a, b)),
        "cotan" => complex_div(complex_trig("cos", a, b), complex_trig("sin", a, b)),
        "asec" => complex_trig("acos", complex_recip((a, b)).0, complex_recip((a, b)).1),
        "acosec" => complex_trig("asin", complex_recip((a, b)).0, complex_recip((a, b)).1),
        "acotan" => complex_trig("atan", complex_recip((a, b)).0, complex_recip((a, b)).1),
        "sinh" => (a.sinh() * b.cos(), a.cosh() * b.sin()),
        "cosh" => (a.cosh() * b.cos(), a.sinh() * b.sin()),
        "tanh" => complex_div(complex_trig("sinh", a, b), complex_trig("cosh", a, b)),
        "sech" => complex_recip(complex_trig("cosh", a, b)),
        "cosech" => complex_recip(complex_trig("sinh", a, b)),
        "cotanh" => complex_div(complex_trig("cosh", a, b), complex_trig("sinh", a, b)),
        "asinh" => {
            // asinh(z) = ln(z + sqrt(z^2 + 1))
            let (z2r, z2i) = complex_mul((a, b), (a, b));
            let (sr, si) = complex_sqrt(z2r + 1.0, z2i);
            complex_ln(a + sr, b + si)
        }
        "acosh" => {
            // acosh(z) = ln(z + sqrt(z^2 - 1))
            let (z2r, z2i) = complex_mul((a, b), (a, b));
            let (sr, si) = complex_sqrt(z2r - 1.0, z2i);
            complex_ln(a + sr, b + si)
        }
        "atanh" => {
            // atanh(z) = 1/2 * ln((1+z)/(1-z))
            let num = (1.0 + a, b);
            let den = (1.0 - a, -b);
            let (qr, qi) = complex_div(num, den);
            let (lr, li) = complex_ln(qr, qi);
            (0.5 * lr, 0.5 * li)
        }
        "asech" => complex_trig("acosh", complex_recip((a, b)).0, complex_recip((a, b)).1),
        "acosech" => complex_trig("asinh", complex_recip((a, b)).0, complex_recip((a, b)).1),
        "acotanh" => complex_trig("atanh", complex_recip((a, b)).0, complex_recip((a, b)).1),
        _ => (f64::NAN, 0.0),
    }
}

fn complex_mul(a: (f64, f64), b: (f64, f64)) -> (f64, f64) {
    (a.0 * b.0 - a.1 * b.1, a.0 * b.1 + a.1 * b.0)
}

fn complex_div(a: (f64, f64), b: (f64, f64)) -> (f64, f64) {
    let denom = b.0 * b.0 + b.1 * b.1;
    if denom == 0.0 {
        return (f64::NAN, f64::NAN);
    }
    (
        (a.0 * b.0 + a.1 * b.1) / denom,
        (a.1 * b.0 - a.0 * b.1) / denom,
    )
}

fn complex_recip(z: (f64, f64)) -> (f64, f64) {
    complex_div((1.0, 0.0), z)
}

fn complex_sqrt(re: f64, im: f64) -> (f64, f64) {
    let r = (re * re + im * im).sqrt();
    let sr = ((r + re) / 2.0).sqrt();
    let si = ((r - re) / 2.0).sqrt();
    (sr, if im >= 0.0 { si } else { -si })
}

fn complex_ln(re: f64, im: f64) -> (f64, f64) {
    let r = (re * re + im * im).sqrt();
    (r.ln(), im.atan2(re))
}

fn complex_asin(a: f64, b: f64) -> (f64, f64) {
    // asin(z) = -i * ln(iz + sqrt(1 - z^2))
    let iz = (-b, a); // i * z
    let (z2r, z2i) = complex_mul((a, b), (a, b));
    let (sr, si) = complex_sqrt(1.0 - z2r, -z2i);
    let (lr, li) = complex_ln(iz.0 + sr, iz.1 + si);
    // -i * (lr + li*i) = li + (-lr)*i => (li, -lr)
    (li, -lr)
}

fn complex_atan(a: f64, b: f64) -> (f64, f64) {
    // atan(z) = i/2 * ln((1-iz)/(1+iz))
    let iz = (-b, a);
    let num = (1.0 - iz.0, -iz.1);
    let den = (1.0 + iz.0, iz.1);
    let (qr, qi) = complex_div(num, den);
    let (lr, li) = complex_ln(qr, qi);
    // i/2 * (lr + li*i) = (i*lr + i*li*i)/2 = (-li/2 + lr/2*i) => (-li/2, lr/2)
    (-li / 2.0, lr / 2.0)
}

/// Coerce a value to a native integer type (e.g. `.byte()`, `.int8()`, `.uint32()`).
/// Wraps out-of-range values using modular arithmetic.
fn native_int_coerce_method(target: &Value, type_name: &str) -> Result<Value, RuntimeError> {
    use num_bigint::BigInt as NumBigInt;

    let big_val: NumBigInt = match target {
        Value::Int(i) => NumBigInt::from(*i),
        Value::BigInt(n) => (**n).clone(),
        Value::Num(f) => {
            if f.is_nan() || f.is_infinite() {
                return Err(RuntimeError::new(format!(
                    "Cannot coerce {} to {}",
                    f, type_name
                )));
            }
            NumBigInt::from(*f as i128)
        }
        Value::Str(s) => {
            if let Ok(i) = s.parse::<i128>() {
                NumBigInt::from(i)
            } else if let Ok(f) = s.parse::<f64>() {
                NumBigInt::from(f as i128)
            } else {
                return Err(RuntimeError::new(format!(
                    "Cannot coerce '{}' to {}",
                    s, type_name
                )));
            }
        }
        Value::Bool(b) => NumBigInt::from(if *b { 1 } else { 0 }),
        Value::Rat(n, d) => {
            if *d == 0 {
                return Err(RuntimeError::new("Division by zero in Rat coercion"));
            }
            NumBigInt::from(*n / *d)
        }
        _ => {
            // Try to coerce through string → parse
            let s = target.to_string_value();
            if let Ok(i) = s.parse::<i128>() {
                NumBigInt::from(i)
            } else {
                NumBigInt::from(0)
            }
        }
    };

    let wrapped = runtime::native_types::wrap_native_int(type_name, &big_val);

    // Convert back to Value
    if let Some(i) = wrapped.to_i64() {
        Ok(Value::Int(i))
    } else {
        Ok(Value::bigint(wrapped))
    }
}
