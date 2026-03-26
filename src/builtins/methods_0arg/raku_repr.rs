use crate::value::{ArrayKind, RuntimeError, Value};
use num_traits::Signed;
use std::sync::Arc;

use super::range_endpoint_display;

/// Helper for .raku representation of a value
fn is_self_array_ref_marker(v: &Value) -> bool {
    matches!(v, Value::Pair(name, _) if name == "__mutsu_self_array_ref")
}

fn raku_array_wrap_counted(inner: &str, kind: ArrayKind, count: usize) -> String {
    match kind {
        ArrayKind::Array | ArrayKind::Shaped => format!("[{}]", inner),
        ArrayKind::List => {
            if count == 1 {
                format!("({},)", inner)
            } else {
                format!("({})", inner)
            }
        }
        ArrayKind::ItemArray => format!("$[{}]", inner),
        ArrayKind::ItemList => {
            if inner.is_empty() {
                "$( )".to_string()
            } else if count == 1 {
                format!("$({},)", inner)
            } else {
                format!("$({})", inner)
            }
        }
    }
}

fn raku_array_wrap(inner: &str, kind: ArrayKind) -> String {
    // Fallback without count (used for self-referencing snapshot rendering)
    raku_array_wrap_counted(inner, kind, 2) // count=2 to avoid trailing comma
}

/// Render a value for `.raku` but strip itemization (Scalar container).
/// In Raku, `@a.raku` renders itemized elements without the `$` prefix:
///   my @a = $[1,2,3]; @a.raku  # [[1, 2, 3],]  (not $[1, 2, 3])
fn raku_value_as_element(v: &Value) -> String {
    match v {
        Value::Array(items, kind) => {
            let decontainerized = match kind {
                ArrayKind::ItemArray => ArrayKind::Array,
                ArrayKind::ItemList => ArrayKind::List,
                other => *other,
            };
            // Re-wrap the value with de-itemized kind for rendering
            let decontainerized_val = Value::Array(items.clone(), decontainerized);
            raku_value(&decontainerized_val)
        }
        _ => raku_value(v),
    }
}

/// Render array contents for `.raku`, extracted from `raku_value` so that
/// cycle detection can happen before entering the rendering loop.
fn raku_value_array(items: &[Value], kind: ArrayKind, v: &Value) -> String {
    // Shaped arrays: Array.new(:shape(d1, d2), [row1], [row2])
    if kind == crate::value::ArrayKind::Shaped
        && let Some(shape) = crate::runtime::utils::shaped_array_shape(v)
        && shape.len() > 1
    {
        let shape_str = shape
            .iter()
            .map(|d| d.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        let rows = items
            .iter()
            .map(raku_value_as_element)
            .collect::<Vec<_>>()
            .join(", ");
        return format!("Array.new(:shape({}), {})", shape_str, rows);
    }
    let snapshot = |k: ArrayKind| {
        let inner = items
            .iter()
            .filter(|item| !is_self_array_ref_marker(item))
            .map(raku_value_as_element)
            .collect::<Vec<_>>()
            .join(", ");
        raku_array_wrap(&inner, k)
    };
    let rendered: Vec<_> = items
        .iter()
        .map(|item| {
            if is_self_array_ref_marker(item) {
                snapshot(kind)
            } else {
                raku_value_as_element(item)
            }
        })
        .collect();
    let count = rendered.len();
    let inner = rendered.join(", ");
    raku_array_wrap_counted(&inner, kind, count)
}

pub fn raku_value(v: &Value) -> String {
    // Cycle detection for recursive data structures: track Arc pointers
    // that we're currently rendering. If we encounter the same pointer
    // again, emit a placeholder instead of recursing infinitely.
    thread_local! {
        static SEEN_PTRS: std::cell::RefCell<Vec<usize>> = const { std::cell::RefCell::new(Vec::new()) };
    }
    match v {
        Value::Array(items, kind) => {
            let ptr = Arc::as_ptr(items) as usize;
            let is_cycle = SEEN_PTRS.with(|seen| seen.borrow().contains(&ptr));
            if is_cycle {
                return raku_array_wrap("...", *kind);
            }
            SEEN_PTRS.with(|seen| seen.borrow_mut().push(ptr));
            let result = raku_value_array(items, *kind, v);
            SEEN_PTRS.with(|seen| {
                let mut s = seen.borrow_mut();
                if let Some(pos) = s.iter().rposition(|p| *p == ptr) {
                    s.remove(pos);
                }
            });
            result
        }
        Value::Seq(items) => {
            let inner = items.iter().map(raku_value).collect::<Vec<_>>().join(", ");
            if items.len() == 1 {
                format!("({},)", inner)
            } else {
                format!("({})", inner)
            }
        }
        Value::Slip(items) => {
            let inner = items.iter().map(raku_value).collect::<Vec<_>>().join(", ");
            format!("slip({})", inner)
        }
        Value::Str(s) => {
            let escaped = s
                .replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace('$', "\\$")
                .replace('@', "\\@")
                .replace('%', "\\%")
                .replace('&', "\\&")
                .replace('{', "\\{");
            format!("\"{}\"", escaped)
        }
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
        Value::FatRat(n, d) => format!("FatRat.new({}, {})", n, d),
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
                // Num.raku must include 'e' so it round-trips as Num, not Rat
                let s = format!("{}", f);
                if s.contains('e') || s.contains('E') {
                    s
                } else {
                    format!("{}e0", s)
                }
            }
        }
        Value::Complex(r, i) => format!("<{}>", crate::value::format_complex(*r, *i)),
        Value::Pair(key, value) => {
            let ident_like = !key.is_empty()
                && key
                    .chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '-');
            if ident_like {
                match value.as_ref() {
                    Value::Bool(true) => format!(":{}", key),
                    _ => format!(":{}({})", key, raku_value(value)),
                }
            } else {
                format!(
                    "{} => {}",
                    raku_value(&Value::str(key.clone())),
                    raku_value(value)
                )
            }
        }
        Value::ValuePair(key, value) => {
            if let Value::Str(key_str) = key.as_ref() {
                let ident_like = !key_str.is_empty()
                    && key_str
                        .chars()
                        .all(|c| c.is_alphanumeric() || c == '_' || c == '-');
                if ident_like {
                    return match value.as_ref() {
                        Value::Bool(true) => format!(":{}", key_str),
                        _ => format!(":{}({})", key_str, raku_value(value)),
                    };
                }
            }
            let key_repr = match key.as_ref() {
                // Non-string keys that would be ambiguous as barewords need parens
                Value::Pair(_, _)
                | Value::ValuePair(_, _)
                | Value::Package(_)
                | Value::Nil
                | Value::Bool(_) => format!("({})", raku_value(key)),
                _ => raku_value(key),
            };
            format!("{} => {}", key_repr, raku_value(value))
        }
        Value::Hash(map) => {
            // Cycle detection for recursive hash structures
            thread_local! {
                static SEEN_HASH_PTRS: std::cell::RefCell<Vec<usize>> = const { std::cell::RefCell::new(Vec::new()) };
            }
            let ptr = Arc::as_ptr(map) as usize;
            let is_cycle = SEEN_HASH_PTRS.with(|seen| seen.borrow().contains(&ptr));
            if is_cycle {
                return "{...}".to_string();
            }
            SEEN_HASH_PTRS.with(|seen| seen.borrow_mut().push(ptr));
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
                        let repr = if matches!(v, Value::Nil) {
                            "Any".to_string()
                        } else {
                            raku_value(v)
                        };
                        format!(":{}({})", k, repr)
                    }
                })
                .collect();
            SEEN_HASH_PTRS.with(|seen| {
                let mut s = seen.borrow_mut();
                if let Some(pos) = s.iter().rposition(|p| *p == ptr) {
                    s.remove(pos);
                }
            });
            format!("{{{}}}", parts.join(", "))
        }
        Value::Nil => "Nil".to_string(),
        Value::Package(name) => name.resolve().to_string(),
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
            let start_repr = match start.as_ref() {
                Value::Whatever | Value::HyperWhatever => "-Inf".to_string(),
                _ => raku_value(start),
            };
            let end_repr = match end.as_ref() {
                Value::Whatever | Value::HyperWhatever => "Inf".to_string(),
                _ => raku_value(end),
            };
            let start_sep = if *excl_start { "^.." } else { ".." };
            let end_sep = if *excl_end { "^" } else { "" };
            format!("{}{}{}{}", start_repr, start_sep, end_sep, end_repr)
        }
        Value::Capture { positional, named } => {
            let mut parts = Vec::new();
            for v in positional {
                // Pairs appearing as positional items in a Capture are rendered
                // with quoted key syntax ("key" => value) to distinguish them
                // from named arguments which use colonpair syntax (:key(value)).
                match v {
                    Value::Pair(k, val) => {
                        parts.push(format!(
                            "{} => {}",
                            raku_value(&Value::str(k.clone())),
                            raku_value(val)
                        ));
                    }
                    Value::ValuePair(k, val) => {
                        parts.push(format!("{} => {}", raku_value(k), raku_value(val)));
                    }
                    _ => parts.push(raku_value(v)),
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
                    parts.push(format!(":{}({})", k, raku_value(v)));
                }
            }
            format!("\\({})", parts.join(", "))
        }
        other => other.to_string_value(),
    }
}

pub(super) fn hash_pick_item(key: &str, value: &Value) -> Value {
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

/// Coerce a value to a native integer type (e.g. `.byte()`, `.int8()`, `.uint32()`).
/// Wraps out-of-range values using modular arithmetic.
pub(super) fn native_int_coerce_method(
    target: &Value,
    type_name: &str,
) -> Result<Value, RuntimeError> {
    use num_bigint::BigInt as NumBigInt;
    use num_traits::ToPrimitive;

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
            // Try to coerce through string -> parse
            let s = target.to_string_value();
            if let Ok(i) = s.parse::<i128>() {
                NumBigInt::from(i)
            } else {
                NumBigInt::from(0)
            }
        }
    };

    let wrapped = crate::runtime::native_types::wrap_native_int(type_name, &big_val);

    // Convert back to Value
    if let Some(i) = wrapped.to_i64() {
        Ok(Value::Int(i))
    } else {
        Ok(Value::bigint(wrapped))
    }
}
