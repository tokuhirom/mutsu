/// Type coercion methods: self, clone, defined, DEFINITE, WHICH, Bool, Str, Int, UInt,
/// Num, Real, Numeric, Bridge
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};
use num_traits::{ToPrimitive, Zero};
use std::sync::Arc;

use super::parse_raku_int_from_str;

/// Build the `X::Str::Numeric` attribute map for a string that cannot be
/// numified, deriving `pos`/`reason` from the same analyzer the numeric
/// operators use (so `"5 foo"` reports `trailing characters after number` at
/// pos 1, not a blanket "must begin with valid digits" at pos 0), and the
/// matching `source-indicator`.
fn str_numeric_exception_attrs(s: &str) -> std::collections::HashMap<String, Value> {
    let (pos, reason) = crate::runtime::str_numeric::str_numeric_failure(s).unwrap_or((
        0,
        "base-10 number must begin with valid digits or '.'".to_string(),
    ));
    let mut ex_attrs = std::collections::HashMap::new();
    ex_attrs.insert("source".to_string(), Value::str(s.to_string()));
    ex_attrs.insert("reason".to_string(), Value::str(reason.clone()));
    ex_attrs.insert("pos".to_string(), Value::int(pos as i64));
    let source_indicator = crate::runtime::str_numeric::build_source_indicator(s, pos);
    ex_attrs.insert(
        "source-indicator".to_string(),
        Value::str(source_indicator.clone()),
    );
    // Include the `⏏` position marker, matching Rakudo:
    // `Cannot convert string to number: trailing characters after number
    //  in '5⏏ foo' (indicated by ⏏)`.
    ex_attrs.insert(
        "message".to_string(),
        Value::str(format!(
            "Cannot convert string to number: {reason} {source_indicator}"
        )),
    );
    ex_attrs
}

/// Build a lazy `Failure` wrapping an `X::Str::Numeric` exception for a string
/// that cannot be numified — the shape raku's `.Int`/`.Num` produce on a bad
/// string (the exception only fires when the Failure is sunk/used).
pub(crate) fn str_numeric_failure(s: &str) -> Value {
    let ex = Value::make_instance(
        Symbol::intern("X::Str::Numeric"),
        str_numeric_exception_attrs(s),
    );
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    failure_attrs.insert("handled".to_string(), Value::FALSE);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// Build the `X::Numeric::CannotConvert` Failure raised when a non-finite Num
/// (`NaN`/`Inf`/`-Inf`) is coerced to `Int`.
fn cannot_convert_to_int_failure(source: &Value, f: f64) -> Value {
    let label = if f.is_nan() {
        "NaN"
    } else if f.is_sign_positive() {
        "Inf"
    } else {
        "-Inf"
    };
    let msg = format!("Cannot convert {label} to Int: not a finite number");
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg));
    attrs.insert("source".to_string(), source.clone());
    attrs.insert("target".to_string(), Value::str_from("Int"));
    let ex = Value::make_instance(Symbol::intern("X::Numeric::CannotConvert"), attrs);
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// Truncate a numeric `Value` toward zero to an `Int`, returning `None` for a
/// non-numeric value. A non-finite `Num` or a zero-denominator `Rational`
/// yields the same lazy `Failure` a direct `.Int` would. Used both by the
/// `.Int` coercion arm and by `Str.Int` after parsing a numeric string form
/// (radix `:16<ff>`, rational `3/4`, ...) through the full numeric grammar,
/// mirroring raku's "numify then truncate" semantics.
fn numeric_to_int(target: &Value) -> Option<Value> {
    Some(match target.view() {
        ValueView::Int(i) => Value::int(i),
        ValueView::BigInt(_) => target.clone(),
        ValueView::Num(f) => {
            if f.is_nan() || f.is_infinite() {
                cannot_convert_to_int_failure(target, f)
            } else {
                Value::int(f as i64)
            }
        }
        ValueView::Rat(_, 0) | ValueView::FatRat(_, 0) => {
            RuntimeError::divide_by_zero_failure_for_method("Int", "Rational")
        }
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => Value::int(n / d),
        ValueView::BigRat(_, d) if d.is_zero() => {
            RuntimeError::divide_by_zero_failure_for_method("Int", "Rational")
        }
        ValueView::BigRat(n, d) => Value::int((n / d).to_i64().unwrap_or(i64::MAX)),
        ValueView::Complex(r, _) => Value::int(r as i64),
        _ => return None,
    })
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    // A Range numerifies to its element count (`.elems`): `.Int`/`.Numeric`/
    // `.Real` yield that count as an `Int`, `.Num` as a `Num`. An infinite
    // range yields `Inf` for the real-valued coercions and fails for `.Int`
    // ("Cannot convert Inf to Int"). Handle this before the per-method arms,
    // which only know how to numerify scalar types.
    if target.is_range() && matches!(method, "Int" | "Numeric" | "Real" | "Num") {
        return Some(Some(range_numeric_coercion(target, method)));
    }
    // A lazy (infinite-backed) array numerifies to its element count, which it
    // cannot report: raku throws `X::Cannot::Lazy` (`Cannot .elems a lazy list`)
    // for every numeric coercion (`.Int`/`.Numeric`/`.Real`/`.Num`/prefix `+`).
    // Guard before the per-method arms, which would return the capped backing
    // length. (`.elems` itself is handled in `dispatch_core_numeric`.)
    if matches!(method, "Int" | "Numeric" | "Real" | "Num") && super::is_lazy_count_source(target) {
        return Some(super::range_elems_lazy_failure("elems"));
    }
    // `.Buf` / `.Blob` on any byte-string (Buf/Blob/utf8/blob8/...) returns a
    // value of the requested role carrying the same bytes. `utf8` (what
    // `.encode` produces) does Blob, so `.Buf`/`.Blob` reinterpret those bytes
    // as a plain `Buf[uint8]` / `Blob[uint8]`. Humming-Bird's HTTPServer uses
    // `"\r\n".encode.Buf` to build its constant delimiters.
    if matches!(method, "Buf" | "Blob")
        && let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
    {
        let bytes = attributes
            .as_map()
            .get("bytes")
            .cloned()
            .unwrap_or_else(|| Value::array(Vec::new()));
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("bytes".to_string(), bytes);
        let target_class = if method == "Buf" { "Buf" } else { "Blob" };
        return Some(Some(Ok(Value::make_instance(
            Symbol::intern(target_class),
            attrs,
        ))));
    }
    // A numeric coercion method invoked on a *type object* of the same type
    // (`Int.Int`, `Num.Num`, `Complex.Complex`) is the identity coercion: raku
    // defines e.g. `method Int() { self }`, so for an undefined invocant (a
    // type object) the result is that same type object rather than a "No such
    // method" error. Zef's URI parser relies on `($auth<port> // Int).Int`
    // yielding the `Int` type object when the port is absent.
    //
    // Only `Int`/`Num`/`Complex` are handled here: raku returns the type object
    // cleanly for these, but `Rat.Rat`/`FatRat.FatRat` throw "must be an object
    // instance" (no `method Rat() { self }`), so those are left to fall through.
    if let ValueView::Package(name) = target.view()
        && matches!(method, "Int" | "Num" | "Complex")
        && name.resolve() == method
    {
        return Some(Some(Ok(target.clone())));
    }
    match method {
        "self" => {
            // For unhandled Failures, .self throws the exception
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
                && class_name == "Failure"
                && !target.is_failure_handled()
                && let Some(ex) = attributes.as_map().get("exception")
            {
                let msg = ex.to_string_value();
                let mut err = crate::value::RuntimeError::new(msg);
                err.exception = Some(Box::new(ex.clone()));
                return Some(Some(Err(err)));
            }
            Some(Some(Ok(target.clone())))
        }
        "clone" => {
            match target.view() {
                ValueView::Package(_) | ValueView::Nil => Some(Some(Ok(target.clone()))),
                // Clone the whole `ArrayData`, not just its elements, so
                // per-slot metadata (`initialized` deletion holes, `default`,
                // `shape`, element type) survives the copy — e.g.
                // `@a[3]:delete; my @b := @a.clone` keeps `@b[3]:exists` False
                // (S02-types/array.t test 108). Elements are `Value`-cloned
                // (shared handles), matching a shallow `.clone` — EXCEPT for a
                // shaped array, whose rows are its own storage: rakudo's
                // `.clone` gives independent containers per dimension
                // (rakudo#3334, S09-multidim/methods.t), and with in-place
                // element writes (container identity §3) shared rows would
                // alias `@a[1;1] = v` into the clone.
                ValueView::Array(items, kind) => {
                    let mut data = (**items).clone();
                    if kind == crate::value::ArrayKind::Shaped || data.shape.is_some() {
                        fn clone_rows(v: &Value) -> Value {
                            match v.view() {
                                ValueView::Array(rows, k) => {
                                    let mut d = (**rows).clone();
                                    for item in d.items.iter_mut() {
                                        *item = clone_rows(item);
                                    }
                                    Value::array_with_kind(crate::gc::Gc::new(d), k)
                                }
                                _ => v.clone(),
                            }
                        }
                        for item in data.items.iter_mut() {
                            *item = clone_rows(item);
                        }
                    }
                    Some(Some(Ok(Value::array_with_kind(
                        crate::gc::Gc::new(data),
                        kind,
                    ))))
                }
                ValueView::Hash(map) => Some(Some(Ok(Value::hash_with_data(Value::hash_arc(
                    (**map).clone(),
                ))))),
                ValueView::Set(data, mutable) => Some(Some(Ok(Value::set_parts(
                    crate::gc::Gc::new((**data).clone()),
                    mutable,
                )))),
                ValueView::Bag(data, mutable) => Some(Some(Ok(Value::bag_parts(
                    crate::gc::Gc::new((**data).clone()),
                    mutable,
                )))),
                ValueView::Mix(data, mutable) => Some(Some(Ok(Value::mix_parts(
                    crate::gc::Gc::new((**data).clone()),
                    mutable,
                )))),
                ValueView::Sub(data) => {
                    // Clone the sub with a new id so state variables are independent
                    let mut new_data = (**data).clone();
                    new_data.id = crate::value::next_instance_id();
                    Some(Some(Ok(Value::sub_value(crate::gc::Gc::new(new_data)))))
                }
                ValueView::Pair(key, value) => {
                    Some(Some(Ok(Value::pair(key.clone(), value.clone()))))
                }
                ValueView::ValuePair(key, value) => {
                    Some(Some(Ok(Value::value_pair(key.clone(), value.clone()))))
                }
                _ => Some(None), // fall through to slow path for instances etc.
            }
        }
        "defined" => {
            // Calling .defined on a Failure marks it as handled
            if let ValueView::Instance { class_name, .. } = target.view()
                && class_name == "Failure"
            {
                target.mark_failure_handled();
            }
            // For junctions, autothread .defined over eigenstates and collapse
            if let ValueView::Junction { kind, values } = target.view() {
                fn value_defined(v: &Value) -> bool {
                    match v.view() {
                        ValueView::Nil | ValueView::Package(_) => false,
                        ValueView::Slip(items) if items.is_empty() => false,
                        ValueView::Instance { class_name, .. } if class_name == "Failure" => false,
                        ValueView::Junction { kind, values } => {
                            let results: Vec<bool> = values.iter().map(value_defined).collect();
                            collapse_junction(&kind, &results)
                        }
                        _ => true,
                    }
                }
                fn collapse_junction(kind: &crate::value::JunctionKind, results: &[bool]) -> bool {
                    use crate::value::JunctionKind;
                    match kind {
                        JunctionKind::Any => results.iter().any(|&b| b),
                        JunctionKind::All => results.iter().all(|&b| b),
                        JunctionKind::One => results.iter().filter(|&&b| b).count() == 1,
                        JunctionKind::None => results.iter().all(|&b| !b),
                    }
                }
                let results: Vec<bool> = values.iter().map(value_defined).collect();
                let collapsed = collapse_junction(&kind, &results);
                Some(Some(Ok(Value::truth(collapsed))))
            } else {
                Some(Some(Ok(Value::truth(match target.view() {
                    ValueView::Nil | ValueView::Package(_) => false,
                    ValueView::Slip(items) if items.is_empty() => false,
                    ValueView::Instance { class_name, .. } if class_name == "Failure" => false,
                    _ => true,
                }))))
            }
        }
        "DEFINITE" => Some(Some(Ok(Value::truth(match target.view() {
            ValueView::Nil | ValueView::Package(_) | ValueView::CustomType(..) => false,
            ValueView::Slip(items) if items.is_empty() => false,
            ValueView::Instance { class_name, .. } if class_name == "Failure" => false,
            _ => true,
        })))),
        "WHICH" => {
            // Determine if this is a value type (ValueObjAt) or reference type (ObjAt)
            let is_value_type = matches!(
                target.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Rat(_, _)
                    | ValueView::BigRat(_, _)
                    | ValueView::FatRat(_, _)
                    | ValueView::Complex(_, _)
                    | ValueView::Set(_, _)
                    | ValueView::Bag(_, _)
                    | ValueView::Mix(_, _)
                    | ValueView::Junction { .. }
                    | ValueView::Nil
            );
            let which_str = match target.view() {
                ValueView::Package(name) => format!("{}|U{}", name.resolve(), name.id()),
                ValueView::CustomType(c) => {
                    format!("{}|U{}", c.name.resolve(), c.id)
                }
                ValueView::Int(n) => format!("Int|{}", n),
                ValueView::BigInt(n) => format!("Int|{}", *n),
                ValueView::Num(n) => format!("Num|{}", n),
                ValueView::Str(s) => format!("Str|{}", *s),
                ValueView::Bool(b) => format!("Bool|{}", if b { 1 } else { 0 }),
                ValueView::Rat(n, d) => format!("Rat|{}/{}", n, d),
                ValueView::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
                ValueView::Complex(r, i) => format!("Complex|{}+{}i", r, i),
                ValueView::Nil => format!("Nil|U{}", Symbol::intern("Nil").id()),
                ValueView::Set(set, _) => {
                    let mut keys: Vec<&String> = set.iter().collect();
                    keys.sort();
                    use std::hash::{Hash, Hasher};
                    let mut hasher = std::collections::hash_map::DefaultHasher::new();
                    for k in &keys {
                        k.hash(&mut hasher);
                    }
                    format!("Set|{:016X}", hasher.finish())
                }
                ValueView::Bag(bag, _) => {
                    let mut pairs: Vec<(&String, &num_bigint::BigInt)> = bag.iter().collect();
                    pairs.sort_by(|a, b| a.0.cmp(b.0));
                    use std::hash::{Hash, Hasher};
                    let mut hasher = std::collections::hash_map::DefaultHasher::new();
                    for (k, v) in &pairs {
                        k.hash(&mut hasher);
                        v.hash(&mut hasher);
                    }
                    format!("Bag|{:016X}", hasher.finish())
                }
                ValueView::Mix(mix, _) => {
                    let mut pairs: Vec<(&String, &f64)> = mix.iter().collect();
                    pairs.sort_by(|a, b| a.0.cmp(b.0));
                    use std::hash::{Hash, Hasher};
                    let mut hasher = std::collections::hash_map::DefaultHasher::new();
                    for (k, v) in &pairs {
                        k.hash(&mut hasher);
                        v.to_bits().hash(&mut hasher);
                    }
                    format!("Mix|{:016X}", hasher.finish())
                }
                ValueView::Sub(sub_data) => {
                    format!(
                        "{}|{}",
                        runtime::utils::value_type_name(target),
                        sub_data.id
                    )
                }
                ValueView::Regex(pattern) => {
                    format!("Regex|{:p}", Arc::as_ptr(&pattern))
                }
                ValueView::RegexWithAdverbs(a) => {
                    format!("Regex|{:p}", Arc::as_ptr(&a.pattern))
                }
                ValueView::Instance { id, .. } => {
                    format!("{}|{}", runtime::utils::value_type_name(target), id)
                }
                ValueView::Junction { kind, values } => {
                    use std::hash::{Hash, Hasher};
                    let mut hasher = std::collections::hash_map::DefaultHasher::new();
                    // Hash the kind
                    match kind {
                        crate::value::JunctionKind::Any => 0u8.hash(&mut hasher),
                        crate::value::JunctionKind::All => 1u8.hash(&mut hasher),
                        crate::value::JunctionKind::One => 2u8.hash(&mut hasher),
                        crate::value::JunctionKind::None => 3u8.hash(&mut hasher),
                    }
                    // Hash each eigenstate's string representation
                    for v in values.iter() {
                        v.to_string_value().hash(&mut hasher);
                    }
                    format!("Junction|{:016X}", hasher.finish())
                }
                ValueView::Seq(items) => {
                    format!("Seq|{:p}", Arc::as_ptr(&items))
                }
                ValueView::Slip(items) => {
                    format!("Slip|{:p}", Arc::as_ptr(&items))
                }
                ValueView::Array(items, ..) => {
                    format!("Array|{:p}", crate::gc::Gc::as_ptr(&items))
                }
                ValueView::Hash(map) => {
                    format!("Hash|{:p}", crate::gc::Gc::as_ptr(&map))
                }
                ValueView::Promise(p) => {
                    format!("Promise|{:p}", p.arc_ptr())
                }
                ValueView::Channel(c) => {
                    format!("Channel|{:p}", c.arc_ptr())
                }
                ValueView::Whatever => {
                    use std::sync::atomic::{AtomicU64, Ordering};
                    static WHATEVER_ID: AtomicU64 = AtomicU64::new(0);
                    // Whatever is a type object singleton
                    let id = WHATEVER_ID.load(Ordering::Relaxed);
                    if id == 0 {
                        WHATEVER_ID.store(1, Ordering::Relaxed);
                    }
                    format!("Whatever|U{}", WHATEVER_ID.load(Ordering::Relaxed))
                }
                _ => {
                    // Fallback: use a global counter to ensure uniqueness
                    // This is not ideal since the same value will get different IDs
                    // on repeated calls, but it prevents false identity collisions.
                    use std::sync::atomic::{AtomicU64, Ordering};
                    static COUNTER: AtomicU64 = AtomicU64::new(1);
                    format!(
                        "{}|{}",
                        runtime::utils::value_type_name(target),
                        COUNTER.fetch_add(1, Ordering::Relaxed)
                    )
                }
            };
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("WHICH".to_string(), Value::str(which_str));
            let objat_class = if is_value_type { "ValueObjAt" } else { "ObjAt" };
            Some(Some(Ok(Value::make_instance(
                Symbol::intern(objat_class),
                attrs,
            ))))
        }
        "Bool" => {
            if matches!(target.view(), ValueView::Instance { .. })
                && (target.does_check("Real") || target.does_check("Numeric"))
            {
                Some(None)
            } else if matches!(
                target.view(),
                ValueView::Regex(_)
                    | ValueView::RegexWithAdverbs(..)
                    | ValueView::Routine { is_regex: true, .. }
            ) {
                // Regex.Bool needs to smartmatch against $_, which requires
                // runtime context. Fall through to the runtime handler.
                Some(None)
            } else {
                // Calling .Bool on a Failure marks it as handled
                if let ValueView::Instance { class_name, .. } = target.view()
                    && class_name == "Failure"
                {
                    target.mark_failure_handled();
                }
                Some(Some(Ok(Value::truth(target.truthy()))))
            }
        }
        "Str" | "Stringy" => Some(match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => {
                // Using a Failure in string context throws the wrapped exception.
                if let Some(ex) = attributes.as_map().get("exception") {
                    Some(Err(RuntimeError::from_exception_value(ex.clone())))
                } else {
                    Some(Err(RuntimeError::new("Failed")))
                }
            }
            // Instant/Duration `.Str` is the same pure rendering as their gist
            // (`value/display.rs::to_string_value`), so handle it natively
            // instead of falling through to the interpreter.
            ValueView::Instance { class_name, .. }
                if class_name == "Instant" || class_name == "Duration" =>
            {
                Some(Ok(Value::str(target.to_string_value())))
            }
            ValueView::Package(_) | ValueView::Instance { .. } => None,
            ValueView::LazyList(_) => None, // fall through to runtime to force the list
            // A lazy (infinite-backed) array stringifies to a bounded `...`
            // placeholder rather than materializing its capped backing.
            ValueView::Array(_, crate::value::ArrayKind::Lazy) => Some(Ok(Value::str_from("..."))),
            ValueView::Enum { .. } => None, // fall through to enum dispatch for string enum support
            ValueView::Str(s) if s.as_str() == "IO::Special" => Some(Ok(Value::str_from(""))),
            ValueView::Rat(_, 0) | ValueView::FatRat(_, 0) => {
                // Zero-denominator Rat/FatRat .Str throws X::Numeric::DivideByZero
                None // fall through to runtime for exception with proper context
            }
            _ => Some(Ok(Value::str(target.to_string_value()))),
        }),
        "Int" => {
            let result = match target.view() {
                ValueView::Int(i) => Value::int(i),
                ValueView::BigInt(_) => target.clone(),
                ValueView::Num(f) => {
                    if f.is_nan() || f.is_infinite() {
                        let msg = format!(
                            "Cannot convert {} to Int: not a finite number",
                            if f.is_nan() {
                                "NaN".to_string()
                            } else if f.is_sign_positive() {
                                "Inf".to_string()
                            } else {
                                "-Inf".to_string()
                            }
                        );
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(msg));
                        attrs.insert("source".to_string(), target.clone());
                        attrs.insert("target".to_string(), Value::str_from("Int"));
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::Numeric::CannotConvert"),
                            attrs,
                        );
                        let mut failure_attrs = std::collections::HashMap::new();
                        failure_attrs.insert("exception".to_string(), ex);
                        return Some(Some(Ok(Value::make_instance(
                            crate::symbol::Symbol::intern("Failure"),
                            failure_attrs,
                        ))));
                    }
                    Value::int(f as i64)
                }
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" || class_name == "Duration" => {
                    let numeric = attributes
                        .as_map()
                        .get("value")
                        .and_then(|v| match v.view() {
                            ValueView::Int(i) => Some(i as f64),
                            ValueView::BigInt(n) => Some(n.to_f64().unwrap_or(f64::INFINITY)),
                            ValueView::Num(f) => Some(f),
                            ValueView::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
                            ValueView::FatRat(n, d) if d != 0 => Some(n as f64 / d as f64),
                            ValueView::BigRat(n, d) if !d.is_zero() => {
                                Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                            }
                            _ => None,
                        })
                        .unwrap_or(0.0);
                    Value::int(numeric as i64)
                }
                ValueView::Rat(_, 0) => {
                    return Some(Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                        "Int", "Rational",
                    ))));
                }
                ValueView::FatRat(_, 0) => {
                    return Some(Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                        "Int", "Rational",
                    ))));
                }
                ValueView::Rat(n, d) if d != 0 => Value::int(n / d),
                ValueView::FatRat(n, d) if d != 0 => Value::int(n / d),
                ValueView::BigRat(_, d) if d.is_zero() => {
                    return Some(Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                        "Int", "Rational",
                    ))));
                }
                ValueView::BigRat(n, d) if !d.is_zero() => {
                    use num_traits::ToPrimitive;
                    Value::int((n / d).to_i64().unwrap_or(i64::MAX))
                }
                ValueView::Str(s) => {
                    if s.trim().is_empty() {
                        // An empty or whitespace-only string coerces to 0, like
                        // `"".Numeric` (a defined-but-empty string, no warning).
                        Value::int(0)
                    } else if let Some(v) = parse_raku_int_from_str(&s) {
                        v
                    } else if let Some(v) =
                        runtime::str_numeric::parse_raku_str_to_numeric(s.trim())
                            .as_ref()
                            .and_then(numeric_to_int)
                    {
                        // raku's `Str.Int` parses via the full numeric grammar
                        // and truncates the result, so numeric string forms the
                        // strict integer parser above rejects — radix `:16<ff>`,
                        // rational `3/4`, ... — still coerce like their value.
                        v
                    } else {
                        // Return a Failure (lazy exception) instead of throwing.
                        return Some(Some(Ok(str_numeric_failure(&s))));
                    }
                }
                ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
                ValueView::Complex(r, _) => Value::int(r as i64),
                ValueView::Hash(h) => Value::int(h.len() as i64),
                ValueView::Array(items, ..) => Value::int(items.len() as i64),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    if let Some(ValueView::Array(bytes, ..)) =
                        attributes.as_map().get("bytes").map(Value::view)
                    {
                        Value::int(bytes.len() as i64)
                    } else {
                        Value::int(0)
                    }
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "UInt" => {
            // First coerce to Int, then check non-negative
            let int_result = match target.view() {
                ValueView::Int(i) => Some(Value::int(i)),
                ValueView::BigInt(_) => Some(target.clone()),
                ValueView::Num(f) if f.is_finite() => Some(Value::int(f.trunc() as i64)),
                ValueView::Rat(n, d) if d != 0 => Some(Value::int(n / d)),
                // A Complex coerces to UInt via its real part (`(5+0i).UInt` is 5);
                // a non-zero imaginary part is not Real and throws X::Numeric::Real
                // (mirroring the `sign`/`.Int` Complex coercion).
                ValueView::Complex(re, im) => {
                    if im != 0.0 {
                        let rendered = if im >= 0.0 {
                            format!("{re}+{im}i")
                        } else {
                            format!("{re}{im}i")
                        };
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Cannot convert {rendered} to Real: imaginary part not zero"
                            )),
                        );
                        attrs.insert("target".to_string(), Value::package(Symbol::intern("Real")));
                        attrs.insert("source".to_string(), target.clone());
                        let ex = Value::make_instance(Symbol::intern("X::Numeric::Real"), attrs);
                        let mut err = RuntimeError::new(
                            "Cannot convert Complex to Real: imaginary part not zero",
                        );
                        err.exception = Some(Box::new(ex));
                        return Some(Some(Err(err)));
                    }
                    if re.is_finite() {
                        Some(Value::int(re.trunc() as i64))
                    } else {
                        None
                    }
                }
                ValueView::Bool(b) => Some(Value::int(if b { 1 } else { 0 })),
                ValueView::Str(s) if s.trim().is_empty() => Some(Value::int(0)),
                ValueView::Str(s) => {
                    if let Some(v) = parse_raku_int_from_str(&s) {
                        Some(v)
                    } else if let Some(v) =
                        runtime::str_numeric::parse_raku_str_to_numeric(s.trim())
                            .as_ref()
                            .and_then(numeric_to_int)
                    {
                        // Same numeric-string forms as `.Int` (radix `:16<ff>`,
                        // rational `3/4`, ...): numify then truncate, then the
                        // non-negative check below applies.
                        Some(v)
                    } else {
                        // Invalid string: same X::Str::Numeric Failure (with the `⏏`
                        // position marker) as `.Int`, rather than a hand-rolled
                        // message that hard-codes the wrong reason and drops the marker.
                        return Some(Some(Ok(str_numeric_failure(&s))));
                    }
                }
                _ => None,
            };
            if let Some(int_val) = int_result {
                // Check non-negative
                let is_neg = match int_val.view() {
                    ValueView::Int(i) => i < 0,
                    ValueView::BigInt(n) => n.sign() == num_bigint::Sign::Minus,
                    _ => false,
                };
                if is_neg {
                    return Some(Some(Err(RuntimeError::new(format!(
                        "Coercion to UInt out of range. Is: {}, should be in 0..^Inf",
                        int_val.to_string_value()
                    )))));
                }
                Some(Some(Ok(int_val)))
            } else {
                Some(None)
            }
        }
        "Num" => {
            let result = match target.view() {
                ValueView::Int(i) => Value::num(i as f64),
                ValueView::BigInt(n) => {
                    use num_traits::ToPrimitive;
                    Value::num(n.to_f64().unwrap_or(f64::INFINITY))
                }
                ValueView::Num(f) => Value::num(f),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" || class_name == "Duration" => {
                    let numeric = attributes
                        .as_map()
                        .get("value")
                        .and_then(|v| match v.view() {
                            ValueView::Int(i) => Some(i as f64),
                            ValueView::BigInt(n) => Some(n.to_f64().unwrap_or(f64::INFINITY)),
                            ValueView::Num(f) => Some(f),
                            ValueView::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
                            ValueView::FatRat(n, d) if d != 0 => Some(n as f64 / d as f64),
                            ValueView::BigRat(n, d) if !d.is_zero() => {
                                Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                            }
                            _ => None,
                        })
                        .unwrap_or(0.0);
                    Value::num(numeric)
                }
                ValueView::Rat(n, d) | ValueView::FatRat(n, d) if d == 0 => Value::num(if n == 0 {
                    f64::NAN
                } else if n > 0 {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                }),
                ValueView::Rat(n, d) if d != 0 => Value::num(n as f64 / d as f64),
                ValueView::FatRat(n, d) if d != 0 => Value::num(n as f64 / d as f64),
                ValueView::BigRat(n, d) if !d.is_zero() => {
                    use num_traits::ToPrimitive;
                    let num = n.to_f64().unwrap_or(0.0);
                    let den = d.to_f64().unwrap_or(1.0);
                    Value::num(num / den)
                }
                ValueView::Str(s) => {
                    let trimmed = s.trim();
                    if trimmed.is_empty() {
                        // An empty or whitespace-only string coerces to 0, like
                        // `"".Numeric`.
                        Value::num(0.0)
                    } else {
                        // Normalize U+2212 MINUS SIGN to ASCII hyphen-minus, then use
                        // the canonical Raku numeric parser (radix prefixes,
                        // underscores, rationals, strict Inf/NaN) so `.Num` agrees
                        // with `.Numeric`/`.Int`/prefix `+`. `.Num` always yields a Num.
                        let normalized = trimmed.replace('\u{2212}', "-");
                        if let Some(v) =
                            crate::runtime::str_numeric::parse_raku_str_to_numeric(&normalized)
                        {
                            let f = v.to_f64();
                            // "-0"/"-0.0" parse as Int/Rat zero, which has no
                            // sign — but `.Num` must yield the IEEE negative
                            // zero (roast S32-num/negative-zero.t). Restore the
                            // sign from the source string when the magnitude is
                            // zero. (The scientific path, e.g. "-0e0", already
                            // returns a signed Num.)
                            let f = if f == 0.0
                                && f.is_sign_positive()
                                && normalized.starts_with('-')
                            {
                                -0.0
                            } else {
                                f
                            };
                            Value::num(f)
                        } else {
                            // An invalid string yields a lazy X::Str::Numeric Failure,
                            // mirroring `.Int` (not an eager X::AdHoc RuntimeError).
                            return Some(Some(Ok(str_numeric_failure(&s))));
                        }
                    }
                }
                ValueView::Bool(b) => Value::num(if b { 1.0 } else { 0.0 }),
                ValueView::Complex(_, _) => return Some(None), // fall through to runtime for $*TOLERANCE check
                ValueView::Array(items, ..) => Value::num(items.len() as f64),
                ValueView::Seq(items) | ValueView::Slip(items) => Value::num(items.len() as f64),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Real" => {
            // Calling .Real on a type object (Package) should warn and return zero
            if let ValueView::Package(name) = target.view() {
                let type_name = name.resolve();
                let zero = match type_name.as_str() {
                    "Int" | "IntStr" => Value::int(0),
                    "Rat" | "FatRat" | "RatStr" => Value::rat_raw(0, 1),
                    "Num" | "NumStr" | "Complex" | "ComplexStr" => Value::num(0.0),
                    _ => return Some(None),
                };
                let msg = format!(
                    "Use of uninitialized value of type {} in numeric context",
                    type_name
                );
                return Some(Some(Err(RuntimeError::warn_signal_with_resume(msg, zero))));
            }
            let result = match target.view() {
                ValueView::Int(i) => Value::int(i),
                ValueView::BigInt(_) => target.clone(),
                ValueView::Num(f) => Value::num(f),
                ValueView::Rat(n, d) => Value::rat_raw(n, d),
                ValueView::FatRat(n, d) => Value::fat_rat_raw(n, d),
                ValueView::BigRat(n, d) => Value::bigrat(n.clone(), d.clone()),
                ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
                ValueView::Complex(r, im) => {
                    if im.abs() <= 1e-15 {
                        Value::num(r)
                    } else {
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert(
                            "message".to_string(),
                            Value::str_from(
                                "Cannot convert Complex to Real: imaginary part not zero",
                            ),
                        );
                        ex_attrs
                            .insert("target".to_string(), Value::package(Symbol::intern("Real")));
                        ex_attrs.insert("source".to_string(), target.clone());
                        let ex = Value::make_instance(Symbol::intern("X::Numeric::Real"), ex_attrs);
                        let mut failure_attrs = std::collections::HashMap::new();
                        failure_attrs.insert("exception".to_string(), ex);
                        return Some(Some(Ok(Value::make_instance(
                            Symbol::intern("Failure"),
                            failure_attrs,
                        ))));
                    }
                }
                ValueView::Str(s) => {
                    // `.Real` yields the natural numeric type (Int/Rat/Num); use the
                    // canonical parser so radix prefixes and underscores work and the
                    // result agrees with `.Numeric`.
                    if let Some(v) =
                        crate::runtime::str_numeric::parse_raku_str_to_numeric(s.trim())
                    {
                        v
                    } else {
                        // Same X::Str::Numeric Failure (typed, with the `⏏` marker)
                        // as `.Int`/`.Numeric`.
                        return Some(Some(Ok(str_numeric_failure(&s))));
                    }
                }
                ValueView::Array(items, ..) => Value::int(items.len() as i64),
                ValueView::Hash(h) => Value::int(h.len() as i64),
                ValueView::Seq(items) => Value::int(items.len() as i64),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Numeric" => {
            // Calling .Numeric on a type object (Package) should warn and return zero
            if let ValueView::Package(name) = target.view() {
                let type_name = name.resolve();
                let zero = match type_name.as_str() {
                    "Int" | "IntStr" => Value::int(0),
                    "Rat" | "RatStr" => Value::rat_raw(0, 1),
                    "FatRat" => Value::fat_rat_raw(0, 1),
                    "Num" | "NumStr" => Value::num(0.0),
                    "Complex" | "ComplexStr" => Value::complex(0.0, 0.0),
                    _ => return Some(None),
                };
                let msg = format!(
                    "Use of uninitialized value of type {} in numeric context",
                    type_name
                );
                return Some(Some(Err(RuntimeError::warn_signal_with_resume(msg, zero))));
            }
            let result = match target.view() {
                ValueView::Int(i) => Value::int(i),
                ValueView::BigInt(_) => target.clone(),
                ValueView::Num(f) => Value::num(f),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" || class_name == "Duration" => {
                    let numeric = attributes
                        .as_map()
                        .get("value")
                        .and_then(|v| match v.view() {
                            ValueView::Int(i) => Some(i as f64),
                            ValueView::BigInt(n) => Some(n.to_f64().unwrap_or(f64::INFINITY)),
                            ValueView::Num(f) => Some(f),
                            ValueView::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
                            ValueView::FatRat(n, d) if d != 0 => Some(n as f64 / d as f64),
                            ValueView::BigRat(n, d) if !d.is_zero() => {
                                Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                            }
                            _ => None,
                        })
                        .unwrap_or(0.0);
                    Value::num(numeric)
                }
                ValueView::Rat(n, d) if d != 0 => Value::num(n as f64 / d as f64),
                ValueView::Str(s) => {
                    if let Some(v) = crate::runtime::str_numeric::parse_raku_str_to_numeric(&s) {
                        v
                    } else {
                        // Same X::Str::Numeric Failure (typed, with the `⏏` marker)
                        // as `.Int`.
                        return Some(Some(Ok(str_numeric_failure(&s))));
                    }
                }
                ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
                ValueView::Complex(r, _) => Value::num(r),
                ValueView::Array(items, ..) => Value::int(items.len() as i64),
                ValueView::Hash(h) => Value::int(h.len() as i64),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    if let Some(ValueView::Array(bytes, ..)) =
                        attributes.as_map().get("bytes").map(Value::view)
                    {
                        Value::int(bytes.len() as i64)
                    } else {
                        Value::int(0)
                    }
                }
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Stash" => {
                    let count = match attributes.as_map().get("symbols").map(Value::view) {
                        Some(ValueView::Hash(map)) => map.len() as i64,
                        _ => 0,
                    };
                    Value::int(count)
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Bridge" => {
            let result = match target.view() {
                ValueView::Int(i) => Value::num(i as f64),
                ValueView::BigInt(n) => Value::num(n.to_f64().unwrap_or(f64::INFINITY)),
                ValueView::Num(f) => Value::num(f),
                ValueView::Rat(n, d) if d != 0 => Value::num(n as f64 / d as f64),
                ValueView::FatRat(n, d) if d != 0 => {
                    Value::num(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                }
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" || class_name == "Duration" => {
                    let bridged = attributes
                        .as_map()
                        .get("value")
                        .and_then(|v| match v.view() {
                            ValueView::Int(i) => Some(i as f64),
                            ValueView::BigInt(n) => Some(n.to_f64().unwrap_or(f64::INFINITY)),
                            ValueView::Num(f) => Some(f),
                            ValueView::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
                            ValueView::FatRat(n, d) if d != 0 => {
                                Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                            }
                            _ => None,
                        })
                        .unwrap_or(0.0);
                    Value::num(bridged)
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        _ => None,
    }
}

/// Numerify a Range to its element count for `.Int`/`.Numeric`/`.Real`/`.Num`.
/// Finite ranges yield the count (as `Num` for `.Num`, else `Int`); an infinite
/// range yields `Inf` for the real-valued coercions and fails for `.Int`.
/// `target` is assumed to be a Range (checked by the caller).
fn range_numeric_coercion(target: &Value, method: &str) -> Result<Value, RuntimeError> {
    if super::is_infinite_range(target) {
        return if method == "Int" {
            Err(RuntimeError::new("Cannot convert Inf to Int".to_string()))
        } else {
            Ok(Value::num(f64::INFINITY))
        };
    }
    let count = match target.view() {
        ValueView::Range(s, e) => (e - s + 1).max(0),
        ValueView::RangeExcl(s, e) | ValueView::RangeExclStart(s, e) => (e - s).max(0),
        ValueView::RangeExclBoth(s, e) => (e - s - 1).max(0),
        // GenericRange (e.g. Rat endpoints `1.5..5.5`) has no closed-form count;
        // materialize it the same way `.elems` does.
        _ => crate::runtime::utils::value_to_list(target).len() as i64,
    };
    Ok(if method == "Num" {
        Value::num(count as f64)
    } else {
        Value::int(count)
    })
}
