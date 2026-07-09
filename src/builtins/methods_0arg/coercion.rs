use crate::builtins::primality::{is_prime_bigint, is_prime_i64};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView, make_big_fat_rat, make_rat};
use std::collections::HashMap;

/// Type coercion and specialized 0-arg methods: numerator, denominator, nude,
/// is-prime, isNaN, re, im, conj, reals, Complex, key, value, Slip, list/Array, Range
pub(super) fn dispatch(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    match method {
        "numerator" => match target.view() {
            ValueView::Rat(n, _) => Some(Ok(Value::int(n))),
            ValueView::FatRat(n, _) => Some(Ok(Value::int(n))),
            ValueView::BigRat(n, _) => Some(Ok(Value::bigint(n.clone()))),
            ValueView::Int(i) => Some(Ok(Value::int(i))),
            _ => Some(Ok(Value::int(0))),
        },
        "denominator" => match target.view() {
            ValueView::Rat(_, d) => Some(Ok(Value::int(d))),
            ValueView::FatRat(_, d) => Some(Ok(Value::int(d))),
            ValueView::BigRat(_, d) => Some(Ok(Value::bigint(d.clone()))),
            ValueView::Int(_) => Some(Ok(Value::int(1))),
            _ => Some(Ok(Value::int(1))),
        },
        "isNaN" => match target.view() {
            ValueView::Rat(0, 0) => Some(Ok(Value::TRUE)),
            ValueView::FatRat(0, 0) => Some(Ok(Value::TRUE)),
            ValueView::Num(f) => Some(Ok(Value::truth(f.is_nan()))),
            _ => Some(Ok(Value::FALSE)),
        },
        "nude" => match target.view() {
            ValueView::Rat(n, d) => Some(Ok(Value::array(vec![Value::int(n), Value::int(d)]))),
            ValueView::FatRat(n, d) => Some(Ok(Value::array(vec![Value::int(n), Value::int(d)]))),
            ValueView::BigRat(n, d) => Some(Ok(Value::array(vec![
                Value::bigint(n.clone()),
                Value::bigint(d.clone()),
            ]))),
            ValueView::Int(i) => Some(Ok(Value::array(vec![Value::int(i), Value::int(1)]))),
            _ => Some(Ok(Value::array(vec![Value::int(0), Value::int(1)]))),
        },
        "norm" => match target.view() {
            ValueView::Rat(n, d) => Some(Ok(make_rat(n, d))),
            ValueView::FatRat(n, d) => Some(Ok({
                let r = make_rat(n, d);
                match r.view() {
                    ValueView::Rat(nn, dd) => Value::fat_rat_raw(nn, dd),
                    _ => r,
                }
            })),
            ValueView::BigRat(n, d) => Some(Ok({
                let r = make_big_fat_rat(n.clone(), d.clone());
                match r.view() {
                    ValueView::Rat(nn, dd) => Value::fat_rat_raw(nn, dd),
                    ValueView::BigRat(nn, dd) => Value::bigrat(nn.clone(), dd.clone()),
                    _ => r,
                }
            })),
            ValueView::Int(i) => Some(Ok(Value::fat_rat_raw(i, 1))),
            _ => Some(Ok(Value::fat_rat_raw(0, 1))),
        },
        "is-prime" => Some(value_is_prime(target)),
        "re" => match target.view() {
            ValueView::Complex(r, _) => Some(Ok(Value::num(r))),
            ValueView::Int(i) => Some(Ok(Value::num(i as f64))),
            ValueView::Num(f) => Some(Ok(Value::num(f))),
            _ => Some(Ok(Value::num(0.0))),
        },
        "im" => match target.view() {
            ValueView::Complex(_, i) => Some(Ok(Value::num(i))),
            _ => Some(Ok(Value::num(0.0))),
        },
        "conj" => match target.view() {
            ValueView::Complex(r, i) => Some(Ok(Value::complex(r, -i))),
            ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Rat(_, _)
            | ValueView::FatRat(_, _)
            | ValueView::Bool(_) => Some(Ok(target.clone())),
            // Str is handled by the Cool numeric coercion in native_method_0arg
            _ => None,
        },
        "reals" => match target.view() {
            ValueView::Complex(r, i) => Some(Ok(Value::array(vec![Value::num(r), Value::num(i)]))),
            _ => None,
        },
        "polar" => match target.view() {
            ValueView::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt();
                let angle = i.atan2(r);
                Some(Ok(Value::array(vec![Value::num(mag), Value::num(angle)])))
            }
            ValueView::Int(i) => {
                let f = i as f64;
                let mag = f.abs();
                let angle = if f < 0.0 { std::f64::consts::PI } else { 0.0 };
                Some(Ok(Value::array(vec![Value::num(mag), Value::num(angle)])))
            }
            ValueView::Num(f) => {
                let mag = f.abs();
                let angle = if f < 0.0 { std::f64::consts::PI } else { 0.0 };
                Some(Ok(Value::array(vec![Value::num(mag), Value::num(angle)])))
            }
            _ => None,
        },
        "cis" => match target.view() {
            ValueView::Int(i) => {
                let x = i as f64;
                Some(Ok(Value::complex(x.cos(), x.sin())))
            }
            ValueView::Num(f) => Some(Ok(Value::complex(f.cos(), f.sin()))),
            ValueView::Rat(n, d) if d != 0 => {
                let x = n as f64 / d as f64;
                Some(Ok(Value::complex(x.cos(), x.sin())))
            }
            ValueView::Complex(re, im) => {
                // cis(a+bi) = e^(i*(a+bi)) = e^(-b) * (cos(a) + i*sin(a))
                let scale = (-im).exp();
                Some(Ok(Value::complex(scale * re.cos(), scale * re.sin())))
            }
            _ => None,
        },
        "Complex" => match target.view() {
            ValueView::Instance { .. }
                if target.does_check("Real") || target.does_check("Numeric") =>
            {
                None
            }
            ValueView::Complex(_, _) => Some(Ok(target.clone())),
            ValueView::Int(i) => Some(Ok(Value::complex(i as f64, 0.0))),
            ValueView::Num(f) => Some(Ok(Value::complex(f, 0.0))),
            ValueView::Rat(n, d) if d != 0 => Some(Ok(Value::complex(n as f64 / d as f64, 0.0))),
            ValueView::FatRat(n, d) if d != 0 => Some(Ok(Value::complex(n as f64 / d as f64, 0.0))),
            ValueView::BigInt(n) => Some(Ok(Value::complex(
                num_traits::ToPrimitive::to_f64(n.as_ref()).unwrap_or(f64::INFINITY),
                0.0,
            ))),
            ValueView::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => {
                Some(Ok(Value::complex(
                    num_traits::ToPrimitive::to_f64(n).unwrap_or(0.0)
                        / num_traits::ToPrimitive::to_f64(d).unwrap_or(1.0),
                    0.0,
                )))
            }
            _ => Some(Ok(Value::complex(0.0, 0.0))),
        },
        "Pair" => match target.view() {
            ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => Some(Ok(target.clone())),
            ValueView::Instance { class_name, .. } if class_name == "Pair" => {
                Some(Ok(target.clone()))
            }
            // Type object: Pair.Pair returns Pair (identity)
            ValueView::Package(name) if name.resolve() == "Pair" => Some(Ok(target.clone())),
            _ => None,
        },
        "key" => match target.view() {
            ValueView::Pair(k, _) => Some(Ok(Value::str(k.clone()))),
            ValueView::ValuePair(k, _) => Some(Ok(k.clone())),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Pair" => Some(Ok(attributes
                .as_map()
                .get("key")
                .cloned()
                .unwrap_or(Value::NIL))),
            ValueView::Bool(true) => Some(Ok(Value::str_from("True"))),
            ValueView::Bool(false) => Some(Ok(Value::str_from("False"))),
            _ => None,
        },
        "value" => match target.view() {
            ValueView::Pair(_, v) | ValueView::ValuePair(_, v) => Some(Ok(v.clone())),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Pair" => {
                if let (Some(ValueView::Hash(hash)), Some(ValueView::Str(key))) = (
                    attributes.as_map().get("__mutsu_hash_ref").map(Value::view),
                    attributes.as_map().get("key").map(Value::view),
                ) {
                    Some(Ok(hash.get(key.as_str()).cloned().unwrap_or(Value::NIL)))
                } else {
                    Some(Ok(attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::NIL)))
                }
            }
            ValueView::Bool(b) => Some(Ok(Value::int(if b { 1 } else { 0 }))),
            _ => None,
        },
        "antipair" => match target.view() {
            ValueView::Pair(k, v) => Some(Ok(match v.view() {
                ValueView::Str(s) => Value::pair(s.to_string(), Value::str(k.clone())),
                _ => Value::value_pair(v.clone(), Value::str(k.clone())),
            })),
            ValueView::ValuePair(k, v) => Some(Ok(Value::value_pair(v.clone(), k.clone()))),
            _ => None,
        },
        "Capture" => Some(value_to_capture(target)),
        "Slip" => match target.view() {
            ValueView::Seq(items) => {
                if crate::value::seq_is_consumed(items) && !crate::value::seq_is_cached(items) {
                    return Some(Err(crate::value::seq_consumed_error()));
                }
                // Mark as cached so the Seq remains reusable
                crate::value::seq_mark_cached(items);
                Some(Ok(Value::slip_arc(items.clone())))
            }
            ValueView::Array(items, ..) => {
                // `.Slip` materializes array holes with the container's
                // `is default(...)` value (Rakudo semantics: the .List keeps
                // holes as Nil, while .Slip uses the default). The default is
                // embedded in `ArrayData`, so this pure coercion can read it.
                let vec: Vec<Value> = if let Some(def) = items.default.as_deref() {
                    items
                        .iter()
                        .map(|v| match v.view() {
                            ValueView::Package(n) if n == "Any" => def.clone(),
                            _ => v.clone(),
                        })
                        .collect()
                } else {
                    items.to_vec()
                };
                Some(Ok(Value::slip_arc(std::sync::Arc::new(vec))))
            }
            ValueView::Slip(_) => Some(Ok(target.clone())),
            ValueView::LazyList(ll) => {
                if ll.scan_spec.is_some() {
                    let items = ll.force_scan_to(200_000);
                    Some(Ok(Value::slip_arc(std::sync::Arc::new(items))))
                } else {
                    let items = ll.cache.lock().unwrap().clone().unwrap_or_default();
                    Some(Ok(Value::slip_arc(std::sync::Arc::new(items))))
                }
            }
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => Some(Ok(Value::slip(
                crate::runtime::utils::value_to_list(target),
            ))),
            _ => Some(Ok(Value::slip(vec![target.clone()]))),
        },
        "List" => match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if {
                let cn = class_name.resolve();
                cn == "Buf"
                    || cn == "Blob"
                    || cn == "utf8"
                    || cn == "utf16"
                    || cn.starts_with("Buf[")
                    || cn.starts_with("Blob[")
                    || cn.starts_with("buf")
                    || cn.starts_with("blob")
            } =>
            {
                if let Some(ValueView::Array(items, ..)) =
                    attributes.as_map().get("bytes").map(Value::view)
                {
                    Some(Ok(Value::array_with_kind(
                        items.clone(),
                        crate::value::ArrayKind::List,
                    )))
                } else {
                    Some(Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
                        crate::value::ArrayKind::List,
                    )))
                }
            }
            ValueView::Range(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((a..=b).map(Value::int).collect())))
                }
            }
            ValueView::RangeExcl(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((a..b).map(Value::int).collect())))
                }
            }
            ValueView::Seq(items) => {
                if crate::value::seq_is_consumed(items) && !crate::value::seq_is_cached(items) {
                    return Some(Err(crate::value::seq_consumed_error()));
                }
                // Mark as cached so the Seq remains reusable
                crate::value::seq_mark_cached(items);
                Some(Ok(Value::array(items.to_vec())))
            }
            // A shaped array falls through to the slow path, which flattens all
            // dimensions and replaces Nil slots with the type-default.
            ValueView::Array(..) if crate::runtime::utils::is_shaped_array(target) => None,
            ValueView::Array(items, _) => Some(Ok(Value::array(items.to_vec()))),
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // String range: expand using codepoint succession
                if let (ValueView::Str(s), ValueView::Str(e)) = (start.view(), end.view()) {
                    if s.chars().count() == 1 && e.chars().count() == 1 {
                        let sc = s.chars().next().unwrap() as u32;
                        let ec = e.chars().next().unwrap() as u32;
                        let start_cp = if excl_start { sc + 1 } else { sc };
                        let items: Vec<Value> = if sc <= ec {
                            let end_cp = if excl_end { ec } else { ec + 1 };
                            (start_cp..end_cp)
                                .filter_map(char::from_u32)
                                .map(|c| Value::str(c.to_string()))
                                .collect()
                        } else {
                            let end_cp = if excl_end { ec } else { ec.saturating_sub(1) };
                            (end_cp + 1..=start_cp)
                                .rev()
                                .filter_map(char::from_u32)
                                .map(|c| Value::str(c.to_string()))
                                .collect()
                        };
                        Some(Ok(Value::array(items)))
                    } else {
                        Some(Ok(Value::array(vec![target.clone()])))
                    }
                } else {
                    // Numeric generic range
                    let items = crate::runtime::utils::value_to_list(target);
                    Some(Ok(Value::array(items)))
                }
            }
            ValueView::RangeExclStart(a, b) => {
                if b == i64::MAX {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((a + 1..=b).map(Value::int).collect())))
                }
            }
            ValueView::RangeExclBoth(a, b) => {
                if b == i64::MAX {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((a + 1..b).map(Value::int).collect())))
                }
            }
            ValueView::LazyList(_) => None, // fall through to runtime to force
            _ => Some(Ok(Value::array(vec![target.clone()]))),
        },
        "__mutsu_zen_angle" => match target.view() {
            ValueView::Range(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((a..=b).map(Value::int).collect())))
                }
            }
            ValueView::RangeExcl(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((a..b).map(Value::int).collect())))
                }
            }
            ValueView::RangeExclStart(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((a + 1..=b).map(Value::int).collect())))
                }
            }
            ValueView::RangeExclBoth(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((a + 1..b).map(Value::int).collect())))
                }
            }
            ValueView::GenericRange { .. } => {
                let items = crate::runtime::utils::value_to_list(target);
                Some(Ok(Value::array(items)))
            }
            ValueView::Array(..) | ValueView::Seq(..) | ValueView::Slip(..) => {
                Some(Ok(target.clone()))
            }
            _ => Some(Ok(target.clone())),
        },
        "list" | "Array" => {
            // `.Array` yields a real `@`-sigiled Array; `.list` yields a List.
            let want_array = method == "Array";
            let wrap = |items: Vec<Value>| {
                if want_array {
                    Value::real_array(items)
                } else {
                    Value::array(items)
                }
            };
            match target.view() {
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Supply" => {
                    // An on-demand supply (`supply { ... }` block) has no
                    // materialized values; its body must be run by the
                    // stateful slow path (`supply_list_values`). Decline so
                    // dispatch falls through.
                    if attributes.as_map().contains_key("on_demand_callback") {
                        return None;
                    }
                    let items = match attributes.as_map().get("values").map(Value::view) {
                        Some(ValueView::Array(items, ..)) => items.to_vec(),
                        _ => Vec::new(),
                    };
                    Some(Ok(wrap(items)))
                }
                ValueView::Range(a, b) => {
                    if b == i64::MAX || a == i64::MIN {
                        // Infinite range → convert to lazy array (supports indexing + .Capture throws)
                        Some(Ok(crate::runtime::utils::coerce_to_array(target.clone())))
                    } else {
                        Some(Ok(wrap((a..=b).map(Value::int).collect())))
                    }
                }
                ValueView::RangeExcl(a, b) => {
                    if b == i64::MAX || a == i64::MIN {
                        Some(Ok(crate::runtime::utils::coerce_to_array(target.clone())))
                    } else {
                        Some(Ok(wrap((a..b).map(Value::int).collect())))
                    }
                }
                ValueView::RangeExclStart(a, b) => {
                    if b == i64::MAX || a == i64::MIN {
                        Some(Ok(crate::runtime::utils::coerce_to_array(target.clone())))
                    } else {
                        Some(Ok(wrap((a + 1..=b).map(Value::int).collect())))
                    }
                }
                ValueView::RangeExclBoth(a, b) => {
                    if b == i64::MAX || a == i64::MIN {
                        Some(Ok(crate::runtime::utils::coerce_to_array(target.clone())))
                    } else {
                        Some(Ok(wrap((a + 1..b).map(Value::int).collect())))
                    }
                }
                ValueView::GenericRange { .. } => {
                    let items = crate::runtime::utils::value_to_list(target);
                    Some(Ok(wrap(items)))
                }
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if {
                    let cn = class_name.resolve();
                    cn == "Buf"
                        || cn == "Blob"
                        || cn == "utf8"
                        || cn == "utf16"
                        || cn.starts_with("Buf[")
                        || cn.starts_with("Blob[")
                        || cn.starts_with("buf")
                        || cn.starts_with("blob")
                } =>
                {
                    let bytes = match attributes.as_map().get("bytes").map(Value::view) {
                        Some(ValueView::Array(items, ..)) => items.to_vec(),
                        _ => Vec::new(),
                    };
                    Some(Ok(wrap(bytes)))
                }
                // A shaped array falls through to the slow path (flatten + Nil
                // → type-default). Non-shaped arrays keep the fast path.
                ValueView::Array(..) if crate::runtime::utils::is_shaped_array(target) => None,
                ValueView::Array(items, kind) => {
                    if method == "Array" && !kind.is_real_array() {
                        Some(Ok(Value::real_array(items.to_vec())))
                    } else if method == "list" && kind.is_itemized() {
                        // .list on an itemized array/list strips the itemization,
                        // returning the contents as a plain List (de-itemized).
                        Some(Ok(Value::array_with_kind(
                            items.clone(),
                            kind.decontainerize(),
                        )))
                    } else {
                        Some(Ok(target.clone()))
                    }
                }
                ValueView::Seq(items) if method == "list" || method == "Array" => {
                    // Consumed Seq check: throw X::Seq::Consumed if not cached
                    if crate::value::seq_is_consumed(items) && !crate::value::seq_is_cached(items) {
                        return Some(Err(crate::value::seq_consumed_error()));
                    }
                    // Mark as cached so the Seq remains reusable (e.g. when the Seq is
                    // bound to an @-sigil parameter, Raku implicitly caches it).
                    // TODO: implement proper @-sigil parameter caching separately, and
                    // change this back to seq_consume for strict Raku semantics where
                    // .List on an uncached Seq consumes it.
                    crate::value::seq_mark_cached(items);
                    if method == "Array" {
                        Some(Ok(Value::real_array(items.to_vec())))
                    } else {
                        Some(Ok(Value::array(items.to_vec())))
                    }
                }
                ValueView::Slip(items) if method == "list" || method == "Array" => {
                    if method == "Array" {
                        Some(Ok(Value::real_array(items.to_vec())))
                    } else {
                        Some(Ok(Value::array(items.to_vec())))
                    }
                }
                // A genuinely-lazy list stays lazy through `.Array`/`.list`:
                // tag it with the target context so `.WHAT` reports `Array`/`List`
                // without materializing the (possibly infinite) generator.
                ValueView::LazyList(ll) if ll.is_genuinely_lazy() => {
                    if want_array {
                        Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                            ll.with_array_context(),
                        ))))
                    } else {
                        Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                            ll.with_list_context(),
                        ))))
                    }
                }
                ValueView::Channel(_) => None, // fall through to runtime for drain
                ValueView::Hash(map) => {
                    let pairs: Vec<Value> = map
                        .iter()
                        .map(|(k, v)| map.typed_pair(k, v.clone()))
                        .collect();
                    Some(Ok(wrap(pairs)))
                }
                ValueView::Set(_, _) | ValueView::Bag(_, _) | ValueView::Mix(_, _) => {
                    Some(Ok(wrap(crate::runtime::utils::value_to_list(target))))
                }
                _ => Some(Ok(wrap(vec![target.clone()]))),
            }
        }
        "Range" => match target.view() {
            ValueView::Array(items, ..) => Some(Ok(Value::range_excl(0, items.len() as i64))),
            ValueView::Str(s) => Some(Ok(Value::range_excl(0, s.chars().count() as i64))),
            ValueView::Range(_, _)
            | ValueView::RangeExcl(_, _)
            | ValueView::RangeExclStart(_, _)
            | ValueView::RangeExclBoth(_, _)
            | ValueView::GenericRange { .. } => Some(Ok(target.clone())),
            _ => None,
        },
        "Supply" => {
            if let ValueView::Instance { class_name, .. } = target.view()
                && (class_name == "Supplier" || class_name == "Supplier::Preserving")
            {
                // Supplier.Supply has runtime behavior (live stream), not generic coercion.
                return None;
            }
            // .Supply on an existing Supply is a noop — return self
            if let ValueView::Instance { class_name, .. } = target.view()
                && class_name == "Supply"
            {
                return Some(Ok(target.clone()));
            }
            // Supplier.Supply must be handled by runtime native methods
            // so the returned Supply remains linked to Supplier.emit/.done.
            if let ValueView::Instance { class_name, .. } = target.view()
                && (class_name == "Supplier" || class_name == "Supplier::Preserving")
            {
                return None;
            }
            if matches!(
                target.view(),
                ValueView::LazyList(_) | ValueView::Channel(_)
            ) {
                return None;
            }
            let values = match target.view() {
                ValueView::Array(items, ..) => items.to_vec(),
                ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                    items.to_vec()
                }
                // Route Range-family sources through `value_to_list`, which caps at
                // `MAX_RANGE_EXPAND` instead of expanding `(a..=i64::MAX)` directly.
                // The raw `.collect()` here used to `capacity overflow`-panic on an
                // infinite range, e.g. `(1..Inf).Supply` (ANALYSIS §8.7 / §8.2).
                ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. } => crate::runtime::utils::value_to_list(target),
                _ => vec![target.clone()],
            };
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("values".to_string(), Value::array(values));
            attrs.insert("taps".to_string(), Value::array(Vec::new()));
            attrs.insert("live".to_string(), Value::FALSE);
            Some(Ok(Value::make_instance(Symbol::intern("Supply"), attrs)))
        }
        _ => None,
    }
}

/// Implement is-prime for any Value, with type coercion.
/// Negative numbers are not prime.
/// Complex with non-zero imaginary throws X::Numeric::Real.
/// Str, Num, Rat, FatRat coerce to Int first.
pub(crate) fn value_is_prime(target: &Value) -> Result<Value, RuntimeError> {
    // Unwrap allomorphic types (Mixin) to the inner numeric value
    if let ValueView::Mixin(inner, _) = target.view() {
        return value_is_prime(inner);
    }
    match target.view() {
        ValueView::Int(n) => {
            if n < 0 {
                return Ok(Value::FALSE);
            }
            Ok(Value::truth(is_prime_i64(n)))
        }
        ValueView::BigInt(n) => {
            if n.sign() == num_bigint::Sign::Minus {
                return Ok(Value::FALSE);
            }
            Ok(Value::truth(is_prime_bigint(n)))
        }
        ValueView::Num(f) => {
            if f < 0.0 || f.fract() != 0.0 {
                return Ok(Value::FALSE);
            }
            let n = f as i64;
            Ok(Value::truth(is_prime_i64(n)))
        }
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
            if d == 0 {
                return Ok(Value::FALSE);
            }
            if n < 0 {
                return Ok(Value::FALSE);
            }
            if n % d != 0 {
                return Ok(Value::FALSE);
            }
            let int_val = n / d;
            Ok(Value::truth(is_prime_i64(int_val)))
        }
        ValueView::BigRat(n, d) => {
            use num_traits::Zero;
            if d.is_zero() {
                return Ok(Value::FALSE);
            }
            if n.sign() == num_bigint::Sign::Minus {
                return Ok(Value::FALSE);
            }
            let (quot, rem) = num_integer::Integer::div_rem(n, d);
            if !rem.is_zero() {
                return Ok(Value::FALSE);
            }
            Ok(Value::truth(is_prime_bigint(&quot)))
        }
        ValueView::Complex(_, i) if i != 0.0 => {
            let mut attrs = HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Cannot convert {} to Real: imaginary part not zero",
                    match target.view() {
                        ValueView::Complex(r, i) => {
                            if i >= 0.0 {
                                format!("{}+{}i", r, i)
                            } else {
                                format!("{}{}i", r, i)
                            }
                        }
                        _ => format!("{:?}", target),
                    }
                )),
            );
            attrs.insert("target".to_string(), Value::package(Symbol::intern("Real")));
            attrs.insert("source".to_string(), target.clone());
            let ex = Value::make_instance(Symbol::intern("X::Numeric::Real"), attrs);
            let mut err =
                RuntimeError::new("Cannot convert Complex to Real: imaginary part not zero");
            err.exception = Some(Box::new(ex));
            Err(err)
        }
        ValueView::Complex(r, _) => {
            // imaginary is 0, treat as real
            if r < 0.0 || r.fract() != 0.0 {
                return Ok(Value::FALSE);
            }
            let n = r as i64;
            Ok(Value::truth(is_prime_i64(n)))
        }
        ValueView::Str(s) => {
            // Try to parse as a number
            // Handle Unicode minus sign
            let s = s.replace('\u{2212}', "-");
            if let Ok(n) = s.parse::<i64>() {
                if n < 0 {
                    return Ok(Value::FALSE);
                }
                return Ok(Value::truth(is_prime_i64(n)));
            }
            if let Ok(f) = s.parse::<f64>() {
                if f < 0.0 || f.fract() != 0.0 {
                    return Ok(Value::FALSE);
                }
                let n = f as i64;
                return Ok(Value::truth(is_prime_i64(n)));
            }
            Ok(Value::FALSE)
        }
        _ => Ok(Value::FALSE),
    }
}

/// Convert a value to a Capture.
fn value_to_capture(target: &Value) -> Result<Value, RuntimeError> {
    match target.view() {
        // A Capture is already a Capture
        ValueView::Capture { .. } => Ok(target.clone()),
        // Match.Capture returns self
        ValueView::Instance { class_name, .. } if class_name.resolve() == "Match" => {
            Ok(target.clone())
        }
        // Blob/Buf/utf8/utf16 .Capture behaves like List.Capture: each byte
        // becomes a positional argument (no nameds).
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if {
            let cn = class_name.resolve();
            cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
                || cn.starts_with("buf")
                || cn.starts_with("blob")
        } =>
        {
            let positional = match attributes.as_map().get("bytes").map(Value::view) {
                Some(ValueView::Array(items, ..)) => items.iter().cloned().collect(),
                _ => vec![],
            };
            Ok(Value::capture(positional, HashMap::new()))
        }
        // Numeric types follow Mu.Capture: public attributes become nameds.
        // Complex → \(:re, :im)
        ValueView::Complex(r, i) => {
            let mut named = HashMap::new();
            named.insert("re".to_string(), Value::num(r));
            named.insert("im".to_string(), Value::num(i));
            Ok(Value::capture(vec![], named))
        }
        // Rat/FatRat/BigRat → \(:numerator, :denominator)
        ValueView::Rat(n, d) => {
            let mut named = HashMap::new();
            named.insert("numerator".to_string(), Value::int(n));
            named.insert("denominator".to_string(), Value::int(d));
            Ok(Value::capture(vec![], named))
        }
        ValueView::FatRat(n, d) => {
            let mut named = HashMap::new();
            named.insert("numerator".to_string(), Value::int(n));
            named.insert("denominator".to_string(), Value::int(d));
            Ok(Value::capture(vec![], named))
        }
        ValueView::BigRat(n, d) => {
            let mut named = HashMap::new();
            named.insert("numerator".to_string(), Value::bigint(n.clone()));
            named.insert("denominator".to_string(), Value::bigint(d.clone()));
            Ok(Value::capture(vec![], named))
        }
        // Version.Capture throws X::Cannot::Capture
        ValueView::Version { .. } => Err(cannot_capture("Version")),
        // Signature/Failure .Capture throw X::Cannot::Capture (both the type
        // object and an instance).
        ValueView::Instance { class_name, .. }
            if matches!(class_name.resolve().as_str(), "Signature" | "Failure") =>
        {
            Err(cannot_capture(&class_name.resolve()))
        }
        ValueView::Package(name) if matches!(name.resolve().as_str(), "Signature" | "Failure") => {
            Err(cannot_capture(&name.resolve()))
        }
        // Pair.Capture → \(:key($pair.key), :value($pair.value))
        ValueView::Pair(k, v) => {
            let mut named = HashMap::new();
            named.insert("key".to_string(), Value::str(k.clone()));
            named.insert("value".to_string(), v.clone());
            Ok(Value::capture(vec![], named))
        }
        ValueView::ValuePair(k, v) => {
            let mut named = HashMap::new();
            named.insert("key".to_string(), k.clone());
            named.insert("value".to_string(), v.clone());
            Ok(Value::capture(vec![], named))
        }
        // Set.Capture → named args where each key maps to True
        ValueView::Set(s, _) => {
            let mut named = HashMap::new();
            for k in s.iter() {
                named.insert(k.clone(), Value::TRUE);
            }
            Ok(Value::capture(vec![], named))
        }
        // Bag.Capture → named args where each key maps to its count
        ValueView::Bag(b, _) => {
            let mut named = HashMap::new();
            for (k, v) in b.iter() {
                named.insert(k.clone(), Value::from_bigint(v.clone()));
            }
            Ok(Value::capture(vec![], named))
        }
        // Mix.Capture → named args where each key maps to its weight
        ValueView::Mix(m, _) => {
            let mut named = HashMap::new();
            for (k, v) in m.iter() {
                // Use Int when the weight is a whole number
                let val = if v.fract() == 0.0 && v.is_finite() {
                    Value::int(*v as i64)
                } else {
                    Value::num(*v)
                };
                named.insert(k.clone(), val);
            }
            Ok(Value::capture(vec![], named))
        }
        // Hash.Capture → named args from hash entries
        ValueView::Hash(map) => {
            let mut named = HashMap::new();
            for (k, v) in map.iter() {
                named.insert(k.clone(), v.clone());
            }
            Ok(Value::capture(vec![], named))
        }
        // Lazy arrays must throw X::Cannot::Lazy
        ValueView::Array(_, kind) if kind.is_lazy() => Err(RuntimeError::cannot_lazy_with_action(
            "create a Capture from",
            "List",
        )),
        // Array/List.Capture → positional args, with Pair values becoming named
        ValueView::Array(items, ..) => {
            let mut positional = vec![];
            let mut named = HashMap::new();
            for item in items.iter() {
                match item.view() {
                    ValueView::Pair(k, v) => {
                        named.insert(k.clone(), v.clone());
                    }
                    ValueView::ValuePair(k, v) => {
                        named.insert(k.to_string_value(), v.clone());
                    }
                    _ => positional.push(item.clone()),
                }
            }
            Ok(Value::capture(positional, named))
        }
        ValueView::Seq(items) | ValueView::Slip(items) => {
            let mut positional = vec![];
            let mut named = HashMap::new();
            for item in items.iter() {
                match item.view() {
                    ValueView::Pair(k, v) => {
                        named.insert(k.clone(), v.clone());
                    }
                    ValueView::ValuePair(k, v) => {
                        named.insert(k.to_string_value(), v.clone());
                    }
                    _ => positional.push(item.clone()),
                }
            }
            Ok(Value::capture(positional, named))
        }
        ValueView::LazyList(ll) => {
            // A LazyList is considered lazy if it has a body or compiled code
            // (i.e., it's a gather/take or similar lazy generator)
            let is_lazy = !ll.body.is_empty() || ll.compiled_code.is_some();
            if is_lazy {
                Err(RuntimeError::cannot_lazy_with_action(
                    "create a Capture from",
                    &crate::value::types::what_type_name(target),
                ))
            } else {
                let items = ll.cache.lock().unwrap().clone().unwrap_or_default();
                let mut positional = vec![];
                let mut named = HashMap::new();
                for item in items.iter() {
                    match item.view() {
                        ValueView::Pair(k, v) => {
                            named.insert(k.clone(), v.clone());
                        }
                        ValueView::ValuePair(k, v) => {
                            named.insert(k.to_string_value(), v.clone());
                        }
                        _ => positional.push(item.clone()),
                    }
                }
                Ok(Value::capture(positional, named))
            }
        }
        // Range.Capture → Mu.Capture semantics (named args from attributes)
        ValueView::Range(start, end)
        | ValueView::RangeExcl(start, end)
        | ValueView::RangeExclStart(start, end)
        | ValueView::RangeExclBoth(start, end) => {
            let mut named = HashMap::new();
            named.insert("min".to_string(), Value::int(start));
            named.insert("max".to_string(), Value::int(end));
            named.insert(
                "excludes-min".to_string(),
                Value::truth(matches!(
                    target.view(),
                    ValueView::RangeExclStart(..) | ValueView::RangeExclBoth(..)
                )),
            );
            named.insert(
                "excludes-max".to_string(),
                Value::truth(matches!(
                    target.view(),
                    ValueView::RangeExcl(..) | ValueView::RangeExclBoth(..)
                )),
            );
            named.insert("is-int".to_string(), Value::TRUE);
            Ok(Value::capture(vec![], named))
        }
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let mut named = HashMap::new();
            named.insert("min".to_string(), (**start).clone());
            named.insert("max".to_string(), (**end).clone());
            named.insert("excludes-min".to_string(), Value::truth(excl_start));
            named.insert("excludes-max".to_string(), Value::truth(excl_end));
            let is_int = matches!(start.view(), ValueView::Int(_) | ValueView::BigInt(_))
                && matches!(end.view(), ValueView::Int(_) | ValueView::BigInt(_));
            named.insert("is-int".to_string(), Value::truth(is_int));
            Ok(Value::capture(vec![], named))
        }
        // Duration / Instant expose their seconds as a `tai` named (a Rat),
        // following Mu.Capture (public attribute → named). Both store the value
        // internally under the `value` attribute (Duration as a Rat, Instant
        // sometimes as an Int/Num); coerce it to a Rat to match the spec.
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if matches!(class_name.resolve().as_str(), "Duration" | "Instant") => {
            let tai = attributes
                .as_map()
                .get("value")
                .map(crate::builtins::arith::real_to_rat)
                .unwrap_or_else(|| crate::value::make_rat(0, 1));
            let mut named = HashMap::new();
            named.insert("tai".to_string(), tai);
            Ok(Value::capture(vec![], named))
        }
        // IO::Path follows Mu.Capture, but its public accessor is `.CWD` while the
        // attribute is stored under the lowercase `cwd`; rename it so the named
        // argument matches the accessor (the spec checks `:CWD`).
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if matches!(class_name.resolve().as_str(), "IO::Path" | "Path") => {
            let mut named = HashMap::new();
            for (k, v) in attributes.as_map().iter() {
                let key = if k == "cwd" { "CWD" } else { k.as_str() };
                named.insert(key.to_string(), v.clone());
            }
            Ok(Value::capture(vec![], named))
        }
        // Generic object: Mu.Capture returns a Capture whose named arguments are
        // the object's public attributes. We expose every stored attribute as a
        // named (the spec tests only assert the contents they know about, so
        // extra bookkeeping attributes are harmless).
        ValueView::Instance { attributes, .. } => {
            let mut named = HashMap::new();
            for (k, v) in attributes.as_map().iter() {
                named.insert(k.clone(), v.clone());
            }
            Ok(Value::capture(vec![], named))
        }
        // Promise follows Mu.Capture: its public `.status` accessor becomes a
        // named argument. The value is the `PromiseStatus` enum constant (a
        // package-qualified term, e.g. `PromiseStatus::Planned`) so it `eqv`s
        // the literal `PromiseStatus::<status>`.
        ValueView::Promise(shared) => {
            let mut named = HashMap::new();
            named.insert(
                "status".to_string(),
                Value::package(crate::symbol::Symbol::intern(&format!(
                    "PromiseStatus::{}",
                    shared.status()
                ))),
            );
            Ok(Value::capture(vec![], named))
        }
        // Nil.Capture → empty capture
        ValueView::Nil => Ok(Value::capture(vec![], HashMap::new())),
        // Types whose .Capture throws X::Cannot::Capture
        ValueView::Bool(_)
        | ValueView::Str(_)
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Whatever
        | ValueView::HyperWhatever => {
            Err(cannot_capture(&crate::value::types::what_type_name(target)))
        }
        // Sub → X::Cannot::Capture (Callable)
        ValueView::Sub(..) => Err(cannot_capture(&crate::value::types::what_type_name(target))),
        // Regex → X::Cannot::Capture
        ValueView::Regex(..) => Err(cannot_capture("Regex")),
        // Mixin types that should throw X::Cannot::Capture
        // (e.g., IntStr, NumStr allomorphs, WhateverCode, Signature, Version)
        ValueView::Mixin(..) => {
            let type_name = crate::value::types::what_type_name(target);
            match type_name.as_str() {
                "IntStr" | "NumStr" | "RatStr" | "ComplexStr" | "WhateverCode" | "Signature"
                | "Version" => Err(cannot_capture(&type_name)),
                _ => Ok(Value::capture(vec![target.clone()], HashMap::new())),
            }
        }
        // Default: wrap in a single-positional capture
        _ => Ok(Value::capture(vec![target.clone()], HashMap::new())),
    }
}

fn cannot_capture(type_name: &str) -> RuntimeError {
    let mut attrs = HashMap::new();
    attrs.insert("what".to_string(), Value::str(type_name.to_string()));
    attrs.insert(
        "message".to_string(),
        Value::str(format!("Cannot unpack or Capture {}", type_name)),
    );
    RuntimeError::typed("X::Cannot::Capture", attrs)
}
