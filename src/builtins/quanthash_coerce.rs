// Pure List/Array/Seq/Hash -> QuantHash coercions: `.Set`/`.Bag`/`.Mix` (and the
// mutable `.SetHash`/`.BagHash` variants, which share the same element-folding
// logic and only differ by the mutable flag set by the caller).
//
// These carry no interpreter state (env / registry / type metadata): they fold a
// value's elements into a Set/Bag/Mix purely from the input. They live in
// `builtins/` as the single authoritative implementation shared by both the
// bytecode VM (native dispatch) and the tree-walking interpreter fallback.
//
// Spec: https://docs.raku.org/type/Any (.Set/.Bag/.Mix coercers) and
//       https://docs.raku.org/language/setbagmix
//
// List-context flattening rule: a List `(...)` invocant flattens its elements
// (nested non-itemized Array/Seq/Hash/QuantHash spill), while an Array `[...]`
// (or itemized) invocant takes each element whole — e.g. `[1,[2,3]].Set` has two
// elements but `(1,[2,3]).Set` has three.

use crate::runtime::Interpreter;
use crate::runtime::utils::value_to_list;
use crate::runtime::utils::{
    quanthash_elem_entry, quanthash_insert_set, record_quanthash_original, str_elem_key,
};
use crate::value::{RuntimeError, Value, ValueView};
use num_bigint::BigInt;
use num_traits::Signed;
use std::collections::{HashMap, HashSet};

/// Coerce `target` to a `Set` (immutable). The caller flips the mutable flag for
/// `.SetHash`.
pub(crate) fn to_set(target: Value, what: &str) -> Result<Value, RuntimeError> {
    // Check for lazy/infinite values
    if Interpreter::is_lazy_for_coerce(&target) {
        return Err(RuntimeError::cannot_lazy_what(what));
    }
    let mut elems = HashSet::new();
    let mut original_keys: HashMap<String, Value> = HashMap::new();
    // `flatten` is true when this item sits in a list-context-flattening
    // position (an element of a List `(...)` invocant): nested Lists/Arrays/
    // Seqs/Hashes spill their contents. It is false for elements of an Array
    // `[...]` invocant, where each element is taken whole (matches Raku:
    // `[1,[2,3]].Set` has two elements, but `(1,[2,3]).Set` has three).
    fn add_item(
        elems: &mut HashSet<String>,
        original_keys: &mut HashMap<String, Value>,
        item: &Value,
        flatten: bool,
    ) {
        match item.view() {
            ValueView::Pair(k, v) => {
                if v.truthy() {
                    elems.insert(str_elem_key(k));
                }
            }
            ValueView::ValuePair(k, v) => {
                if v.truthy() {
                    quanthash_insert_set(elems, original_keys, k);
                }
            }
            ValueView::Hash(h) if flatten => {
                // An object hash contributes its key OBJECTS (decoded from the
                // `.WHICH` store keys); a plain hash its string keys.
                for (k, v) in h.iter() {
                    if v.truthy() {
                        if h.has_typed_keys() {
                            quanthash_insert_set(elems, original_keys, &h.typed_key(k));
                        } else {
                            elems.insert(str_elem_key(k));
                        }
                    }
                }
            }
            ValueView::Array(inner, kind) if flatten && !kind.is_itemized() => {
                for inner_item in inner.iter() {
                    add_item(elems, original_keys, inner_item, true);
                }
            }
            ValueView::Seq(inner) | ValueView::Slip(inner) if flatten => {
                for inner_item in inner.iter() {
                    add_item(elems, original_keys, inner_item, true);
                }
            }
            _ => {
                quanthash_insert_set(elems, original_keys, item);
            }
        }
    }
    match target.view() {
        // Always return the immutable variant; the caller flips it for `.SetHash`.
        ValueView::Set(s, _) => return Ok(Value::set_parts(s.clone(), false)),
        // A List `(...)` invocant flattens its elements in list context; an
        // Array `[...]` (or itemized) invocant takes each element whole.
        ValueView::Array(items, crate::value::ArrayKind::List) => {
            for item in items.iter() {
                add_item(&mut elems, &mut original_keys, item, true);
            }
        }
        ValueView::Array(items, ..) => {
            for item in items.iter() {
                add_item(&mut elems, &mut original_keys, item, false);
            }
        }
        ValueView::Seq(items) | ValueView::Slip(items) => {
            for item in items.iter() {
                add_item(&mut elems, &mut original_keys, item, true);
            }
        }
        ValueView::Hash(items) => {
            // An object hash's Set keeps the key OBJECTS (`:{42 => "a"}.Set`
            // is `Set.new(42)`, an Int element); plain hashes keep Str keys.
            for (k, v) in items.iter() {
                if v.truthy() {
                    if items.has_typed_keys() {
                        quanthash_insert_set(&mut elems, &mut original_keys, &items.typed_key(k));
                    } else {
                        elems.insert(str_elem_key(k));
                    }
                }
            }
        }
        ValueView::Bag(b, _) => {
            for k in b.keys() {
                record_quanthash_original(&mut original_keys, k, &b.typed_key(k));
                elems.insert(k.clone());
            }
        }
        ValueView::Mix(m, _) => {
            // A Mix coerces to a Set keeping only keys with a positive weight
            // (`(a=>2,b=>-1,c=>0).Mix.Set` == `Set(a)`); Raku's Setty drops
            // zero and negative weights.
            for (k, w) in m.iter() {
                if *w > 0.0 {
                    record_quanthash_original(&mut original_keys, k, &m.typed_key(k));
                    elems.insert(k.clone());
                }
            }
        }
        ValueView::Pair(k, v) => {
            if v.truthy() {
                elems.insert(str_elem_key(k));
            }
        }
        ValueView::ValuePair(k, v) => {
            if v.truthy() {
                quanthash_insert_set(&mut elems, &mut original_keys, k);
            }
        }
        // Instance types composing Baggy: delegate to internal bag data
        ValueView::Instance { attributes, .. } if attributes.contains_key("__baggy_data__") => {
            let bag_data = attributes.as_map().get("__baggy_data__").unwrap().clone();
            return to_set(bag_data, what);
        }
        _ if target.is_range() => {
            for item in value_to_list(&target) {
                add_item(&mut elems, &mut original_keys, &item, false);
            }
        }
        _ => {
            quanthash_insert_set(&mut elems, &mut original_keys, &target);
        }
    }
    Ok(Value::set_typed(elems, original_keys))
}

/// Bag weight of a pair value: a non-positive weight drops the key, a fractional
/// or string weight is coerced to an `Int`. `what` only affects which lazy error
/// is raised by the caller.
fn pair_weight(v: &Value) -> Result<BigInt, RuntimeError> {
    match v.view() {
        ValueView::Int(i) => Ok(BigInt::from(i)),
        // Weights can exceed i64::MAX (e.g. `{a => 10**20}.Bag`); a BigInt weight
        // is preserved verbatim rather than truncated.
        ValueView::BigInt(n) => Ok((**n).clone()),
        ValueView::Num(n) => {
            if n.is_nan() || n.is_infinite() {
                return Err(RuntimeError::new(format!(
                    "X::Numeric::CannotConvert: Cannot convert {} to Int",
                    v.to_string_value()
                )));
            }
            Ok(BigInt::from(n as i64))
        }
        ValueView::Rat(n, d) if d != 0 => Ok(BigInt::from(n / d)),
        ValueView::FatRat(n, d) if d != 0 => Ok(BigInt::from(n / d)),
        ValueView::Bool(b) => Ok(BigInt::from(i64::from(b))),
        ValueView::Complex(_, _) => Err(RuntimeError::new(
            "X::Numeric::CannotConvert: Cannot convert Complex to Int".to_string(),
        )),
        ValueView::Str(s) => {
            // Strings must be numeric to be valid bag weights
            match s.parse::<BigInt>() {
                Ok(i) => Ok(i),
                Err(_) => Err(RuntimeError::new(format!(
                    "X::Str::Numeric: Cannot convert string '{}' to a number",
                    *s
                ))),
            }
        }
        _ => Ok(BigInt::from(i64::from(v.truthy()))),
    }
}

/// Coerce `target` to a `Bag` (immutable). `what` is the coercer name (`Bag` /
/// `BagHash`) used for the lazy error message. The caller flips the mutable flag
/// for `.BagHash`.
pub(crate) fn to_bag(target: Value, what: &str) -> Result<Value, RuntimeError> {
    // Check for lazy/infinite inputs
    if Interpreter::is_lazy_for_coerce(&target) {
        return Err(RuntimeError::cannot_lazy_what(what));
    }
    let mut counts: HashMap<String, BigInt> = HashMap::new();
    let mut original_keys: HashMap<String, Value> = HashMap::new();

    fn add_item(
        counts: &mut HashMap<String, BigInt>,
        original_keys: &mut HashMap<String, Value>,
        item: &Value,
    ) -> Result<(), RuntimeError> {
        match item.view() {
            ValueView::Pair(k, v) => {
                let weight = pair_weight(v)?;
                if weight.is_positive() {
                    *counts.entry(str_elem_key(k)).or_default() += weight;
                }
            }
            ValueView::ValuePair(k, v) => {
                let (key, elem) = quanthash_elem_entry(k);
                let weight = pair_weight(v)?;
                if weight.is_positive() {
                    record_quanthash_original(original_keys, &key, &elem);
                    *counts.entry(key).or_default() += weight;
                }
            }
            _ => {
                let (key, elem) = quanthash_elem_entry(item);
                record_quanthash_original(original_keys, &key, &elem);
                *counts.entry(key).or_default() += 1;
            }
        }
        Ok(())
    }

    /// Flatten items from a value into the bag. `flatten` is true when the
    /// value sits in a list-context-flattening position (an element of a
    /// List `(...)` invocant): nested non-itemized arrays, seqs, hashes and
    /// quant-hashes spill. When false (an element of an Array `[...]`
    /// invocant) the value is taken whole as a single key (matches Raku:
    /// `[1,[2,3]].Bag` has two keys, `(1,[2,3]).Bag` has three).
    fn flatten_into(
        counts: &mut HashMap<String, BigInt>,
        original_keys: &mut HashMap<String, Value>,
        value: &Value,
        flatten: bool,
    ) -> Result<(), RuntimeError> {
        match value.view() {
            ValueView::Array(items, kind) if flatten && !kind.is_itemized() => {
                for item in items.iter() {
                    flatten_into(counts, original_keys, item, true)?;
                }
            }
            ValueView::Seq(items) | ValueView::Slip(items) if flatten => {
                for item in items.iter() {
                    flatten_into(counts, original_keys, item, true)?;
                }
            }
            ValueView::Hash(h) if flatten => {
                for (k, v) in h.iter() {
                    let weight = pair_weight(v)?;
                    if weight.is_positive() {
                        // Object hashes contribute their key OBJECTS.
                        let key = if h.has_typed_keys() {
                            let (key, obj) = quanthash_elem_entry(&h.typed_key(k));
                            record_quanthash_original(original_keys, &key, &obj);
                            key
                        } else {
                            str_elem_key(k)
                        };
                        *counts.entry(key).or_default() += weight;
                    }
                }
            }
            ValueView::Set(s, _) if flatten => {
                for k in s.iter() {
                    record_quanthash_original(original_keys, k, &s.typed_key(k));
                    counts.insert(k.clone(), BigInt::from(1));
                }
            }
            ValueView::Mix(m, _) if flatten => {
                for (k, v) in m.iter() {
                    record_quanthash_original(original_keys, k, &m.typed_key(k));
                    counts.insert(k.clone(), BigInt::from(*v as i64));
                }
            }
            ValueView::Bag(b, _) if flatten => {
                for (k, v) in b.iter() {
                    record_quanthash_original(original_keys, k, &b.typed_key(k));
                    *counts.entry(k.clone()).or_default() += v.clone();
                }
            }
            _ => {
                add_item(counts, original_keys, value)?;
            }
        }
        Ok(())
    }

    match target.view() {
        // Always return the immutable variant; the caller flips it for `.BagHash`.
        ValueView::Bag(b, _) => return Ok(Value::bag_parts(b.clone(), false)),
        ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => {
            add_item(&mut counts, &mut original_keys, &target)?;
        }
        _ if target.is_range() => {
            for item in value_to_list(&target) {
                let (key, elem) = quanthash_elem_entry(&item);
                record_quanthash_original(&mut original_keys, &key, &elem);
                *counts.entry(key).or_default() += 1;
            }
        }
        // A List `(...)` invocant flattens its elements in list context; an
        // Array `[...]` (or itemized) invocant takes each element whole.
        ValueView::Array(items, crate::value::ArrayKind::List) => {
            for item in items.iter() {
                flatten_into(&mut counts, &mut original_keys, item, true)?;
            }
        }
        ValueView::Seq(items) => {
            for item in items.iter() {
                flatten_into(&mut counts, &mut original_keys, item, true)?;
            }
        }
        ValueView::Array(items, _) => {
            for item in items.iter() {
                flatten_into(&mut counts, &mut original_keys, item, false)?;
            }
        }
        _ => {
            // Flatten the target into a list and process each item.
            // This handles tuples/lists like (@a, %x).Bag where arrays
            // and hashes need to be expanded.
            let items = value_to_list(&target);
            if items.is_empty()
                && !matches!(target.view(), ValueView::Array(_, _) | ValueView::Hash(_))
            {
                // Single non-collection value
                add_item(&mut counts, &mut original_keys, &target)?;
            } else {
                for item in &items {
                    flatten_into(&mut counts, &mut original_keys, item, true)?;
                }
            }
        }
    }
    Ok(Value::bag_typed_big(counts, original_keys))
}

/// Mix weight of a pair value: like `pair_weight` but keeps the fractional
/// `Real` weight (and raises `X::OutOfRange`/`X::Numeric::Real`/`X::Str::Numeric`
/// for Inf/NaN/Complex/non-numeric strings, matching Raku).
pub(crate) fn mix_pair_weight(v: &Value) -> Result<f64, RuntimeError> {
    match v.view() {
        ValueView::Int(i) => Ok(i as f64),
        ValueView::Num(n) => {
            if n.is_infinite() {
                let mut err = RuntimeError::new(format!(
                    "Value out of range. Is: {}, should be in -Inf^..^Inf",
                    if n > 0.0 { "Inf" } else { "-Inf" }
                ));
                err.exception = Some(Box::new(Value::make_instance(
                    crate::symbol::Symbol::intern("X::OutOfRange"),
                    [
                        ("what".to_string(), Value::str_from("Value")),
                        ("got".to_string(), Value::num(n)),
                        ("range".to_string(), Value::str_from("-Inf^..^Inf")),
                    ]
                    .into_iter()
                    .collect::<crate::value::AttrMap>(),
                )));
                Err(err)
            } else if n.is_nan() {
                let mut err =
                    RuntimeError::new("Value out of range. Is: NaN, should be in -Inf^..^Inf");
                err.exception = Some(Box::new(Value::make_instance(
                    crate::symbol::Symbol::intern("X::OutOfRange"),
                    [
                        ("what".to_string(), Value::str_from("Value")),
                        ("got".to_string(), Value::num(n)),
                        ("range".to_string(), Value::str_from("-Inf^..^Inf")),
                    ]
                    .into_iter()
                    .collect::<crate::value::AttrMap>(),
                )));
                Err(err)
            } else {
                Ok(n)
            }
        }
        ValueView::Rat(n, d) if d != 0 => Ok(n as f64 / d as f64),
        ValueView::Bool(b) => Ok(if b { 1.0 } else { 0.0 }),
        ValueView::Complex(_, _) => {
            let mut err = RuntimeError::new(
                "Cannot convert Complex to Real; use .re or .im to extract components",
            );
            err.exception = Some(Box::new(Value::make_instance(
                crate::symbol::Symbol::intern("X::Numeric::Real"),
                [
                    ("source".to_string(), v.clone()),
                    (
                        "reason".to_string(),
                        Value::str_from("Complex to Real conversion"),
                    ),
                    ("target".to_string(), Value::str_from("Real")),
                ]
                .into_iter()
                .collect::<crate::value::AttrMap>(),
            )));
            Err(err)
        }
        ValueView::Str(s) => {
            // Try to parse the string as a number
            if let Ok(n) = s.parse::<f64>() {
                if n.is_infinite() || n.is_nan() {
                    let mut err = RuntimeError::new(format!(
                        "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}' (Str)",
                        *s
                    ));
                    err.exception = Some(Box::new(Value::make_instance(
                        crate::symbol::Symbol::intern("X::Str::Numeric"),
                        [
                            ("source".to_string(), Value::str(s.to_string())),
                            (
                                "reason".to_string(),
                                Value::str_from(
                                    "base-10 number must begin with valid digits or '.'",
                                ),
                            ),
                        ]
                        .into_iter()
                        .collect::<crate::value::AttrMap>(),
                    )));
                    return Err(err);
                }
                Ok(n)
            } else {
                let mut err = RuntimeError::new(format!(
                    "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}' (Str)",
                    *s
                ));
                err.exception = Some(Box::new(Value::make_instance(
                    crate::symbol::Symbol::intern("X::Str::Numeric"),
                    [
                        ("source".to_string(), Value::str(s.to_string())),
                        (
                            "reason".to_string(),
                            Value::str_from("base-10 number must begin with valid digits or '.'"),
                        ),
                    ]
                    .into_iter()
                    .collect::<crate::value::AttrMap>(),
                )));
                Err(err)
            }
        }
        _ => {
            if v.truthy() {
                Ok(1.0)
            } else {
                Ok(0.0)
            }
        }
    }
}

/// The exact numeric weight of a Mix pair value, preserving its `Rat`/`Int`
/// representation so repeated keys accumulate with exact rational arithmetic
/// (`0.1 + 0.02` must sum to `0.12`, not the lossy f64 `0.12000000000000001`).
/// Delegates to [`mix_pair_weight`] for the Inf/NaN/Complex/bad-Str validation
/// errors, then returns the exact `Value` rather than that validated f64.
pub(crate) fn mix_pair_weight_value(v: &Value) -> Result<Value, RuntimeError> {
    let f = mix_pair_weight(v)?;
    match v.view() {
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::BigRat(..)
        | ValueView::FatRat(..) => Ok(v.clone()),
        ValueView::Bool(b) => Ok(Value::int(i64::from(b))),
        // Str / other coercible types fall back to the validated f64 — none of
        // these carry exactly-representable rationals through this path.
        _ => Ok(Value::num(f)),
    }
}

/// Accumulate `w` into `weights[key]` using exact `Value` arithmetic
/// (`arith_add` keeps `Rat + Rat` exact). Mix weights are folded as `Value`s
/// during construction and only lowered to `f64` at the coercion boundary.
fn mix_accum(
    weights: &mut HashMap<String, Value>,
    key: String,
    w: Value,
) -> Result<(), RuntimeError> {
    use std::collections::hash_map::Entry;
    match weights.entry(key) {
        Entry::Occupied(mut e) => {
            let sum = crate::builtins::arith::arith_add(e.get().clone(), w)?;
            *e.get_mut() = sum;
        }
        Entry::Vacant(e) => {
            e.insert(w);
        }
    }
    Ok(())
}

fn mix_add_item_with_keys(
    weights: &mut HashMap<String, Value>,
    original_keys: &mut HashMap<String, Value>,
    item: &Value,
    flatten: bool,
) -> Result<(), RuntimeError> {
    match item.view() {
        ValueView::Pair(k, v) => {
            mix_accum(weights, str_elem_key(k), mix_pair_weight_value(v)?)?;
        }
        ValueView::ValuePair(k, v) => {
            let w = mix_pair_weight_value(v)?;
            let (key, elem) = quanthash_elem_entry(k);
            record_quanthash_original(original_keys, &key, &elem);
            mix_accum(weights, key, w)?;
        }
        ValueView::Hash(h) if flatten => {
            for (k, v) in h.iter() {
                let w = mix_pair_weight_value(v)?;
                if w.to_f64() != 0.0 {
                    // Object hashes contribute their key OBJECTS.
                    let key = if h.has_typed_keys() {
                        let (key, obj) = quanthash_elem_entry(&h.typed_key(k));
                        record_quanthash_original(original_keys, &key, &obj);
                        key
                    } else {
                        str_elem_key(k)
                    };
                    mix_accum(weights, key, w)?;
                }
            }
        }
        // A nested non-itemized array / seq flattens one level in
        // list-context (List `(...)` invocant); an Array `[...]` invocant
        // element (flatten == false) is kept whole.
        ValueView::Array(items, kind) if flatten && !kind.is_itemized() => {
            for sub_item in items.iter() {
                mix_add_item_with_keys(weights, original_keys, sub_item, true)?;
            }
        }
        ValueView::Seq(items) | ValueView::Slip(items) if flatten => {
            for sub_item in items.iter() {
                mix_add_item_with_keys(weights, original_keys, sub_item, true)?;
            }
        }
        ValueView::Set(s, _) if flatten => {
            for k in s.iter() {
                record_quanthash_original(original_keys, k, &s.typed_key(k));
                mix_accum(weights, k.clone(), Value::int(1))?;
            }
        }
        ValueView::Bag(b, _) if flatten => {
            for (k, v) in b.iter() {
                record_quanthash_original(original_keys, k, &b.typed_key(k));
                mix_accum(weights, k.clone(), Value::bigint(v.clone()))?;
            }
        }
        ValueView::Mix(m, _) if flatten => {
            for (k, v) in m.iter() {
                record_quanthash_original(original_keys, k, &m.typed_key(k));
                // A Mix already stores f64 weights, so this fold is inherently
                // f64-precision (there is no exact source to recover here).
                mix_accum(weights, k.clone(), Value::num(*v))?;
            }
        }
        _ => {
            let (key, elem) = quanthash_elem_entry(item);
            record_quanthash_original(original_keys, &key, &elem);
            mix_accum(weights, key, Value::int(1))?;
        }
    }
    Ok(())
}

/// Coerce `target` to a `Mix` (immutable). The caller flips the mutable flag (and
/// registers `MixHash` type metadata) for `.MixHash`.
pub(crate) fn to_mix(target: Value, what: &str) -> Result<Value, RuntimeError> {
    // Check for lazy iterables
    if Interpreter::is_lazy_for_set_ops(&target) {
        let mut err = RuntimeError::new(format!("Cannot .{} a lazy list", what));
        err.exception = Some(Box::new(Value::make_instance(
            crate::symbol::Symbol::intern("X::Cannot::Lazy"),
            [("what".to_string(), Value::str_from(what))]
                .into_iter()
                .collect::<crate::value::AttrMap>(),
        )));
        return Err(err);
    }
    // Weights are folded as exact `Value`s (so `0.1 + 0.02` stays `0.12`) and
    // lowered to the stored `f64` representation only at the return boundary.
    let mut weights: HashMap<String, Value> = HashMap::new();
    let mut original_keys: HashMap<String, Value> = HashMap::new();
    match target.view() {
        // Always return the immutable variant; the caller flips it for `.MixHash`.
        ValueView::Mix(m, _) => return Ok(Value::mix_parts(m.clone(), false)),
        // A List `(...)` invocant flattens its elements in list context; an
        // Array `[...]` (or itemized) invocant takes each element whole.
        ValueView::Array(items, crate::value::ArrayKind::List) => {
            for item in items.iter() {
                mix_add_item_with_keys(&mut weights, &mut original_keys, item, true)?;
            }
        }
        ValueView::Seq(items) | ValueView::Slip(items) => {
            for item in items.iter() {
                mix_add_item_with_keys(&mut weights, &mut original_keys, item, true)?;
            }
        }
        ValueView::Array(items, ..) => {
            for item in items.iter() {
                mix_add_item_with_keys(&mut weights, &mut original_keys, item, false)?;
            }
        }
        _ if matches!(
            target.view(),
            ValueView::Set(_, _)
                | ValueView::Bag(_, _)
                | ValueView::Pair(..)
                | ValueView::ValuePair(..)
                | ValueView::Hash(_)
        ) =>
        {
            mix_add_item_with_keys(&mut weights, &mut original_keys, &target, true)?;
        }
        _ if target.is_range() => {
            for item in value_to_list(&target) {
                let (key, elem) = quanthash_elem_entry(&item);
                record_quanthash_original(&mut original_keys, &key, &elem);
                mix_accum(&mut weights, key, Value::int(1))?;
            }
        }
        _ => {
            let (key, elem) = quanthash_elem_entry(&target);
            record_quanthash_original(&mut original_keys, &key, &elem);
            mix_accum(&mut weights, key, Value::int(1))?;
        }
    }
    // Lower the exact-`Value` weights to the stored `f64` representation.
    let weights: HashMap<String, f64> = weights.into_iter().map(|(k, v)| (k, v.to_f64())).collect();
    Ok(Value::mix_with_original_keys(weights, original_keys))
}

/// Coerce `target` to a mutable `MixHash`. This is `.Mix` plus the mutable flag
/// and the embedded `MixHash` type metadata (`value_type = Real`,
/// `declared_type = MixHash`). The metadata lives *in* the Mix backing store (not
/// in any interpreter-owned side table — container type metadata has been
/// embedded in the value since #2952), so this is a pure value operation with no
/// interpreter state, mirroring the interpreter's `dispatch_to_mix_with_what` +
/// `tag_container_metadata` path exactly. Single authoritative impl shared by the
/// VM native dispatch and the interpreter fallback.
pub(crate) fn to_mixhash(target: Value) -> Result<Value, RuntimeError> {
    let mut coerced = to_mix(target, "MixHash")?;
    // `to_mix` always returns a Mix for the receivers we coerce; if that
    // ever changes, `with_mix_mut` runs no closure and hands the value back
    // unmodified rather than panic.
    coerced.with_mix_mut(|arc, is_mut| {
        let data = crate::gc::Gc::make_mut(arc);
        data.value_type = Some("Real".to_string());
        data.key_type = None;
        data.declared_type = Some("MixHash".to_string());
        *is_mut = true;
    });
    Ok(coerced)
}
