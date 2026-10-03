use super::*;
use crate::value::ValueMap;

/// The concrete key(s) a hash-initializer pair stores its value under. A Junction
/// key threads over its members (`"a"|"b" => 1` stores 1 under both `a` and `b`,
/// per Rakudo); every other key is itself.
pub(crate) fn hash_pair_keys(key: &Value) -> Vec<Value> {
    if let ValueView::Junction { values, .. } = key.view() {
        values.iter().cloned().collect()
    } else {
        vec![key.clone()]
    }
}

/// ADR-0049 slice 2: a Hash value is a `Scalar` container, so a stored `Nil`
/// decays to `Any` (an untyped hash's own default -- these builders never see
/// a pre-existing typed/`is default(...)` container to decay against, so
/// `Any` is exactly what `Interpreter::typed_container_default` would compute
/// here anyway). Applied only at genuine pair-value inserts, not at the
/// separate odd-trailing-key fallback (a pre-existing, out-of-scope
/// divergence unrelated to this ADR).
fn decay_nil_hash_value(v: Value) -> Value {
    if v.is_nil() {
        Value::package(crate::symbol::wk::any())
    } else {
        v
    }
}

/// ADR-0040 slice 2 (the Hash half): a `Hash` value is a `Scalar` container,
/// so an aggregate stored as a hash value is one item and renders itemized
/// (`%h<a>.raku` is `$[1, 2]`). Composed with the ADR-0049 `Nil` decay, which
/// runs first (a decayed `Nil` becomes `Any`, which never itemizes).
fn hash_stored_value(v: Value) -> Value {
    // Decontainerize first. Building a Hash from a list of Pairs is an
    // ASSIGNMENT of their values, not a bind — `%a = %reset.pairs` must copy
    // what `%reset`'s elements hold, never adopt the elements themselves. Since
    // ADR-0036 slice 3 makes `.pairs` hand out those elements' own `Scalar`
    // containers, storing the pair value as-is aliased the two hashes together,
    // so a later in-place mutation of one silently rewrote the other
    // (roast/S03-metaops/infix.t's `%a = %reset.pairs` reset lines).
    decay_nil_hash_value(v)
        .deitemize_element()
        .itemize_for_element_store()
}

/// Is this parameter name a **sigilless** binding (`\c`)?
///
/// The gate on [`shape_value_for_sigiled_target`] at the exit-writeback sites:
/// only a sigilless parameter is the caller's container rather than having one
/// of its own, so only it needs its final value re-shaped for the caller's
/// sigil. A plain `@`/`%` parameter already carries the caller's own container,
/// metadata and all, and re-coercing that strips it (a `%_` parameter relayed
/// into `CompUnit::DependencySpecification.new` lost its values that way).
pub(crate) fn param_is_sigilless(name: &str) -> bool {
    name.as_bytes()
        .first()
        .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
}

/// Shape a value the way a store through `target`'s own sigil would.
///
/// A sigilless alias is not a container of its own: `\c := @a` makes `c` BE
/// `@a`, so `c = LIST` is `@a.STORE(LIST)` and `@a` stays an `Array`. The
/// writeback that carries a sigilless parameter's final value back to the
/// caller's variable passed it through verbatim, so `sub f(\c) { c = ('x','y') }`
/// left the caller's `my @a` holding a bare `List` and a `my %h` holding that
/// same List rather than a Hash. That is `Crane.add(@a, :path(), :value(...),
/// :in-place)` — `add-to-positional`'s `container = $value` through a
/// `\container` chain.
///
/// A `$`-sigiled or bare target keeps the value untouched, and so does a value
/// that is ALREADY the target's own shape — returning it verbatim is what keeps
/// this off the overwhelmingly common path, where the writeback carries a plain
/// `@`/`%` parameter's container back and re-coercing it would strip its
/// embedded type metadata and its itemization.
///
/// Only a value of the wrong shape is rebuilt, and only then is the scalar
/// itemization a relay hop puts on it unwrapped: each `\c`-to-`\c` hop binds
/// through an itemized holder, so a two-hop chain arrives as `$("x", "y")`
/// where the one-hop one arrives as `("x", "y")`, and both have to land as
/// `["x", "y"]`.
pub(crate) fn shape_value_for_sigiled_target(target: &str, val: &Value) -> Value {
    // An object under `@`/`%` is a `my @a is Foo` container (Tuple, ValueList):
    // it already IS the variable's own shape, so the writeback of an unchanged
    // `\c` parameter must not rebuild it into an Array/Hash.
    // TODO: `\c = $obj` (a lone object stored through a sigilless alias) should
    // wrap it as `[$obj]`; telling that apart needs the class's Positional/
    // Associative role, which this registry-free helper cannot see.
    if matches!(target.as_bytes().first(), Some(b'@' | b'%'))
        && matches!(val.view(), ValueView::Instance { .. })
    {
        return val.clone();
    }
    match target.as_bytes().first() {
        Some(b'@') => {
            if matches!(val.view(), ValueView::Array(_, k) if k.is_real_array()) {
                return val.clone();
            }
            let val = val.clone().deitemize_element().into_descalarized();
            if matches!(val.view(), ValueView::Array(_, k) if k.is_real_array()) {
                val
            } else {
                Value::real_array(crate::runtime::utils::value_to_list(&val))
            }
        }
        Some(b'%') => {
            if matches!(val.view(), ValueView::Hash(_)) && !val.hash_is_itemized() {
                return val.clone();
            }
            let val = val.clone().deitemize_element().into_descalarized();
            if matches!(val.view(), ValueView::Hash(_)) {
                val
            } else {
                coerce_to_hash(val)
            }
        }
        _ => val.clone(),
    }
}

pub(crate) fn coerce_to_hash(value: Value) -> Value {
    let mix_weight_value = crate::value::mix_weight_to_value;
    let value = value.into_descalarized();
    match value.view() {
        ValueView::Hash(_) => value.clone(),
        ValueView::Array(items, ..) => {
            // Flatten nested Hashes into pairs before building the hash.
            // This handles `%h = %h1, %h2` where each hash should be merged.
            // Itemized arrays ($[...]) are NOT flattened — they are treated
            // as opaque items, matching Raku's Scalar-container semantics.
            let mut flat: Vec<Value> = Vec::with_capacity(items.len());
            for item in items.iter() {
                if let ValueView::Hash(h) = item.view() {
                    // An object hash stores `.WHICH` keys — flatten via the
                    // original key objects (a plain hash's typed_pair is the
                    // plain `Pair(str_key, v)` as before).
                    for (k, v) in h.iter() {
                        flat.push(h.typed_pair(k, v.clone()));
                    }
                } else {
                    flat.push(item.clone());
                }
            }
            let mut map = ValueMap::default();
            let mut original_keys: ValueMap = ValueMap::default();
            let mut i = 0;
            while i < flat.len() {
                if let ValueView::Pair(k, v) = flat[i].view() {
                    // A Pair value built by `key => $var` is a write-through
                    // `ContainerRef`; storing into a Hash decontainerizes (copies
                    // the value), matching Raku (`%h = k => $v; $v = 2` leaves
                    // `%h<k>` unchanged).
                    map.insert(k.clone(), hash_stored_value(v.deref_container()));
                    i += 1;
                } else if let ValueView::ValuePair(k, v) = flat[i].view() {
                    let dv = hash_stored_value(v.deref_container());
                    for kk in hash_pair_keys(k) {
                        let str_key = kk.to_string_value();
                        if !matches!(kk.view(), ValueView::Str(_)) {
                            original_keys.insert(str_key.clone(), kk.clone());
                        }
                        map.insert(str_key, dv.clone());
                    }
                    i += 1;
                } else {
                    let key_val = &flat[i];
                    let str_key = key_val.to_string_value();
                    if !matches!(key_val.view(), ValueView::Str(_)) {
                        original_keys.insert(str_key.clone(), key_val.clone());
                    }
                    // ADR-0049 slice 5: an unpaired trailing key gets the
                    // standard `Package("Any")` gap marker instead of a raw
                    // `Value::NIL` -- `Nil` is no longer a hole sentinel.
                    // (The separate divergence from raku's actual "Odd
                    // number of elements" die here is unrelated and
                    // out-of-scope -- see `decay_nil_hash_value`'s doc
                    // comment above.)
                    let val = if i + 1 < flat.len() {
                        flat[i + 1].clone()
                    } else {
                        Value::package(crate::symbol::wk::any())
                    };
                    map.insert(str_key, val.itemize_for_element_store());
                    i += 2;
                }
            }
            set_hash_original_keys(Value::hash(map), original_keys)
        }
        ValueView::Seq(_) | ValueView::HyperSeq(_) | ValueView::RaceSeq(_) | ValueView::Slip(_) => {
            let items = crate::runtime::utils::value_to_list(&value);
            let items = &items[..];
            let mut map = ValueMap::default();
            let mut original_keys: ValueMap = ValueMap::default();
            let mut i = 0;
            while i < items.len() {
                if let ValueView::Pair(k, v) = items[i].view() {
                    map.insert(k.clone(), hash_stored_value(v.deref_container()));
                    i += 1;
                } else if let ValueView::ValuePair(k, v) = items[i].view() {
                    let dv = hash_stored_value(v.deref_container());
                    for kk in hash_pair_keys(k) {
                        let str_key = kk.to_string_value();
                        if !matches!(kk.view(), ValueView::Str(_)) {
                            original_keys.insert(str_key.clone(), kk.clone());
                        }
                        map.insert(str_key, dv.clone());
                    }
                    i += 1;
                } else {
                    let key_val = &items[i];
                    let str_key = key_val.to_string_value();
                    if !matches!(key_val.view(), ValueView::Str(_)) {
                        original_keys.insert(str_key.clone(), key_val.clone());
                    }
                    // ADR-0049 slice 5: see the twin comment on the Array
                    // arm above.
                    let val = if i + 1 < items.len() {
                        items[i + 1].clone()
                    } else {
                        Value::package(crate::symbol::wk::any())
                    };
                    map.insert(str_key, val.itemize_for_element_store());
                    i += 2;
                }
            }
            set_hash_original_keys(Value::hash(map), original_keys)
        }
        ValueView::Pair(k, v) => {
            let mut map = ValueMap::default();
            map.insert(k.clone(), hash_stored_value(v.deref_container()));
            Value::hash(map)
        }
        ValueView::ValuePair(k, v) => {
            let mut map = ValueMap::default();
            let mut original_keys: ValueMap = ValueMap::default();
            let dv = hash_stored_value(v.deref_container());
            for kk in hash_pair_keys(k) {
                let str_key = kk.to_string_value();
                if !matches!(kk.view(), ValueView::Str(_)) {
                    original_keys.insert(str_key.clone(), kk.clone());
                }
                map.insert(str_key, dv.clone());
            }
            set_hash_original_keys(Value::hash(map), original_keys)
        }
        ValueView::Set(items, _) => {
            let mut map = ValueMap::default();
            let mut original_keys: ValueMap = ValueMap::default();
            let mut has_typed = false;
            // The store key is a `.WHICH` string; the produced Hash is
            // display-string-keyed with the element object recorded.
            for key in items.iter() {
                let typed = items.typed_key(key);
                let display = typed.to_string_value();
                map.insert(display.clone(), Value::TRUE);
                if !matches!(typed.view(), ValueView::Str(_)) {
                    has_typed = true;
                    original_keys.insert(display, typed);
                }
            }
            let mut result = Value::hash(map);
            if has_typed {
                original_keys.insert("__mutsu_setty_origin".to_string(), Value::TRUE);
                result = set_hash_original_keys(result, original_keys);
            }
            result
        }
        ValueView::Bag(items, _) => {
            let mut map = ValueMap::default();
            let mut original_keys: ValueMap = ValueMap::default();
            let mut has_typed = false;
            for (key, count) in items.iter() {
                let typed = items.typed_key(key);
                let display = typed.to_string_value();
                map.insert(display.clone(), Value::from_bigint(count.clone()));
                if !matches!(typed.view(), ValueView::Str(_)) {
                    has_typed = true;
                    original_keys.insert(display, typed);
                }
            }
            let mut result = Value::hash(map);
            if has_typed {
                original_keys.insert("__mutsu_setty_origin".to_string(), Value::TRUE);
                result = set_hash_original_keys(result, original_keys);
            }
            result
        }
        ValueView::Mix(items, _) => {
            let mut map = ValueMap::default();
            let mut original_keys: ValueMap = ValueMap::default();
            let mut has_typed = false;
            for (key, weight) in items.iter() {
                let typed = items.typed_key(key);
                let display = typed.to_string_value();
                map.insert(display.clone(), mix_weight_value(*weight));
                if !matches!(typed.view(), ValueView::Str(_)) {
                    has_typed = true;
                    original_keys.insert(display, typed);
                }
            }
            let mut result = Value::hash(map);
            if has_typed {
                original_keys.insert("__mutsu_setty_origin".to_string(), Value::TRUE);
                result = set_hash_original_keys(result, original_keys);
            }
            result
        }
        ValueView::Range(a, b) => {
            let items: Vec<Value> = (a..=b).map(Value::int).collect();
            coerce_to_hash(Value::array_with_kind(
                crate::value::Value::array_arc(items),
                ArrayKind::List,
            ))
        }
        ValueView::RangeExcl(a, b) => {
            let items: Vec<Value> = (a..b).map(Value::int).collect();
            coerce_to_hash(Value::array_with_kind(
                crate::value::Value::array_arc(items),
                ArrayKind::List,
            ))
        }
        ValueView::RangeExclStart(a, b) => {
            let items: Vec<Value> = (a + 1..=b).map(Value::int).collect();
            coerce_to_hash(Value::array_with_kind(
                crate::value::Value::array_arc(items),
                ArrayKind::List,
            ))
        }
        ValueView::RangeExclBoth(a, b) => {
            let items: Vec<Value> = (a + 1..b).map(Value::int).collect();
            coerce_to_hash(Value::array_with_kind(
                crate::value::Value::array_arc(items),
                ArrayKind::List,
            ))
        }
        ValueView::Nil => Value::hash(ValueMap::default()),
        ValueView::Instance { .. } if value.is_match_instance() => {
            // %($/) returns the named captures hash
            value
                .match_named()
                .unwrap_or_else(|| Value::hash(ValueMap::default()))
        }
        _ => {
            // ADR-0049 slice 5: same rationale as the two odd-trailing-key
            // arms above -- a single scalar coerced to a Hash with no paired
            // value gets the standard `Package("Any")` gap marker instead of
            // a raw `Value::NIL`.
            let mut map = ValueMap::default();
            map.insert(
                value.to_string_value(),
                Value::package(crate::symbol::wk::any()),
            );
            Value::hash(map)
        }
    }
}

pub(crate) fn build_hash_from_items(items: Vec<Value>) -> Result<Value, RuntimeError> {
    // Stringify each non-`Str` key and record the original in `original_keys` so
    // an object-hash re-tag (`%h{Any} = ..., Foo, $o`) and `.antipairs`/`.invert`
    // can recover the real key. A bare type object stringifies to its gist here;
    // the empty-string-with-warning coercion is a *plain* (`Str`-keyed) hash
    // semantic, applied only by the interpreter-aware `build_hash_from_items_warning`.
    build_hash_from_items_with_key_coercion(items, |kk| {
        Ok((
            Value::hash_key_encode(kk),
            !matches!(kk.view(), ValueView::Str(_)),
        ))
    })
}

/// Build a `Hash` from a flat item list, mapping each *bare* (non-`Pair`) key
/// value to its string key via `encode_key`. `encode_key` returns
/// `(string_key, record_original)`: when `record_original` is true the original
/// key `Value` is remembered in the hash's `original_keys` side table (so
/// `.keys` can recover a non-`Str` key); a type object coerced to `""` returns
/// `false`, matching Rakudo's plain-`Str` `""` key. Pair/Hash flattening and
/// the "Odd number of elements" error are handled here regardless of the hook.
/// Cost: O(e), e = items of the initializer list (plus pairs of any bare Hash
/// item flattened in); one hash insert each. Backs `%h = @pairs` and `%(@a)`.
pub(crate) fn build_hash_from_items_with_key_coercion<F>(
    items: Vec<Value>,
    mut encode_key: F,
) -> Result<Value, RuntimeError>
where
    F: FnMut(&Value) -> Result<(String, bool), RuntimeError>,
{
    let total_items = items.len();
    let last_item = items.last().cloned();
    let mut map = ValueMap::default();
    let mut original_keys: ValueMap = ValueMap::default();
    // An itemized Pair (`$(:a(1))`) or a Pair held in a `:=` element cell (e.g.
    // a classify bucket element) still counts as a hash initializer pair; an
    // itemized *hash* stays opaque and dies "Odd number" like raku. A package
    // stash item (`%(Foo::EXPORT::DEFAULT::, Bar::EXPORT::DEFAULT::)`, a
    // `sub EXPORT` re-export) is a Map of its symbols and flattens likewise.
    let items: Vec<Value> = items
        .iter()
        .map(crate::builtins::map_hash_coerce::unwrap_contained_pair)
        .map(|v| crate::builtins::map_hash_coerce::stash_symbols(&v).unwrap_or(v))
        .collect();
    let mut iter = items.into_iter();
    while let Some(item) = iter.next() {
        match item.view() {
            ValueView::Pair(key, boxed_val) => {
                map.insert(key.clone(), hash_stored_value(boxed_val.clone()));
            }
            // A bare (non-itemized) hash in list context flattens into its
            // key=>value pairs (`%m = (%h,)` / `%(%h,)`). A hash sourced from a
            // `$` scalar carries the per-holder itemization flag (set by
            // `itemize_value`) and stays an opaque single element — matching
            // Raku, where `%m = ($hashitem,)` dies "Odd number". An object hash
            // stores `.WHICH` keys: flatten via the original key objects, so a
            // plain target sees their stringifications and an object-hash
            // target (re-keyed by `tag_container_metadata`) keeps the objects.
            ValueView::Hash(h) if !item.hash_is_itemized() => {
                if h.has_typed_keys() {
                    for (k, v) in h.iter() {
                        let key_obj = h.typed_key(k);
                        let str_key = Value::hash_key_encode(&key_obj);
                        if !matches!(key_obj.view(), ValueView::Str(_)) {
                            original_keys.insert(str_key.clone(), key_obj);
                        }
                        map.insert(str_key, hash_stored_value(v.clone()));
                    }
                } else {
                    for (k, v) in h.iter() {
                        map.insert(k.clone(), hash_stored_value(v.clone()));
                    }
                }
            }
            // A Junction key (`"a"|"b" => 1`) is not itself a key: it threads,
            // storing the value under each of its members (`%h<a> == %h<b> == 1`),
            // matching Rakudo. Every other non-Str key stringifies as usual.
            ValueView::ValuePair(key, boxed_val) => {
                let boxed_val = hash_stored_value(boxed_val.clone());
                for kk in hash_pair_keys(key) {
                    let (str_key, record_original) = encode_key(&kk)?;
                    if record_original {
                        original_keys.insert(str_key.clone(), kk.clone());
                    }
                    map.insert(str_key, boxed_val.clone());
                }
            }
            _ => {
                let Some(value) = iter.next() else {
                    return Err(crate::builtins::map_hash_coerce::odd_number_error(
                        total_items,
                        last_item.as_ref(),
                    ));
                };
                let (str_key, record_original) = encode_key(&item)?;
                if record_original {
                    original_keys.insert(str_key.clone(), item.clone());
                }
                map.insert(str_key, hash_stored_value(value));
            }
        }
    }
    Ok(set_hash_original_keys(Value::hash(map), original_keys))
}

/// Coerce a value into a real `Array` (the list-assign / `.Array` tail).
///
/// ADR-0040 slice 2: every element of the resulting real `Array` is a
/// `Scalar` container, so aggregates are itemized on the way in — this is the
/// single hook that turns §1.3's rows 01-18 and 23 green, because every
/// downstream element producer (`[i]`, slices, `.head`/`.tail`/`.first`,
/// `map`/`grep`/`sort`/`reverse`, `.pairs`/`.kv`, the implicit topic) simply
/// copies the flag along (§1.6.3).
pub(crate) fn coerce_to_array(value: Value) -> Value {
    // An unbounded range of any element type becomes its lazy `.succ`
    // sequence in array context, not a capped prefix.
    if let Some(lazy) = crate::runtime::utils::infinite_range_to_lazy_array(&value) {
        return lazy;
    }
    crate::value::array_coerce::coerce_finite_to_array(value)
}

/// [`coerce_to_array`] for the list-destructuring staging temp: each element
/// keeps the itemization its source gave it
/// ([`crate::value::array_coerce::coerce_finite_to_array_unitemized`]).
pub(crate) fn coerce_to_staging_array(value: Value) -> Value {
    if let Some(lazy) = crate::runtime::utils::infinite_range_to_lazy_array(&value) {
        return lazy;
    }
    crate::value::array_coerce::coerce_finite_to_array_unitemized(value)
}

pub(crate) fn coerce_to_str(value: &Value) -> String {
    value.to_str_context()
}
