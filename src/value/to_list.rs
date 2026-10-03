//! `value_to_list`: the list-context expansion of a value (`for`, list
//! assignment, `.list`). A pure function of the value, so it lives in `value`
//! (#10779); `runtime::utils` re-exports it.

use crate::value::to_list_range::generic_range_to_list;
use crate::value::{Value, ValueView};

/// Maximum number of elements when expanding an infinite range to a list.
pub(crate) const MAX_RANGE_EXPAND: i64 = 1_000_000;

/// Expand `val` to its list-context elements: an itemized value stays one
/// item, ranges/Seqs/hashes/QuantHashes/Bufs spill their elements.
pub(crate) fn value_to_list(val: &Value) -> Vec<Value> {
    match val.view() {
        ValueView::Array(_, kind) if kind.is_itemized() => vec![val.clone()],
        // A role mixin over a (non-itemized) list-ish value lists as the inner
        // value does. An itemized inner keeps one item — `for $x` where
        // `my $x = (1,2); $x does R` iterates once, like the plain itemized
        // scalar — and a scalar mixin stays one item with its mixin identity.
        // Iteration METHODS (`.map`/`.grep`) unwrap itemization separately via
        // `Interpreter::mixin_iteration_target`.
        ValueView::Mixin(inner, _)
            if matches!(
                inner.view(),
                ValueView::Array(_, kind) if !kind.is_itemized()
            ) || matches!(
                inner.view(),
                ValueView::Seq(_)
                    | ValueView::HyperSeq(_)
                    | ValueView::RaceSeq(_)
                    | ValueView::Slip(_)
                    | ValueView::LazyList(_)
                    | ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. }
                    | ValueView::Set(..)
                    | ValueView::Bag(..)
                    | ValueView::Mix(..)
            ) || (matches!(inner.view(), ValueView::Hash(_)) && !inner.hash_is_itemized()) =>
        {
            value_to_list(inner)
        }
        // Iterating an array reads each slot the way `@a[$i]` does, so a hole
        // yields the container's `is default(...)` value (`items_with_default`).
        ValueView::Array(items, ..) => items.items_with_default().into_owned(),
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            items.to_vec()
        }
        ValueView::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
        // `Capture.list` is its positional part (`Any.list` on a Capture), so
        // `\(1, 2).join(",")`, `.grep`, `.reverse`, `for \(1, 2)` see 1 and 2.
        ValueView::Capture { positional, .. } => positional.to_vec(),
        // An itemized hash (`item %h` / `$(%h)`) is a single list element and does
        // NOT flatten to its pairs (mirrors the itemized-Array arm above).
        ValueView::Hash(_) if val.hash_is_itemized() => vec![val.clone()],
        // `typed_pair` decontainerizes element cells so the pair value matches a
        // `%h<k>` read / `.values` (see t/bind-hash-value-pairs.t).
        ValueView::Hash(items) => items
            .iter()
            .map(|(k, v)| items.typed_pair(k, v.clone()))
            .collect(),
        ValueView::Range(a, b) => {
            let end = b.min(a + MAX_RANGE_EXPAND);
            (a..=end).map(Value::int).collect()
        }
        ValueView::RangeExcl(a, b) => {
            let end = b.min(a + MAX_RANGE_EXPAND);
            (a..end).map(Value::int).collect()
        }
        ValueView::RangeExclStart(a, b) => {
            let start = a + 1;
            let end = b.min(start + MAX_RANGE_EXPAND);
            (start..=end).map(Value::int).collect()
        }
        ValueView::RangeExclBoth(a, b) => {
            let start = a + 1;
            let end = b.min(start + MAX_RANGE_EXPAND);
            (start..end).map(Value::int).collect()
        }
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => generic_range_to_list(val, start, end, excl_start, excl_end),
        ValueView::Set(items, _) => items
            .iter()
            .map(|s| {
                crate::value::quanthash_keys::quanthash_typed_pair(items.typed_key(s), Value::TRUE)
            })
            .collect(),
        ValueView::Bag(items, _) => items
            .iter()
            .map(|(k, v)| {
                crate::value::quanthash_keys::quanthash_typed_pair(
                    items.typed_key(k),
                    Value::from_bigint(v.clone()),
                )
            })
            .collect(),
        ValueView::Mix(items, _) => items
            .iter()
            .map(|(k, v)| {
                crate::value::quanthash_keys::quanthash_typed_pair(
                    items.typed_key(k),
                    crate::value::mix_weight_to_value(*v),
                )
            })
            .collect(),
        ValueView::Slip(items) => items.to_vec(),
        // A Uni (and its NFC/NFD/NFKC/NFKD forms) does `Positional[uint32]`:
        // in list context it flattens to its codepoints, not to itself as one
        // scalar (`for $str.NFC { }` / `$str.NFC.map(&wcwidth)` iterate Ints).
        ValueView::Uni(u) => u
            .codepoints()
            .into_iter()
            .map(|c| Value::int(c as i64))
            .collect(),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => {
            // A package Stash is Map-like in list context: iterate its symbol
            // table as key/value pairs. PseudoStashes use the same visible
            // `symbols` representation; their frame metadata remains hidden.
            if crate::value::types::is_stash_class_name(&class_name.resolve()) {
                if let Some(ValueView::Hash(symbols)) =
                    attributes.as_map().get("symbols").map(Value::view)
                {
                    return symbols
                        .iter()
                        .map(|(key, value)| symbols.typed_pair(key, value.clone()))
                        .collect();
                }
                return Vec::new();
            }
            // A WalkList flattens to its candidate closures in list context, so
            // `my @cands = $x.WALK(...)` yields the per-level candidates.
            if class_name.resolve() == "WalkList"
                && let Some(items) = walk_list_candidates(&attributes)
            {
                return items;
            }
            // Backtrace is Positional: list context yields its frames
            // (`$!.backtrace.any`, `$bt>>.file`, `for $e.backtrace {...}`).
            if class_name.resolve() == "Backtrace"
                && let Some(frames) = attributes.as_map().get("frames")
            {
                return value_to_list(frames);
            }
            // Rakudo itemizes IO::Path::Parts in list context despite the type
            // doing Iterable; its explicit `.flat` method exposes the parts.
            if class_name.resolve() == "IO::Path::Parts" {
                return vec![val.clone()];
            }
            // Buf/Blob are Positional: list context yields the elements
            // (`$blob.rotor(3)`, `for $buf { }` iterate bytes, as in rakudo).
            if let Some(elems) = crate::value::value_buf::buf_elems(&attributes) {
                return elems;
            }
            // An `is Array` subclass instance is Positional: in list context it
            // flattens to its backing storage elements (`for @$vec { ... }`).
            if let Some(storage) = attributes.as_map().get("__mutsu_array_storage") {
                return value_to_list(storage);
            }
            // An `is Hash`/`is Map` subclass instance is Associative: in list
            // context it flattens to its backing storage's pairs (`$h.list`,
            // `for $h { ... }`), mirroring the Array/List arm above.
            if let Some(storage) = attributes.as_map().get("__mutsu_hash_storage") {
                return value_to_list(storage);
            }
            vec![val.clone()]
        }
        // Nil is a single scalar item in list context (e.g. `for Nil { }` does
        // one iteration); it is not an empty list. Fall through to the scalar arm.
        _ => vec![val.clone()],
    }
}

/// Extract the candidate closures from a `WalkList` instance's attributes,
/// honoring its `reversed` flag. Returns `None` if the attributes are not
/// shaped like a WalkList.
pub(crate) fn walk_list_candidates(attributes: &crate::value::InstanceAttrs) -> Option<Vec<Value>> {
    let map = attributes.as_map();
    let Some(ValueView::Array(items, ..)) = map.get("candidates").map(Value::view) else {
        return None;
    };
    let mut cands = items.to_vec();
    if matches!(
        map.get("reversed").map(Value::view),
        Some(ValueView::Bool(true))
    ) {
        cands.reverse();
    }
    Some(cands)
}

/// The `(key, value)` entries of a `Stash`/`PseudoStash`, each value left as
/// the symbol's own container (a root `our $x` is published as its shared
/// cell), so `.kv` / `.values` hand out writable containers as rakudo's do.
/// Empty for anything that is not a stash.
// Cost: O(n), n = symbols in the stash.
pub(crate) fn stash_symbol_entries(value: &Value) -> Vec<(Value, Value)> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = value.view()
    else {
        return Vec::new();
    };
    if !crate::value::types::is_stash_class_name(&class_name.resolve()) {
        return Vec::new();
    }
    match attributes.as_map().get("symbols").map(Value::view) {
        Some(ValueView::Hash(symbols)) => symbols
            .iter()
            .map(|(key, value)| (symbols.typed_key(key), value.clone()))
            .collect(),
        _ => Vec::new(),
    }
}
