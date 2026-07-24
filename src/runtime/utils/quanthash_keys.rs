//! WHICH-keyed element storage for Set/Bag/Mix (QuantHash).
//!
//! Element identity is `.WHICH`-based (raku Setty/Baggy/Mixy semantics): `1`,
//! `"1"`, `1.0` and `<1>` are four distinct elements, and two instances of the
//! same class are distinct unless they are the same object. The backing stores
//! (`SetData.elements` / `BagData.counts` / `MixData.weights`) keep
//! `value_which_key(elem)` as the string key; `original_keys` records the
//! element object for every key whose element is not a plain `Str` — a plain
//! `Str` roundtrips losslessly through its `"Str|<s>"` key, so recording it
//! would only duplicate memory (`typed_key` decodes it back). This mirrors the
//! object-hash pattern (PR #5333) with that Str-sparseness refinement.
//!
//! Every construction site that turns raw element values into store keys must
//! go through these helpers; combining ALREADY-keyed stores (set operators)
//! must thread `original_keys` alongside the string keys (copy entries via
//! `typed_key` + `record_quanthash_original`).

use crate::value::{Value, ValueView};
use std::collections::{HashMap, HashSet};

/// Storage key + decontainerized element for a QuantHash element.
pub(crate) fn quanthash_elem_entry(v: &Value) -> (String, Value) {
    let elem = v.deref_container();
    (super::value_which_key(&elem), elem)
}

/// Storage key for an element already known to be a plain string (a `Pair`
/// string key, a plain-hash key). Equals `value_which_key(Value::str(s))`.
pub(crate) fn str_elem_key(s: &str) -> String {
    format!("Str|{}", s)
}

/// Record `key -> elem` in an original-keys map unless the element is a plain
/// `Str` (whose key decodes losslessly, see module doc).
pub(crate) fn record_quanthash_original(
    originals: &mut HashMap<String, Value>,
    key: &str,
    elem: &Value,
) {
    if !matches!(elem.view(), ValueView::Str(_)) && !originals.contains_key(key) {
        originals.insert(key.to_string(), elem.clone());
    }
}

/// Build the `elem => weight` Pair for a QuantHash entry from the decoded
/// element object (Str elements keep the string-keyed Pair form).
pub(crate) fn quanthash_typed_pair(elem: Value, v: Value) -> Value {
    match elem.view() {
        ValueView::Str(s) => Value::pair(s.to_string(), v),
        _ => Value::value_pair(elem, v),
    }
}

/// Merge a source QuantHash's recorded original keys into an accumulator
/// (set-operator results keep the element objects of both operands; extra
/// entries for keys dropped from the result are harmless — `typed_key` only
/// looks up keys that exist).
pub(crate) fn extend_quanthash_originals(
    dst: &mut HashMap<String, Value>,
    src: &Option<HashMap<String, Value>>,
) {
    if let Some(m) = src {
        for (k, v) in m {
            dst.entry(k.clone()).or_insert_with(|| v.clone());
        }
    }
}

/// Insert one element into a Set store (key + original in one step).
pub(crate) fn quanthash_insert_set(
    elems: &mut HashSet<String>,
    originals: &mut HashMap<String, Value>,
    item: &Value,
) {
    let (key, elem) = quanthash_elem_entry(item);
    record_quanthash_original(originals, &key, &elem);
    elems.insert(key);
}
