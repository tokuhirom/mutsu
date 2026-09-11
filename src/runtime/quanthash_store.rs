//! `STORE` on a mutable QuantHash (`SetHash`/`BagHash`/`MixHash`) —
//! re-initialize the container from the assigned list.
//!
//! Raku reaches this through `%h = ...` on a tied variable and through an
//! explicit `$b.STORE(...)`; mutsu additionally routes the QuantHash-subclass
//! declaration (`my %b is AccountableBagHash = ...`) here via
//! `vm_baggy_subclass_delegate.rs`, so the folding lives in one place.
//!
//! The store keys are the same `.WHICH`-derived keys every other QuantHash
//! write uses (`runtime::utils::quanthash_elem_entry`), and non-`Str` elements
//! are recorded in `original_keys` so they decode back to the element object.
//! Building them with the raw stringification instead — which this code did
//! until 2026-09 — made `$b.STORE(a => 42); $b<a>` read 0: the reader looks up
//! `Str|a` and the store had written `a`.

use crate::runtime::utils::{quanthash_elem_entry, record_quanthash_original};
use crate::value::{Value, ValueView};
use std::collections::HashMap;

/// One folded entry: the storage key, the element object behind it, and the
/// weight the assigned list gave it.
struct Entry {
    key: String,
    elem: Value,
    weight: f64,
}

/// Flatten `args` into the `key => weight` entries a QuantHash `STORE` assigns.
///
/// Two shapes are accepted, matching Rakudo's `STORE(+@values)` /
/// `STORE(@keys, @values)` candidates: a single list (Pairs give explicit
/// weights, bare elements count 1) and two parallel lists zipped as keys to
/// weights.
fn fold_entries(args: &[Value]) -> Vec<Entry> {
    let mut items: Vec<Value> = Vec::new();
    for arg in args {
        match arg.view() {
            ValueView::Array(elems, _) => items.extend(elems.iter().cloned()),
            ValueView::Seq(elems) => items.extend(elems.iter().cloned()),
            ValueView::Slip(elems) => items.extend(elems.iter().cloned()),
            _ => items.push(arg.clone()),
        }
    }
    let has_pairs = items
        .iter()
        .any(|v| matches!(v.view(), ValueView::Pair(..) | ValueView::ValuePair(..)));
    let weight_of = |v: &Value| match v.view() {
        ValueView::Int(i) => i as f64,
        ValueView::Num(f) => f,
        ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
        _ => 1.0,
    };
    if has_pairs {
        return items
            .iter()
            .map(|v| match v.view() {
                ValueView::Pair(k, w) => {
                    let elem = Value::str(k.clone());
                    let (key, elem) = quanthash_elem_entry(&elem);
                    Entry {
                        key,
                        elem,
                        weight: weight_of(w),
                    }
                }
                ValueView::ValuePair(k, w) => {
                    let (key, elem) = quanthash_elem_entry(k);
                    Entry {
                        key,
                        elem,
                        weight: weight_of(w),
                    }
                }
                _ => {
                    let (key, elem) = quanthash_elem_entry(v);
                    Entry {
                        key,
                        elem,
                        weight: 1.0,
                    }
                }
            })
            .collect();
    }
    if args.len() == 2
        && matches!(args[0].view(), ValueView::Array(..))
        && matches!(args[1].view(), ValueView::Array(..))
    {
        let keys = match args[0].view() {
            ValueView::Array(k, _) => k.to_vec(),
            _ => Vec::new(),
        };
        let weights = match args[1].view() {
            ValueView::Array(v, _) => v.to_vec(),
            _ => Vec::new(),
        };
        return keys
            .iter()
            .zip(weights.iter())
            .map(|(k, w)| {
                let (key, elem) = quanthash_elem_entry(k);
                Entry {
                    key,
                    elem,
                    weight: weight_of(w),
                }
            })
            .collect();
    }
    items
        .iter()
        .map(|v| {
            let (key, elem) = quanthash_elem_entry(v);
            Entry {
                key,
                elem,
                weight: 1.0,
            }
        })
        .collect()
}

/// Build the replacement container for `STORE` on `target`, or `None` when
/// `target` is not a mutable QuantHash.
pub(crate) fn quanthash_store(target: &Value, args: &[Value]) -> Option<Value> {
    let entries = match target.view() {
        ValueView::Set(_, true) | ValueView::Bag(_, true) | ValueView::Mix(_, true) => {
            fold_entries(args)
        }
        _ => return None,
    };
    let mut originals: HashMap<String, Value> = HashMap::new();
    for e in &entries {
        record_quanthash_original(&mut originals, &e.key, &e.elem);
    }
    let stored = match target.view() {
        ValueView::Set(..) => {
            let elems = entries
                .iter()
                .filter(|e| e.weight != 0.0)
                .map(|e| e.key.clone())
                .collect();
            Value::set_hash_typed(elems, originals)
        }
        ValueView::Bag(..) => {
            let mut counts: HashMap<String, i64> = HashMap::new();
            for e in &entries {
                *counts.entry(e.key.clone()).or_insert(0) += e.weight as i64;
            }
            counts.retain(|_, c| *c > 0);
            Value::bag_hash_typed(counts, originals)
        }
        _ => {
            let mut weights: HashMap<String, f64> = HashMap::new();
            for e in &entries {
                *weights.entry(e.key.clone()).or_insert(0.0) += e.weight;
            }
            Value::mix_hash_with_original_keys(weights, originals)
        }
    };
    Some(stored)
}
