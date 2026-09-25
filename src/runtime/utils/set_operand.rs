//! How a set operator reads one operand: the three QuantHash coercions
//! (`.Set` / `.Bag` / `.Mix` of an operand) shared by every set operator and
//! every form of it (#9451).
//!
//! Each operator promotes both operands to the higher of their levels (Set <
//! Bag < Mix) and folds them there, so an operand is only ever read as a key
//! set ([`coerce_to_set`]), a count map ([`operand_bag_counts`]) or a weight
//! map ([`operand_mix_weights`]). There used to be a private copy of those
//! reads per operator and per form, and they disagreed: `(a => 2, "b", "b")`
//! weighed `a` as 2 under `(+)` but was taken as one opaque Pair element under
//! `(-)`, and the `[∪]` form counted `b` once where `∪` counted it twice.
use super::*;
use crate::value::ValueMap;

/// A set operator's promotion level.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum SetLevel {
    Set,
    Bag,
    Mix,
}

/// The value a set operator folds for `val`: its `Scalar` container and any
/// role mixin stripped (see [`quanthash_operand`]), and a Baggy subclass
/// instance (`class Foo is Bag`) replaced by the QuantHash it carries in
/// `__baggy_data__`.
pub(crate) fn set_operand(val: &Value) -> Value {
    let val = quanthash_operand(val);
    if let ValueView::Instance { attributes, .. } = val.view()
        && let Some(inner) = attributes.as_map().get("__baggy_data__")
    {
        return quanthash_operand(inner).clone();
    }
    val.clone()
}

/// The level an operand promotes a set operator to. A QuantHash type object
/// is an ordinary element here (`(Bag) ∪ (Bag)` is `Set(Bag)`); only `(+)`
/// promotes on one -- see `SetOp::operand_level`.
pub(crate) fn set_level(val: &Value) -> SetLevel {
    match set_operand(val).view() {
        ValueView::Mix(_, _) => SetLevel::Mix,
        ValueView::Bag(_, _) => SetLevel::Bag,
        _ => SetLevel::Set,
    }
}

/// The level a QuantHash type object names (`Mix`/`MixHash` -> Mix,
/// `Bag`/`BagHash` -> Bag), if `val` is one.
pub(crate) fn type_object_set_level(val: &Value) -> Option<SetLevel> {
    match set_operand(val).view() {
        ValueView::Package(sym) => match sym.resolve().as_str() {
            "Mix" | "MixHash" => Some(SetLevel::Mix),
            "Bag" | "BagHash" => Some(SetLevel::Bag),
            _ => None,
        },
        _ => None,
    }
}

/// Whether coercing `val` would have to reify an infinite list. `-Inf..0` is
/// lazy too: Rakudo would iterate it forever.
pub(crate) fn is_lazy_set_operand(val: &Value) -> bool {
    fn infinite(v: &Value) -> bool {
        match v.view() {
            ValueView::HyperWhatever | ValueView::Whatever => true,
            ValueView::Num(n) => n.is_infinite(),
            ValueView::Rat(_, d) | ValueView::FatRat(_, d) => d == 0,
            ValueView::Mixin(inner, _) => infinite(inner),
            _ => false,
        }
    }
    match set_operand(val).view() {
        ValueView::LazyList(_) => true,
        ValueView::Range(_, end)
        | ValueView::RangeExcl(_, end)
        | ValueView::RangeExclStart(_, end)
        | ValueView::RangeExclBoth(_, end) => end == i64::MAX,
        ValueView::GenericRange { start, end, .. } => infinite(start) || infinite(end),
        _ => false,
    }
}

/// Whether a bare scalar operand or list element contributes a key at all:
/// the `Any` type object is the uninitialized-scalar seed (`my $s; $s ∪= 0`
/// unions from the empty set), and an empty stringification is the older
/// `Nil` seed.
fn contributes(elem: &Value) -> bool {
    !elem.is_any_type_object() && !elem.to_string_value().is_empty()
}

/// Add one list element's weight to a count map: a Pair weighs its value,
/// anything else counts once.
fn bag_add_item(result: &mut HashMap<String, BigInt>, originals: &mut ValueMap, item: &Value) {
    match item.view() {
        ValueView::Pair(k, v) => {
            *result.entry(str_elem_key(k)).or_default() += bag_weight(v);
        }
        ValueView::ValuePair(k, v) => {
            let (key, elem) = quanthash_elem_entry(k);
            record_quanthash_original(originals, &key, &elem);
            *result.entry(key).or_default() += bag_weight(v);
        }
        _ => {
            let (key, elem) = quanthash_elem_entry(item);
            if contributes(&elem) {
                record_quanthash_original(originals, &key, &elem);
                *result.entry(key).or_default() += 1;
            }
        }
    }
}

/// An operand read as a Bag: element -> count, positive counts only.
/// Counts are arbitrary-precision (`BagData.counts` is a `BigInt` map).
pub(crate) fn operand_bag_counts(val: &Value, originals: &mut ValueMap) -> HashMap<String, BigInt> {
    let val = &set_operand(val);
    let mut result: HashMap<String, BigInt> = match val.view() {
        ValueView::Bag(b, _) => {
            extend_quanthash_originals(originals, &b.original_keys);
            resolve_bag_tab_keys(&b)
        }
        ValueView::Set(s, _) => {
            extend_quanthash_originals(originals, &s.original_keys);
            s.iter().map(|k| (k.clone(), BigInt::from(1))).collect()
        }
        ValueView::Mix(m, _) => {
            extend_quanthash_originals(originals, &m.original_keys);
            m.iter()
                .map(|(k, v)| (k.clone(), BigInt::from(*v as i64)))
                .collect()
        }
        ValueView::Hash(map) => {
            let mut result = HashMap::new();
            for (k, v) in map.iter() {
                let key = hash_elem_key(&map, k, originals);
                *result.entry(key).or_default() += bag_weight(v);
            }
            result
        }
        _ if val.as_list_items().is_some() || val.is_range() => {
            let mut result = HashMap::new();
            for item in quanthash_operand_list(val) {
                bag_add_item(&mut result, originals, &item);
            }
            result
        }
        _ => {
            let mut result = HashMap::new();
            bag_add_item(&mut result, originals, val);
            result
        }
    };
    result.retain(|_, v| v.is_positive());
    result
}

/// The Mix weight of a Pair value or a Hash value: its numeric value, and
/// the truthiness of anything that is not a number.
fn mix_weight_of(v: &Value) -> f64 {
    let v = v.deref_container().deitemize_element();
    match v.view() {
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(_, _)
        | ValueView::FatRat(_, _)
        | ValueView::BigRat(_, _) => v.to_f64(),
        _ => {
            if v.truthy() {
                1.0
            } else {
                0.0
            }
        }
    }
}

/// Add one list element's weight to a weight map: a Pair weighs its value,
/// anything else counts once.
fn mix_add_item(result: &mut HashMap<String, f64>, originals: &mut ValueMap, item: &Value) {
    let (key, weight) = match item.view() {
        ValueView::Pair(k, v) => (str_elem_key(k), mix_weight_of(v)),
        ValueView::ValuePair(k, v) => {
            let (key, elem) = quanthash_elem_entry(k);
            record_quanthash_original(originals, &key, &elem);
            (key, mix_weight_of(v))
        }
        _ => {
            let (key, elem) = quanthash_elem_entry(item);
            if !contributes(&elem) {
                return;
            }
            record_quanthash_original(originals, &key, &elem);
            (key, 1.0)
        }
    };
    let e = result.entry(key).or_insert(0.0);
    *e = crate::builtins::mix_weight::add(*e, weight);
}

/// An operand read as a Mix: element -> weight, non-zero weights only.
pub(crate) fn operand_mix_weights(val: &Value, originals: &mut ValueMap) -> HashMap<String, f64> {
    let val = &set_operand(val);
    let mut result: HashMap<String, f64> = match val.view() {
        ValueView::Mix(m, _) => {
            extend_quanthash_originals(originals, &m.original_keys);
            m.weights.clone()
        }
        ValueView::Bag(b, _) => {
            extend_quanthash_originals(originals, &b.original_keys);
            resolve_bag_tab_keys(&b)
                .iter()
                .map(|(k, v)| (k.clone(), bigint_to_f64_sat(v)))
                .collect()
        }
        ValueView::Set(s, _) => {
            extend_quanthash_originals(originals, &s.original_keys);
            s.iter().map(|k| (k.clone(), 1.0)).collect()
        }
        ValueView::Hash(map) => {
            let mut result = HashMap::new();
            for (k, v) in map.iter() {
                let key = hash_elem_key(&map, k, originals);
                let e = result.entry(key).or_insert(0.0);
                *e = crate::builtins::mix_weight::add(*e, mix_weight_of(v));
            }
            result
        }
        _ if val.as_list_items().is_some() || val.is_range() => {
            let mut result = HashMap::new();
            for item in quanthash_operand_list(val) {
                mix_add_item(&mut result, originals, &item);
            }
            result
        }
        _ => {
            let mut result = HashMap::new();
            mix_add_item(&mut result, originals, val);
            result
        }
    };
    result.retain(|_, v| *v != 0.0);
    result
}
