use super::*;
use crate::value::ValueMap;

/// The Set-element store key for a hash entry, recording the key object in
/// `originals` when it is not a plain Str: an object hash stores `.WHICH`
/// keys already (decode the object and re-key — identical for well-formed
/// stores); a plain hash key is a Str element (`"Str|<k>"`).
pub(crate) fn hash_elem_key(
    h: &crate::value::HashData,
    k: &str,
    originals: &mut ValueMap,
) -> String {
    if h.has_typed_keys() {
        let (key, obj) = quanthash_elem_entry(&h.typed_key(k));
        record_quanthash_original(originals, &key, &obj);
        key
    } else {
        str_elem_key(k)
    }
}

/// List view for quanthash coercion of a whole OPERAND: an itemized
/// list/array (`$`-param bound, `.item`ed) contributes its ELEMENTS — raku
/// decontainerizes coercion invocants (`$(<a b>).Bag` has two keys) — unlike
/// `value_to_list`'s one-element treatment used by list assignment.
pub(crate) fn quanthash_operand_list(v: &Value) -> Vec<Value> {
    match v.view() {
        ValueView::Array(items, kind) if kind.is_itemized() => value_to_list(
            &Value::array_with_kind(items.clone(), kind.decontainerize()),
        ),
        _ => value_to_list(v),
    }
}

/// The value a QuantHash coercion should actually fold: the operand with its
/// `Scalar` container and any role mixin stripped.
///
/// A mixin WRAPS a value without replacing it — rakudo's `%h does R` is a
/// `Hash+{R}`, still a Hash, and `@a but R` is still an Array — so `.Set` /
/// `.Bag` / `.Mix` and every set operator must fold the inner value's elements.
/// Without the strip a mixin fell through to the "unknown scalar" arm and
/// contributed the WHOLE hash as one element, which is why `self (-) %allowed`
/// inside a role's `STORE` never cancelled anything (Hash::Restricted).
pub(crate) fn quanthash_operand(val: &Value) -> &Value {
    strip_quanthash_mixin(val.descalarize())
}

/// The mixin-only half of [`quanthash_operand`], for positions where stripping
/// the `Scalar` container would change the flattening rule (an itemized
/// `$(...)` element is taken whole) but the mixin wrapper must still be seen
/// through.
///
/// Only a ROLE mixin is stripped. An **allomorph** is a `Mixin` too (`<1>` is
/// `Mixin(Int(1), {Str => "1"})`, see
/// `parser::primary::container::allomorph::make_allomorphic_value`) and its
/// whole point is to be a distinct element from the value it wraps:
/// `(1, "1", 1.0, <1>).Set` has four elements. A role mixin carries a
/// `__mutsu_role__<name>` marker, which is what tells the two apart — the same
/// discriminator `dispatch_mixin_method_call`'s `.clone` arm uses.
pub(crate) fn strip_quanthash_mixin(val: &Value) -> &Value {
    let mut val = val;
    while let ValueView::Mixin(inner, mixins) = val.view() {
        if !mixins.keys().any(|k| k.starts_with("__mutsu_role__")) {
            break;
        }
        val = inner.as_ref().descalarize();
    }
    val
}

/// The nested-element form of [`strip_quanthash_mixin`].
///
/// A role-mixed AGGREGATE flattens its contents in list context, exactly as the
/// bare aggregate would (`(1, %h).Set` is `Set(1, "a")` in rakudo for a
/// `%h does R` holding `a => 1`). A role-mixed SCALAR keeps its own identity
/// instead: `(5, 5 but R).Set` has two elements, keyed `Int` and `Int+{R}`. So
/// the strip applies only when what it uncovers is something the surrounding
/// flattening arms would spill anyway.
pub(crate) fn strip_quanthash_mixin_elem(val: &Value) -> &Value {
    let stripped = strip_quanthash_mixin(val);
    if std::ptr::eq(stripped, val) {
        return val;
    }
    let spills = matches!(
        stripped.view(),
        ValueView::Hash(_)
            | ValueView::Array(_, _)
            | ValueView::Seq(_)
            | ValueView::Slip(_)
            | ValueView::Set(_, _)
            | ValueView::Bag(_, _)
            | ValueView::Mix(_, _)
            | ValueView::Pair(_, _)
            | ValueView::ValuePair(_, _)
    ) || stripped.is_range();
    if spills { stripped } else { val }
}

pub(crate) fn coerce_to_set(val: &Value, originals: &mut ValueMap) -> HashSet<String> {
    fn insert_set_elem(elems: &mut HashSet<String>, originals: &mut ValueMap, value: &Value) {
        let pair_selected = |weight: &Value| weight.truthy() || weight.is_nil();
        let value = strip_quanthash_mixin_elem(value);
        match value.view() {
            ValueView::Set(items, _) => {
                extend_quanthash_originals(originals, &items.original_keys);
                elems.extend(items.iter().cloned());
            }
            ValueView::Bag(items, _) => {
                extend_quanthash_originals(originals, &items.original_keys);
                for (k, v) in items.iter() {
                    if v.is_positive() {
                        elems.insert(k.clone());
                    }
                }
            }
            ValueView::Mix(items, _) => {
                extend_quanthash_originals(originals, &items.original_keys);
                for (k, v) in items.iter() {
                    if *v != 0.0 {
                        elems.insert(k.clone());
                    }
                }
            }
            ValueView::Hash(items) => {
                for (k, v) in items.iter() {
                    if v.truthy() || v.is_nil() {
                        let key = hash_elem_key(&items, k, originals);
                        elems.insert(key);
                    }
                }
            }
            // A bare (non-itemized) List value flattens fully -- matches
            // raku's slurpy-argument flattening (`f(1, (2, (3,4)))` is 3
            // elements). A real Array or an ITEMIZED List/Array (`$(1,)`,
            // `$[1]`) does NOT: it is a single opaque element, keyed by its
            // own `.WHICH`, and falls through to the catch-all below.
            // Without this guard, an Array element that is itself a
            // (possibly itemized) List got decontainerized down to its own
            // content here even though `∈`/`grep`'s membership check (which
            // never recurses past a container's own top-level items) does
            // not -- `(5,) ∈ @b` and `(5,) ∈ (∩'s decontainerized member
            // set)` disagreed (#8570).
            ValueView::Array(items, ArrayKind::List) => {
                for item in items.iter() {
                    insert_set_elem(elems, originals, item);
                }
            }
            ValueView::Seq(items) => {
                for item in items.iter() {
                    insert_set_elem(elems, originals, item);
                }
            }
            ValueView::Slip(items) => {
                for item in items.iter() {
                    insert_set_elem(elems, originals, item);
                }
            }
            _ if value.is_range() => {
                for item in value_to_list(value) {
                    insert_set_elem(elems, originals, &item);
                }
            }
            ValueView::Pair(key, weight) => {
                if pair_selected(weight) {
                    elems.insert(str_elem_key(key));
                }
            }
            ValueView::ValuePair(key, weight) => {
                if pair_selected(weight) {
                    quanthash_insert_set(elems, originals, key);
                }
            }
            _ => {
                let (key, elem) = quanthash_elem_entry(value);
                record_quanthash_original(originals, &key, &elem);
                elems.insert(key);
            }
        }
    }

    let val = &set_operand(val);
    match val.view() {
        ValueView::Set(s, _) => {
            extend_quanthash_originals(originals, &s.original_keys);
            s.elements.clone()
        }
        ValueView::Bag(b, _) => {
            extend_quanthash_originals(originals, &b.original_keys);
            let resolved = resolve_bag_tab_keys(&b);
            resolved.keys().cloned().collect()
        }
        ValueView::Mix(m, _) => {
            extend_quanthash_originals(originals, &m.original_keys);
            m.keys().cloned().collect()
        }
        ValueView::Hash(items) => {
            let mut elems = HashSet::new();
            for (k, v) in items.iter() {
                if v.truthy() || v.is_nil() {
                    let key = hash_elem_key(&items, k, originals);
                    elems.insert(key);
                }
            }
            elems
        }
        _ if val.as_list_items().is_some() => {
            let mut elems = HashSet::new();
            for item in val.as_list_items().unwrap().iter() {
                insert_set_elem(&mut elems, originals, item);
            }
            elems
        }
        ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => {
            let mut elems = HashSet::new();
            insert_set_elem(&mut elems, originals, val);
            elems
        }
        _ if val.is_range() => {
            let mut elems = HashSet::new();
            for item in value_to_list(val) {
                insert_set_elem(&mut elems, originals, &item);
            }
            elems
        }
        _ => {
            let mut s = HashSet::new();
            let (key, elem) = quanthash_elem_entry(val);
            record_quanthash_original(originals, &key, &elem);
            s.insert(key);
            s
        }
    }
}

/// Coerce a value to a QuantHash (Set/Bag/Mix) for use as a single operand to set operators.
/// - Set/Bag/Mix pass through as-is
/// - Hash: include keys with truthy values as Set elements
/// - List/Array: convert items to Set (excluding Pairs with falsy value)
/// - Pair with falsy value → empty Set
/// - Other scalars → Set with one element
pub(crate) fn coerce_value_to_quanthash(val: &Value) -> Value {
    let val = quanthash_operand(val);
    match val.view() {
        ValueView::Set(_, _) | ValueView::Bag(_, _) | ValueView::Mix(_, _) => val.clone(),
        ValueView::Hash(h) => {
            let mut set = HashSet::new();
            let mut originals = ValueMap::default();
            for (k, v) in h.iter() {
                if v.truthy() {
                    let key = hash_elem_key(&h, k, &mut originals);
                    set.insert(key);
                }
            }
            Value::set_typed(set, originals)
        }
        _ if val.as_list_items().is_some() => {
            let mut set = HashSet::new();
            let mut originals = ValueMap::default();
            for item in val.as_list_items().unwrap().iter() {
                match item.view() {
                    ValueView::Pair(k, v) => {
                        if v.truthy() {
                            set.insert(str_elem_key(k));
                        }
                    }
                    // ADR-0021 P3a: a positional-flavour Pair (e.g. a
                    // hash-derived `.pairs` element, which mints
                    // ValuePair) reaches here just as often as the named
                    // flavour — mirror the arm above rather than falling
                    // through to the scalar catch-all, which would insert
                    // the whole Pair as one element instead of its key.
                    ValueView::ValuePair(k, v) => {
                        if v.truthy() {
                            match k.view() {
                                ValueView::Str(s) => {
                                    set.insert(str_elem_key(&s));
                                }
                                _ => {
                                    let (key, elem) = quanthash_elem_entry(k);
                                    record_quanthash_original(&mut originals, &key, &elem);
                                    set.insert(key);
                                }
                            }
                        }
                    }
                    ValueView::Hash(h) => {
                        for (k, v) in h.iter() {
                            if v.truthy() {
                                let key = hash_elem_key(&h, k, &mut originals);
                                set.insert(key);
                            }
                        }
                    }
                    _ => {
                        quanthash_insert_set(&mut set, &mut originals, item);
                    }
                }
            }
            Value::set_typed(set, originals)
        }
        ValueView::Pair(k, v) => {
            let mut set = HashSet::new();
            if v.truthy() {
                set.insert(str_elem_key(k));
            }
            Value::set(set)
        }
        // ADR-0021 P3a: same widening as the list-branch arm above, for a
        // single positional-flavour Pair coerced directly to a QuantHash.
        ValueView::ValuePair(k, v) => {
            let mut set = HashSet::new();
            let mut originals = ValueMap::default();
            if v.truthy() {
                match k.view() {
                    ValueView::Str(s) => {
                        set.insert(str_elem_key(&s));
                    }
                    _ => {
                        let (key, elem) = quanthash_elem_entry(k);
                        record_quanthash_original(&mut originals, &key, &elem);
                        set.insert(key);
                    }
                }
            }
            Value::set_typed(set, originals)
        }
        // A Range enumerates its elements (`@n (<=) (1..49)`), mirroring
        // `coerce_to_set` above — without this arm it fell to the scalar
        // catch-all and became a one-element Set of the string "1..49".
        _ if val.is_range() => {
            let mut set = HashSet::new();
            let mut originals = ValueMap::default();
            for item in value_to_list(val) {
                quanthash_insert_set(&mut set, &mut originals, &item);
            }
            Value::set_typed(set, originals)
        }
        _ => {
            let mut set = HashSet::new();
            let mut originals = ValueMap::default();
            let (key, elem) = quanthash_elem_entry(val);
            record_quanthash_original(&mut originals, &key, &elem);
            set.insert(key);
            Value::set_typed(set, originals)
        }
    }
}

/// Resolve Bag entries that use the internal "key\tweight" tab format
/// into plain key→weight entries.
///
/// Weights stay arbitrary-precision throughout: `BagData.counts` is a `BigInt`
/// map precisely so a weight may exceed `i64::MAX`, and saturating here (as
/// this used to) turned `(a => 10**30).Bag` into `i64::MAX` before any operator
/// even ran. The embedded weight in the tab format is parsed as a `BigInt` for
/// the same reason.
pub(crate) fn resolve_bag_tab_keys(bag: &HashMap<String, BigInt>) -> HashMap<String, BigInt> {
    let mut result: HashMap<String, BigInt> = HashMap::new();
    for (k, c) in bag.iter() {
        if let Some((base, raw_weight)) = k.split_once('\t') {
            let weight = match raw_weight {
                "True" => BigInt::from(1),
                "False" => BigInt::from(0),
                _ => raw_weight
                    .parse::<BigInt>()
                    .unwrap_or_else(|_| BigInt::from(1)),
            };
            *result.entry(base.to_string()).or_default() += weight * c;
        } else {
            *result.entry(k.clone()).or_default() += c;
        }
    }
    // Remove zero/negative entries for Bag semantics
    result.retain(|_, v| v.is_positive());
    result
}
