use super::*;

pub(crate) fn to_complex_parts(val: &Value) -> Option<(f64, f64)> {
    match val.view() {
        ValueView::Complex(r, i) => Some((r, i)),
        _ => to_float_value(val).map(|v| (v, 0.0)),
    }
}

/// Whether a default-order sort over `items` must leave the pure
/// [`compare_values`] fast path: an object element may define its own
/// `Str`/`Stringy`, which only the dispatched `infix:<cmp>` honours (see
/// `sort_items_generic`). Pure layers decline and let the interpreter decide.
// Cost: O(e), e = elements (one tag probe each).
pub(crate) fn sort_needs_dispatched_cmp<'a>(items: impl IntoIterator<Item = &'a Value>) -> bool {
    items
        .into_iter()
        .any(|v| matches!(v.deref_container().view(), ValueView::Instance { .. }))
}

pub(crate) fn compare_values(a: &Value, b: &Value) -> i32 {
    fn compare_infinite_num_against_nonnumeric_str(num: f64, s: &str) -> Option<i32> {
        if !num.is_infinite() || s.trim().parse::<f64>().is_ok() {
            return None;
        }
        Some(if num.is_sign_positive() {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Less
        } as i32)
    }

    // First-class element containers (`ContainerRef`, e.g. a `:=`-bound array
    // slot or a `.grep` rw alias) compare by their inner value — `.sort`/min/max
    // over a grepped/bound array must order the values, not the cells.
    if matches!(a.view(), ValueView::ContainerRef(_))
        || matches!(b.view(), ValueView::ContainerRef(_))
    {
        return compare_values(&a.deref_container(), &b.deref_container());
    }
    // A Bool numifies (False → 0, True → 1) when compared against a number or
    // another Bool, so `0 cmp False` / `0 <=> False` is Same, matching Rakudo.
    // Without this, a Bool falls through to the string-comparison fallback below
    // ("0".cmp("False")), which mis-orders it and breaks `min`/`max` tie-breaking
    // (`min False, 0` must keep the first argument). Normalize a Bool operand to
    // its Int value; recursion terminates because neither operand is a Bool after
    // normalization.
    if matches!(a.view(), ValueView::Bool(_)) || matches!(b.view(), ValueView::Bool(_)) {
        let normalize = |v: &Value| match v.view() {
            ValueView::Bool(flag) => Value::int(if flag { 1 } else { 0 }),
            _ => v.clone(),
        };
        return compare_values(&normalize(a), &normalize(b));
    }
    // List-like values (Array/Seq/Slip/List) compare element-wise, like Raku's
    // `cmp`/`<=>` on lists: the first differing element decides, and if one list
    // is a prefix of the other the shorter sorts Less. This drives multi-key
    // `.sort` where a 1-arity block returns a list of keys
    // (`.sort({ .Int, .comb.sum, .Str })`) — without it the list keys fall to the
    // string-gist fallback below and mis-order.
    if let (Some(al), Some(bl)) = (a.as_list_items(), b.as_list_items()) {
        for (ax, bx) in al.iter().zip(bl.iter()) {
            let c = compare_values(ax, bx);
            if c != 0 {
                return c;
            }
        }
        return al.len().cmp(&bl.len()) as i32;
    }
    match (a.view(), b.view()) {
        (
            ValueView::Version {
                parts: ap,
                plus: apl,
                minus: ami,
                ..
            },
            ValueView::Version {
                parts: bp,
                plus: bpl,
                minus: bmi,
                ..
            },
        ) => crate::runtime::version_cmp(ap, apl, ami, bp, bpl, bmi) as i32,
        (ValueView::Int(a), ValueView::Int(b)) => a.cmp(&b) as i32,
        (ValueView::BigInt(a), ValueView::BigInt(b)) => a.as_ref().cmp(b.as_ref()) as i32,
        (ValueView::BigInt(a), ValueView::Int(b)) => {
            a.as_ref().cmp(&num_bigint::BigInt::from(b)) as i32
        }
        (ValueView::Int(a), ValueView::BigInt(b)) => {
            num_bigint::BigInt::from(a).cmp(b.as_ref()) as i32
        }
        (ValueView::Num(a), ValueView::Num(b)) => {
            // NaN sorts after everything (including Inf)
            match (a.is_nan(), b.is_nan()) {
                (true, true) => 0,
                (true, false) => 1,
                (false, true) => -1,
                _ => a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32,
            }
        }
        (ValueView::BigInt(a), ValueView::Num(b)) => {
            a.as_ref()
                .to_f64()
                .unwrap_or(if a.as_ref().is_positive() {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                })
                .partial_cmp(&b)
                .unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (ValueView::Num(a), ValueView::BigInt(b)) => {
            a.partial_cmp(&b.as_ref().to_f64().unwrap_or(if b.as_ref().is_positive() {
                f64::INFINITY
            } else {
                f64::NEG_INFINITY
            }))
            .unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (ValueView::Int(a), ValueView::Num(b)) => (a as f64)
            .partial_cmp(&b)
            .unwrap_or(std::cmp::Ordering::Equal)
            as i32,
        (ValueView::Num(a), ValueView::Int(b)) => {
            a.partial_cmp(&(b as f64))
                .unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (ValueView::Num(a), ValueView::Rat(n, d)) => {
            let rat_f = if d != 0 {
                n as f64 / d as f64
            } else {
                f64::NAN
            };
            a.partial_cmp(&rat_f).unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (ValueView::Rat(n, d), ValueView::Num(b)) => {
            let rat_f = if d != 0 {
                n as f64 / d as f64
            } else {
                f64::NAN
            };
            rat_f.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (ValueView::Num(n), ValueView::Str(s)) => {
            if let Some(ord) = compare_infinite_num_against_nonnumeric_str(n, &s) {
                ord
            } else {
                a.string_value_cow().cmp(&b.string_value_cow()) as i32
            }
        }
        (ValueView::Str(s), ValueView::Num(n)) => {
            if let Some(ord) = compare_infinite_num_against_nonnumeric_str(n, &s) {
                -ord
            } else {
                a.string_value_cow().cmp(&b.string_value_cow()) as i32
            }
        }
        // Pair/ValuePair comparison: compare by key first, then by value
        (ValueView::Pair(ak, av), ValueView::Pair(bk, bv)) => {
            let key_cmp = compare_values(&Value::str(ak.clone()), &Value::str(bk.clone()));
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        (ValueView::ValuePair(ak, av), ValueView::ValuePair(bk, bv)) => {
            let key_cmp = compare_values(ak, bk);
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        (ValueView::Pair(ak, av), ValueView::ValuePair(bk, bv)) => {
            let key_cmp = compare_values(&Value::str(ak.clone()), bk);
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        (ValueView::ValuePair(ak, av), ValueView::Pair(bk, bv)) => {
            let key_cmp = compare_values(ak, &Value::str(bk.clone()));
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        // Enum values: compare by their integer value
        (ValueView::Enum { value: av, .. }, ValueView::Enum { value: bv, .. }) => {
            av.as_i64().cmp(&bv.as_i64()) as i32
        }
        _ => {
            if let (Some((an, ad)), Some((bn, bd))) = (to_rat_parts(a), to_rat_parts(b)) {
                let cmp = compare_rat_parts((an, ad), (bn, bd)) as i32;
                if cmp != 0 {
                    return cmp;
                }
                // For allomorphic types (IntStr, RatStr, etc.), break numeric ties
                // with string comparison
                if matches!(a.view(), ValueView::Mixin(..))
                    || matches!(b.view(), ValueView::Mixin(..))
                {
                    return a.string_value_cow().cmp(&b.string_value_cow()) as i32;
                }
                return cmp;
            }
            // Big rationals (BigRat / big FatRat / BigInt vs Rat mixes) compare
            // numerically too — without this branch a `.sort` over values past
            // i64 falls to the string fallback and mis-orders them.
            if let (Some(ap), Some(bp)) = (
                crate::runtime::utils::to_big_rat_parts(a),
                crate::runtime::utils::to_big_rat_parts(b),
            ) {
                let cmp = crate::runtime::utils::compare_big_rat_parts(ap, bp)
                    .unwrap_or(std::cmp::Ordering::Equal) as i32;
                if cmp != 0 {
                    return cmp;
                }
                if matches!(a.view(), ValueView::Mixin(..))
                    || matches!(b.view(), ValueView::Mixin(..))
                {
                    return a.string_value_cow().cmp(&b.string_value_cow()) as i32;
                }
                return cmp;
            }
            a.string_value_cow().cmp(&b.string_value_cow()) as i32
        }
    }
}

/// Whether a value has the boxed integer representation used by native
/// integer values.  `nqp::box_i` preserves an `Int` subclass as an instance,
/// so callers that accept an `Int` argument must recognize its reserved
/// payload just like the ordinary `Int` and `BigInt` variants.
pub(crate) fn is_integer_value(v: &Value) -> bool {
    match v.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => true,
        ValueView::Mixin(inner, _) => is_integer_value(inner),
        ValueView::Instance { attributes, .. } => {
            attributes.as_map().contains_key("__mutsu_int_value")
        }
        _ => false,
    }
}
