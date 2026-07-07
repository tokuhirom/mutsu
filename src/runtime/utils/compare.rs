use super::*;

pub(crate) fn to_float_value(val: &Value) -> Option<f64> {
    match val.view() {
        ValueView::ContainerRef(cell) => to_float_value(&cell.lock().unwrap()),
        ValueView::Mixin(inner, _) => to_float_value(inner),
        ValueView::Num(f) => Some(f),
        ValueView::Int(i) => Some(i as f64),
        ValueView::BigInt(n) => n.to_f64(),
        ValueView::Rat(n, d) => {
            if d != 0 {
                Some(n as f64 / d as f64)
            } else if n > 0 {
                Some(f64::INFINITY)
            } else if n < 0 {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        ValueView::FatRat(n, d) => {
            if d != 0 {
                Some(n as f64 / d as f64)
            } else if n > 0 {
                Some(f64::INFINITY)
            } else if n < 0 {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        ValueView::BigRat(n, d) => {
            if !d.is_zero() {
                if let (Some(nn), Some(dd)) = (n.to_f64(), d.to_f64())
                    && nn.is_finite()
                    && dd.is_finite()
                {
                    Some(nn / dd)
                } else {
                    let scale_pow = 30u32;
                    let scale = num_bigint::BigInt::from(10u8).pow(scale_pow);
                    let scaled = (n * &scale) / d;
                    if let Some(scaled_f) = scaled
                        .to_f64()
                        .or_else(|| scaled.to_string().parse::<f64>().ok())
                    {
                        Some(scaled_f / 10f64.powi(scale_pow as i32))
                    } else if n.is_zero() {
                        Some(0.0)
                    } else if n.is_positive() {
                        Some(f64::INFINITY)
                    } else {
                        Some(f64::NEG_INFINITY)
                    }
                }
            } else if n.is_positive() {
                Some(f64::INFINITY)
            } else if n.is_negative() {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        ValueView::Complex(r, i) => {
            if i == 0.0 {
                Some(r)
            } else {
                None
            }
        }
        ValueView::Enum { value, .. } => Some(value.as_i64() as f64),
        ValueView::Bool(b) => Some(if b { 1.0 } else { 0.0 }),
        ValueView::Str(s) => s.trim().parse::<f64>().ok(),
        ValueView::Nil => Some(0.0),
        ValueView::Set(items, _) => Some(items.len() as f64),
        ValueView::Bag(items, _) => Some(items.len() as f64),
        ValueView::Mix(items, _) => Some(items.len() as f64),
        ValueView::Hash(items) => Some(items.len() as f64),
        _ if val.as_list_items().is_some() => Some(val.as_list_items().unwrap().len() as f64),
        ValueView::LazyList(ll) => {
            if let Some(cached) = ll.cache.lock().unwrap().as_ref() {
                Some(cached.len() as f64)
            } else {
                Some(0.0)
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => attributes.as_map().get("value").and_then(to_float_value),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Match" => attributes.as_map().get("str").and_then(to_float_value),
        ValueView::Instance { attributes, .. } => attributes
            .as_map()
            .get("__mutsu_int_value")
            .and_then(to_float_value),
        // A type object (e.g. `Any`, `Str`, `Rat`, a user class) numifies to 0 in
        // numeric context (Raku warns "Use of uninitialized value"). This makes
        // `(Any) == 0` / `@a[oob] == 0` true, matching Rakudo. (`Mu` alone has no
        // Numeric and dies in raku, but treating it as 0 is a harmless divergence.)
        ValueView::Package(_) => Some(0.0),
        _ => None,
    }
}

pub(crate) fn to_complex_parts(val: &Value) -> Option<(f64, f64)> {
    match val.view() {
        ValueView::Complex(r, i) => Some((r, i)),
        _ => to_float_value(val).map(|v| (v, 0.0)),
    }
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
    match (a.view(), b.view()) {
        (ValueView::Version { parts: ap, .. }, ValueView::Version { parts: bp, .. }) => {
            version_cmp_parts(ap, bp) as i32
        }
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
            if let Some(ord) = compare_infinite_num_against_nonnumeric_str(n, s) {
                ord
            } else {
                a.to_string_value().cmp(&b.to_string_value()) as i32
            }
        }
        (ValueView::Str(s), ValueView::Num(n)) => {
            if let Some(ord) = compare_infinite_num_against_nonnumeric_str(n, s) {
                -ord
            } else {
                a.to_string_value().cmp(&b.to_string_value()) as i32
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
                    return a.to_string_value().cmp(&b.to_string_value()) as i32;
                }
                return cmp;
            }
            a.to_string_value().cmp(&b.to_string_value()) as i32
        }
    }
}

pub(crate) fn to_int(v: &Value) -> i64 {
    match v.view() {
        // Phase 2 element container: a `:=`-bound element cell that leaked into
        // a numeric context reads through to its inner value.
        ValueView::ContainerRef(cell) => to_int(&cell.lock().unwrap()),
        ValueView::Mixin(inner, _) => to_int(inner),
        ValueView::Int(i) => i,
        ValueView::BigInt(n) => {
            use num_traits::ToPrimitive;
            n.as_ref()
                .to_i64()
                .unwrap_or(if **n > num_bigint::BigInt::from(0i64) {
                    i64::MAX
                } else {
                    i64::MIN
                })
        }
        ValueView::Num(f) => f as i64,
        ValueView::Range(a, b) => {
            if b >= a {
                b - a + 1
            } else {
                0
            }
        }
        ValueView::RangeExcl(a, b) | ValueView::RangeExclStart(a, b) => {
            if b > a {
                b - a
            } else {
                0
            }
        }
        ValueView::RangeExclBoth(a, b) => {
            if b > a + 1 {
                b - a - 1
            } else {
                0
            }
        }
        ValueView::Rat(n, d) => {
            if d != 0 {
                n / d
            } else {
                0
            }
        }
        ValueView::Complex(r, _) => r as i64,
        ValueView::Str(s) => s.parse().unwrap_or(0),
        ValueView::Array(items, ..) => items.len() as i64,
        ValueView::Hash(items) => items.len() as i64,
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            items.len() as i64
        }
        ValueView::Slip(items) => items.len() as i64,
        ValueView::Capture { positional, .. } => positional.len() as i64,
        ValueView::Instance { attributes, .. } => attributes
            .as_map()
            .get("__mutsu_int_value")
            .map_or(0, to_int),
        _ => 0,
    }
}
