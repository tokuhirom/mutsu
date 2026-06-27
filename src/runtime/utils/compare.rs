use super::*;

pub(crate) fn to_float_value(val: &Value) -> Option<f64> {
    match val {
        Value::ContainerRef(cell) => to_float_value(&cell.lock().unwrap()),
        Value::Mixin(inner, _) => to_float_value(inner),
        Value::Num(f) => Some(*f),
        Value::Int(i) => Some(*i as f64),
        Value::BigInt(n) => n.to_f64(),
        Value::Rat(n, d) => {
            if *d != 0 {
                Some(*n as f64 / *d as f64)
            } else if *n > 0 {
                Some(f64::INFINITY)
            } else if *n < 0 {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        Value::FatRat(n, d) => {
            if *d != 0 {
                Some(*n as f64 / *d as f64)
            } else if *n > 0 {
                Some(f64::INFINITY)
            } else if *n < 0 {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        Value::BigRat(n, d) => {
            let (n, d) = (n.as_ref(), d.as_ref());
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
        Value::Complex(r, i) => {
            if *i == 0.0 {
                Some(*r)
            } else {
                None
            }
        }
        Value::Enum { value, .. } => Some(value.as_i64() as f64),
        Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
        Value::Str(s) => s.trim().parse::<f64>().ok(),
        Value::Nil => Some(0.0),
        Value::Set(items, _) => Some(items.len() as f64),
        Value::Bag(items, _) => Some(items.len() as f64),
        Value::Mix(items, _) => Some(items.len() as f64),
        Value::Hash(items) => Some(items.len() as f64),
        _ if val.as_list_items().is_some() => Some(val.as_list_items().unwrap().len() as f64),
        Value::LazyList(ll) => {
            if let Some(cached) = ll.cache.lock().unwrap().as_ref() {
                Some(cached.len() as f64)
            } else {
                Some(0.0)
            }
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => attributes.as_map().get("value").and_then(to_float_value),
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Match" => attributes.as_map().get("str").and_then(to_float_value),
        Value::Instance { attributes, .. } => attributes
            .as_map()
            .get("__mutsu_int_value")
            .and_then(to_float_value),
        // A type object (e.g. `Any`, `Str`, `Rat`, a user class) numifies to 0 in
        // numeric context (Raku warns "Use of uninitialized value"). This makes
        // `(Any) == 0` / `@a[oob] == 0` true, matching Rakudo. (`Mu` alone has no
        // Numeric and dies in raku, but treating it as 0 is a harmless divergence.)
        Value::Package(_) => Some(0.0),
        _ => None,
    }
}

pub(crate) fn to_complex_parts(val: &Value) -> Option<(f64, f64)> {
    match val {
        Value::Complex(r, i) => Some((*r, *i)),
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
    if matches!(a, Value::ContainerRef(_)) || matches!(b, Value::ContainerRef(_)) {
        return compare_values(&a.deref_container(), &b.deref_container());
    }
    match (a, b) {
        (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
            version_cmp_parts(ap, bp) as i32
        }
        (Value::Int(a), Value::Int(b)) => a.cmp(b) as i32,
        (Value::BigInt(a), Value::BigInt(b)) => a.as_ref().cmp(b.as_ref()) as i32,
        (Value::BigInt(a), Value::Int(b)) => a.as_ref().cmp(&num_bigint::BigInt::from(*b)) as i32,
        (Value::Int(a), Value::BigInt(b)) => num_bigint::BigInt::from(*a).cmp(b.as_ref()) as i32,
        (Value::Num(a), Value::Num(b)) => {
            // NaN sorts after everything (including Inf)
            match (a.is_nan(), b.is_nan()) {
                (true, true) => 0,
                (true, false) => 1,
                (false, true) => -1,
                _ => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal) as i32,
            }
        }
        (Value::BigInt(a), Value::Num(b)) => {
            a.as_ref()
                .to_f64()
                .unwrap_or(if a.as_ref().is_positive() {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                })
                .partial_cmp(b)
                .unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::Num(a), Value::BigInt(b)) => {
            a.partial_cmp(&b.as_ref().to_f64().unwrap_or(if b.as_ref().is_positive() {
                f64::INFINITY
            } else {
                f64::NEG_INFINITY
            }))
            .unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::Int(a), Value::Num(b)) => (*a as f64)
            .partial_cmp(b)
            .unwrap_or(std::cmp::Ordering::Equal) as i32,
        (Value::Num(a), Value::Int(b)) => a
            .partial_cmp(&(*b as f64))
            .unwrap_or(std::cmp::Ordering::Equal) as i32,
        (Value::Num(a), Value::Rat(n, d)) => {
            let rat_f = if *d != 0 {
                *n as f64 / *d as f64
            } else {
                f64::NAN
            };
            a.partial_cmp(&rat_f).unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::Rat(n, d), Value::Num(b)) => {
            let rat_f = if *d != 0 {
                *n as f64 / *d as f64
            } else {
                f64::NAN
            };
            rat_f.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::Num(n), Value::Str(s)) => {
            if let Some(ord) = compare_infinite_num_against_nonnumeric_str(*n, s) {
                ord
            } else {
                a.to_string_value().cmp(&b.to_string_value()) as i32
            }
        }
        (Value::Str(s), Value::Num(n)) => {
            if let Some(ord) = compare_infinite_num_against_nonnumeric_str(*n, s) {
                -ord
            } else {
                a.to_string_value().cmp(&b.to_string_value()) as i32
            }
        }
        // Pair/ValuePair comparison: compare by key first, then by value
        (Value::Pair(ak, av), Value::Pair(bk, bv)) => {
            let key_cmp = compare_values(&Value::str(ak.clone()), &Value::str(bk.clone()));
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        (Value::ValuePair(ak, av), Value::ValuePair(bk, bv)) => {
            let key_cmp = compare_values(ak, bk);
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        (Value::Pair(ak, av), Value::ValuePair(bk, bv)) => {
            let key_cmp = compare_values(&Value::str(ak.clone()), bk);
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        (Value::ValuePair(ak, av), Value::Pair(bk, bv)) => {
            let key_cmp = compare_values(ak, &Value::str(bk.clone()));
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        // Enum values: compare by their integer value
        (Value::Enum { value: av, .. }, Value::Enum { value: bv, .. }) => {
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
                if matches!(a, Value::Mixin(..)) || matches!(b, Value::Mixin(..)) {
                    return a.to_string_value().cmp(&b.to_string_value()) as i32;
                }
                return cmp;
            }
            a.to_string_value().cmp(&b.to_string_value()) as i32
        }
    }
}

pub(crate) fn to_int(v: &Value) -> i64 {
    match v {
        // Phase 2 element container: a `:=`-bound element cell that leaked into
        // a numeric context reads through to its inner value.
        Value::ContainerRef(cell) => to_int(&cell.lock().unwrap()),
        Value::Mixin(inner, _) => to_int(inner),
        Value::Int(i) => *i,
        Value::BigInt(n) => {
            use num_traits::ToPrimitive;
            n.as_ref()
                .to_i64()
                .unwrap_or(if **n > num_bigint::BigInt::from(0i64) {
                    i64::MAX
                } else {
                    i64::MIN
                })
        }
        Value::Num(f) => *f as i64,
        Value::Range(a, b) => {
            if b >= a {
                b - a + 1
            } else {
                0
            }
        }
        Value::RangeExcl(a, b) | Value::RangeExclStart(a, b) => {
            if b > a {
                b - a
            } else {
                0
            }
        }
        Value::RangeExclBoth(a, b) => {
            if *b > *a + 1 {
                b - a - 1
            } else {
                0
            }
        }
        Value::Rat(n, d) => {
            if *d != 0 {
                n / d
            } else {
                0
            }
        }
        Value::Complex(r, _) => *r as i64,
        Value::Str(s) => s.parse().unwrap_or(0),
        Value::Array(items, ..) => items.len() as i64,
        Value::Hash(items) => items.len() as i64,
        Value::Seq(items) | Value::HyperSeq(items) | Value::RaceSeq(items) => items.len() as i64,
        Value::Slip(items) => items.len() as i64,
        Value::Capture { positional, .. } => positional.len() as i64,
        Value::Instance { attributes, .. } => attributes
            .as_map()
            .get("__mutsu_int_value")
            .map_or(0, to_int),
        _ => 0,
    }
}
