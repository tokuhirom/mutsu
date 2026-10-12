//! Sampling from a `Range` without enumerating it.

use crate::value::{Value, ValueView};

/// Efficiently sample one random element from a Range without enumerating all elements.
/// Uses raw u64 entropy for full bit coverage on large ranges.
// Cost: O(1) on an Int range, O(e) on a non-integer numeric one, e = elements.
pub(super) fn sample_one_from_range(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Range(start, end) => {
            if end < start {
                Some(Value::NIL)
            } else {
                Some(
                    range_pick_one_i64(
                        start, end,
                    ),
                )
            }
        }
        ValueView::RangeExcl(start, end) => {
            let hi = end.saturating_sub(1);
            if start > hi {
                Some(Value::NIL)
            } else {
                Some(
                    range_pick_one_i64(
                        start, hi,
                    ),
                )
            }
        }
        ValueView::RangeExclStart(start, end) => {
            let lo = start.saturating_add(1);
            if lo > end {
                Some(Value::NIL)
            } else {
                Some(
                    range_pick_one_i64(lo, end),
                )
            }
        }
        ValueView::RangeExclBoth(start, end) => {
            let lo = start.saturating_add(1);
            let hi = end.saturating_sub(1);
            if lo > hi {
                Some(Value::NIL)
            } else {
                Some(range_pick_one_i64(lo, hi))
            }
        }
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            // Try integer (Int/BigInt) endpoints first
            if let Some(result) =
                generic_range_pick_one(
                    start, end, excl_start, excl_end,
                )
            {
                return Some(result);
            }
            // Non-integer numeric endpoints (Rat/Num/FatRat): enumerate via
            // `.succ` semantics so the picked element keeps its endpoint type
            // (`(1.1..3.1).roll` yields a Rat, not a Num) — reuse value_to_list,
            // which already expands the range preserving type, then pick one.
            if start.is_numeric() {
                let pool = crate::runtime::utils::value_to_list(target);
                if pool.is_empty() {
                    return Some(Value::NIL);
                }
                let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize
                    % pool.len();
                return Some(pool[idx].clone());
            }
            None
        }
        _ => None,
    }
}

/// Fast path for `.pick(n)` on integer Range types.
/// Returns None if target is not an integer range (caller should fall back).
// Cost: O(k) for k picks of an integer Range of any size (O(r) for `.pick(*)`, r = range
// size, capped at 10^8); `None` for any other receiver.
pub(super) fn range_pick_n_fast(target: &Value, arg: &Value) -> Option<Value> {
    // Determine effective inclusive bounds
    let (start, end, is_generic, generic_start, generic_end, excl_start, excl_end) =
        match target.view() {
            ValueView::Range(a, b) => (a, b, false, None, None, false, false),
            ValueView::RangeExcl(a, b) => (a, b - 1, false, None, None, false, false),
            ValueView::RangeExclStart(a, b) => (a + 1, b, false, None, None, false, false),
            ValueView::RangeExclBoth(a, b) => (a + 1, b - 1, false, None, None, false, false),
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => (
                0,
                0,
                true,
                Some(start.clone()),
                Some(end.clone()),
                excl_start,
                excl_end,
            ),
            _ => return None,
        };

    // Determine count from arg
    let is_whatever = matches!(arg.view(), ValueView::Whatever)
        || matches!(arg.view(), ValueView::Num(f) if f.is_infinite() && f.is_sign_positive());

    if is_generic {
        let gs = generic_start.as_ref().unwrap();
        let ge = generic_end.as_ref().unwrap();
        // Check if endpoints are integer-like
        if !matches!(
            gs.as_ref().view(),
            ValueView::Int(_) | ValueView::BigInt(_) | ValueView::Bool(_)
        ) || !matches!(
            ge.as_ref().view(),
            ValueView::Int(_) | ValueView::BigInt(_) | ValueView::Bool(_)
        ) {
            return None;
        }

        if is_whatever {
            // pick(*) on a huge range — cannot materialize, return what we can
            // Actually for GenericRange with BigInt endpoints, pick(*) means return all
            // elements shuffled. For very large ranges this is impossible, so fall back.
            return None;
        }

        let count = match arg.view() {
            ValueView::Int(n) => n.max(0) as usize,
            ValueView::Num(f) => (f as i64).max(0) as usize,
            ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as usize,
            ValueView::Str(s) => s.trim().parse::<i64>().unwrap_or(0).max(0) as usize,
            _ => return None,
        };

        if count == 0 {
            return Some(Value::seq(Vec::new()));
        }

        let items = generic_range_pick_n(gs, ge, excl_start, excl_end, count)?;
        Some(Value::seq(items))
    } else {
        if end < start {
            return Some(Value::seq(Vec::new()));
        }

        if is_whatever {
            // pick(*) — return all elements shuffled
            // Only feasible for ranges that fit in memory
            let range_size = (end as i128) - (start as i128) + 1;
            if range_size > 100_000_000 {
                // Too large to materialize, fall back
                return None;
            }
            let items = range_pick_n_i64(start, end, range_size as usize);
            return Some(Value::seq(items));
        }

        let count = match arg.view() {
            ValueView::Int(n) => n.max(0) as usize,
            ValueView::Num(f) => (f as i64).max(0) as usize,
            ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as usize,
            ValueView::Str(s) => s.trim().parse::<i64>().unwrap_or(0).max(0) as usize,
            _ => return None,
        };

        if count == 0 {
            return Some(Value::seq(Vec::new()));
        }

        let items = range_pick_n_i64(start, end, count);
        Some(Value::seq(items))
    }
}

pub(crate) fn range_pick_one_i64(start: i64, end: i64) -> Value {
    let range_size = (end as u128).wrapping_sub(start as u128).wrapping_add(1);
    let idx = random_u128_in_range(range_size);
    Value::int(start.wrapping_add(idx as i64))
}

/// Generate a random u128 in [0, range_size) with full bit coverage.
fn random_u128_in_range(range_size: u128) -> u128 {
    if range_size <= 1 {
        return 0;
    }
    // For small ranges that fit in 53 bits of precision, single rand() suffices
    if range_size <= (1u128 << 53) {
        let idx = (crate::builtins::rng::builtin_rand() * range_size as f64) as u128;
        return idx % range_size;
    }
    // For larger ranges, use raw u64 with full 64-bit entropy
    let raw = crate::builtins::rng::builtin_rand_u64() as u128;
    if range_size <= (1u128 << 64) {
        return raw % range_size;
    }
    // For ranges > 2^64, combine two u64 values
    let raw2 = crate::builtins::rng::builtin_rand_u64() as u128;
    let combined = (raw << 64) | raw2;
    combined % range_size
}

/// Pick one random element from a GenericRange.
/// Returns None if the range has non-integer endpoints (fallback needed).
pub(crate) fn generic_range_pick_one(
    start: &Value,
    end: &Value,
    excl_start: bool,
    excl_end: bool,
) -> Option<Value> {
    let s = value_to_bigint(start)?;
    let e = value_to_bigint(end)?;

    let effective_start = if excl_start { s + 1_i64 } else { s };
    let effective_end = if excl_end { e - 1_i64 } else { e };

    if effective_end < effective_start {
        return Some(Value::NIL);
    }

    let range_size = &effective_end - &effective_start + 1_i64;
    let offset = random_bigint_in_range(&range_size);
    let result = effective_start + offset;
    Some(bigint_to_value(result))
}

/// Convert a Value to BigInt if it represents an integer.
fn value_to_bigint(v: &Value) -> Option<num_bigint::BigInt> {
    use num_bigint::BigInt as NumBigInt;
    match v.view() {
        ValueView::Int(n) => Some(NumBigInt::from(n)),
        ValueView::BigInt(n) => Some((**n).clone()),
        ValueView::Bool(b) => Some(NumBigInt::from(b as i64)),
        _ => None,
    }
}

/// Generate a random BigInt in [0, range_size).
pub(crate) fn random_bigint_in_range(range_size: &num_bigint::BigInt) -> num_bigint::BigInt {
    use num_bigint::BigInt as NumBigInt;
    use num_traits::{ToPrimitive, Zero};

    if range_size.is_zero() {
        return NumBigInt::from(0_i64);
    }

    // For ranges that fit in u64, use raw u64 for full precision
    if let Some(n) = range_size.to_u64() {
        if n <= (1u64 << 53) {
            let idx = (crate::builtins::rng::builtin_rand() * n as f64) as u64 % n;
            return NumBigInt::from(idx);
        }
        // For larger u64 ranges, use raw u64 to avoid float precision loss
        let raw = crate::builtins::rng::builtin_rand_u64();
        return NumBigInt::from(raw % n);
    }

    // For larger ranges, generate random bytes using raw u64
    let (_, bytes) = range_size.to_bytes_be();
    let num_bytes = bytes.len();
    let mut result_bytes = vec![0u8; num_bytes];

    // Generate random bytes 8 at a time using full u64 entropy
    let mut i = 0;
    while i < num_bytes {
        let r = crate::builtins::rng::builtin_rand_u64();
        let r_bytes = r.to_le_bytes();
        for &b in &r_bytes {
            if i < num_bytes {
                result_bytes[i] = b;
                i += 1;
            }
        }
    }

    let candidate = NumBigInt::from_bytes_be(num_bigint::Sign::Plus, &result_bytes);
    // Modulo to bring into range
    candidate % range_size
}

/// Convert a BigInt to the appropriate Value (Int if it fits, BigInt otherwise).
fn bigint_to_value(n: num_bigint::BigInt) -> Value {
    use num_traits::ToPrimitive;
    if let Some(i) = n.to_i64() {
        Value::int(i)
    } else {
        Value::bigint(n)
    }
}

/// Pick n random unique elements from an i64 range [start, end] (inclusive).
/// Used by the 1-arg pick method.
pub(crate) fn range_pick_n_i64(start: i64, end: i64, count: usize) -> Vec<Value> {
    use std::collections::HashSet;

    let range_size = (end as u128).wrapping_sub(start as u128).wrapping_add(1);
    let actual_count = if range_size <= usize::MAX as u128 {
        count.min(range_size as usize)
    } else {
        count
    };

    // For small ranges relative to count, use Fisher-Yates on indices
    if range_size <= 10_000_000 && (range_size as usize) <= actual_count * 4 {
        let mut indices: Vec<i64> = (start..=end).collect();
        let len = indices.len();
        for i in (1..len).rev() {
            let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize % (i + 1);
            indices.swap(i, j);
        }
        indices.truncate(actual_count);
        return indices.into_iter().map(Value::int).collect();
    }

    // For large ranges, use rejection sampling with a HashSet
    let mut seen = HashSet::with_capacity(actual_count);
    let mut result = Vec::with_capacity(actual_count);
    while result.len() < actual_count {
        let idx = random_u128_in_range(range_size);
        let val = start.wrapping_add(idx as i64);
        if seen.insert(val) {
            result.push(Value::int(val));
        }
    }
    result
}

/// Pick n random unique elements from a GenericRange.
/// Returns None if the range has non-integer endpoints.
pub(crate) fn generic_range_pick_n(
    start: &Value,
    end: &Value,
    excl_start: bool,
    excl_end: bool,
    count: usize,
) -> Option<Vec<Value>> {
    use num_bigint::BigInt as NumBigInt;
    use std::collections::HashSet;

    let s = value_to_bigint(start)?;
    let e = value_to_bigint(end)?;

    let effective_start = if excl_start { s + 1_i64 } else { s };
    let effective_end = if excl_end { e - 1_i64 } else { e };

    if effective_end < effective_start {
        return Some(Vec::new());
    }

    let range_size = &effective_end - &effective_start + 1_i64;
    // Determine actual count (capped by range size)
    let actual_count = if let Some(rs) = num_traits::ToPrimitive::to_usize(&range_size) {
        count.min(rs)
    } else {
        count // range is huge, count is always less
    };

    // Use rejection sampling with BigInt
    let mut seen: HashSet<NumBigInt> = HashSet::with_capacity(actual_count);
    let mut result = Vec::with_capacity(actual_count);
    while result.len() < actual_count {
        let offset = random_bigint_in_range(&range_size);
        let val = &effective_start + &offset;
        if seen.insert(val.clone()) {
            result.push(bigint_to_value(val));
        }
    }
    Some(result)
}
