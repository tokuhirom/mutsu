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
                    crate::builtins::methods_0arg::dispatch_core_range::range_pick_one_i64(
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
                    crate::builtins::methods_0arg::dispatch_core_range::range_pick_one_i64(
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
                    crate::builtins::methods_0arg::dispatch_core_range::range_pick_one_i64(lo, end),
                )
            }
        }
        ValueView::RangeExclBoth(start, end) => {
            let lo = start.saturating_add(1);
            let hi = end.saturating_sub(1);
            if lo > hi {
                Some(Value::NIL)
            } else {
                Some(crate::builtins::methods_0arg::dispatch_core_range::range_pick_one_i64(lo, hi))
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
                crate::builtins::methods_0arg::dispatch_core_range::generic_range_pick_one(
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
    use crate::builtins::methods_0arg::dispatch_core_range::{
        generic_range_pick_n, range_pick_n_i64,
    };

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
