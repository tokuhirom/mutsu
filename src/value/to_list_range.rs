//! The `GenericRange` arm of [`value_to_list`](super::to_list::value_to_list):
//! numeric, string (`.succ`) and `Date` ranges. Split out of `to_list.rs` to
//! keep both files under the 500-line convention.

use super::to_list::MAX_RANGE_EXPAND;
use crate::value::compare::compare_values;
use crate::value::identity::values_identical;
use crate::value::radix_numeric::coerce_to_numeric;
use crate::value::{Value, ValueView};
use std::sync::Arc;

/// Expand a `GenericRange` (`val`, with its `start`/`end` and exclusivity) to
/// its elements, capped at `MAX_RANGE_EXPAND` for an unbounded end.
pub(super) fn generic_range_to_list(
    val: &Value,
    start: &Arc<Value>,
    end: &Arc<Value>,
    excl_start: bool,
    excl_end: bool,
) -> Vec<Value> {
    let next_numeric = |v: &Value| -> Option<Value> {
        match v.view() {
            ValueView::Int(i) => Some(Value::int(i + 1)),
            ValueView::BigInt(n) => Some(Value::bigint(n.as_ref() + 1)),
            ValueView::Num(f) => Some(Value::num(f + 1.0)),
            ValueView::Rat(n, d) => Some(crate::value::make_rat(n + d, d)),
            ValueView::FatRat(n, d) => Some(Value::fat_rat_raw(n + d, d)),
            ValueView::BigRat(n, d) => Some(Value::bigrat(n + d, d.clone())),
            _ if v.is_numeric() => Some(Value::num(v.to_f64() + 1.0)),
            _ => None,
        }
    };
    // String ranges: expand as character sequences
    if let (ValueView::Str(a), ValueView::Str(b)) = (start.as_ref().view(), end.as_ref().view()) {
        if a.chars().count() == 1 && b.chars().count() == 1 {
            let s = a.chars().next().unwrap() as u32;
            let e = b.chars().next().unwrap() as u32;
            let s = if excl_start { s + 1 } else { s };
            if excl_end {
                (s..e)
                    .filter_map(char::from_u32)
                    .map(|c| Value::str(c.to_string()))
                    .collect()
            } else {
                (s..=e)
                    .filter_map(char::from_u32)
                    .map(|c| Value::str(c.to_string()))
                    .collect()
            }
        } else if !excl_start
            && !excl_end
            && a.chars().count() == b.chars().count()
            && a.chars().count() > 1
        {
            // Equal-length inclusive string ranges use Raku's per-position
            // "odometer": each character position independently ranges from
            // a[i] to b[i] (ascending if a[i] <= b[i], else descending), and
            // the positions form a mixed-radix counter with the rightmost
            // varying fastest. E.g. "r2".."t3" is {r,s,t} x {2,3} =
            // (r2 r3 s2 s3 t2 t3), NOT the string-succ sequence
            // (r2 r3 r4 ... t3). A range whose start sorts after its end is
            // empty (e.g. "ba".."ab").
            if a.as_str() > b.as_str() {
                return Vec::new();
            }
            let axes: Vec<Vec<char>> = a
                .chars()
                .zip(b.chars())
                .map(|(lo, hi)| {
                    let (lo, hi) = (lo as u32, hi as u32);
                    if lo <= hi {
                        (lo..=hi).filter_map(char::from_u32).collect()
                    } else {
                        (hi..=lo).rev().filter_map(char::from_u32).collect()
                    }
                })
                .collect();
            let limit = MAX_RANGE_EXPAND as usize;
            let mut result = Vec::new();
            let mut idx = vec![0usize; axes.len()];
            'odometer: loop {
                if result.len() >= limit {
                    break;
                }
                let s: String = idx.iter().zip(axes.iter()).map(|(&i, ax)| ax[i]).collect();
                result.push(Value::str(s));
                // Increment the odometer, rightmost wheel fastest.
                let mut pos = axes.len();
                loop {
                    if pos == 0 {
                        break 'odometer;
                    }
                    pos -= 1;
                    idx[pos] += 1;
                    if idx[pos] < axes[pos].len() {
                        break;
                    }
                    idx[pos] = 0;
                }
            }
            result
        } else {
            let split_numeric_core = |s: &str| -> Option<(String, String, String)> {
                let mut start_byte = None;
                let mut end_byte = None;
                for (idx, ch) in s.char_indices() {
                    if ch.is_ascii_digit() {
                        if start_byte.is_none() {
                            start_byte = Some(idx);
                        }
                    } else if start_byte.is_some() && end_byte.is_none() {
                        end_byte = Some(idx);
                        break;
                    }
                }
                let start = start_byte?;
                let end = end_byte.unwrap_or(s.len());
                if end <= start {
                    return None;
                }
                Some((
                    s[..start].to_string(),
                    s[start..end].to_string(),
                    s[end..].to_string(),
                ))
            };
            if let (Some((ap, an, asuf)), Some((bp, bn, bsuf))) =
                (split_numeric_core(&a), split_numeric_core(&b))
                && ap == bp
                && asuf == bsuf
                && let (Ok(mut n), Ok(e)) = (an.parse::<i128>(), bn.parse::<i128>())
            {
                if excl_start {
                    n += 1;
                }
                if n > e {
                    return Vec::new();
                }
                let width = an.len().max(bn.len());
                let pad = an.starts_with('0') || bn.starts_with('0');
                let mut result = Vec::new();
                let limit = MAX_RANGE_EXPAND as usize;
                while n <= e && result.len() < limit {
                    if excl_end && n == e {
                        break;
                    }
                    let digits = if pad {
                        format!("{n:0width$}")
                    } else {
                        n.to_string()
                    };
                    result.push(Value::str(format!("{ap}{digits}{asuf}")));
                    n += 1;
                }
                return result;
            }
            // Multi-char string ranges: use string succession
            let mut result = Vec::new();
            let mut current = if excl_start {
                crate::value::str_increment::string_succ(&a)
            } else {
                a.to_string()
            };
            let limit = MAX_RANGE_EXPAND as usize;
            while current.as_str() <= b.as_str() && result.len() < limit {
                if excl_end && current.as_str() == b.as_str() {
                    break;
                }
                result.push(Value::str(current.clone()));
                current = crate::value::str_increment::string_succ(&current);
            }
            result
        }
    } else if let (ValueView::Str(a), ValueView::HyperWhatever | ValueView::Whatever) =
        (start.as_ref().view(), end.as_ref().view())
    {
        let mut result = Vec::new();
        let mut current = if excl_start {
            crate::value::str_increment::string_succ(&a)
        } else {
            a.to_string()
        };
        let limit = MAX_RANGE_EXPAND as usize;
        while result.len() < limit {
            result.push(Value::str(current.clone()));
            current = crate::value::str_increment::string_succ(&current);
        }
        result
    } else if let ValueView::Str(a) = start.as_ref().view() {
        // Start is a Str — iterate as strings (preserving type).
        // In Raku, "1"..9 produces ("1", "2", ..., "9").
        // When end is numeric, compare numerically to determine bounds.
        let end_is_numeric = end.as_ref().is_numeric()
            || matches!(
                end.as_ref().view(),
                ValueView::Whatever | ValueView::HyperWhatever
            );
        let end_str = match end.as_ref().view() {
            ValueView::Str(s) => (**s).clone(),
            _ => end.as_ref().to_string_value(),
        };
        let end_f64 = end.as_ref().to_f64();
        let mut result = Vec::new();
        let mut current = if excl_start {
            crate::value::str_increment::string_succ(&a)
        } else {
            a.to_string()
        };
        let limit = MAX_RANGE_EXPAND as usize;
        while result.len() < limit {
            let in_range = if end_is_numeric {
                // Compare current string numerically against end
                let cur_numeric = coerce_to_numeric(Value::str(current.clone()));
                let cur_f64 = cur_numeric.to_f64();
                if excl_end {
                    cur_f64 < end_f64
                } else {
                    cur_f64 <= end_f64
                }
            } else {
                // String comparison
                if excl_end {
                    current.as_str() < end_str.as_str()
                } else {
                    current.as_str() <= end_str.as_str()
                }
            };
            if !in_range {
                break;
            }
            result.push(Value::str(current.clone()));
            current = crate::value::str_increment::string_succ(&current);
        }
        result
    } else {
        // Numeric GenericRange: expand using .succ semantics to preserve endpoint type.
        let start_num = if start.is_numeric() {
            Some(start.as_ref().clone())
        } else {
            None
        };
        let end_num = if matches!(
            end.as_ref().view(),
            ValueView::Whatever | ValueView::HyperWhatever
        ) {
            Some(Value::num(f64::INFINITY))
        } else if end.is_numeric() {
            Some(end.as_ref().clone())
        } else if let ValueView::Str(s) = end.as_ref().view() {
            let coerced = coerce_to_numeric(Value::str((**s).clone()));
            if coerced.is_numeric() {
                Some(coerced)
            } else {
                None
            }
        } else {
            None
        };
        let (start_num, end_num) = match (start_num, end_num) {
            (Some(s), Some(e)) => (s, e),
            _ => {
                // Check for Date-like instances with .succ support
                let is_date_like = |v: &Value| -> bool {
                    if let ValueView::Instance { attributes, .. } = v.view() {
                        attributes.contains_key("year")
                            && attributes.contains_key("month")
                            && attributes.contains_key("day")
                            && !attributes.contains_key("hour")
                    } else {
                        false
                    }
                };
                if is_date_like(start.as_ref()) && is_date_like(end.as_ref()) {
                    use crate::value::temporal_core::{
                        civil_to_epoch_days, date_attrs, epoch_days_to_civil,
                        make_date_with_formatter,
                    };
                    let (sy, sm, sd) =
                        if let ValueView::Instance { attributes, .. } = start.as_ref().view() {
                            date_attrs(&(attributes).as_map())
                        } else {
                            unreachable!()
                        };
                    let (ey, em, ed) =
                        if let ValueView::Instance { attributes, .. } = end.as_ref().view() {
                            date_attrs(&(attributes).as_map())
                        } else {
                            unreachable!()
                        };
                    let start_days = civil_to_epoch_days(sy, sm, sd);
                    let end_days = civil_to_epoch_days(ey, em, ed);
                    let formatter =
                        if let ValueView::Instance { attributes, .. } = start.as_ref().view() {
                            attributes.as_map().get("formatter").cloned()
                        } else {
                            None
                        };
                    let mut result = Vec::new();
                    let first_day = if excl_start {
                        start_days + 1
                    } else {
                        start_days
                    };
                    let limit = MAX_RANGE_EXPAND as usize;
                    let mut d = first_day;
                    while result.len() < limit {
                        if d > end_days || (excl_end && d == end_days) {
                            break;
                        }
                        let (y, m, dd) = epoch_days_to_civil(d);
                        result.push(make_date_with_formatter(y, m, dd, formatter.clone()));
                        d += 1;
                    }
                    return result;
                }
                return vec![val.clone()];
            }
        };
        let s_f = start_num.to_f64();
        let e_f = end_num.to_f64();
        if s_f.is_infinite() || s_f.is_nan() || e_f.is_nan() {
            // Degenerate numeric range whose *start* is non-finite (or
            // end is NaN) — the normal `.succ` expansion below cannot
            // run. (A right-infinite range with a *finite* start, e.g.
            // `-17..^Inf`, falls through to the normal branch, which
            // expands up to the cap.)
            //   * Empty when the start strictly exceeds the end
            //     (`Inf..0`); NaN comparisons are false, so NaN ranges
            //     are not empty. `(Inf..0).elems == 0`.
            //   * A `+Inf` start yields no usable values (Rakudo:
            //     `(Inf..Inf)[^5]` / `(Inf..NaN)[^5]` are all Nil), so it
            //     is empty too.
            //   * A `-Inf`/`NaN` start never advances under `.succ`
            //     (`-Inf+1 == -Inf`, `NaN+1 == NaN`), so the range yields
            //     its start ad infinitum — materialize up to the cap.
            if s_f > e_f || s_f == f64::INFINITY {
                Vec::new()
            } else {
                let limit = MAX_RANGE_EXPAND as usize;
                vec![start_num; limit]
            }
        } else {
            let mut result = Vec::new();
            let mut current = if excl_start {
                next_numeric(&start_num).unwrap_or(start_num)
            } else {
                start_num
            };
            let limit = MAX_RANGE_EXPAND as usize;
            while result.len() < limit {
                let cmp = compare_values(&current, &end_num);
                if cmp > 0 || (excl_end && cmp == 0) {
                    break;
                }
                result.push(current.clone());
                let Some(next) = next_numeric(&current) else {
                    break;
                };
                if values_identical(&next, &current) {
                    break;
                }
                current = next;
            }
            result
        }
    }
}
