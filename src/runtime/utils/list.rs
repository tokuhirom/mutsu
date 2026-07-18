use super::*;
use crate::value::ValueView;

pub(crate) fn char_idx_to_byte(text: &str, idx: usize) -> usize {
    if idx == 0 {
        return 0;
    }
    for (count, (b, _)) in text.char_indices().enumerate() {
        if count == idx {
            return b;
        }
    }
    text.len()
}

/// Coerce a list element to a form that binds *positionally* when passed as the
/// topic/argument of a matcher or comparator block.
///
/// mutsu uses the `Value` variant to distinguish a call-site named argument
/// (`Pair`, excluded from positional arity everywhere in dispatch) from a
/// positional pair *value* (`ValuePair`). Iterating a Hash yields
/// `Pair` elements, so passing such an element straight to a block (e.g.
/// `%h.first({ .value > 1 })` / `%h.sort({ ... })`) would bind it as a *named*
/// argument, leaving the block with zero positionals ("expected N got 0"). Map
/// `Pair` to `ValuePair` so the element binds as `$_`/`$^a`.
pub(crate) fn pair_as_positional(val: &Value) -> Value {
    match val.view() {
        ValueView::Pair(k, v) => Value::value_pair(Value::str(k.clone()), v.clone()),
        _ => val.clone(),
    }
}

/// Extract the candidate closures from a `WalkList` instance's attributes,
/// honoring its `reversed` flag. Returns `None` if the attributes are not
/// shaped like a WalkList.
pub(crate) fn walk_list_candidates(attributes: &crate::value::InstanceAttrs) -> Option<Vec<Value>> {
    let map = attributes.as_map();
    let Some(ValueView::Array(items, ..)) = map.get("candidates").map(Value::view) else {
        return None;
    };
    let mut cands = items.to_vec();
    if matches!(
        map.get("reversed").map(Value::view),
        Some(ValueView::Bool(true))
    ) {
        cands.reverse();
    }
    Some(cands)
}

pub(crate) fn value_to_list(val: &Value) -> Vec<Value> {
    match val.view() {
        ValueView::Array(_, kind) if kind.is_itemized() => vec![val.clone()],
        // A role mixin over a (non-itemized) list-ish value lists as the inner
        // value does. An itemized inner keeps one item — `for $x` where
        // `my $x = (1,2); $x does R` iterates once, like the plain itemized
        // scalar — and a scalar mixin stays one item with its mixin identity.
        // Iteration METHODS (`.map`/`.grep`) unwrap itemization separately via
        // `Interpreter::mixin_iteration_target`.
        ValueView::Mixin(inner, _)
            if matches!(
                inner.view(),
                ValueView::Array(_, kind) if !kind.is_itemized()
            ) || matches!(
                inner.view(),
                ValueView::Seq(_)
                    | ValueView::HyperSeq(_)
                    | ValueView::RaceSeq(_)
                    | ValueView::Slip(_)
                    | ValueView::LazyList(_)
                    | ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. }
                    | ValueView::Set(..)
                    | ValueView::Bag(..)
                    | ValueView::Mix(..)
            ) || (matches!(inner.view(), ValueView::Hash(_)) && !inner.hash_is_itemized()) =>
        {
            value_to_list(inner)
        }
        ValueView::Array(items, ..) => items.to_vec(),
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            items.to_vec()
        }
        ValueView::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
        // An itemized hash (`item %h` / `$(%h)`) is a single list element and does
        // NOT flatten to its pairs (mirrors the itemized-Array arm above).
        ValueView::Hash(_) if val.hash_is_itemized() => vec![val.clone()],
        // `typed_pair` decontainerizes element cells so the pair value matches a
        // `%h<k>` read / `.values` (see t/bind-hash-value-pairs.t).
        ValueView::Hash(items) => items
            .iter()
            .map(|(k, v)| items.typed_pair(k, v.clone()))
            .collect(),
        ValueView::Range(a, b) => {
            let end = b.min(a + MAX_RANGE_EXPAND);
            (a..=end).map(Value::int).collect()
        }
        ValueView::RangeExcl(a, b) => {
            let end = b.min(a + MAX_RANGE_EXPAND);
            (a..end).map(Value::int).collect()
        }
        ValueView::RangeExclStart(a, b) => {
            let start = a + 1;
            let end = b.min(start + MAX_RANGE_EXPAND);
            (start..=end).map(Value::int).collect()
        }
        ValueView::RangeExclBoth(a, b) => {
            let start = a + 1;
            let end = b.min(start + MAX_RANGE_EXPAND);
            (start..end).map(Value::int).collect()
        }
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
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
            if let (ValueView::Str(a), ValueView::Str(b)) =
                (start.as_ref().view(), end.as_ref().view())
            {
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
                        crate::runtime::Interpreter::string_succ(&a)
                    } else {
                        a.to_string()
                    };
                    let limit = MAX_RANGE_EXPAND as usize;
                    while current.as_str() <= b.as_str() && result.len() < limit {
                        if excl_end && current.as_str() == b.as_str() {
                            break;
                        }
                        result.push(Value::str(current.clone()));
                        current = crate::runtime::Interpreter::string_succ(&current);
                    }
                    result
                }
            } else if let (ValueView::Str(a), ValueView::HyperWhatever | ValueView::Whatever) =
                (start.as_ref().view(), end.as_ref().view())
            {
                let mut result = Vec::new();
                let mut current = if excl_start {
                    crate::runtime::Interpreter::string_succ(&a)
                } else {
                    a.to_string()
                };
                let limit = MAX_RANGE_EXPAND as usize;
                while result.len() < limit {
                    result.push(Value::str(current.clone()));
                    current = crate::runtime::Interpreter::string_succ(&current);
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
                    crate::runtime::Interpreter::string_succ(&a)
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
                    current = crate::runtime::Interpreter::string_succ(&current);
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
                            use crate::builtins::methods_0arg::temporal::{
                                civil_to_epoch_days, date_attrs, epoch_days_to_civil,
                                make_date_with_formatter,
                            };
                            let (sy, sm, sd) = if let ValueView::Instance { attributes, .. } =
                                start.as_ref().view()
                            {
                                date_attrs(&(attributes).as_map())
                            } else {
                                unreachable!()
                            };
                            let (ey, em, ed) = if let ValueView::Instance { attributes, .. } =
                                end.as_ref().view()
                            {
                                date_attrs(&(attributes).as_map())
                            } else {
                                unreachable!()
                            };
                            let start_days = civil_to_epoch_days(sy, sm, sd);
                            let end_days = civil_to_epoch_days(ey, em, ed);
                            let formatter = if let ValueView::Instance { attributes, .. } =
                                start.as_ref().view()
                            {
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
                        vec![start_num.clone(); limit]
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
        ValueView::Set(items, _) => items
            .iter()
            .map(|s| Value::pair(s.clone(), Value::TRUE))
            .collect(),
        ValueView::Bag(items, _) => items
            .iter()
            .map(|(k, v)| Value::pair(k.clone(), Value::from_bigint(v.clone())))
            .collect(),
        ValueView::Mix(items, _) => items
            .iter()
            .map(|(k, v)| Value::pair(k.clone(), crate::value::mix_weight_to_value(*v)))
            .collect(),
        ValueView::Slip(items) => items.to_vec(),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => {
            // A WalkList flattens to its candidate closures in list context, so
            // `my @cands = $x.WALK(...)` yields the per-level candidates.
            if class_name.resolve() == "WalkList"
                && let Some(items) = walk_list_candidates(&attributes)
            {
                return items;
            }
            // Backtrace is Positional: list context yields its frames
            // (`$!.backtrace.any`, `$bt>>.file`, `for $e.backtrace {...}`).
            if class_name.resolve() == "Backtrace"
                && let Some(frames) = attributes.as_map().get("frames")
            {
                return value_to_list(frames);
            }
            if let Some(ValueView::Array(items, ..)) =
                attributes.as_map().get("__array_items").map(Value::view)
            {
                return items.to_vec();
            }
            // An `is Array` subclass instance is Positional: in list context it
            // flattens to its backing storage elements (`for @$vec { ... }`).
            if let Some(storage) = attributes.as_map().get("__mutsu_array_storage") {
                return value_to_list(storage);
            }
            vec![val.clone()]
        }
        // Nil is a single scalar item in list context (e.g. `for Nil { }` does
        // one iteration); it is not an empty list. Fall through to the scalar arm.
        _ => vec![val.clone()],
    }
}

/// True when a subscript range dimension has no usable finite end: `1..*` /
/// `1^..Inf` style (a Whatever end lowers to `Inf`, and an integer-range end of
/// `i64::MAX` is the same thing forced through an int range). Expanding such a
/// range eagerly would allocate ~2^63 elements; a subscript instead clamps it
/// to the axis length (see `expand_unbounded_range_dim`).
pub(crate) fn subscript_range_end_unbounded(dim: &Value) -> bool {
    match dim.view() {
        ValueView::Range(_, b)
        | ValueView::RangeExcl(_, b)
        | ValueView::RangeExclStart(_, b)
        | ValueView::RangeExclBoth(_, b) => b == i64::MAX,
        ValueView::GenericRange { end, .. } => match end.as_ref().view() {
            ValueView::Whatever => true,
            ValueView::Int(i) => i == i64::MAX,
            ValueView::Num(f) => f.is_infinite() && f.is_sign_positive(),
            ValueView::Rat(n, 0) | ValueView::FatRat(n, 0) => n > 0,
            _ => false,
        },
        _ => false,
    }
}

/// Expand an unbounded-end subscript range dimension against a known axis
/// length: `@a[1^..*;1]` selects rows 2..len-1 at that level. Only
/// unbounded-end ranges are handled — a bounded range keeps the generic
/// expansion (preserving its out-of-bounds Nil semantics). Returns None for
/// non-ranges, bounded ranges, or a non-integer start.
pub(crate) fn expand_unbounded_range_dim(dim: &Value, len: usize) -> Option<Vec<Value>> {
    if !subscript_range_end_unbounded(dim) {
        return None;
    }
    let (start, excl_start) = match dim.view() {
        ValueView::Range(a, _) | ValueView::RangeExcl(a, _) => (a, false),
        ValueView::RangeExclStart(a, _) | ValueView::RangeExclBoth(a, _) => (a, true),
        ValueView::GenericRange {
            start, excl_start, ..
        } => match start.as_ref().view() {
            ValueView::Int(i) => (i, excl_start),
            ValueView::Num(f) if f.fract() == 0.0 => (f as i64, excl_start),
            ValueView::Rat(n, d) if d != 0 && n % d == 0 => (n / d, excl_start),
            _ => return None,
        },
        _ => return None,
    };
    let start = if excl_start { start + 1 } else { start };
    let start = start.max(0);
    if len == 0 || start >= len as i64 {
        return Some(Vec::new());
    }
    Some((start..len as i64).map(Value::int).collect())
}
