use super::*;

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
/// (`Value::Pair`, excluded from positional arity everywhere in dispatch) from a
/// positional pair *value* (`Value::ValuePair`). Iterating a Hash yields
/// `Value::Pair` elements, so passing such an element straight to a block (e.g.
/// `%h.first({ .value > 1 })` / `%h.sort({ ... })`) would bind it as a *named*
/// argument, leaving the block with zero positionals ("expected N got 0"). Map
/// `Value::Pair` to `Value::ValuePair` so the element binds as `$_`/`$^a`.
pub(crate) fn pair_as_positional(val: &Value) -> Value {
    match val {
        Value::Pair(k, v) => Value::ValuePair(Box::new(Value::str(k.clone())), v.clone()),
        other => other.clone(),
    }
}

/// Extract the candidate closures from a `WalkList` instance's attributes,
/// honoring its `reversed` flag. Returns `None` if the attributes are not
/// shaped like a WalkList.
pub(crate) fn walk_list_candidates(attributes: &crate::value::InstanceAttrs) -> Option<Vec<Value>> {
    let map = attributes.as_map();
    let Some(Value::Array(items, ..)) = map.get("candidates") else {
        return None;
    };
    let mut cands = items.to_vec();
    if matches!(map.get("reversed"), Some(Value::Bool(true))) {
        cands.reverse();
    }
    Some(cands)
}

pub(crate) fn value_to_list(val: &Value) -> Vec<Value> {
    match val {
        Value::Array(items, kind) if kind.is_itemized() => vec![val.clone()],
        Value::Array(items, ..) => items.to_vec(),
        Value::Seq(items) | Value::HyperSeq(items) | Value::RaceSeq(items) => items.to_vec(),
        Value::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
        // An itemized hash (`item %h` / `$(%h)`) is a single list element and does
        // NOT flatten to its pairs (mirrors the itemized-Array arm above).
        Value::Hash(items) if items.itemized => vec![val.clone()],
        // `typed_pair` decontainerizes element cells so the pair value matches a
        // `%h<k>` read / `.values` (see t/bind-hash-value-pairs.t).
        Value::Hash(items) => items
            .iter()
            .map(|(k, v)| items.typed_pair(k, v.clone()))
            .collect(),
        Value::Range(a, b) => {
            let end = (*b).min(*a + MAX_RANGE_EXPAND);
            (*a..=end).map(Value::Int).collect()
        }
        Value::RangeExcl(a, b) => {
            let end = (*b).min(*a + MAX_RANGE_EXPAND);
            (*a..end).map(Value::Int).collect()
        }
        Value::RangeExclStart(a, b) => {
            let start = *a + 1;
            let end = (*b).min(start + MAX_RANGE_EXPAND);
            (start..=end).map(Value::Int).collect()
        }
        Value::RangeExclBoth(a, b) => {
            let start = *a + 1;
            let end = (*b).min(start + MAX_RANGE_EXPAND);
            (start..end).map(Value::Int).collect()
        }
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let next_numeric = |v: &Value| -> Option<Value> {
                match v {
                    Value::Int(i) => Some(Value::Int(i + 1)),
                    Value::BigInt(n) => Some(Value::bigint(n.as_ref() + 1)),
                    Value::Num(f) => Some(Value::Num(*f + 1.0)),
                    Value::Rat(n, d) => Some(crate::value::make_rat(n + d, *d)),
                    Value::FatRat(n, d) => Some(Value::FatRat(n + d, *d)),
                    Value::BigRat(n, d) => {
                        Some(Value::bigrat(n.as_ref() + d.as_ref(), (**d).clone()))
                    }
                    other if other.is_numeric() => Some(Value::Num(other.to_f64() + 1.0)),
                    _ => None,
                }
            };
            // String ranges: expand as character sequences
            if let (Value::Str(a), Value::Str(b)) = (start.as_ref(), end.as_ref()) {
                if a.chars().count() == 1 && b.chars().count() == 1 {
                    let s = a.chars().next().unwrap() as u32;
                    let e = b.chars().next().unwrap() as u32;
                    let s = if *excl_start { s + 1 } else { s };
                    if *excl_end {
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
                        (split_numeric_core(a), split_numeric_core(b))
                        && ap == bp
                        && asuf == bsuf
                        && let (Ok(mut n), Ok(e)) = (an.parse::<i128>(), bn.parse::<i128>())
                    {
                        if *excl_start {
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
                            if *excl_end && n == e {
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
                    let mut current = if *excl_start {
                        crate::runtime::Interpreter::string_succ(a)
                    } else {
                        a.to_string()
                    };
                    let limit = MAX_RANGE_EXPAND as usize;
                    while current.as_str() <= b.as_str() && result.len() < limit {
                        if *excl_end && current.as_str() == b.as_str() {
                            break;
                        }
                        result.push(Value::str(current.clone()));
                        current = crate::runtime::Interpreter::string_succ(&current);
                    }
                    result
                }
            } else if let (Value::Str(a), Value::HyperWhatever | Value::Whatever) =
                (start.as_ref(), end.as_ref())
            {
                let mut result = Vec::new();
                let mut current = if *excl_start {
                    crate::runtime::Interpreter::string_succ(a)
                } else {
                    a.to_string()
                };
                let limit = MAX_RANGE_EXPAND as usize;
                while result.len() < limit {
                    result.push(Value::str(current.clone()));
                    current = crate::runtime::Interpreter::string_succ(&current);
                }
                result
            } else if let Value::Str(a) = start.as_ref() {
                // Start is a Str — iterate as strings (preserving type).
                // In Raku, "1"..9 produces ("1", "2", ..., "9").
                // When end is numeric, compare numerically to determine bounds.
                let end_is_numeric = end.as_ref().is_numeric()
                    || matches!(end.as_ref(), Value::Whatever | Value::HyperWhatever);
                let end_str = match end.as_ref() {
                    Value::Str(s) => (**s).clone(),
                    other => other.to_string_value(),
                };
                let end_f64 = end.as_ref().to_f64();
                let mut result = Vec::new();
                let mut current = if *excl_start {
                    crate::runtime::Interpreter::string_succ(a)
                } else {
                    a.to_string()
                };
                let limit = MAX_RANGE_EXPAND as usize;
                while result.len() < limit {
                    let in_range = if end_is_numeric {
                        // Compare current string numerically against end
                        let cur_numeric = coerce_to_numeric(Value::str(current.clone()));
                        let cur_f64 = cur_numeric.to_f64();
                        if *excl_end {
                            cur_f64 < end_f64
                        } else {
                            cur_f64 <= end_f64
                        }
                    } else {
                        // String comparison
                        if *excl_end {
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
                let end_num = if matches!(end.as_ref(), Value::Whatever | Value::HyperWhatever) {
                    Some(Value::Num(f64::INFINITY))
                } else if end.is_numeric() {
                    Some(end.as_ref().clone())
                } else if let Value::Str(s) = end.as_ref() {
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
                            if let Value::Instance { attributes, .. } = v {
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
                            let (sy, sm, sd) =
                                if let Value::Instance { attributes, .. } = start.as_ref() {
                                    date_attrs(&(attributes).as_map())
                                } else {
                                    unreachable!()
                                };
                            let (ey, em, ed) =
                                if let Value::Instance { attributes, .. } = end.as_ref() {
                                    date_attrs(&(attributes).as_map())
                                } else {
                                    unreachable!()
                                };
                            let start_days = civil_to_epoch_days(sy, sm, sd);
                            let end_days = civil_to_epoch_days(ey, em, ed);
                            let formatter =
                                if let Value::Instance { attributes, .. } = start.as_ref() {
                                    attributes.as_map().get("formatter").cloned()
                                } else {
                                    None
                                };
                            let mut result = Vec::new();
                            let first_day = if *excl_start {
                                start_days + 1
                            } else {
                                start_days
                            };
                            let limit = MAX_RANGE_EXPAND as usize;
                            let mut d = first_day;
                            while result.len() < limit {
                                if d > end_days || (*excl_end && d == end_days) {
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
                if start_num.to_f64().is_infinite()
                    || start_num.to_f64().is_nan()
                    || end_num.to_f64().is_nan()
                {
                    vec![val.clone()]
                } else {
                    let mut result = Vec::new();
                    let mut current = if *excl_start {
                        next_numeric(&start_num).unwrap_or(start_num)
                    } else {
                        start_num
                    };
                    let limit = MAX_RANGE_EXPAND as usize;
                    while result.len() < limit {
                        let cmp = compare_values(&current, &end_num);
                        if cmp > 0 || (*excl_end && cmp == 0) {
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
        Value::Set(items, _) => items
            .iter()
            .map(|s| Value::Pair(s.clone(), Box::new(Value::Bool(true))))
            .collect(),
        Value::Bag(items, _) => items
            .iter()
            .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::from_bigint(v.clone()))))
            .collect(),
        Value::Mix(items, _) => items
            .iter()
            .map(|(k, v)| Value::Pair(k.clone(), Box::new(crate::value::mix_weight_to_value(*v))))
            .collect(),
        Value::Slip(items) => items.to_vec(),
        Value::Instance {
            class_name,
            attributes,
            ..
        } => {
            // A WalkList flattens to its candidate closures in list context, so
            // `my @cands = $x.WALK(...)` yields the per-level candidates.
            if class_name.resolve() == "WalkList"
                && let Some(items) = walk_list_candidates(attributes)
            {
                return items;
            }
            if let Some(Value::Array(items, ..)) = attributes.as_map().get("__array_items") {
                return items.to_vec();
            }
            vec![val.clone()]
        }
        // Nil is a single scalar item in list context (e.g. `for Nil { }` does
        // one iteration); it is not an empty list. Fall through to the scalar arm.
        other => vec![other.clone()],
    }
}
