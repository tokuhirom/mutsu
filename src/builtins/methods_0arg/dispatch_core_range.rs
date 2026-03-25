/// Range and collection selection methods: head, tail, pick, roll, pickpairs,
/// grab, grabpairs, first, min, max, excludes-min, excludes-max, bounds,
/// is-int, minmax, infinite, of, keyof
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

use super::raku_repr::hash_pick_item;
use super::{is_infinite_range, sample_weighted_bag_key, sample_weighted_mix_key};

/// Efficiently sample one random element from a Range without enumerating all elements.
fn sample_one_from_range(target: &Value) -> Option<Value> {
    let random_i64 = |lo: i64, hi: i64| -> Value {
        if hi <= lo {
            return Value::Int(lo);
        }
        let span = (hi as i128 - lo as i128 + 1) as f64;
        let mut offset = (crate::builtins::rng::builtin_rand() * span) as i128;
        let max_offset = hi as i128 - lo as i128;
        if offset > max_offset {
            offset = max_offset;
        }
        Value::Int((lo as i128 + offset) as i64)
    };
    match target {
        Value::Range(start, end) => Some(random_i64(*start, *end)),
        Value::RangeExcl(start, end) => {
            if *start >= *end {
                Some(Value::Nil)
            } else {
                Some(random_i64(*start, end.saturating_sub(1)))
            }
        }
        Value::RangeExclStart(start, end) => {
            if *start >= *end {
                Some(Value::Nil)
            } else {
                Some(random_i64(start.saturating_add(1), *end))
            }
        }
        Value::RangeExclBoth(start, end) => {
            if start.saturating_add(1) >= *end {
                Some(Value::Nil)
            } else {
                Some(random_i64(start.saturating_add(1), end.saturating_sub(1)))
            }
        }
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            // Handle integer (Int/BigInt) endpoints efficiently
            if let (Some(s), Some(e)) = (to_bigint_value(start), to_bigint_value(end)) {
                use num_bigint::BigInt as NumBigInt;
                use num_traits::{One, Zero};
                let lo = if *excl_start {
                    &s + NumBigInt::one()
                } else {
                    s
                };
                let hi = if *excl_end { &e - NumBigInt::one() } else { e };
                if lo > hi {
                    return Some(Value::Nil);
                }
                let span = &hi - &lo + NumBigInt::one();
                let rand_val = crate::builtins::rng::builtin_rand();
                use num_traits::ToPrimitive;
                let span_f64 = span.to_f64().unwrap_or(f64::MAX);
                let offset_f64 = rand_val * span_f64;
                let offset = if offset_f64 >= span_f64 {
                    &span - NumBigInt::one()
                } else if offset_f64 < 0.0 || offset_f64.is_nan() {
                    NumBigInt::zero()
                } else {
                    use num_bigint::ToBigInt;
                    (offset_f64 as i128)
                        .to_bigint()
                        .unwrap_or_else(NumBigInt::zero)
                };
                let result = lo + offset;
                // Return Int if it fits
                if let Some(i) = result.to_i64() {
                    return Some(Value::Int(i));
                }
                return Some(Value::BigInt(std::sync::Arc::new(result)));
            }
            // Float endpoints
            if let (Some(s), Some(e)) =
                (runtime::to_float_value(start), runtime::to_float_value(end))
                && s.is_finite()
                && e.is_finite()
            {
                let lo = if *excl_start { s + 1.0 } else { s };
                let hi = if *excl_end { e - 1.0 } else { e };
                if lo > hi {
                    return Some(Value::Nil);
                }
                let span = hi - lo + 1.0;
                let idx = (crate::builtins::rng::builtin_rand() * span) as i64;
                let idx = idx.min((span - 1.0) as i64);
                return Some(Value::Num(lo + idx as f64));
            }
            None
        }
        _ => None,
    }
}

/// Convert a Value to BigInt if it's an integer type.
fn to_bigint_value(v: &Value) -> Option<num_bigint::BigInt> {
    use num_bigint::ToBigInt;
    match v {
        Value::Int(i) => i.to_bigint(),
        Value::BigInt(b) => Some(b.as_ref().clone()),
        _ => None,
    }
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "head" => Some(match target {
            Value::Array(items, ..) => Some(Ok(items.first().cloned().unwrap_or(Value::Nil))),
            Value::Range(start, _)
            | Value::RangeExcl(start, _)
            | Value::RangeExclBoth(start, _) => Some(Ok(Value::Int(*start))),
            Value::RangeExclStart(start, _) => Some(Ok(Value::Int(*start + 1))),
            Value::GenericRange {
                start, excl_start, ..
            } => {
                let s = (**start).clone();
                if *excl_start {
                    if let Value::Int(n) = &s {
                        Some(Ok(Value::Int(n + 1)))
                    } else {
                        let items = runtime::value_to_list(target);
                        Some(Ok(items.first().cloned().unwrap_or(Value::Nil)))
                    }
                } else {
                    Some(Ok(s))
                }
            }
            _ => {
                let items = runtime::value_to_list(target);
                Some(Ok(items.first().cloned().unwrap_or(Value::Nil)))
            }
        }),
        "tail" => Some(match target {
            Value::Array(items, ..) => Some(Ok(items.last().cloned().unwrap_or(Value::Nil))),
            Value::Instance { class_name, .. } if class_name == "Supply" => None,
            _ => {
                let items = runtime::value_to_list(target);
                Some(Ok(items.last().cloned().unwrap_or(Value::Nil)))
            }
        }),
        "pick" => Some(match target {
            Value::Mix(_) => Some(Err(RuntimeError::new(
                "Cannot call .pick on a Mix (immutable)",
            ))),
            Value::Bag(items) => Some(Ok(sample_weighted_bag_key(items).unwrap_or(Value::Nil))),
            Value::Set(items) => {
                if items.is_empty() {
                    Some(Ok(Value::Nil))
                } else {
                    let keys: Vec<&String> = items.iter().collect();
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * keys.len() as f64) as usize;
                    if idx >= keys.len() {
                        idx = keys.len() - 1;
                    }
                    Some(Ok(Value::str(keys[idx].clone())))
                }
            }
            Value::Hash(items) => {
                if items.is_empty() {
                    Some(Ok(Value::Nil))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    let (key, value) = items.iter().nth(idx).expect("index in range");
                    Some(Ok(hash_pick_item(key, value)))
                }
            }
            _ => {
                // Try efficient range sampling first
                if let Some(v) = sample_one_from_range(target) {
                    return Some(Some(Ok(v)));
                }
                let items = if crate::runtime::utils::is_shaped_array(target) {
                    crate::runtime::utils::shaped_array_leaves(target)
                } else {
                    runtime::value_to_list(target)
                };
                if items.is_empty() {
                    Some(Ok(Value::Nil))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    Some(Ok(items[idx].clone()))
                }
            }
        }),
        "roll" => {
            if let Value::Mix(items) = target {
                return Some(Some(Ok(
                    sample_weighted_mix_key(items).unwrap_or(Value::Nil)
                )));
            }
            if let Value::Bag(items) = target {
                return Some(Some(Ok(
                    sample_weighted_bag_key(items).unwrap_or(Value::Nil)
                )));
            }
            if let Value::Set(items) = target {
                if items.is_empty() {
                    return Some(Some(Ok(Value::Nil)));
                }
                let keys: Vec<&String> = items.iter().collect();
                let mut idx = (crate::builtins::rng::builtin_rand() * keys.len() as f64) as usize;
                if idx >= keys.len() {
                    idx = keys.len() - 1;
                }
                return Some(Some(Ok(Value::str(keys[idx].clone()))));
            }
            // Try efficient range sampling first
            if let Some(v) = sample_one_from_range(target) {
                return Some(Some(Ok(v)));
            }
            let items = if crate::runtime::utils::is_shaped_array(target) {
                crate::runtime::utils::shaped_array_leaves(target)
            } else {
                runtime::value_to_list(target)
            };
            if items.is_empty() {
                Some(Some(Ok(Value::Nil)))
            } else {
                let mut idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                Some(Some(Ok(items[idx].clone())))
            }
        }
        "pickpairs" => Some(match target {
            Value::Bag(items) => {
                if items.is_empty() {
                    Some(Ok(Value::Nil))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    let (key, count) = items.iter().nth(idx).expect("index in range");
                    Some(Ok(Value::Pair(key.clone(), Box::new(Value::Int(*count)))))
                }
            }
            Value::Set(items) => {
                if items.is_empty() {
                    Some(Ok(Value::Nil))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    let key = items.iter().nth(idx).expect("index in range");
                    Some(Ok(Value::Pair(key.clone(), Box::new(Value::Bool(true)))))
                }
            }
            Value::Mix(items) => {
                if items.is_empty() {
                    Some(Ok(Value::Nil))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    let (key, weight) = items.iter().nth(idx).expect("index in range");
                    Some(Ok(Value::Pair(key.clone(), Box::new(Value::Num(*weight)))))
                }
            }
            _ => None,
        }),
        "grab" | "grabpairs" => Some(match target {
            Value::Bag(_) => Some(Err(RuntimeError::immutable("Bag", method))),
            Value::Set(_) => Some(Err(RuntimeError::immutable("Set", method))),
            _ => None,
        }),
        "first" => Some(match target {
            Value::Array(items, ..) => Some(Ok(items.first().cloned().unwrap_or(Value::Nil))),
            _ => None,
        }),
        "min" => Some(match target {
            Value::Array(items, ..) => Some(Ok(items
                .iter()
                .filter(|v| crate::runtime::types::value_is_defined(v))
                .cloned()
                .min_by(|a, b| {
                    let cmp = crate::runtime::compare_values(a, b);
                    if cmp < 0 {
                        std::cmp::Ordering::Less
                    } else if cmp > 0 {
                        std::cmp::Ordering::Greater
                    } else {
                        std::cmp::Ordering::Equal
                    }
                })
                .unwrap_or(Value::Num(f64::INFINITY)))),
            Value::Range(a, _) => Some(Ok(if *a == i64::MIN {
                Value::Num(f64::NEG_INFINITY)
            } else {
                Value::Int(*a)
            })),
            Value::RangeExcl(a, _) => Some(Ok(if *a == i64::MIN {
                Value::Num(f64::NEG_INFINITY)
            } else {
                Value::Int(*a)
            })),
            Value::RangeExclStart(a, _) => Some(Ok(if *a == i64::MIN {
                Value::Num(f64::NEG_INFINITY)
            } else {
                Value::Int(*a)
            })),
            Value::RangeExclBoth(a, _) => Some(Ok(if *a == i64::MIN {
                Value::Num(f64::NEG_INFINITY)
            } else {
                Value::Int(*a)
            })),
            Value::GenericRange { start, .. } => {
                let s = start.as_ref();
                Some(Ok(match s {
                    Value::Whatever | Value::HyperWhatever => Value::Num(f64::NEG_INFINITY),
                    _ => s.clone(),
                }))
            }
            Value::Hash(_) => None,
            Value::Package(_) | Value::Instance { .. } => None,
            _ => Some(Ok(target.clone())),
        }),
        "max" => Some(match target {
            Value::Array(items, ..) => Some(Ok(items
                .iter()
                .filter(|v| crate::runtime::types::value_is_defined(v))
                .cloned()
                .max_by(|a, b| {
                    let cmp = crate::runtime::compare_values(a, b);
                    if cmp < 0 {
                        std::cmp::Ordering::Less
                    } else if cmp > 0 {
                        std::cmp::Ordering::Greater
                    } else {
                        std::cmp::Ordering::Equal
                    }
                })
                .unwrap_or(Value::Num(f64::NEG_INFINITY)))),
            Value::Range(_, b) => Some(Ok(if *b == i64::MAX {
                Value::Num(f64::INFINITY)
            } else {
                Value::Int(*b)
            })),
            Value::RangeExcl(_, b) | Value::RangeExclStart(_, b) | Value::RangeExclBoth(_, b) => {
                Some(Ok(if *b == i64::MAX {
                    Value::Num(f64::INFINITY)
                } else {
                    Value::Int(*b)
                }))
            }
            Value::GenericRange { end, .. } => {
                let e = end.as_ref();
                Some(Ok(match e {
                    Value::Whatever | Value::HyperWhatever => Value::Num(f64::INFINITY),
                    _ => e.clone(),
                }))
            }
            Value::Hash(_) => None,
            Value::Package(_) | Value::Instance { .. } => None,
            _ => Some(Ok(target.clone())),
        }),
        "excludes-min" => Some(match target {
            Value::Range(..) => Some(Ok(Value::Bool(false))),
            Value::RangeExcl(..) => Some(Ok(Value::Bool(false))),
            Value::RangeExclStart(..) => Some(Ok(Value::Bool(true))),
            Value::RangeExclBoth(..) => Some(Ok(Value::Bool(true))),
            Value::GenericRange { excl_start, .. } => Some(Ok(Value::Bool(*excl_start))),
            _ => None,
        }),
        "excludes-max" => Some(match target {
            Value::Range(..) => Some(Ok(Value::Bool(false))),
            Value::RangeExcl(..) => Some(Ok(Value::Bool(true))),
            Value::RangeExclStart(..) => Some(Ok(Value::Bool(false))),
            Value::RangeExclBoth(..) => Some(Ok(Value::Bool(true))),
            Value::GenericRange { excl_end, .. } => Some(Ok(Value::Bool(*excl_end))),
            _ => None,
        }),
        "bounds" => Some(match target {
            Value::Range(a, b)
            | Value::RangeExcl(a, b)
            | Value::RangeExclStart(a, b)
            | Value::RangeExclBoth(a, b) => Some(Ok(Value::array(vec![
                if *a == i64::MIN {
                    Value::Num(f64::NEG_INFINITY)
                } else {
                    Value::Int(*a)
                },
                if *b == i64::MAX {
                    Value::Num(f64::INFINITY)
                } else {
                    Value::Int(*b)
                },
            ]))),
            Value::GenericRange { start, end, .. } => {
                let s = match start.as_ref() {
                    Value::Whatever | Value::HyperWhatever => Value::Num(f64::NEG_INFINITY),
                    v => v.clone(),
                };
                let e = match end.as_ref() {
                    Value::Whatever | Value::HyperWhatever => Value::Num(f64::INFINITY),
                    v => v.clone(),
                };
                Some(Ok(Value::array(vec![s, e])))
            }
            _ => None,
        }),
        "is-int" => Some(match target {
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..) => Some(Ok(Value::Bool(true))),
            Value::GenericRange { start, end, .. } => {
                let s_int = matches!(
                    start.as_ref(),
                    Value::Int(_) | Value::Bool(_) | Value::Whatever | Value::HyperWhatever
                );
                let e_int = matches!(
                    end.as_ref(),
                    Value::Int(_) | Value::Bool(_) | Value::Whatever | Value::HyperWhatever
                );
                Some(Ok(Value::Bool(s_int && e_int)))
            }
            _ => None,
        }),
        "minmax" => Some(match target {
            Value::Range(a, b) => Some(Ok(Value::array(vec![Value::Int(*a), Value::Int(*b)]))),
            Value::RangeExcl(a, b) => {
                Some(Ok(Value::array(vec![Value::Int(*a), Value::Int(*b - 1)])))
            }
            Value::RangeExclStart(a, b) => {
                Some(Ok(Value::array(vec![Value::Int(*a + 1), Value::Int(*b)])))
            }
            Value::RangeExclBoth(a, b) => Some(Ok(Value::array(vec![
                Value::Int(*a + 1),
                Value::Int(*b - 1),
            ]))),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s_is_special = matches!(start.as_ref(), Value::Num(f) if f.is_infinite() || f.is_nan())
                    || matches!(start.as_ref(), Value::Whatever | Value::HyperWhatever);
                let e_is_special = matches!(end.as_ref(), Value::Num(f) if f.is_infinite() || f.is_nan())
                    || matches!(end.as_ref(), Value::Whatever | Value::HyperWhatever);
                if (*excl_start && s_is_special) || (*excl_end && e_is_special) {
                    return Some(Some(Err(RuntimeError::new(
                        "Cannot determine minmax with excluded infinite endpoints",
                    ))));
                }
                let min_val = if *excl_start {
                    match start.as_ref() {
                        Value::Int(i) => Value::Int(*i + 1),
                        _ => start.as_ref().clone(),
                    }
                } else {
                    match start.as_ref() {
                        Value::Whatever | Value::HyperWhatever => Value::Num(f64::NEG_INFINITY),
                        _ => start.as_ref().clone(),
                    }
                };
                let max_val = if *excl_end {
                    match end.as_ref() {
                        Value::Int(i) => Value::Int(*i - 1),
                        _ => end.as_ref().clone(),
                    }
                } else {
                    match end.as_ref() {
                        Value::Whatever | Value::HyperWhatever => Value::Num(f64::INFINITY),
                        _ => end.as_ref().clone(),
                    }
                };
                Some(Ok(Value::array(vec![min_val, max_val])))
            }
            Value::Array(..) | Value::Hash(_) => None,
            _ => None,
        }),
        "infinite" => Some(match target {
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => Some(Ok(Value::Bool(is_infinite_range(target)))),
            _ => None,
        }),
        "of" => Some(match target {
            Value::Hash(_) | Value::Array(..) => None,
            Value::Package(name) if name.resolve() == "Hash" || name.resolve() == "Array" => {
                Some(Ok(Value::Package(Symbol::intern("Mu"))))
            }
            Value::Package(name) | Value::CustomType { name, .. } => {
                let n = name.resolve();
                if matches!(n.as_ref(), "Bag" | "BagHash")
                    || n.starts_with("Bag[")
                    || n.starts_with("BagHash[")
                {
                    Some(Ok(Value::Package(Symbol::intern("UInt"))))
                } else if matches!(n.as_ref(), "Set" | "SetHash")
                    || n.starts_with("Set[")
                    || n.starts_with("SetHash[")
                {
                    Some(Ok(Value::Package(Symbol::intern("Bool"))))
                } else if matches!(n.as_ref(), "Mix" | "MixHash")
                    || n.starts_with("Mix[")
                    || n.starts_with("MixHash[")
                {
                    Some(Ok(Value::Package(Symbol::intern("Real"))))
                } else {
                    None
                }
            }
            Value::Bag(_) => Some(Ok(Value::Package(Symbol::intern("UInt")))),
            Value::Set(_) => Some(Ok(Value::Package(Symbol::intern("Bool")))),
            Value::Mix(_) => Some(Ok(Value::Package(Symbol::intern("Real")))),
            _ => None,
        }),
        "keyof" => Some(match target {
            Value::Bag(_) | Value::Set(_) | Value::Mix(_) => {
                Some(Ok(Value::Package(Symbol::intern("Mu"))))
            }
            Value::Hash(_) => None,
            Value::Package(name) | Value::CustomType { name, .. } => {
                let n = name.resolve();
                if let Some(bracket_pos) = n.find('[') {
                    let base = &n[..bracket_pos];
                    if matches!(
                        base,
                        "Bag" | "Set" | "Mix" | "BagHash" | "SetHash" | "MixHash" | "Hash"
                    ) {
                        let param = n[bracket_pos + 1..].trim_end_matches(']');
                        Some(Ok(Value::Package(Symbol::intern(param))))
                    } else {
                        None
                    }
                } else if matches!(
                    n.as_ref(),
                    "Bag" | "Set" | "Mix" | "BagHash" | "SetHash" | "MixHash"
                ) {
                    Some(Ok(Value::Package(Symbol::intern("Mu"))))
                } else if n == "Hash" {
                    Some(Ok(Value::Package(Symbol::intern("Str(Any)"))))
                } else {
                    None
                }
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } => {
                if let Some(constraint) = attributes.get("__keyof_constraint") {
                    return Some(Some(Ok(constraint.clone())));
                }
                let cn = class_name.resolve();
                if cn == "Bag"
                    || cn == "Set"
                    || cn == "Mix"
                    || cn == "BagHash"
                    || cn == "SetHash"
                    || cn == "MixHash"
                {
                    Some(Ok(Value::Package(Symbol::intern("Mu"))))
                } else {
                    None
                }
            }
            _ => None,
        }),
        _ => None,
    }
}
