/// Range and collection selection methods: head, tail, pick, roll, pickpairs,
/// grab, grabpairs, first, min, max, excludes-min, excludes-max, bounds,
/// is-int, minmax, infinite, of, keyof
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

use super::raku_repr::hash_pick_item;
use super::{is_infinite_range, sample_weighted_bag_key, sample_weighted_mix_key};

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
