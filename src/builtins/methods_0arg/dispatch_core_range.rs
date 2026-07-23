/// Range and collection selection methods: head, tail, pick, roll, pickpairs,
/// grab, grabpairs, first, min, max, excludes-min, excludes-max, bounds,
/// is-int, minmax, infinite, of, keyof
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

use super::{is_infinite_range, sample_weighted_bag_key, sample_weighted_mix_key};

/// Efficiently sample one random element from a Range without enumerating all elements.
/// Uses raw u64 entropy for full bit coverage on large ranges.
fn sample_one_from_range(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Range(start, end) => {
            if end < start {
                Some(Value::NIL)
            } else {
                Some(range_pick_one_i64(start, end))
            }
        }
        ValueView::RangeExcl(start, end) => {
            let hi = end.saturating_sub(1);
            if start > hi {
                Some(Value::NIL)
            } else {
                Some(range_pick_one_i64(start, hi))
            }
        }
        ValueView::RangeExclStart(start, end) => {
            let lo = start.saturating_add(1);
            if lo > end {
                Some(Value::NIL)
            } else {
                Some(range_pick_one_i64(lo, end))
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
            if let Some(result) = generic_range_pick_one(start, end, excl_start, excl_end) {
                return Some(result);
            }
            // Float endpoints
            if let (Some(s), Some(e)) =
                (runtime::to_float_value(start), runtime::to_float_value(end))
                && s.is_finite()
                && e.is_finite()
            {
                let lo = if excl_start { s + 1.0 } else { s };
                let hi = if excl_end { e - 1.0 } else { e };
                if lo > hi {
                    return Some(Value::NIL);
                }
                let span = hi - lo + 1.0;
                let idx = (crate::builtins::rng::builtin_rand() * span) as i64;
                let idx = idx.min((span - 1.0) as i64);
                return Some(Value::num(lo + idx as f64));
            }
            None
        }
        _ => None,
    }
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "head" => Some(match target.view() {
            // User-defined class instances may have a `head` attribute or
            // method — defer to runtime dispatch so the user accessor wins
            // over the list-like fallback.
            ValueView::Instance { .. } => return None,
            // Argless `.head` reads the backing store raw (Rakudo's Array.head
            // candidate): a hole at index 0 yields `Nil`, not the vivified
            // `Any` that iteration (`for @a`, `.head(n)`) would produce.
            ValueView::Array(items, ..) => Some(Ok(if items.hole_at(0) {
                Value::NIL
            } else {
                items.first().cloned().unwrap_or(Value::NIL)
            })),
            ValueView::Range(start, end) => {
                if start > end {
                    Some(Ok(Value::NIL))
                } else {
                    Some(Ok(Value::int(start)))
                }
            }
            ValueView::RangeExcl(start, end) => {
                if start >= end {
                    Some(Ok(Value::NIL))
                } else {
                    Some(Ok(Value::int(start)))
                }
            }
            ValueView::RangeExclBoth(start, end) => {
                if start + 1 >= end {
                    Some(Ok(Value::NIL))
                } else {
                    Some(Ok(Value::int(start + 1)))
                }
            }
            ValueView::RangeExclStart(start, end) => {
                if start >= end {
                    Some(Ok(Value::NIL))
                } else {
                    Some(Ok(Value::int(start + 1)))
                }
            }
            ValueView::GenericRange {
                start, excl_start, ..
            } => {
                let items = runtime::value_to_list(target);
                if items.is_empty() {
                    Some(Ok(Value::NIL))
                } else if excl_start {
                    if let ValueView::Int(n) = start.view() {
                        Some(Ok(Value::int(n + 1)))
                    } else {
                        Some(Ok(items.first().cloned().unwrap_or(Value::NIL)))
                    }
                } else {
                    Some(Ok((**start).clone()))
                }
            }
            _ => {
                let items = runtime::value_to_list(target);
                Some(Ok(items.first().cloned().unwrap_or(Value::NIL)))
            }
        }),
        "tail" => Some(match target.view() {
            // User-defined class instances may have a `tail` attribute or
            // method — defer to runtime dispatch so the user accessor wins
            // over the list-like fallback.
            ValueView::Instance { .. } => return None,
            ValueView::Array(items, ..) => Some(Ok(items.last().cloned().unwrap_or(Value::NIL))),
            _ => {
                let items = runtime::value_to_list(target);
                Some(Ok(items.last().cloned().unwrap_or(Value::NIL)))
            }
        }),
        "pick" => Some(match target.view() {
            ValueView::Mix(_, _) => Some(Err(RuntimeError::new(
                "Cannot call .pick on a Mix (immutable)",
            ))),
            ValueView::Bag(items, _) => {
                Some(Ok(sample_weighted_bag_key(&items).unwrap_or(Value::NIL)))
            }
            ValueView::Set(items, _) => {
                if items.is_empty() {
                    Some(Ok(Value::NIL))
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
            ValueView::Hash(items) => {
                if items.is_empty() {
                    Some(Ok(Value::NIL))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    let (key, value) = items.iter().nth(idx).expect("index in range");
                    // typed_pair reconstructs an object hash's real key object
                    // from its `.WHICH` store key (plain hashes get the plain
                    // `Pair(str_key, v)` as before).
                    Some(Ok(items.typed_pair(key, value.clone())))
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
                    Some(Ok(Value::NIL))
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
            if let ValueView::Mix(items, _) = target.view() {
                return Some(Some(Ok(
                    sample_weighted_mix_key(&items).unwrap_or(Value::NIL)
                )));
            }
            if let ValueView::Bag(items, _) = target.view() {
                return Some(Some(Ok(
                    sample_weighted_bag_key(&items).unwrap_or(Value::NIL)
                )));
            }
            if let ValueView::Set(items, _) = target.view() {
                if items.is_empty() {
                    return Some(Some(Ok(Value::NIL)));
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
                Some(Some(Ok(Value::NIL)))
            } else {
                let mut idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                Some(Some(Ok(items[idx].clone())))
            }
        }
        "pickpairs" => Some(match target.view() {
            ValueView::Bag(items, _) => {
                if items.is_empty() {
                    Some(Ok(Value::NIL))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    let (key, count) = items.iter().nth(idx).expect("index in range");
                    Some(Ok(Value::pair(
                        key.clone(),
                        Value::from_bigint(count.clone()),
                    )))
                }
            }
            ValueView::Set(items, _) => {
                if items.is_empty() {
                    Some(Ok(Value::NIL))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    let key = items.iter().nth(idx).expect("index in range");
                    Some(Ok(Value::pair(key.clone(), Value::TRUE)))
                }
            }
            ValueView::Mix(items, _) => {
                if items.is_empty() {
                    Some(Ok(Value::NIL))
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    let (key, weight) = items.iter().nth(idx).expect("index in range");
                    Some(Ok(Value::pair(
                        key.clone(),
                        crate::value::mix_weight_to_value(*weight),
                    )))
                }
            }
            _ => None,
        }),
        "grab" | "grabpairs" => Some(match target.view() {
            ValueView::Bag(_, false) => Some(Err(RuntimeError::immutable("Bag", method))),
            ValueView::Set(_, false) => Some(Err(RuntimeError::immutable("Set", method))),
            ValueView::Mix(_, false) => Some(Err(RuntimeError::immutable("Mix", method))),
            _ => None,
        }),
        "first" => Some(match target.view() {
            ValueView::Array(items, ..) => Some(Ok(items.first().cloned().unwrap_or(Value::NIL))),
            _ => None,
        }),
        "min" => Some(match target.view() {
            ValueView::Array(items, ..) => Some(Ok(items
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
                .unwrap_or(Value::num(f64::INFINITY)))),
            ValueView::Range(a, _) => Some(Ok(if a == i64::MIN {
                Value::num(f64::NEG_INFINITY)
            } else {
                Value::int(a)
            })),
            ValueView::RangeExcl(a, _) => Some(Ok(if a == i64::MIN {
                Value::num(f64::NEG_INFINITY)
            } else {
                Value::int(a)
            })),
            ValueView::RangeExclStart(a, _) => Some(Ok(if a == i64::MIN {
                Value::num(f64::NEG_INFINITY)
            } else {
                Value::int(a)
            })),
            ValueView::RangeExclBoth(a, _) => Some(Ok(if a == i64::MIN {
                Value::num(f64::NEG_INFINITY)
            } else {
                Value::int(a)
            })),
            ValueView::GenericRange { start, .. } => {
                let s = start.as_ref();
                Some(Ok(match s.view() {
                    ValueView::Whatever | ValueView::HyperWhatever => Value::num(f64::NEG_INFINITY),
                    _ => s.clone(),
                }))
            }
            ValueView::Hash(_) => None,
            ValueView::Package(_) | ValueView::Instance { .. } => None,
            // A Seq/Slip is a materialized list; defer to the interpreter,
            // which re-dispatches its elements as an Array so min/max compute
            // over them. (The `_` arm below would return the Seq itself.)
            ValueView::Seq(..) | ValueView::Slip(..) => None,
            _ => Some(Ok(target.clone())),
        }),
        "max" => Some(match target.view() {
            ValueView::Array(items, ..) => Some(Ok(items
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
                .unwrap_or(Value::num(f64::NEG_INFINITY)))),
            ValueView::Range(_, b) => Some(Ok(if b == i64::MAX {
                Value::num(f64::INFINITY)
            } else {
                Value::int(b)
            })),
            ValueView::RangeExcl(_, b)
            | ValueView::RangeExclStart(_, b)
            | ValueView::RangeExclBoth(_, b) => Some(Ok(if b == i64::MAX {
                Value::num(f64::INFINITY)
            } else {
                Value::int(b)
            })),
            ValueView::GenericRange { end, .. } => {
                let e = end.as_ref();
                Some(Ok(match e.view() {
                    ValueView::Whatever | ValueView::HyperWhatever => Value::num(f64::INFINITY),
                    _ => e.clone(),
                }))
            }
            ValueView::Hash(_) => None,
            ValueView::Package(_) | ValueView::Instance { .. } => None,
            // A Seq/Slip is a materialized list; defer to the interpreter,
            // which re-dispatches its elements as an Array so min/max compute
            // over them. (The `_` arm below would return the Seq itself.)
            ValueView::Seq(..) | ValueView::Slip(..) => None,
            _ => Some(Ok(target.clone())),
        }),
        "excludes-min" => Some(match target.view() {
            ValueView::Range(..) => Some(Ok(Value::FALSE)),
            ValueView::RangeExcl(..) => Some(Ok(Value::FALSE)),
            ValueView::RangeExclStart(..) => Some(Ok(Value::TRUE)),
            ValueView::RangeExclBoth(..) => Some(Ok(Value::TRUE)),
            ValueView::GenericRange { excl_start, .. } => Some(Ok(Value::truth(excl_start))),
            _ => None,
        }),
        "excludes-max" => Some(match target.view() {
            ValueView::Range(..) => Some(Ok(Value::FALSE)),
            ValueView::RangeExcl(..) => Some(Ok(Value::TRUE)),
            ValueView::RangeExclStart(..) => Some(Ok(Value::FALSE)),
            ValueView::RangeExclBoth(..) => Some(Ok(Value::TRUE)),
            ValueView::GenericRange { excl_end, .. } => Some(Ok(Value::truth(excl_end))),
            _ => None,
        }),
        "bounds" => Some(match target.view() {
            ValueView::Range(a, b)
            | ValueView::RangeExcl(a, b)
            | ValueView::RangeExclStart(a, b)
            | ValueView::RangeExclBoth(a, b) => Some(Ok(Value::array(vec![
                if a == i64::MIN {
                    Value::num(f64::NEG_INFINITY)
                } else {
                    Value::int(a)
                },
                if b == i64::MAX {
                    Value::num(f64::INFINITY)
                } else {
                    Value::int(b)
                },
            ]))),
            ValueView::GenericRange { start, end, .. } => {
                let s = match start.as_ref().view() {
                    ValueView::Whatever | ValueView::HyperWhatever => Value::num(f64::NEG_INFINITY),
                    _ => start.as_ref().clone(),
                };
                let e = match end.as_ref().view() {
                    ValueView::Whatever | ValueView::HyperWhatever => Value::num(f64::INFINITY),
                    _ => end.as_ref().clone(),
                };
                Some(Ok(Value::array(vec![s, e])))
            }
            _ => None,
        }),
        "is-int" => Some(match target.view() {
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..) => Some(Ok(Value::TRUE)),
            ValueView::GenericRange { start, end, .. } => {
                let s_int = matches!(
                    start.as_ref().view(),
                    ValueView::Int(_)
                        | ValueView::Bool(_)
                        | ValueView::Whatever
                        | ValueView::HyperWhatever
                );
                let e_int = matches!(
                    end.as_ref().view(),
                    ValueView::Int(_)
                        | ValueView::Bool(_)
                        | ValueView::Whatever
                        | ValueView::HyperWhatever
                );
                Some(Ok(Value::truth(s_int && e_int)))
            }
            _ => None,
        }),
        "minmax" => Some(match target.view() {
            ValueView::Range(a, b) => Some(Ok(Value::array(vec![Value::int(a), Value::int(b)]))),
            ValueView::RangeExcl(a, b) => {
                Some(Ok(Value::array(vec![Value::int(a), Value::int(b - 1)])))
            }
            ValueView::RangeExclStart(a, b) => {
                Some(Ok(Value::array(vec![Value::int(a + 1), Value::int(b)])))
            }
            ValueView::RangeExclBoth(a, b) => {
                Some(Ok(Value::array(vec![Value::int(a + 1), Value::int(b - 1)])))
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s_is_special = matches!(start.as_ref().view(), ValueView::Num(f) if f.is_infinite() || f.is_nan())
                    || matches!(
                        start.as_ref().view(),
                        ValueView::Whatever | ValueView::HyperWhatever
                    );
                let e_is_special = matches!(end.as_ref().view(), ValueView::Num(f) if f.is_infinite() || f.is_nan())
                    || matches!(
                        end.as_ref().view(),
                        ValueView::Whatever | ValueView::HyperWhatever
                    );
                if (excl_start && s_is_special) || (excl_end && e_is_special) {
                    return Some(Some(Err(RuntimeError::new(
                        "Cannot determine minmax with excluded infinite endpoints",
                    ))));
                }
                let min_val = if excl_start {
                    match start.as_ref().view() {
                        ValueView::Int(i) => Value::int(i + 1),
                        _ => start.as_ref().clone(),
                    }
                } else {
                    match start.as_ref().view() {
                        ValueView::Whatever | ValueView::HyperWhatever => {
                            Value::num(f64::NEG_INFINITY)
                        }
                        _ => start.as_ref().clone(),
                    }
                };
                let max_val = if excl_end {
                    match end.as_ref().view() {
                        ValueView::Int(i) => Value::int(i - 1),
                        _ => end.as_ref().clone(),
                    }
                } else {
                    match end.as_ref().view() {
                        ValueView::Whatever | ValueView::HyperWhatever => Value::num(f64::INFINITY),
                        _ => end.as_ref().clone(),
                    }
                };
                Some(Ok(Value::array(vec![min_val, max_val])))
            }
            ValueView::Array(..) | ValueView::Hash(_) => None,
            _ => None,
        }),
        "infinite" => Some(match target.view() {
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => Some(Ok(Value::truth(is_infinite_range(target)))),
            _ => None,
        }),
        "of" => Some(match target.view() {
            ValueView::Hash(_) | ValueView::Array(..) => None,
            ValueView::Package(name) if name.resolve() == "Hash" || name.resolve() == "Array" => {
                Some(Ok(Value::package(Symbol::intern("Mu"))))
            }
            ValueView::Package(_) | ValueView::CustomType(_) => {
                let name = match target.view() {
                    ValueView::Package(name) => name,
                    ValueView::CustomType(c) => c.name,
                    _ => unreachable!(),
                };
                let n = name.resolve();
                if matches!(n.as_ref(), "Bag" | "BagHash")
                    || n.starts_with("Bag[")
                    || n.starts_with("BagHash[")
                {
                    Some(Ok(Value::package(Symbol::intern("UInt"))))
                } else if matches!(n.as_ref(), "Set" | "SetHash")
                    || n.starts_with("Set[")
                    || n.starts_with("SetHash[")
                {
                    Some(Ok(Value::package(Symbol::intern("Bool"))))
                } else if matches!(n.as_ref(), "Mix" | "MixHash")
                    || n.starts_with("Mix[")
                    || n.starts_with("MixHash[")
                {
                    Some(Ok(Value::package(Symbol::intern("Real"))))
                } else {
                    None
                }
            }
            ValueView::Bag(_, _) => Some(Ok(Value::package(Symbol::intern("UInt")))),
            ValueView::Set(_, _) => Some(Ok(Value::package(Symbol::intern("Bool")))),
            ValueView::Mix(_, _) => Some(Ok(Value::package(Symbol::intern("Real")))),
            _ => None,
        }),
        "keyof" => Some(match target.view() {
            ValueView::Bag(_, _) | ValueView::Set(_, _) | ValueView::Mix(_, _) => {
                // Fall through to runtime to check container type metadata
                // for parameterized types (e.g., Mix[Int])
                None
            }
            ValueView::Hash(_) => None,
            ValueView::Package(_) | ValueView::CustomType(_) => {
                let name = match target.view() {
                    ValueView::Package(name) => name,
                    ValueView::CustomType(c) => c.name,
                    _ => unreachable!(),
                };
                let n = name.resolve();
                if let Some(bracket_pos) = n.find('[') {
                    let base = &n[..bracket_pos];
                    if matches!(
                        base,
                        "Bag" | "Set" | "Mix" | "BagHash" | "SetHash" | "MixHash" | "Hash"
                    ) {
                        let param = n[bracket_pos + 1..].trim_end_matches(']');
                        Some(Ok(Value::package(Symbol::intern(param))))
                    } else {
                        None
                    }
                } else if matches!(
                    n.as_ref(),
                    "Bag" | "Set" | "Mix" | "BagHash" | "SetHash" | "MixHash"
                ) {
                    Some(Ok(Value::package(Symbol::intern("Mu"))))
                } else if n == "Hash" {
                    Some(Ok(Value::package(Symbol::intern("Str(Any)"))))
                } else {
                    None
                }
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
                if let Some(constraint) = attributes.as_map().get("__keyof_constraint") {
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
                    Some(Ok(Value::package(Symbol::intern("Mu"))))
                } else {
                    None
                }
            }
            _ => None,
        }),
        _ => None,
    }
}

/// Pick one random element from an i64 range [start, end] (inclusive).
/// Public alias for use from methods_narg roll.
pub(crate) fn range_pick_one_i64_pub(start: i64, end: i64) -> Value {
    range_pick_one_i64(start, end)
}

fn range_pick_one_i64(start: i64, end: i64) -> Value {
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

/// Public alias for generic_range_pick_one.
pub(crate) fn generic_range_pick_one_pub(
    start: &Value,
    end: &Value,
    excl_start: bool,
    excl_end: bool,
) -> Option<Value> {
    generic_range_pick_one(start, end, excl_start, excl_end)
}

/// Pick one random element from a GenericRange.
/// Returns None if the range has non-integer endpoints (fallback needed).
fn generic_range_pick_one(
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
fn random_bigint_in_range(range_size: &num_bigint::BigInt) -> num_bigint::BigInt {
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
