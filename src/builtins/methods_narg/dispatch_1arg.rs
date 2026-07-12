use super::allomorph::{allomorph_accepts, out_of_range_failure};
use super::base::{
    BaseDigits, f64_to_rat, parse_radix_checked, range_pick_n_fast, rat_base_repeating, rat_to_base,
};
use super::buf::{
    buf_class_name, buf_get_bytes, buf_get_int_items, is_buf_like, make_buf_from_int_items,
    out_of_range_error, range_bounds, read_f32_ne, read_f64_ne, read_int_method_info,
    read_int_value, resolve_buf_index, to_int_val,
};
use super::flatten::{flatten_target, is_hammer_pair, parse_flat_depth};
use super::fmt_contains::{
    contains_value_recursive, fmt_joinable_target, fmt_single_or_pair, pair_key_value,
};
use super::indent::str_indent;
use super::numeric::{
    compute_roots, int_to_subscript, int_to_superscript, sample_weighted_bag_key,
    sample_weighted_mix_key,
};
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, RuntimeError, Value, ValueView};
use num_bigint::BigInt;
use num_traits::{Signed, ToPrimitive, Zero};

pub(crate) fn native_method_1arg(
    target: &Value,
    method_sym: Symbol,
    arg: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    // Scalar containers are transparent for method dispatch (no .VAR at this arity).
    let target = target.descalarize();
    // Instance with __baggy_data__: delegate to the inner Bag/Set for collection methods
    if let ValueView::Instance { attributes, .. } = target.view()
        && let Some(inner) = attributes.as_map().get("__baggy_data__")
        && !matches!(
            method,
            "WHAT" | "WHICH" | "raku" | "gist" | "Str" | "perl" | "isa" | "^name"
        )
    {
        return native_method_1arg(inner, method_sym, arg);
    }
    // Cool numeric coercion: when a Str calls a numeric 1-arg method, coerce to numeric first.
    // Also coerce the arg if it's a Str for numeric methods.
    {
        let numeric_1arg_methods: &[&str] = &[
            "exp", "log", "round", "roots", "unpolar", "base", "polymod", "expmod", "atan2",
        ];
        if numeric_1arg_methods.contains(&method) {
            let coerced_target = if let ValueView::Str(s) = target.view() {
                if let Ok(i) = s.parse::<i64>() {
                    Some(Value::int(i))
                } else if let Ok(f) = s.parse::<f64>() {
                    Some(Value::num(f))
                } else {
                    return None;
                }
            } else {
                None
            };
            let coerced_arg = if let ValueView::Str(s) = arg.view() {
                if let Ok(i) = s.parse::<i64>() {
                    Some(Value::int(i))
                } else if let Ok(f) = s.parse::<f64>() {
                    Some(Value::num(f))
                } else {
                    None
                }
            } else {
                None
            };
            if coerced_target.is_some() || coerced_arg.is_some() {
                let t = coerced_target.as_ref().unwrap_or(target);
                let a = coerced_arg.as_ref().unwrap_or(arg);
                return native_method_1arg(t, method_sym, a);
            }
        }
    }
    match method {
        // ACCEPTS for allomorphic types (IntStr, RatStr, NumStr, ComplexStr)
        "ACCEPTS" if matches!(target.view(), ValueView::Mixin(_, m) if m.contains_key("Str")) => {
            // Instance args need the interpreter to call .Numeric, so return None
            allomorph_accepts(target, arg).map(|result| Ok(Value::truth(result)))
        }
        // ACCEPTS for Set/Bag/Mix types: equality check
        "ACCEPTS" if matches!(target.view(), ValueView::Set(..)) => {
            let result = match (target.view(), arg.view()) {
                (ValueView::Set(set1, _), ValueView::Set(set2, _)) => {
                    set1.len() == set2.len() && set1.iter().all(|k| set2.contains(k))
                }
                _ => false,
            };
            Some(Ok(Value::truth(result)))
        }
        "ACCEPTS" if matches!(target.view(), ValueView::Bag(..)) => {
            let result = match (target.view(), arg.view()) {
                (ValueView::Bag(bag1, _), ValueView::Bag(bag2, _)) => {
                    bag1.len() == bag2.len() && bag1.iter().all(|(k, v)| bag2.get(k) == Some(v))
                }
                _ => false,
            };
            Some(Ok(Value::truth(result)))
        }
        "ACCEPTS" if matches!(target.view(), ValueView::Mix(..)) => {
            let result = match (target.view(), arg.view()) {
                (ValueView::Mix(mix1, _), ValueView::Mix(mix2, _)) => {
                    mix1.len() == mix2.len()
                        && mix1.iter().all(|(k, v)| {
                            mix2.get(k)
                                .copied()
                                .is_some_and(|v2| (v - v2).abs() < f64::EPSILON)
                        })
                }
                _ => false,
            };
            Some(Ok(Value::truth(result)))
        }
        // ACCEPTS for Pair: checks if the argument has the matching key->value
        "ACCEPTS"
            if matches!(
                target.view(),
                ValueView::Pair(..) | ValueView::ValuePair(..)
            ) =>
        {
            let (pk, pv) = match target.view() {
                ValueView::Pair(k, v) => (k.to_string(), v.clone()),
                ValueView::ValuePair(k, v) => (k.to_string_value(), v.clone()),
                _ => unreachable!(),
            };
            let result = match arg.view() {
                ValueView::Bag(data, _) => {
                    let count = data.counts.get(&pk).cloned().unwrap_or_else(BigInt::zero);
                    Value::from_bigint(count) == pv
                }
                ValueView::Mix(data, _) => {
                    let w = data.weights.get(&pk).copied().unwrap_or(0.0);
                    let mv = if w.fract() == 0.0 {
                        Value::int(w as i64)
                    } else {
                        Value::num(w)
                    };
                    mv == pv
                }
                ValueView::Set(data, _) => {
                    let in_set = data.elements.contains(&pk);
                    Value::truth(in_set) == pv
                }
                ValueView::Hash(items) => {
                    let hv = items.get(&pk).cloned().unwrap_or(Value::int(0));
                    hv == pv
                }
                ValueView::Pair(ok, ov) => pk == ok.as_str() && *ov == pv,
                ValueView::ValuePair(ok, ov) => {
                    let tk = match target.view() {
                        ValueView::Pair(k, _) => Value::str(k.to_string()),
                        ValueView::ValuePair(k, _) => k.clone(),
                        _ => unreachable!(),
                    };
                    tk == *ok && *ov == pv
                }
                ValueView::Instance { .. } | ValueView::Package(_) => return None,
                _ => false,
            };
            Some(Ok(Value::truth(result)))
        }
        // ACCEPTS for Range: value ~~ Range containment, Range ~~ Range subset
        "ACCEPTS" if target.is_range() => {
            let result = if arg.is_range() {
                // Range ~~ Range: subset check — delegate to pure_smart_match
                crate::vm::vm_smart_match::pure_smart_match(arg, target).unwrap_or(false)
            } else {
                // Value ~~ Range: containment check
                runtime::Interpreter::value_in_range(arg, target)
            };
            Some(Ok(Value::truth(result)))
        }
        "Str" => {
            // Int.Str(:superscript) and Int.Str(:subscript)
            if let ValueView::Pair(key, val) = arg.view()
                && val.truthy()
            {
                let int_val = match target.view() {
                    ValueView::Int(i) => Some(i),
                    ValueView::BigInt(bi) => bi.to_i64(),
                    ValueView::Num(f) => Some(f as i64),
                    ValueView::Bool(b) => Some(if b { 1 } else { 0 }),
                    _ => {
                        let s = target.to_string_value();
                        s.parse::<i64>().ok()
                    }
                };
                if let Some(n) = int_val {
                    match key.as_str() {
                        "superscript" => {
                            return Some(Ok(Value::str(int_to_superscript(n))));
                        }
                        "subscript" => {
                            return Some(Ok(Value::str(int_to_subscript(n))));
                        }
                        _ => {}
                    }
                }
            }
            // Default: just stringify
            Some(Ok(Value::str(target.to_string_value())))
        }
        "chop" => {
            // Type objects (Package) should throw
            if let ValueView::Package(type_name) = target.view() {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let s = target.to_string_value();
            let n = match arg.view() {
                ValueView::Int(i) => i.max(0) as usize,
                ValueView::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    bi.to_usize().unwrap_or(usize::MAX)
                }
                ValueView::Num(f) => (f as i64).max(0) as usize,
                _ => arg.to_string_value().parse::<usize>().unwrap_or(1),
            };
            let char_count = s.chars().count();
            let keep = char_count.saturating_sub(n);
            let result: String = s.chars().take(keep).collect();
            Some(Ok(Value::str(result)))
        }
        "uniprop" => {
            let prop_name = arg.to_string_value();
            match target.view() {
                ValueView::Package(_) => {
                    let msg = "Cannot resolve caller uniprop".to_string();
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Multi::NoMatch"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(&msg);
                    err.exception = Some(Box::new(ex));
                    return Some(Err(err));
                }
                ValueView::Int(i) => {
                    let cp = i as u32;
                    return Some(Ok(
                        crate::builtins::uniprop::unicode_property_value_for_codepoint(
                            cp,
                            Some(&prop_name),
                        ),
                    ));
                }
                _ => {}
            }
            let s = target.to_string_value();
            if s.is_empty() {
                return Some(Ok(Value::NIL));
            }
            let ch = s.chars().next().unwrap();
            Some(Ok(crate::builtins::uniprop::unicode_property_value(
                ch, &prop_name,
            )))
        }
        "unimatch" => {
            let prop_value = arg.to_string_value();
            match target.view() {
                ValueView::Package(_) => {
                    let msg = "Cannot resolve caller unimatch".to_string();
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Multi::NoMatch"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(&msg);
                    err.exception = Some(Box::new(ex));
                    return Some(Err(err));
                }
                ValueView::Int(i) => {
                    let cp = i as u32;
                    return Some(Ok(crate::builtins::uniprop::unimatch_for_codepoint(
                        cp,
                        &prop_value,
                        None,
                    )));
                }
                _ => {}
            }
            let s = target.to_string_value();
            if s.is_empty() {
                return Some(Ok(Value::NIL));
            }
            let ch = s.chars().next().unwrap();
            Some(Ok(Value::truth(crate::builtins::uniprop::unimatch(
                ch,
                &prop_value,
                None,
            ))))
        }
        "uniprops" => {
            let prop_name = arg.to_string_value();
            let s = target.to_string_value();
            if s.is_empty() {
                return Some(Ok(Value::array(vec![])));
            }
            let props: Vec<Value> = s
                .chars()
                .map(|ch| crate::builtins::uniprop::unicode_property_value(ch, &prop_name))
                .collect();
            Some(Ok(Value::array(props)))
        }
        "contains" => {
            if let ValueView::Package(type_name) = arg.view() {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller contains({}:U)",
                    type_name,
                ))));
            }
            let s = target.to_string_value();
            Some(Ok(contains_value_recursive(&s, arg)))
        }
        // starts-with / ends-with: the plain `.starts-with($needle)` form (a
        // single positional argument) is a pure prefix/suffix check on a Str
        // receiver, so handle it natively here. The case-/mark-insensitive forms
        // (`:i`/`:ignorecase`/`:m`/`:ignoremark`) carry a second (Pair) argument
        // and so never reach this 1-arg path — they keep falling through to the
        // interpreter's `dispatch_prefix_suffix_check` (runtime/methods_string.rs).
        "starts-with" | "ends-with" if matches!(target.view(), ValueView::Str(_)) => {
            if let ValueView::Package(type_name) = arg.view() {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name
                ))));
            }
            let text = target.to_string_value();
            let needle = arg.to_string_value();
            let ok = if method == "starts-with" {
                text.starts_with(needle.as_str())
            } else {
                text.ends_with(needle.as_str())
            };
            Some(Ok(Value::truth(ok)))
        }
        "samemark" => {
            let target_str = target.to_string_value();
            let source_str = arg.to_string_value();
            Some(Ok(Value::str(crate::builtins::samemark_string(
                &target_str,
                &source_str,
            ))))
        }
        "samecase" => {
            let source_str = target.to_string_value();
            let pattern_str = arg.to_string_value();
            Some(Ok(Value::str(crate::builtins::samecase_string(
                &source_str,
                &pattern_str,
            ))))
        }
        "decode" => {
            let encoding = arg.to_string_value();
            crate::builtins::decode_buf_method(target, Some(&encoding))
        }
        "Rat" => {
            // .Rat(epsilon) — use continued fraction algorithm with given epsilon
            let epsilon = match arg.view() {
                ValueView::Num(f) => f,
                ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
                ValueView::Int(i) => i as f64,
                _ => 1e-6,
            };
            let result = match target.view() {
                ValueView::Rat(_, _) => target.clone(),
                ValueView::Int(i) => Value::rat_raw(i, 1),
                ValueView::Num(f) => {
                    if f.is_nan() {
                        Value::rat_raw(0, 0)
                    } else if f.is_infinite() {
                        if f.is_sign_positive() {
                            Value::rat_raw(1, 0)
                        } else {
                            Value::rat_raw(-1, 0)
                        }
                    } else {
                        crate::builtins::num_to_rat_with_epsilon(f, epsilon)
                    }
                }
                ValueView::FatRat(n, d) => Value::rat_raw(n, d),
                ValueView::Str(s) => {
                    if let Ok(f) = s.parse::<f64>() {
                        crate::builtins::num_to_rat_with_epsilon(f, epsilon)
                    } else {
                        Value::rat_raw(0, 1)
                    }
                }
                _ => Value::rat_raw(0, 1),
            };
            Some(Ok(result))
        }
        "FatRat" => {
            // .FatRat or .FatRat(epsilon) — convert to FatRat
            let result = match target.view() {
                ValueView::FatRat(_, _) => target.clone(),
                ValueView::Int(i) => Value::fat_rat_raw(i, 1),
                ValueView::Rat(n, d) => Value::fat_rat_raw(n, d),
                ValueView::Num(f) => {
                    let denom = 1_000_000i64;
                    let numer = (f * denom as f64).round() as i64;
                    Value::fat_rat_raw(numer, denom)
                }
                _ => Value::fat_rat_raw(0, 1),
            };
            Some(Ok(result))
        }
        "index" => {
            // Fall through to runtime dispatch for type objects, named args (Pairs),
            // array of needles, and multi-arg calls handled by dispatch_index
            if matches!(
                arg.view(),
                ValueView::Package(_) | ValueView::Pair(..) | ValueView::Array(..)
            ) {
                return None;
            }
            let s = target.to_string_value();
            let needle = arg.to_string_value();
            match s.find(&needle) {
                Some(pos) => {
                    let char_pos = s[..pos].chars().count();
                    Some(Ok(Value::int(char_pos as i64)))
                }
                None => Some(Ok(Value::NIL)),
            }
        }
        "substr" => {
            crate::builtins::substr::native_substr_slice(&target.to_string_value(), arg, None)
        }
        "indent" => {
            let s = target.to_string_value();
            let (result, warning) = str_indent(&s, arg);
            if let Some(warn_msg) = warning {
                return Some(Err(crate::value::RuntimeError::warn_signal_with_resume(
                    warn_msg,
                    Value::str(result),
                )));
            }
            Some(Ok(Value::str(result)))
        }
        "AT-POS" => {
            let idx = match arg.view() {
                ValueView::Int(i) if i >= 0 => i as usize,
                ValueView::Num(f) if f >= 0.0 => f as usize,
                _ => return Some(Ok(Value::NIL)),
            };
            if let Some(items) = target.as_list_items() {
                Some(Ok(items.get(idx).cloned().unwrap_or(Value::NIL)))
            } else {
                match target.view() {
                    ValueView::Str(s) => {
                        let ch = s.chars().nth(idx).map(|c| Value::str(c.to_string()));
                        Some(Ok(ch.unwrap_or(Value::NIL)))
                    }
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "Match" => {
                        if let Some(ValueView::Array(positional, ..)) =
                            attributes.as_map().get("list").map(Value::view)
                        {
                            return Some(Ok(positional.get(idx).cloned().unwrap_or(Value::NIL)));
                        }
                        Some(Ok(Value::NIL))
                    }
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                        if let Some(ValueView::Array(bytes, ..)) =
                            attributes.as_map().get("bytes").map(Value::view)
                        {
                            return Some(Ok(bytes.get(idx).cloned().unwrap_or(Value::int(0))));
                        }
                        Some(Ok(Value::int(0)))
                    }
                    _ => None,
                }
            }
        }
        "split" => {
            if let ValueView::Instance { class_name, .. } = target.view()
                && (class_name == "Supply"
                    || class_name == "IO::Handle"
                    || class_name == "IO::Pipe"
                    || class_name == "IO::CatHandle")
            {
                return None;
            }
            // IO::Spec::* has its own split method
            if let ValueView::Package(name) = target.view()
                && name.resolve().starts_with("IO::Spec")
            {
                return None;
            }
            crate::builtins::split::native_split_method(target, std::slice::from_ref(arg))
        }
        "comb" => {
            // Supply/IO targets have their own comb semantics in the interpreter.
            if let ValueView::Instance { class_name, .. } = target.view()
                && (class_name == "Supply"
                    || class_name == "IO::Handle"
                    || class_name == "IO::Path"
                    || class_name == "IO::Pipe"
                    || class_name == "IO::CatHandle")
            {
                return None;
            }
            // Pure Int-chunk / Str-fixed split (shared with the interpreter via
            // builtins::comb); Regex/Sub/bare matchers return None -> interpreter.
            crate::builtins::comb::native_comb_method(target, std::slice::from_ref(arg))
        }
        "lines" => {
            if let ValueView::Instance { class_name, .. } = target.view()
                && class_name == "Supply"
            {
                return None;
            }
            let s = target.to_string_value();
            if let ValueView::Pair(key, value) = arg.view() {
                if key == "chomp" {
                    let lines: Vec<Value> =
                        crate::builtins::split_lines_with_chomp(&s, value.truthy())
                            .into_iter()
                            .map(Value::str)
                            .collect();
                    return Some(Ok(Value::seq(lines)));
                }
                return None;
            }

            let mut lines = crate::builtins::split_lines_chomped(&s);
            let limit = match arg.view() {
                ValueView::Int(i) => Some(i.max(0) as usize),
                ValueView::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    Some(bi.to_usize().unwrap_or(usize::MAX))
                }
                ValueView::Whatever => None,
                ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                ValueView::Num(f) if f >= 0.0 => Some(f as usize),
                ValueView::Rat(n, d) if d == 0 && n > 0 => None,
                _ => return None,
            };
            if let Some(n) = limit {
                lines.truncate(n);
            }
            let lines: Vec<Value> = lines.into_iter().map(Value::str).collect();
            Some(Ok(Value::seq(lines)))
        }
        "words" => {
            let s = target.to_string_value();
            let limit = match arg.view() {
                ValueView::Int(i) => Some(i.max(0) as usize),
                ValueView::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    Some(bi.to_usize().unwrap_or(usize::MAX))
                }
                ValueView::Whatever => None,
                ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                ValueView::Num(f) if f >= 0.0 => Some(f as usize),
                ValueView::Rat(n, d) if d == 0 && n > 0 => None,
                _ => return None,
            };
            let mut words: Vec<Value> = s
                .split_whitespace()
                .map(|w| Value::str(w.to_string()))
                .collect();
            if let Some(n) = limit {
                words.truncate(n);
            }
            Some(Ok(Value::seq(words)))
        }
        "join" => {
            // Shaped arrays: join over leaves
            if crate::runtime::utils::is_shaped_array(target) {
                let leaves = crate::runtime::utils::shaped_array_leaves(target);
                let sep = arg.to_string_value();
                let joined = leaves
                    .iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join(&sep);
                return Some(Ok(Value::str(joined)));
            }
            if let Some(items) = target.as_list_items() {
                // If any item is an Instance, fall through to runtime
                // so user-defined Str() methods can be called. A `ContainerRef`
                // element (grep rw alias / `:=`-bound slot) is decontainerized
                // first so a cell-wrapped Instance is also routed to runtime.
                if items.iter().any(|v| {
                    v.with_deref(|inner| matches!(inner.view(), ValueView::Instance { .. }))
                }) {
                    return None;
                }
                let sep = arg.to_string_value();
                let joined = items
                    .iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join(&sep);
                return Some(Ok(Value::str(joined)));
            }
            match target.view() {
                ValueView::Capture { positional, .. } => {
                    let sep = arg.to_string_value();
                    let joined = positional
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                ValueView::Pair(k, v) => {
                    let sep = arg.to_string_value();
                    Some(Ok(Value::str(format!(
                        "{}{}{}",
                        k,
                        sep,
                        v.to_string_value()
                    ))))
                }
                ValueView::ValuePair(k, v) => {
                    let sep = arg.to_string_value();
                    Some(Ok(Value::str(format!(
                        "{}{}{}",
                        k.to_string_value(),
                        sep,
                        v.to_string_value()
                    ))))
                }
                ValueView::Hash(map) => {
                    let sep = arg.to_string_value();
                    let joined = map
                        .iter()
                        .map(|(k, v)| format!("{}\t{}", k, v.to_string_value()))
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                // Scalar values: .join returns the value as a string
                ValueView::Str(_)
                | ValueView::Int(_)
                | ValueView::Num(_)
                | ValueView::Rat(..)
                | ValueView::Bool(_)
                | ValueView::Instance { .. }
                | ValueView::Nil => Some(Ok(Value::str(target.to_string_value()))),
                // Other types (LazyList, etc.) fall through to the runtime handler
                _ => None,
            }
        }
        "flat" => {
            if is_hammer_pair(arg) {
                return Some(Ok(flatten_target(target, None, true)));
            }
            if let Some(depth) = parse_flat_depth(arg) {
                return Some(Ok(flatten_target(target, Some(depth), false)));
            }
            None
        }
        "head" => {
            let n: i64 = match arg.view() {
                ValueView::Int(i) => i,
                ValueView::Rat(num, den) => {
                    if den == 0 {
                        0
                    } else {
                        num / den
                    }
                }
                ValueView::Num(f) => f as i64,
                ValueView::BigInt(bi) => {
                    // For very large BigInts that don't fit in i64:
                    // negative => treat as negative (returns empty), positive => clamp to MAX
                    bi.to_i64()
                        .unwrap_or(if bi.sign() == num_bigint::Sign::Minus {
                            -1
                        } else {
                            i64::MAX
                        })
                }
                _ => return None,
            };
            if n <= 0 {
                return Some(Ok(Value::seq(vec![])));
            }
            let n = n as usize;
            match target.view() {
                ValueView::Array(items, ..) => {
                    Some(Ok(Value::seq(items[..n.min(items.len())].to_vec())))
                }
                ValueView::Range(a, b) => {
                    let items: Vec<Value> = (a..=b).take(n).map(Value::int).collect();
                    Some(Ok(Value::seq(items)))
                }
                _ => {
                    let items = runtime::value_to_list(target);
                    Some(Ok(Value::seq(items[..n.min(items.len())].to_vec())))
                }
            }
        }
        "tail" => match target.view() {
            ValueView::Array(items, ..) => {
                let n = match arg.view() {
                    ValueView::Int(i) => i as usize,
                    _ => return None,
                };
                let start = items.len().saturating_sub(n);
                Some(Ok(Value::seq(items[start..].to_vec())))
            }
            ValueView::Instance { class_name, .. } if class_name == "Supply" => None,
            _ => {
                let n = match arg.view() {
                    ValueView::Int(i) => i as usize,
                    _ => return None,
                };
                let items = runtime::value_to_list(target);
                let start = items.len().saturating_sub(n);
                Some(Ok(Value::seq(items[start..].to_vec())))
            }
        },
        "combinations" => {
            let items = target
                .as_list_items()
                .map(|items| items.to_vec())
                .unwrap_or_else(|| runtime::value_to_list(target));
            match arg.view() {
                ValueView::Range(a, b) => Some(Ok(Value::seq_arc(
                    crate::builtins::methods_0arg::collection::combinations_range(&items, a, b)
                        .into(),
                ))),
                ValueView::RangeExcl(a, b) => Some(Ok(Value::seq_arc(
                    crate::builtins::methods_0arg::collection::combinations_range(&items, a, b - 1)
                        .into(),
                ))),
                ValueView::RangeExclStart(a, b) => Some(Ok(Value::seq_arc(
                    crate::builtins::methods_0arg::collection::combinations_range(&items, a + 1, b)
                        .into(),
                ))),
                ValueView::RangeExclBoth(a, b) => Some(Ok(Value::seq_arc(
                    crate::builtins::methods_0arg::collection::combinations_range(
                        &items,
                        a + 1,
                        b - 1,
                    )
                    .into(),
                ))),
                ValueView::GenericRange {
                    start,
                    end,
                    excl_start,
                    excl_end,
                } => {
                    let mut lo = runtime::to_int(start);
                    let mut hi = runtime::to_int(end);
                    if excl_start {
                        lo += 1;
                    }
                    if excl_end {
                        hi -= 1;
                    }
                    Some(Ok(Value::seq_arc(
                        crate::builtins::methods_0arg::collection::combinations_range(
                            &items, lo, hi,
                        )
                        .into(),
                    )))
                }
                _ => {
                    let k = runtime::to_int(arg);
                    if k < 0 {
                        Some(Ok(Value::seq_arc(Vec::new().into())))
                    } else {
                        Some(Ok(Value::seq_arc(
                            crate::builtins::methods_0arg::collection::combinations_k(
                                &items, k as usize,
                            )
                            .into(),
                        )))
                    }
                }
            }
        }
        "batch" => {
            // `.batch(N)` and the named `.batch(:elems(N))` are equivalent.
            let n = match arg.view() {
                ValueView::Int(i) => i,
                ValueView::Pair(key, val) if key == "elems" || key == "batch" => {
                    val.to_f64() as i64
                }
                _ => return None,
            };
            if n < 1 {
                let message = format!(
                    "Batching sublist length is out of range. Is: {n}, should be in 1..^Inf"
                );
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "what".to_string(),
                    Value::str_from("Batching sublist length"),
                );
                attrs.insert("got".to_string(), Value::int(n));
                attrs.insert("range".to_string(), Value::str_from("1..^Inf"));
                attrs.insert("message".to_string(), Value::str(message.clone()));
                let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                let mut err = RuntimeError::new(message);
                err.exception = Some(Box::new(ex));
                return Some(Err(err));
            }
            let n = n as usize;
            // A Blob/Buf batches its byte *values* (it is iterated as a list of
            // its bytes), not as a single opaque element.
            let items = match crate::builtins::methods_narg::buf::buf_get_bytes(target) {
                Some(bytes) => bytes.into_iter().map(|b| Value::int(b as i64)).collect(),
                None => runtime::value_to_list(target),
            };
            let batches: Vec<Value> = items
                .chunks(n)
                .map(|chunk| Value::array(chunk.to_vec()))
                .collect();
            Some(Ok(Value::seq(batches)))
        }
        "rindex" => {
            // Fall through to runtime dispatch for arrays (list of needles)
            // and type objects
            if matches!(
                arg.view(),
                ValueView::Array(..) | ValueView::Package(_) | ValueView::Pair(..)
            ) {
                return None;
            }
            let s = target.to_string_value();
            let needle = arg.to_string_value();
            match s.rfind(&needle) {
                Some(pos) => {
                    let char_pos = s[..pos].chars().count();
                    Some(Ok(Value::int(char_pos as i64)))
                }
                None => Some(Ok(Value::NIL)),
            }
        }
        "fmt" => {
            // A Format object argument is handled by the slow-path Format dispatch
            // (arity-aware batching, separators, X::Str::Sprintf::Directives::Count).
            if matches!(arg.view(), ValueView::Instance { class_name, .. } if class_name.resolve() == "Format")
            {
                return None;
            }
            let fmt = arg.to_string_value();
            if let ValueView::Hash(items) = target.view() {
                // Hash.fmt(format): format each key-value pair, join with "\n"
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(&fmt, &[Value::str(k.to_string()), v.clone()])
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                Some(Ok(Value::str(rendered)))
            } else if let ValueView::Bag(items, _) = target.view() {
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(
                            &fmt,
                            &[Value::str(k.clone()), Value::from_bigint(v.clone())],
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                Some(Ok(Value::str(rendered)))
            } else if let ValueView::Set(items, _) = target.view() {
                let rendered = items
                    .iter()
                    .map(|k| {
                        runtime::format_sprintf_args(&fmt, &[Value::str(k.clone()), Value::TRUE])
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                Some(Ok(Value::str(rendered)))
            } else if let ValueView::Mix(items, _) = target.view() {
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(&fmt, &[Value::str(k.clone()), Value::num(*v)])
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                Some(Ok(Value::str(rendered)))
            } else if let Some((k, v)) = pair_key_value(target) {
                // Pair.fmt(format): format key and value
                let rendered = runtime::format_sprintf_args(&fmt, &[k, v]);
                Some(Ok(Value::str(rendered)))
            } else if fmt_joinable_target(target) {
                // Use as_list_items() to bypass itemization — methods like
                // .fmt still iterate over inner elements of $[...] / $(...).
                let items: Vec<Value> = if let Some(inner) = target.as_list_items() {
                    inner.to_vec()
                } else {
                    runtime::value_to_list(target)
                };
                let rendered = items
                    .into_iter()
                    .map(|item| fmt_single_or_pair(&fmt, &item))
                    .collect::<Vec<_>>()
                    .join(" ");
                Some(Ok(Value::str(rendered)))
            } else {
                let rendered = runtime::format_sprintf(&fmt, Some(target));
                Some(Ok(Value::str(rendered)))
            }
        }
        "sprintf" => {
            // Method form: '%f'.sprintf(value) — target is the format string
            let fmt = target.to_string_value();
            let rendered = runtime::format_sprintf(&fmt, Some(arg));
            Some(Ok(Value::str(rendered)))
        }
        "zprintf" => {
            // Method form: '%f'.zprintf(value) — like sprintf but with zprintf semantics
            let fmt = target.to_string_value();
            let rendered = runtime::format_zprintf(&fmt, Some(arg));
            Some(Ok(Value::str(rendered)))
        }
        "parse-base" => {
            let radix = match arg.view() {
                ValueView::Int(n) => n,
                _ => return None,
            };
            let s = target.to_string_value();
            Some(crate::builtins::parse_base::parse_base(&s, radix))
        }
        "base" => match target.view() {
            ValueView::Int(i) => {
                let radix = match arg.view() {
                    ValueView::Int(r) if (2..=36).contains(&r) => r as u32,
                    ValueView::Int(_) => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                    ValueView::Str(s) => match s.parse::<u32>() {
                        Ok(r) if (2..=36).contains(&r) => r,
                        _ => {
                            return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                        }
                    },
                    _ => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                };
                let negative = i < 0;
                let mut n = if negative { (-i) as u64 } else { i as u64 };
                if n == 0 {
                    return Some(Ok(Value::str_from("0")));
                }
                let digits = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
                let mut buf = Vec::new();
                while n > 0 {
                    buf.push(digits[(n % radix as u64) as usize]);
                    n /= radix as u64;
                }
                if negative {
                    buf.push(b'-');
                }
                buf.reverse();
                Some(Ok(Value::str(String::from_utf8(buf).unwrap())))
            }
            ValueView::BigInt(n) => {
                let radix = match arg.view() {
                    ValueView::Int(r) if (2..=36).contains(&r) => r as u32,
                    ValueView::Int(_) => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                    ValueView::Str(s) => match s.parse::<u32>() {
                        Ok(r) if (2..=36).contains(&r) => r,
                        _ => {
                            return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                        }
                    },
                    _ => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                };
                use num_traits::{Signed, Zero};
                let negative = n.is_negative();
                let mut val = if negative {
                    -n.as_ref().clone()
                } else {
                    n.as_ref().clone()
                };
                if val.is_zero() {
                    return Some(Ok(Value::str_from("0")));
                }
                let digits = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
                let radix_big = num_bigint::BigInt::from(radix);
                let mut buf = Vec::new();
                while !val.is_zero() {
                    let rem = &val % &radix_big;
                    use num_traits::ToPrimitive;
                    let digit_idx = rem.to_usize().unwrap_or(0);
                    buf.push(digits[digit_idx]);
                    val /= &radix_big;
                }
                if negative {
                    buf.push(b'-');
                }
                buf.reverse();
                Some(Ok(Value::str(String::from_utf8(buf).unwrap())))
            }
            ValueView::Num(f) => {
                if f.is_infinite() || f.is_nan() {
                    return Some(Err(RuntimeError::new(format!(
                        "X::Numeric::CannotConvert: Cannot convert {} to base",
                        if f.is_nan() {
                            "NaN"
                        } else if f > 0.0 {
                            "Inf"
                        } else {
                            "-Inf"
                        },
                    ))));
                }
                let radix = match arg.view() {
                    ValueView::Int(r) if (2..=36).contains(&r) => r as u32,
                    ValueView::Int(_) => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                    ValueView::Str(s) => match s.parse::<u32>() {
                        Ok(r) if (2..=36).contains(&r) => r,
                        _ => {
                            return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                        }
                    },
                    _ => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                };
                // Convert Num to Rat for precise base conversion
                let (n, d) = f64_to_rat(f);
                Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
            }
            ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
                let radix = match parse_radix_checked(arg)? {
                    Ok(r) => r,
                    Err(_) => return Some(Ok(out_of_range_failure("base requires radix 2..36"))),
                };
                Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
            }
            // Handle Instance types (Duration, Instant, etc.) by
            // extracting their numeric value
            ValueView::Instance { attributes, .. } => {
                let radix = match parse_radix_checked(arg)? {
                    Ok(r) => r,
                    Err(_) => return Some(Ok(out_of_range_failure("base requires radix 2..36"))),
                };
                if let Some(val) = attributes.as_map().get("value") {
                    match val.view() {
                        ValueView::Int(i) => {
                            Some(Ok(Value::str(rat_to_base(i, 1, radix, BaseDigits::Auto))))
                        }
                        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
                            Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
                        }
                        ValueView::Num(f) => {
                            let (n, d) = f64_to_rat(f);
                            Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
        "base-repeating" => {
            let radix = match parse_radix_checked(arg)? {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };
            let (n, d) = match target.view() {
                ValueView::Int(i) => (i, 1i64),
                ValueView::Rat(n, d) => (n, d),
                ValueView::Num(f) => f64_to_rat(f),
                _ => return None,
            };
            let (non_repeating, repeating) = rat_base_repeating(n, d, radix);
            Some(Ok(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                    Value::str(non_repeating),
                    Value::str(repeating),
                ])),
                ArrayKind::List,
            )))
        }
        "round" => {
            // Unwrap allomorphic types (IntStr, NumStr, RatStr, ComplexStr)
            // to get the underlying numeric value for the scale
            let unwrapped_arg = if let ValueView::Mixin(inner, _) = arg.view() {
                inner.as_ref()
            } else {
                arg
            };
            // Determine the scale type category for return type selection
            // Int/IntStr -> Int, Num/NumStr/Complex/ComplexStr -> Num,
            // Rat/RatStr -> Rat
            #[derive(Clone, Copy)]
            enum RoundResult {
                Int,
                Num,
                Rat,
            }
            let scale_type = match unwrapped_arg.view() {
                ValueView::Int(_) | ValueView::BigInt(_) => RoundResult::Int,
                ValueView::Num(_) => RoundResult::Num,
                ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _) => {
                    RoundResult::Rat
                }
                ValueView::Complex(_, _) => RoundResult::Num,
                _ => return None,
            };
            let scale = match unwrapped_arg.view() {
                ValueView::Int(i) => i as f64,
                ValueView::Num(f) => f,
                ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
                ValueView::FatRat(n, d) if d != 0 => n as f64 / d as f64,
                ValueView::Complex(re, _) => re,
                _ => return None,
            };
            fn raku_round(v: f64) -> f64 {
                (v + 0.5).floor()
            }
            fn round_real(x: f64, scale: f64) -> f64 {
                if scale == 0.0 {
                    raku_round(x)
                } else {
                    raku_round(x / scale) * scale
                }
            }
            // Unwrap target allomorphic types too
            let unwrapped_target = if let ValueView::Mixin(inner, _) = target.view() {
                inner.as_ref()
            } else {
                target
            };
            // Handle Complex target separately — always returns Complex
            if let ValueView::Complex(re, im) = unwrapped_target.view() {
                let rr = round_real(re, scale);
                let ri = round_real(im, scale);
                return Some(Ok(Value::complex(rr, ri)));
            }
            let x = match unwrapped_target.view() {
                ValueView::Int(i) => i as f64,
                ValueView::BigInt(bi) => bi.to_f64().unwrap_or(0.0),
                ValueView::Num(f) => f,
                ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
                ValueView::FatRat(n, d) if d != 0 => n as f64 / d as f64,
                _ => return None,
            };
            let result = round_real(x, scale);
            // Return type depends on the scale type
            match scale_type {
                RoundResult::Int => {
                    let r = result.floor();
                    if r >= i64::MIN as f64 && r <= i64::MAX as f64 {
                        Some(Ok(Value::int(r as i64)))
                    } else {
                        Some(Ok(Value::num(r)))
                    }
                }
                RoundResult::Num => Some(Ok(Value::num(result))),
                RoundResult::Rat => {
                    let (n, d) = f64_to_rat(result);
                    Some(Ok(Value::rat_raw(n, d)))
                }
            }
        }
        "pick" => {
            if matches!(target.view(), ValueView::Mix(_, _)) {
                return Some(Err(RuntimeError::new(
                    "Cannot call .pick on a Mix (immutable)",
                )));
            }
            // Callable args (e.g. WhateverCode `* / 2`) need interpreter to invoke;
            // fall through to the runtime.
            if matches!(arg.view(), ValueView::Sub(_) | ValueView::WeakSub(_)) {
                return None;
            }
            // For Bag/BagHash, use weighted picking without expanding to a flat list
            if let ValueView::Bag(bag, _) = target.view() {
                if let ValueView::Num(f) = arg.view()
                    && f.is_nan()
                {
                    return Some(Err(RuntimeError::new("Cannot convert NaN to Int")));
                }
                let total_items: i128 = bag
                    .values()
                    .map(crate::runtime::utils::bigint_to_i128_sat)
                    .sum();
                let count: i128 = match arg.view() {
                    ValueView::Whatever => total_items,
                    ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => total_items,
                    ValueView::Int(n) => n.max(0) as i128,
                    ValueView::Num(f) => (f as i64).max(0) as i128,
                    ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as i128,
                    _ => 0i128,
                };
                if count == 0 || bag.is_empty() {
                    return Some(Ok(Value::seq(Vec::new())));
                }
                // Build a mutable copy of counts for without-replacement picking
                let mut counts: Vec<(String, i128)> = bag
                    .iter()
                    .filter(|(_, c)| c.is_positive())
                    .map(|(k, c)| (k.clone(), crate::runtime::utils::bigint_to_i128_sat(c)))
                    .collect();
                let mut total: i128 = counts.iter().map(|(_, c)| *c).sum();
                let pick_count = (count as usize).min(total as usize);
                let mut result = Vec::with_capacity(pick_count);
                for _ in 0..pick_count {
                    if total <= 0 {
                        break;
                    }
                    let needle_f = crate::builtins::rng::builtin_rand() * total as f64;
                    let mut needle = needle_f as i128;
                    if needle >= total {
                        needle = total - 1;
                    }
                    let mut picked_idx = counts.len() - 1;
                    let mut cum: i128 = 0;
                    for (i, (_, c)) in counts.iter().enumerate() {
                        cum += *c;
                        if needle < cum {
                            picked_idx = i;
                            break;
                        }
                    }
                    result.push(Value::str(counts[picked_idx].0.clone()));
                    counts[picked_idx].1 -= 1;
                    total -= 1;
                    if counts[picked_idx].1 == 0 {
                        counts.swap_remove(picked_idx);
                    }
                }
                return Some(Ok(Value::seq(result)));
            }
            // For Set, pick returns keys (strings) without replacement
            if let ValueView::Set(set, _) = target.view() {
                if let ValueView::Num(f) = arg.view()
                    && f.is_nan()
                {
                    return Some(Err(RuntimeError::new("Cannot convert NaN to Int")));
                }
                let mut keys: Vec<String> = set.iter().cloned().collect();
                let count: usize = match arg.view() {
                    ValueView::Whatever => keys.len(),
                    ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => keys.len(),
                    ValueView::Int(n) => n.max(0) as usize,
                    ValueView::Num(f) => (f as i64).max(0) as usize,
                    ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as usize,
                    _ => 0,
                };
                let pick_count = count.min(keys.len());
                // Fisher-Yates shuffle for without-replacement
                let len = keys.len();
                for i in (1..len).rev() {
                    let j =
                        (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize % (i + 1);
                    keys.swap(i, j);
                }
                keys.truncate(pick_count);
                return Some(Ok(Value::seq(keys.into_iter().map(Value::str).collect())));
            }
            // Fast path for integer ranges — avoid materializing
            if let Some(result) = range_pick_n_fast(target, arg) {
                return Some(Ok(result));
            }
            // .pick(**) — lazy infinite shuffled cycles
            if matches!(arg.view(), ValueView::HyperWhatever) {
                let pool = runtime::value_to_list(target);
                if pool.is_empty() {
                    return Some(Ok(Value::seq(Vec::new())));
                }
                // Pre-generate several cycles of shuffled picks
                let num_cycles = 4;
                let mut cached = Vec::with_capacity(pool.len() * num_cycles);
                for _ in 0..num_cycles {
                    let mut cycle = pool.clone();
                    let len = cycle.len();
                    for i in (1..len).rev() {
                        let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize
                            % (i + 1);
                        cycle.swap(i, j);
                    }
                    cached.extend(cycle);
                }
                return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                    crate::value::LazyList::new_cached(cached),
                ))));
            }
            // NaN check for general .pick path
            if let ValueView::Num(f) = arg.view()
                && f.is_nan()
            {
                return Some(Err(RuntimeError::new("Cannot convert NaN to Int")));
            }
            let mut items = runtime::value_to_list(target);
            Some(Ok(match arg.view() {
                ValueView::Whatever => {
                    // .pick(*) — Fisher-Yates shuffle
                    let len = items.len();
                    for i in (1..len).rev() {
                        let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize
                            % (i + 1);
                        items.swap(i, j);
                    }
                    Value::seq(items)
                }
                ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                    // .pick(Inf) — same as .pick(*)
                    let len = items.len();
                    for i in (1..len).rev() {
                        let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize
                            % (i + 1);
                        items.swap(i, j);
                    }
                    Value::seq(items)
                }
                ValueView::Num(f) => {
                    // .pick(<num>) — truncate to int
                    let count = (f as i64).max(0) as usize;
                    if count == 0 || items.is_empty() {
                        Value::seq(Vec::new())
                    } else {
                        let mut result = Vec::with_capacity(count.min(items.len()));
                        for _ in 0..count.min(items.len()) {
                            let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                                as usize
                                % items.len();
                            result.push(items.swap_remove(idx));
                        }
                        Value::seq(result)
                    }
                }
                ValueView::Rat(n, d) if d != 0 => {
                    // .pick(<rat>) — truncate to int
                    let count = (n / d).max(0) as usize;
                    if count == 0 || items.is_empty() {
                        Value::seq(Vec::new())
                    } else {
                        let mut result = Vec::with_capacity(count.min(items.len()));
                        for _ in 0..count.min(items.len()) {
                            let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                                as usize
                                % items.len();
                            result.push(items.swap_remove(idx));
                        }
                        Value::seq(result)
                    }
                }
                ValueView::Int(n) => {
                    let count = n.max(0) as usize;
                    if count == 0 || items.is_empty() {
                        Value::seq(Vec::new())
                    } else {
                        let mut result = Vec::with_capacity(count.min(items.len()));
                        for _ in 0..count.min(items.len()) {
                            let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                                as usize
                                % items.len();
                            result.push(items.swap_remove(idx));
                        }
                        Value::seq(result)
                    }
                }
                ValueView::Str(s) => {
                    let count = s.trim().parse::<i64>().unwrap_or(0).max(0) as usize;
                    if count == 0 || items.is_empty() {
                        Value::seq(Vec::new())
                    } else {
                        let mut result = Vec::with_capacity(count.min(items.len()));
                        for _ in 0..count.min(items.len()) {
                            let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                                as usize
                                % items.len();
                            result.push(items.swap_remove(idx));
                        }
                        Value::seq(result)
                    }
                }
                _ => return None,
            }))
        }
        "pickpairs" => {
            if let ValueView::Bag(bag, _) = target.view() {
                let count = match arg.view() {
                    ValueView::Whatever => bag.len(),
                    ValueView::Int(n) => n.max(0) as usize,
                    ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => bag.len(),
                    ValueView::Num(f) => (f as i64).max(0) as usize,
                    ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as usize,
                    _ => return None,
                };
                if count == 0 || bag.is_empty() {
                    return Some(Ok(Value::seq(Vec::new())));
                }
                let mut pairs: Vec<(String, BigInt)> =
                    bag.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                let pick_count = count.min(pairs.len());
                let mut result = Vec::with_capacity(pick_count);
                for _ in 0..pick_count {
                    if pairs.is_empty() {
                        break;
                    }
                    let idx = (crate::builtins::rng::builtin_rand() * pairs.len() as f64) as usize
                        % pairs.len();
                    let (key, count) = pairs.swap_remove(idx);
                    result.push(Value::pair(key, Value::from_bigint(count)));
                }
                return Some(Ok(Value::seq(result)));
            }
            None
        }
        "grab" | "grabpairs" => match target.view() {
            ValueView::Bag(_, false) => Some(Err(RuntimeError::immutable("Bag", method))),
            ValueView::Set(_, false) => Some(Err(RuntimeError::immutable("Set", method))),
            ValueView::Mix(_, false) => Some(Err(RuntimeError::immutable("Mix", method))),
            // NaN check for grab/grabpairs on mutable types
            ValueView::Set(_, true) | ValueView::Bag(_, true) | ValueView::Mix(_, true) if matches!(arg.view(), ValueView::Num(f) if f.is_nan()) => {
                Some(Err(RuntimeError::new("Cannot convert NaN to Int")))
            }
            _ => None,
        },
        "roll" => {
            if matches!(target.view(), ValueView::Package(_)) {
                return None;
            }
            let count = match arg.view() {
                ValueView::Int(i) if i > 0 => Some(i as usize),
                ValueView::Int(_) => Some(0),
                ValueView::Num(f) if f.is_nan() => {
                    return Some(Err(RuntimeError::new("Cannot convert NaN to Int")));
                }
                ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                ValueView::Num(f) if f < 0.0 => Some(0),
                ValueView::Num(f) => Some(f as usize),
                ValueView::Whatever => None,
                ValueView::Str(s) => {
                    let parsed = s.trim().parse::<i64>().ok()?;
                    Some(parsed.max(0) as usize)
                }
                _ => return None,
            };
            if let ValueView::Mix(items, _) = target.view() {
                if count.is_none() {
                    let generated = 131_072usize;
                    let mut out = Vec::with_capacity(generated);
                    for _ in 0..generated {
                        if let Some(v) = sample_weighted_mix_key(&items) {
                            out.push(v);
                        }
                    }
                    return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                        crate::value::LazyList::new_cached(out),
                    ))));
                }
                let count = count.unwrap_or(0);
                if count == 0 {
                    return Some(Ok(Value::seq(Vec::new())));
                }
                let mut result = Vec::with_capacity(count);
                for _ in 0..count {
                    if let Some(v) = sample_weighted_mix_key(&items) {
                        result.push(v);
                    }
                }
                return Some(Ok(Value::seq(result)));
            }
            if let ValueView::Bag(items, _) = target.view() {
                if count.is_none() {
                    let generated = 131_072usize;
                    let mut out = Vec::with_capacity(generated);
                    for _ in 0..generated {
                        if let Some(v) = sample_weighted_bag_key(&items) {
                            out.push(v);
                        }
                    }
                    return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                        crate::value::LazyList::new_cached(out),
                    ))));
                }
                let count = count.unwrap_or(0);
                if count == 0 {
                    return Some(Ok(Value::seq(Vec::new())));
                }
                let mut result = Vec::with_capacity(count);
                for _ in 0..count {
                    if let Some(v) = sample_weighted_bag_key(&items) {
                        result.push(v);
                    }
                }
                return Some(Ok(Value::seq(result)));
            }
            if let ValueView::Set(items, _) = target.view() {
                let keys: Vec<&String> = items.iter().collect();
                if keys.is_empty() {
                    return Some(Ok(Value::seq(Vec::new())));
                }
                if count.is_none() {
                    let generated = 131_072usize;
                    let mut out = Vec::with_capacity(generated);
                    for _ in 0..generated {
                        let mut idx =
                            (crate::builtins::rng::builtin_rand() * keys.len() as f64) as usize;
                        if idx >= keys.len() {
                            idx = keys.len() - 1;
                        }
                        out.push(Value::str(keys[idx].clone()));
                    }
                    return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                        crate::value::LazyList::new_cached(out),
                    ))));
                }
                let count = count.unwrap_or(0);
                if count == 0 {
                    return Some(Ok(Value::seq(Vec::new())));
                }
                let mut result = Vec::with_capacity(count);
                for _ in 0..count {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * keys.len() as f64) as usize;
                    if idx >= keys.len() {
                        idx = keys.len() - 1;
                    }
                    result.push(Value::str(keys[idx].clone()));
                }
                return Some(Ok(Value::seq(result)));
            }
            let sample_from_range = |range: &Value| -> Option<Value> {
                let random_i64 = |lo: i64, hi: i64| -> Value {
                    use crate::builtins::methods_0arg::dispatch_core_range::range_pick_one_i64_pub;
                    if hi <= lo {
                        return Value::int(lo);
                    }
                    range_pick_one_i64_pub(lo, hi)
                };
                match range.view() {
                    ValueView::Range(start, end) => Some(random_i64(start, end)),
                    ValueView::RangeExcl(start, end) => {
                        if start >= end {
                            Some(Value::NIL)
                        } else {
                            Some(random_i64(start, end.saturating_sub(1)))
                        }
                    }
                    ValueView::RangeExclStart(start, end) => {
                        if start >= end {
                            Some(Value::NIL)
                        } else {
                            Some(random_i64(start.saturating_add(1), end))
                        }
                    }
                    ValueView::RangeExclBoth(start, end) => {
                        if start.saturating_add(1) >= end {
                            Some(Value::NIL)
                        } else {
                            Some(random_i64(start.saturating_add(1), end.saturating_sub(1)))
                        }
                    }
                    ValueView::GenericRange {
                        start,
                        end,
                        excl_start,
                        excl_end,
                    } => {
                        // Try BigInt-based fast path for integer ranges
                        if let Some(result) = crate::builtins::methods_0arg::dispatch_core_range::generic_range_pick_one_pub(start, end, excl_start, excl_end) {
                            return Some(result);
                        }
                        if let (Some(s), Some(e)) =
                            (runtime::to_float_value(start), runtime::to_float_value(end))
                            && s.is_finite()
                            && e.is_finite()
                        {
                            let mut vals = Vec::new();
                            let mut cur = if excl_start { s + 1.0 } else { s };
                            let limit = 10_000usize;
                            while vals.len() < limit {
                                if (excl_end && cur >= e) || (!excl_end && cur > e) {
                                    break;
                                }
                                vals.push(Value::num(cur));
                                cur += 1.0;
                            }
                            if !vals.is_empty() {
                                let idx = (crate::builtins::rng::builtin_rand() * vals.len() as f64)
                                    as usize
                                    % vals.len();
                                return Some(vals[idx].clone());
                            }
                        }
                        Some(start.as_ref().clone())
                    }
                    _ => None,
                }
            };

            let items = if target.is_range() {
                Vec::new()
            } else {
                runtime::value_to_list(target)
            };
            if count.is_none() {
                if !target.is_range() && items.is_empty() {
                    return Some(Ok(Value::seq(Vec::new())));
                }
                if target.is_range() {
                    // A range's pool isn't a finite Vec (it may be huge or
                    // infinite), so it still needs a per-pull sampler rather
                    // than `SequenceSpec::RollPool`'s static pool. Generate a
                    // bounded eager prefix as before.
                    let generated = 1024usize;
                    let mut out = Vec::with_capacity(generated);
                    for _ in 0..generated {
                        if let Some(v) = sample_from_range(target) {
                            out.push(v);
                        }
                    }
                    return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                        crate::value::LazyList::new_cached(out),
                    ))));
                }
                // A finite pool: `.roll(*)` is a genuinely infinite Seq (each
                // pull an independent random pick), so represent it as a
                // sequence-spec lazy list (like `1...*`) instead of eagerly
                // generating a fixed-size prefix. This renders Rakudo's
                // `(...)` gist placeholder and can be pulled indefinitely.
                return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                    crate::value::LazyList::new_sequence(
                        Vec::new(),
                        crate::value::SequenceSpec::RollPool(items),
                    ),
                ))));
            }
            let count = count.unwrap_or(0);
            if count == 0 || (!target.is_range() && items.is_empty()) {
                return Some(Ok(Value::seq(Vec::new())));
            }
            let mut result = Vec::with_capacity(count);
            for _ in 0..count {
                if target.is_range() {
                    if let Some(v) = sample_from_range(target) {
                        result.push(v);
                    }
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    result.push(items[idx].clone());
                }
            }
            Some(Ok(Value::seq(result)))
        }
        "log" => {
            let base_complex = match arg.view() {
                ValueView::Int(i) => Some((i as f64, 0.0)),
                ValueView::Num(f) => Some((f, 0.0)),
                ValueView::Rat(n, d) if d != 0 => Some((n as f64 / d as f64, 0.0)),
                ValueView::Complex(r, i) => Some((r, i)),
                _ => None,
            };
            let target_complex = match target.view() {
                ValueView::Int(i) => Some((i as f64, 0.0)),
                ValueView::Num(f) => Some((f, 0.0)),
                ValueView::Rat(n, d) if d != 0 => Some((n as f64 / d as f64, 0.0)),
                ValueView::Complex(r, i) => Some((r, i)),
                _ => None,
            };
            match (target_complex, base_complex) {
                (Some((xr, xi)), Some((br, bi))) => {
                    if bi == 0.0 && xi == 0.0 {
                        if br.is_finite() && br > 0.0 && br != 1.0 && xr > 0.0 {
                            return Some(Ok(Value::num(xr.ln() / br.ln())));
                        }
                        return Some(Ok(Value::num(f64::NAN)));
                    }
                    let ln_x_mag = (xr * xr + xi * xi).sqrt().ln();
                    let ln_x_arg = xi.atan2(xr);
                    let ln_b_mag = (br * br + bi * bi).sqrt().ln();
                    let ln_b_arg = bi.atan2(br);
                    let denom = ln_b_mag * ln_b_mag + ln_b_arg * ln_b_arg;
                    if denom == 0.0 {
                        return Some(Ok(Value::num(f64::NAN)));
                    }
                    let re = (ln_x_mag * ln_b_mag + ln_x_arg * ln_b_arg) / denom;
                    let im = (ln_x_arg * ln_b_mag - ln_x_mag * ln_b_arg) / denom;
                    Some(Ok(Value::complex(re, im)))
                }
                _ => None,
            }
        }
        "exp" => {
            // $x.exp($base) = $base ** $x
            // Get base as real or complex
            let (base_r, base_i) = match arg.view() {
                ValueView::Int(i) => (i as f64, 0.0),
                ValueView::Num(f) => (f, 0.0),
                ValueView::Rat(n, d) if d != 0 => (n as f64 / d as f64, 0.0),
                ValueView::Complex(r, i) => (r, i),
                _ => return None,
            };
            // Get exponent as real or complex
            let (exp_r, exp_i) = match target.view() {
                ValueView::Int(i) => (i as f64, 0.0),
                ValueView::Num(f) => (f, 0.0),
                ValueView::Rat(n, d) if d != 0 => (n as f64 / d as f64, 0.0),
                ValueView::Complex(r, i) => (r, i),
                _ => return None,
            };
            // Compute base^exp via exp(exp * ln(base))
            // ln(base) for complex: ln(|base|) + i*arg(base)
            let ln_r = (base_r * base_r + base_i * base_i).sqrt().ln();
            let ln_i = base_i.atan2(base_r);
            // exp * ln(base): (exp_r + exp_i*i) * (ln_r + ln_i*i)
            let prod_r = exp_r * ln_r - exp_i * ln_i;
            let prod_i = exp_r * ln_i + exp_i * ln_r;
            // exp(prod_r + prod_i*i)
            let ea = prod_r.exp();
            let result_r = ea * prod_i.cos();
            let result_i = ea * prod_i.sin();
            if result_i.abs() < 1e-15 && base_i == 0.0 && exp_i == 0.0 {
                Some(Ok(Value::num(result_r)))
            } else {
                Some(Ok(Value::complex(result_r, result_i)))
            }
        }
        "unpolar" => {
            // $magnitude.unpolar($angle) = $magnitude * cis($angle)
            let mag = match target.view() {
                ValueView::Int(i) => i as f64,
                ValueView::Num(f) => f,
                ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
                _ => return None,
            };
            let angle = match arg.view() {
                ValueView::Int(i) => i as f64,
                ValueView::Num(f) => f,
                ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
                _ => return None,
            };
            Some(Ok(Value::complex(mag * angle.cos(), mag * angle.sin())))
        }
        "roots" => {
            // Instance types need runtime coercion via .Numeric
            if matches!(target.view(), ValueView::Instance { .. }) {
                return None;
            }
            Some(Ok(compute_roots(target, arg)))
        }
        "atan2" => {
            // User-defined types need runtime coercion via .Numeric/.Bridge
            if matches!(target.view(), ValueView::Instance { .. })
                || matches!(arg.view(), ValueView::Instance { .. })
            {
                return None;
            }
            let y = runtime::to_float_value(target).unwrap_or(0.0);
            let x = runtime::to_float_value(arg).unwrap_or(0.0);
            Some(Ok(Value::num(y.atan2(x))))
        }
        // Buf/Blob read-num methods (1 arg: offset, uses NativeEndian)
        "read-num32" | "read-num64" => {
            if let ValueView::Package(type_name) = target.view() {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name,
                ))));
            }
            let bytes = buf_get_bytes(target)?;
            let offset_i64 = to_int_val(arg);
            let size: usize = if method == "read-num32" { 4 } else { 8 };
            if offset_i64 < 0
                || (offset_i64 as usize)
                    .checked_add(size)
                    .is_none_or(|end| end > bytes.len())
            {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    offset_i64,
                    bytes.len()
                ))));
            }
            let offset = offset_i64 as usize;
            let result = if size == 4 {
                read_f32_ne(&bytes[offset..offset + 4])
            } else {
                read_f64_ne(&bytes[offset..offset + 8])
            };
            Some(Ok(Value::num(result)))
        }
        // Buf/Blob read-int/uint methods (1 arg: offset, uses NativeEndian)
        "read-uint8" | "read-int8" | "read-uint16" | "read-int16" | "read-uint32"
        | "read-int32" | "read-uint64" | "read-int64" | "read-uint128" | "read-int128" => {
            if let ValueView::Package(type_name) = target.view() {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name,
                ))));
            }
            let bytes = buf_get_bytes(target)?;
            let offset_i64 = to_int_val(arg);
            let (size, signed) = read_int_method_info(method);
            if offset_i64 < 0
                || (offset_i64 as usize)
                    .checked_add(size)
                    .is_none_or(|end| end > bytes.len())
            {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    offset_i64,
                    bytes.len()
                ))));
            }
            let offset = offset_i64 as usize;
            // NativeEndian default
            let endian: i64 = if cfg!(target_endian = "little") { 1 } else { 2 };
            Some(Ok(read_int_value(
                &bytes[offset..offset + size],
                size,
                signed,
                endian,
            )))
        }
        "AT-KEY" => match target.view() {
            ValueView::Hash(map) => {
                let key = arg.to_string_value();
                Some(Ok(map.get(&key).cloned().unwrap_or(Value::NIL)))
            }
            ValueView::Set(data, _) => {
                let key = arg.to_string_value();
                Some(Ok(Value::truth(data.elements.contains(&key))))
            }
            ValueView::Bag(data, _) => {
                let key = arg.to_string_value();
                let count = data.counts.get(&key).cloned().unwrap_or_else(BigInt::zero);
                Some(Ok(Value::from_bigint(count)))
            }
            ValueView::Mix(data, _) => {
                let key = arg.to_string_value();
                let weight = data.weights.get(&key).copied().unwrap_or(0.0);
                Some(Ok(crate::value::mix_weight_to_value(weight)))
            }
            ValueView::Nil => Some(Ok(Value::package(Symbol::intern("Any")))),
            ValueView::Package(name) if matches!(name.resolve().as_str(), "Any" | "Mu") => {
                Some(Ok(Value::package(Symbol::intern("Any"))))
            }
            _ => None,
        },
        "EXISTS-KEY" => match target.view() {
            ValueView::Hash(map) => {
                let key = arg.to_string_value();
                Some(Ok(Value::truth(map.contains_key(&key))))
            }
            ValueView::Set(data, _) => {
                let key = arg.to_string_value();
                Some(Ok(Value::truth(data.elements.contains(&key))))
            }
            ValueView::Bag(data, _) => {
                let key = arg.to_string_value();
                Some(Ok(Value::truth(data.counts.contains_key(&key))))
            }
            ValueView::Mix(data, _) => {
                let key = arg.to_string_value();
                Some(Ok(Value::truth(data.weights.contains_key(&key))))
            }
            ValueView::Nil => Some(Ok(Value::FALSE)),
            ValueView::Package(name) if matches!(name.resolve().as_str(), "Any" | "Mu") => {
                Some(Ok(Value::FALSE))
            }
            _ => None,
        },
        "subbuf" => {
            if !is_buf_like(target) {
                return None;
            }
            let items = buf_get_int_items(target)?;
            let cn = buf_class_name(target);
            // subbuf(Range)
            if let Some((start, end)) = range_bounds(arg) {
                let len = items.len() as i64;
                if end <= start || start >= len {
                    return Some(Ok(make_buf_from_int_items(&cn, &[])));
                }
                let s = start.max(0) as usize;
                let e = (end as usize).min(items.len());
                return Some(Ok(make_buf_from_int_items(&cn, &items[s..e])));
            }
            // subbuf(start) - from start to end
            let start = resolve_buf_index(arg, items.len());
            if start < 0 || start as usize > items.len() {
                return Some(Err(out_of_range_error(start, 0, items.len() as i64)));
            }
            Some(Ok(make_buf_from_int_items(&cn, &items[start as usize..])))
        }
        "isa" => {
            // Instance, Mixin(Instance), and Package values need interpreter
            // access for user-defined class hierarchies, role checks, and
            // subset type resolution, so fall through to the runtime handler.
            let needs_interpreter = match target.view() {
                ValueView::Instance { .. } | ValueView::Package(_) => true,
                ValueView::Mixin(inner, _) => {
                    matches!(inner.as_ref().view(), ValueView::Instance { .. })
                }
                _ => false,
            };
            if needs_interpreter {
                return None;
            }
            let type_name = match arg.view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Str(name) => name.to_string(),
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                _ => {
                    // For defined values, extract the type name (e.g., 3.isa(4) checks Int)
                    // This matches Raku's behavior where .isa on a defined value
                    // uses the value's type, not its string representation.
                    crate::value::types::what_type_name(arg)
                }
            };
            Some(Ok(Value::truth(target.isa_check(&type_name))))
        }
        _ => None,
    }
}
