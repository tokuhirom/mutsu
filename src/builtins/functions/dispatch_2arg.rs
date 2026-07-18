use super::flat::join_flat;
use super::math::is_extrema_named_pair;
use crate::builtins::rng::builtin_rand;
use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};

fn failure_exception(value: &Value) -> Option<Value> {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Failure" => attributes.as_map().get("exception").cloned(),
        ValueView::Mixin(inner, mixins) => {
            if let Some(mixed) = mixins.get("Failure")
                && let Some(ex) = failure_exception(mixed)
            {
                return Some(ex);
            }
            failure_exception(inner)
        }
        _ => None,
    }
}

fn minmax_two(arg1: &Value, arg2: &Value, want_max: bool) -> Result<Value, RuntimeError> {
    let ex1 = failure_exception(arg1);
    let ex2 = failure_exception(arg2);
    if ex1.is_some() && ex2.is_some() {
        let ex = ex1.unwrap_or(Value::NIL);
        let mut err = RuntimeError::new(ex.to_string_value());
        err.exception = Some(Box::new(ex));
        return Err(err);
    }
    if ex1.is_some() {
        return Ok(arg1.clone());
    }
    if ex2.is_some() {
        return Ok(arg2.clone());
    }
    if matches!(arg1.view(), ValueView::Package(name) if name == "Any") {
        return Ok(arg2.clone());
    }
    if matches!(arg2.view(), ValueView::Package(name) if name == "Any") {
        return Ok(arg1.clone());
    }

    let cmp = crate::runtime::compare_values(arg1, arg2);
    Ok(if (want_max && cmp >= 0) || (!want_max && cmp <= 0) {
        arg1.clone()
    } else {
        arg2.clone()
    })
}

pub(crate) fn native_function_2arg(
    name: &str,
    arg1: &Value,
    arg2: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match name {
        "combinations" => {
            // combinations($n_or_iterable, $k_or_range)
            // If first arg is an iterable, use its elements; otherwise treat as numeric n
            let items = match arg1.view() {
                ValueView::Array(..)
                | ValueView::Seq(..)
                | ValueView::Slip(..)
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
                | ValueView::Hash(..) => runtime::value_to_list(arg1),
                _ => {
                    let n = runtime::to_int(arg1);
                    if n <= 0 {
                        Vec::new()
                    } else {
                        (0..n).map(Value::int).collect()
                    }
                }
            };
            // Dispatch based on $k type (Int or Range)
            crate::builtins::native_method_1arg(
                &Value::array(items),
                crate::symbol::Symbol::intern("combinations"),
                arg2,
            )
        }
        "roll" => {
            let count = match arg1.view() {
                ValueView::Int(i) if i > 0 => Some(i as usize),
                ValueView::Int(_) => Some(0),
                ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                ValueView::Whatever => None,
                ValueView::Str(s) => {
                    let parsed = s.trim().parse::<i64>().ok()?;
                    Some(parsed.max(0) as usize)
                }
                _ => return None,
            };
            let items = crate::runtime::utils::value_to_list(arg2);
            if count.is_none() {
                if items.is_empty() {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let generated = 1024usize;
                let mut out = Vec::with_capacity(generated);
                for _ in 0..generated {
                    let mut idx = (builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    out.push(items[idx].clone());
                }
                return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                    crate::value::LazyList::new_cached(out),
                ))));
            }
            let count = count.unwrap_or(0);
            if items.is_empty() || count == 0 {
                return Some(Ok(Value::array(Vec::new())));
            }
            let mut result = Vec::with_capacity(count);
            for _ in 0..count {
                let mut idx = (builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                result.push(items[idx].clone());
            }
            Some(Ok(Value::array(result)))
        }
        "pick" => {
            // pick($count, @list) — sub form delegates to method .pick($count)
            let list = Value::array(runtime::value_to_list(arg2));
            crate::builtins::native_method_1arg(&list, crate::symbol::Symbol::intern("pick"), arg1)
        }
        "atan2" => {
            // atan2(y, x)
            if matches!(arg1.view(), ValueView::Instance { .. })
                || matches!(arg2.view(), ValueView::Instance { .. })
            {
                return None;
            }
            let y = runtime::to_float_value(arg1).unwrap_or(0.0);
            let x = runtime::to_float_value(arg2).unwrap_or(0.0);
            Some(Ok(Value::num(y.atan2(x))))
        }
        "roots" => {
            // roots($number, $n) — compute nth roots of $number
            Some(Ok(crate::builtins::methods_narg::compute_roots(arg1, arg2)))
        }
        "chop" => {
            // Type objects (Package) should throw
            if let ValueView::Package(type_name) = arg1.view() {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let s = arg1.to_string_value();
            let n = match arg2.view() {
                ValueView::Int(i) => i.max(0) as usize,
                ValueView::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    bi.to_usize().unwrap_or(usize::MAX)
                }
                ValueView::Num(f) => (f as i64).max(0) as usize,
                _ => arg2.to_string_value().parse::<usize>().unwrap_or(1),
            };
            let char_count = s.chars().count();
            let keep = char_count.saturating_sub(n);
            let result: String = s.chars().take(keep).collect();
            Some(Ok(Value::str(result)))
        }
        // `join(sep, list)`: flatten `list` (slurpy/`flat` semantics) and join.
        // The previous inline version ignored the separator for a `Range` arg
        // (`join("-", 1..4)` -> "1 2 3 4"); the shared `join_flat` fixes it.
        // Returns `None` for an un-realized lazy list -> the interpreter forces it.
        "join" => join_flat(&arg1.to_string_value(), std::slice::from_ref(arg2))
            .map(|joined| Ok(Value::str(joined))),
        "rotate" => {
            if let Some(shape) = crate::runtime::utils::shaped_array_shape(arg1) {
                if shape.len() > 1 {
                    return Some(Err(RuntimeError::illegal_on_fixed_dimension_array(
                        "rotate",
                    )));
                }
                // 1D shaped array: rotate the leaves
                let leaves = crate::runtime::utils::shaped_array_leaves(arg1);
                let len = leaves.len() as i64;
                if len == 0 {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let count = runtime::to_int(arg2);
                let n = ((count % len) + len) % len;
                let n = n as usize;
                let mut rotated = Vec::with_capacity(leaves.len());
                rotated.extend_from_slice(&leaves[n..]);
                rotated.extend_from_slice(&leaves[..n]);
                return Some(Ok(Value::array(rotated)));
            }
            match arg1.view() {
                ValueView::Array(items, ..) => {
                    let count = runtime::to_int(arg2);
                    let len = items.len() as i64;
                    if len == 0 {
                        return Some(Ok(Value::array(Vec::new())));
                    }
                    let n = ((count % len) + len) % len;
                    let n = n as usize;
                    let mut rotated = Vec::with_capacity(items.len());
                    rotated.extend_from_slice(&items[n..]);
                    rotated.extend_from_slice(&items[..n]);
                    Some(Ok(Value::array(rotated)))
                }
                _ => Some(Ok(Value::NIL)),
            }
        }
        "index" => {
            // Skip native path for junctions — fall through to interpreter for auto-threading
            if matches!(arg1.view(), ValueView::Junction { .. })
                || matches!(arg2.view(), ValueView::Junction { .. })
            {
                return None;
            }
            // Fall through to runtime for arrays (list of needles): the runtime
            // handler returns the smallest index at which any needle matches.
            if matches!(arg2.view(), ValueView::Array(..)) {
                return None;
            }
            let s = arg1.to_string_value();
            let needle = arg2.to_string_value();
            Some(Ok(match s.find(&needle) {
                Some(pos) => Value::int(s[..pos].chars().count() as i64),
                None => Value::NIL,
            }))
        }
        "indices" => {
            // Fall through to runtime -- handles named params, overlap, etc.
            None
        }
        "rindex" => {
            // Fall through to runtime for arrays (list of needles)
            if matches!(arg2.view(), ValueView::Array(..)) {
                return None;
            }
            let s = arg1.to_string_value();
            let needle = arg2.to_string_value();
            Some(Ok(match s.rfind(&needle) {
                Some(pos) => Value::int(s[..pos].chars().count() as i64),
                None => Value::NIL,
            }))
        }
        "substr" => {
            if matches!(arg1.view(), ValueView::Junction { .. })
                || matches!(arg2.view(), ValueView::Junction { .. })
            {
                return None;
            }
            crate::builtins::substr::native_substr_slice(&arg1.to_string_value(), arg2, None)
        }
        "samemark" => {
            let target = arg1.to_string_value();
            let source = arg2.to_string_value();
            Some(Ok(Value::str(crate::builtins::samemark_string(
                &target, &source,
            ))))
        }
        "samecase" => {
            let source = arg1.to_string_value();
            let pattern = arg2.to_string_value();
            Some(Ok(Value::str(crate::builtins::samecase_string(
                &source, &pattern,
            ))))
        }
        "log" => {
            if matches!(arg1.view(), ValueView::Instance { .. })
                || matches!(arg2.view(), ValueView::Instance { .. })
            {
                return None;
            }
            let x = runtime::to_float_value(arg1).unwrap_or(f64::NAN);
            let base_val = runtime::to_float_value(arg2).unwrap_or(f64::NAN);
            if base_val.is_finite() && base_val > 0.0 && base_val != 1.0 && x > 0.0 {
                Some(Ok(Value::num(x.ln() / base_val.ln())))
            } else {
                Some(Ok(Value::num(f64::NAN)))
            }
        }
        "exp" => {
            // exp($x, $base) = $base ** $x
            // Fast path for real args
            if !matches!(arg1.view(), ValueView::Complex(..))
                && !matches!(arg2.view(), ValueView::Complex(..))
            {
                let x = runtime::to_float_value(arg1).unwrap_or(f64::NAN);
                let base = runtime::to_float_value(arg2).unwrap_or(f64::NAN);
                return Some(Ok(Value::num(base.powf(x))));
            }
            let (base_r, base_i) = match arg2.view() {
                ValueView::Int(i) => (i as f64, 0.0),
                ValueView::Num(f) => (f, 0.0),
                ValueView::Rat(n, d) if d != 0 => (n as f64 / d as f64, 0.0),
                ValueView::Complex(r, i) => (r, i),
                _ => return None,
            };
            let (exp_r, exp_i) = match arg1.view() {
                ValueView::Int(i) => (i as f64, 0.0),
                ValueView::Num(f) => (f, 0.0),
                ValueView::Rat(n, d) if d != 0 => (n as f64 / d as f64, 0.0),
                ValueView::Complex(r, i) => (r, i),
                _ => return None,
            };
            let ln_r = (base_r * base_r + base_i * base_i).sqrt().ln();
            let ln_i = base_i.atan2(base_r);
            let prod_r = exp_r * ln_r - exp_i * ln_i;
            let prod_i = exp_r * ln_i + exp_i * ln_r;
            let ea = prod_r.exp();
            let result_r = ea * prod_i.cos();
            let result_i = ea * prod_i.sin();
            Some(Ok(Value::complex(result_r, result_i)))
        }
        "round" => {
            let x = runtime::to_float_value(arg1)?;
            let scale = runtime::to_float_value(arg2)?;
            // Raku-style rounding: (x + 0.5).floor()
            fn raku_round_f64(x: f64) -> f64 {
                (x + 0.5).floor()
            }
            if scale == 0.0 {
                Some(Ok(Value::int(raku_round_f64(x) as i64)))
            } else {
                let factor = (1.0 / scale).abs();
                Some(Ok(Value::num(raku_round_f64(x * factor) / factor)))
            }
        }
        "min" => {
            if is_extrema_named_pair(arg1) || is_extrema_named_pair(arg2) {
                return None;
            }
            Some(minmax_two(arg1, arg2, false))
        }
        "max" => {
            if is_extrema_named_pair(arg1) || is_extrema_named_pair(arg2) {
                return None;
            }
            Some(minmax_two(arg1, arg2, true))
        }
        "words" => {
            let s = arg1.to_string_value();
            let limit = match arg2.view() {
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
        "uniprop" => {
            // uniprop(target, property_name)
            let prop_name = arg2.to_string_value();
            match arg1.view() {
                ValueView::Int(i) => {
                    let cp = i as u32;
                    Some(Ok(
                        crate::builtins::uniprop::unicode_property_value_for_codepoint(
                            cp,
                            Some(&prop_name),
                        ),
                    ))
                }
                _ => {
                    let s = arg1.to_string_value();
                    if s.is_empty() {
                        return Some(Ok(Value::NIL));
                    }
                    let ch = s.chars().next().unwrap();
                    Some(Ok(crate::builtins::uniprop::unicode_property_value(
                        ch, &prop_name,
                    )))
                }
            }
        }
        "unimatch" => {
            // unimatch(target, property_value)
            let prop_value = arg2.to_string_value();
            match arg1.view() {
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
                    Some(Err(err))
                }
                ValueView::Int(i) => {
                    let cp = i as u32;
                    Some(Ok(crate::builtins::uniprop::unimatch_for_codepoint(
                        cp,
                        &prop_value,
                        None,
                    )))
                }
                _ => {
                    let s = arg1.to_string_value();
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
            }
        }
        _ => None,
    }
}
