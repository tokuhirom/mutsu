#![allow(clippy::result_large_err)]

use super::allomorph::out_of_range_failure;
use super::base::{BaseDigits, f64_to_rat, rat_to_base};
use super::buf::{
    bigint_to_value, buf_class_name, buf_get_bytes, buf_get_int_items, is_buf_like,
    make_buf_from_int_items, out_of_range_error, read_f32_endian, read_f64_endian,
    read_int_method_info, read_int_value, read_ubits_from_bytes, resolve_buf_index,
    resolve_buf_len, to_int_val,
};
use super::flatten::{flatten_target, is_hammer_pair, parse_flat_depth};
use super::fmt_contains::{fmt_joinable_target, fmt_single_or_pair};
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
use num_bigint::BigInt;
use num_traits::Zero;

pub(crate) fn native_method_2arg(
    target: &Value,
    method_sym: Symbol,
    arg1: &Value,
    arg2: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    // Scalar containers are transparent for method dispatch (no .VAR at this arity).
    let target = target.descalarize();
    if method == "flat" {
        let (depth, hammer) = if let Some(depth) = parse_flat_depth(arg1) {
            (Some(depth), is_hammer_pair(arg2))
        } else if let Some(depth) = parse_flat_depth(arg2) {
            (Some(depth), is_hammer_pair(arg1))
        } else {
            (None, false)
        };
        if let Some(depth) = depth {
            if hammer {
                return Some(Ok(flatten_target(target, Some(depth), true)));
            }
            return Some(Ok(flatten_target(target, Some(depth), false)));
        }
        return None;
    }

    // `.substr-eq($needle, $pos)` with a plain non-negative Int position is a
    // pure substring comparison on a Str receiver. Whatever / negative /
    // out-of-range positions and the case-/mark-insensitive named-arg forms
    // (`:i`/`:m`, which arrive as an extra Pair argument) keep the interpreter's
    // position resolution + Failure semantics (runtime/methods_string.rs).
    if method == "substr-eq"
        && let Value::Str(_) = &target
    {
        if let Value::Package(type_name) = arg1 {
            return Some(Err(RuntimeError::new(format!(
                "Cannot resolve caller substr-eq({}:U)",
                type_name
            ))));
        }
        let Value::Int(pos) = arg2.descalarize() else {
            return None;
        };
        if *pos < 0 {
            return None;
        }
        let text = target.to_string_value();
        let len = text.chars().count() as i64;
        if *pos > len {
            return None;
        }
        let needle = arg1.to_string_value();
        let substr: String = text
            .chars()
            .skip(*pos as usize)
            .take(needle.chars().count())
            .collect();
        return Some(Ok(Value::Bool(substr == needle)));
    }

    if method == "split" {
        if let Value::Instance { class_name, .. } = target
            && (class_name == "Supply" || class_name == "IO::Handle" || class_name == "IO::Pipe")
        {
            return None;
        }
        if let Value::Package(name) = target
            && name.resolve().starts_with("IO::Spec")
        {
            return None;
        }
        return crate::builtins::split::native_split_method(target, &[arg1.clone(), arg2.clone()]);
    }

    if method == "comb" {
        // Supply/IO targets keep their interpreter comb semantics.
        if let Value::Instance { class_name, .. } = target
            && (class_name == "Supply"
                || class_name == "IO::Handle"
                || class_name == "IO::Path"
                || class_name == "IO::Pipe")
        {
            return None;
        }
        // `.comb(matcher, limit)`: pure Int/Str split shared with the
        // interpreter; Regex/Sub/bare matchers return None -> interpreter.
        return crate::builtins::comb::native_comb_method(target, &[arg1.clone(), arg2.clone()]);
    }

    match method {
        "expmod" => Some(crate::builtins::expmod(target, arg1, arg2)),
        "unimatch" => {
            // target.unimatch(prop_value, prop_name)
            let prop_value = arg1.to_string_value();
            let prop_name = arg2.to_string_value();
            match target {
                Value::Int(i) => {
                    let cp = *i as u32;
                    Some(Ok(crate::builtins::uniprop::unimatch_for_codepoint(
                        cp,
                        &prop_value,
                        Some(&prop_name),
                    )))
                }
                _ => {
                    let s = target.to_string_value();
                    if s.is_empty() {
                        return Some(Ok(Value::Nil));
                    }
                    let ch = s.chars().next().unwrap();
                    Some(Ok(Value::Bool(crate::builtins::uniprop::unimatch(
                        ch,
                        &prop_value,
                        Some(&prop_name),
                    ))))
                }
            }
        }
        "fmt" => {
            // A Format object argument is handled by the slow-path Format dispatch.
            if matches!(arg1, Value::Instance { class_name, .. } if class_name.resolve() == "Format")
            {
                return None;
            }
            let fmt_str = arg1.to_string_value();
            let sep = arg2.to_string_value();
            if let Value::Hash(items) = target {
                // Hash.fmt(format, separator)
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(
                            &fmt_str,
                            &[Value::str(k.to_string()), v.clone()],
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else if let Value::Bag(items, _) = target {
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(
                            &fmt_str,
                            &[Value::str(k.clone()), Value::from_bigint(v.clone())],
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else if let Value::Set(items, _) = target {
                let rendered = items
                    .iter()
                    .map(|k| {
                        runtime::format_sprintf_args(
                            &fmt_str,
                            &[Value::str(k.clone()), Value::Bool(true)],
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else if let Value::Mix(items, _) = target {
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(
                            &fmt_str,
                            &[Value::str(k.clone()), Value::Num(*v)],
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else if fmt_joinable_target(target) {
                let items: Vec<Value> = if let Some(inner) = target.as_list_items() {
                    inner.to_vec()
                } else {
                    runtime::value_to_list(target)
                };
                let rendered = items
                    .into_iter()
                    .map(|item| fmt_single_or_pair(&fmt_str, &item))
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else {
                Some(Err(RuntimeError::new(
                    "Too many positionals passed; expected 1 or 2 arguments but got 3",
                )))
            }
        }
        "substr" => crate::builtins::substr::native_substr_slice(
            &target.to_string_value(),
            arg1,
            Some(arg2),
        ),
        "base" => {
            let radix = match arg1 {
                Value::Int(r) if (2..=36).contains(r) => *r as u32,
                Value::Str(s) => match s.parse::<u32>() {
                    Ok(r) if (2..=36).contains(&r) => r,
                    _ => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                },
                _ => {
                    return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                }
            };
            let digits_mode = match arg2 {
                Value::Int(d) if *d < 0 => {
                    return Some(Ok(out_of_range_failure("digits must be non-negative")));
                }
                Value::Int(d) => BaseDigits::Fixed(*d as u32),
                Value::Whatever => BaseDigits::Whatever,
                _ => None?,
            };
            match target {
                Value::Int(i) => Some(Ok(Value::str(rat_to_base(*i, 1, radix, digits_mode)))),
                Value::Num(f) => {
                    let (n, d) = f64_to_rat(*f);
                    Some(Ok(Value::str(rat_to_base(n, d, radix, digits_mode))))
                }
                Value::Rat(n, d) | Value::FatRat(n, d) => {
                    Some(Ok(Value::str(rat_to_base(*n, *d, radix, digits_mode))))
                }
                Value::Instance { attributes, .. } => {
                    if let Some(val) = attributes.as_map().get("value") {
                        match val {
                            Value::Int(i) => {
                                Some(Ok(Value::str(rat_to_base(*i, 1, radix, digits_mode))))
                            }
                            Value::Rat(n, d) | Value::FatRat(n, d) => {
                                Some(Ok(Value::str(rat_to_base(*n, *d, radix, digits_mode))))
                            }
                            Value::Num(f) => {
                                let (n, d) = f64_to_rat(*f);
                                Some(Ok(Value::str(rat_to_base(n, d, radix, digits_mode))))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        "read-ubits" | "read-bits" => {
            let bytes = buf_get_bytes(target)?;
            let from = runtime::to_int(arg1);
            let bits = runtime::to_int(arg2);
            if from < 0 || bits < 0 {
                return Some(Err(RuntimeError::new(
                    "bit offset/length must be non-negative",
                )));
            }
            let from = from as usize;
            let bits = bits as usize;
            let total_bits = bytes.len().saturating_mul(8);
            if from.checked_add(bits).is_none_or(|end| end > total_bits) {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    from, total_bits
                ))));
            }
            let unsigned = read_ubits_from_bytes(&bytes, from, bits);
            if method == "read-ubits" || bits == 0 {
                return Some(Ok(bigint_to_value(unsigned)));
            }
            let sign_bit = BigInt::from(1u8) << (bits - 1);
            let signed = if (&unsigned & &sign_bit).is_zero() {
                unsigned
            } else {
                unsigned - (BigInt::from(1u8) << bits)
            };
            Some(Ok(bigint_to_value(signed)))
        }
        // Buf/Blob read-num methods (2 args: offset + endian)
        "read-num32" | "read-num64" => {
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name,
                ))));
            }
            let bytes = buf_get_bytes(target)?;
            let offset_i64 = to_int_val(arg1);
            let endian_val = match arg2 {
                Value::Enum { value, .. } => value.as_i64(),
                Value::Int(i) => *i,
                _ => 0, // NativeEndian
            };
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
                read_f32_endian(&bytes[offset..offset + 4], endian_val)
            } else {
                read_f64_endian(&bytes[offset..offset + 8], endian_val)
            };
            Some(Ok(Value::Num(result)))
        }
        // Buf/Blob read-int/uint methods (2 args: offset + endian)
        "read-uint8" | "read-int8" | "read-uint16" | "read-int16" | "read-uint32"
        | "read-int32" | "read-uint64" | "read-int64" | "read-uint128" | "read-int128" => {
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name,
                ))));
            }
            let bytes = buf_get_bytes(target)?;
            let offset_i64 = to_int_val(arg1);
            let endian_val = match arg2 {
                Value::Enum { value, .. } => value.as_i64(),
                Value::Int(i) => *i,
                _ => 0, // NativeEndian
            };
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
            Some(Ok(read_int_value(
                &bytes[offset..offset + size],
                size,
                signed,
                endian_val,
            )))
        }
        "subbuf" => {
            if !is_buf_like(target) {
                return None;
            }
            let items = buf_get_int_items(target)?;
            let cn = buf_class_name(target);
            let len = items.len();
            let start = resolve_buf_index(arg1, len);
            if start < 0 {
                return Some(Err(out_of_range_error(start, 0, len as i64)));
            }
            if start as usize > len {
                return Some(Err(out_of_range_error(start, 0, len as i64)));
            }
            // arg2 is the length
            let sub_len = resolve_buf_len(arg2, len, start as usize);
            if sub_len < 0 {
                return Some(Err(out_of_range_error(sub_len, 0, len as i64)));
            }
            let s = start as usize;
            let available = len - s;
            let take = (sub_len as usize).min(available);
            Some(Ok(make_buf_from_int_items(&cn, &items[s..s + take])))
        }
        "subbuf-rw" => {
            if !is_buf_like(target) {
                return None;
            }
            let items = buf_get_int_items(target)?;
            let cn = buf_class_name(target);
            let len = items.len();
            let start = resolve_buf_index(arg1, len);
            if start < 0 || start as usize > len {
                return Some(Err(out_of_range_error(start, 0, len as i64)));
            }
            let sub_len = resolve_buf_len(arg2, len, start as usize);
            if sub_len < 0 {
                return Some(Err(out_of_range_error(sub_len, 0, len as i64)));
            }
            let s = start as usize;
            let available = len - s;
            let take = (sub_len as usize).min(available);
            Some(Ok(make_buf_from_int_items(&cn, &items[s..s + take])))
        }
        _ => None,
    }
}
