use super::allomorph::out_of_range_failure;
use super::base::{BaseDigits, f64_to_rat, rat_to_base};
use super::buf::{
    bigint_to_value, buf_class_name, buf_get_int_items, buf_get_raw_bytes, is_buf_like,
    make_buf_from_int_items, out_of_range_error, read_ubits_from_bytes, resolve_buf_index,
    resolve_buf_len,
};
use super::flatten::{flatten_target, is_hammer_pair, parse_flat_depth};
use super::fmt_contains::{fmt_joinable_target, fmt_single_or_pair};
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};
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
    // Cost: O(n), n = bytes of the invocant.
    if method == "naive-word-wrapper" {
        return crate::builtins::naive_word_wrapper::native_naive_word_wrapper(
            target,
            &[arg1, arg2],
        );
    }
    // Cost: O(n), n = chars in a string bound/value or the error label.
    // The `Range.in-range` row's implementation (`method_table::range`).
    if method == "in-range" {
        return crate::builtins::method_table::range::in_range_what(
            target,
            &[arg1.clone(), arg2.clone()],
        );
    }
    // `Backtrace` introspection with two arguments -- a starting index plus a
    // named flag (`.next-interesting-index(2, :named)`), or two named flags.
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name == "Backtrace"
        && let Some(result) = crate::builtins::backtrace_methods::dispatch(
            &attributes,
            method,
            &[arg1.clone(), arg2.clone()],
        )
    {
        return Some(result);
    }
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
    // Cost: O(m) amortized, m = chars of the needle, once the invocant's
    // grapheme index is cached (`$pos` is resolved through it).
    if method == "substr-eq"
        && let ValueView::Str(_) = target.view()
    {
        if let ValueView::Package(type_name) = arg1.view() {
            return Some(Err(RuntimeError::new(format!(
                "Cannot resolve caller substr-eq({}:U)",
                type_name
            ))));
        }
        let ValueView::Int(pos) = arg2.descalarize().view() else {
            return None;
        };
        if pos < 0 {
            return None;
        }
        let needle = arg1.to_string_value();
        return crate::builtins::grapheme_index::with_str_index(target, |text, idx| {
            if pos as usize > idx.len() {
                return None;
            }
            let eq = crate::builtins::str_prim::eq_at(
                text,
                idx,
                pos as usize,
                &needle,
                crate::builtins::str_prim::Fold::Exact,
            );
            Some(Ok(Value::truth(eq)))
        });
    }

    if method == "split" {
        if let ValueView::Instance { class_name, .. } = target.view()
            && (class_name == "Supply"
                || class_name == "IO::Handle"
                || class_name == "IO::Pipe"
                || class_name == "IO::CatHandle")
        {
            return None;
        }
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "IO::Path"
            && attributes.contains_key("resource")
        {
            return None;
        }
        if let ValueView::Package(name) = target.view()
            && name.as_str().starts_with("IO::Spec")
        {
            return None;
        }
        return crate::builtins::split::native_split_method(target, &[arg1.clone(), arg2.clone()]);
    }

    if method == "comb" {
        // Supply/IO targets keep their interpreter comb semantics.
        if let ValueView::Instance { class_name, .. } = target.view()
            && (class_name == "Supply"
                || class_name == "IO::Handle"
                || class_name == "IO::Path"
                || class_name == "IO::Pipe"
                || class_name == "IO::CatHandle")
        {
            return None;
        }
        // `.comb(matcher, limit)`: pure Int/Str split shared with the
        // interpreter; Regex/Sub/bare matchers return None -> interpreter.
        return crate::builtins::comb::native_comb_method(target, &[arg1.clone(), arg2.clone()]);
    }

    match method {
        // The `unimatch($value, $property)` row's handler
        // (`method_table::unicode`), for the receivers with no table shape.
        // Cost: O(1), a table lookup.
        "unimatch" => crate::builtins::method_table::unicode::unimatch_in(
            target,
            &[arg1.clone(), arg2.clone()],
        ),
        "fmt" => {
            // A Format object argument is handled by the slow-path Format dispatch.
            if matches!(arg1.view(), ValueView::Instance { class_name, .. } if class_name.resolve() == "Format")
            {
                return None;
            }
            let fmt_str = arg1.to_string_value();
            let sep = arg2.to_string_value();
            if let ValueView::Hash(items) = target.view() {
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
            } else if let ValueView::Bag(items, _) = target.view() {
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(
                            &fmt_str,
                            &[items.typed_key(k), Value::from_bigint(v.clone())],
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else if let ValueView::Set(items, _) = target.view() {
                let rendered = items
                    .iter()
                    .map(|k| {
                        runtime::format_sprintf_args(&fmt_str, &[items.typed_key(k), Value::TRUE])
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else if let ValueView::Mix(items, _) = target.view() {
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(
                            &fmt_str,
                            &[items.typed_key(k), Value::num(*v)],
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
        // Cost: O(k), k = chars returned, once the invocant's grapheme index is
        // cached (see `native_substr_slice`).
        "substr" => crate::builtins::substr::native_substr_slice(target, arg1, Some(arg2)),
        "base" => {
            let radix = match arg1.view() {
                ValueView::Int(r) if (2..=36).contains(&r) => r as u32,
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
            let digits_mode = match arg2.view() {
                ValueView::Int(d) if d < 0 => {
                    return Some(Ok(out_of_range_failure("digits must be non-negative")));
                }
                ValueView::Int(d) => BaseDigits::Fixed(d as u32),
                ValueView::Whatever => BaseDigits::Whatever,
                _ => None?,
            };
            match target.view() {
                ValueView::Int(i) => Some(Ok(Value::str(rat_to_base(i, 1, radix, digits_mode)))),
                ValueView::Num(f) => {
                    let (n, d) = f64_to_rat(f);
                    Some(Ok(Value::str(rat_to_base(n, d, radix, digits_mode))))
                }
                ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
                    Some(Ok(Value::str(rat_to_base(n, d, radix, digits_mode))))
                }
                ValueView::Instance { attributes, .. } => {
                    if let Some(val) = attributes.as_map().get("value") {
                        match val.view() {
                            ValueView::Int(i) => {
                                Some(Ok(Value::str(rat_to_base(i, 1, radix, digits_mode))))
                            }
                            ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
                                Some(Ok(Value::str(rat_to_base(n, d, radix, digits_mode))))
                            }
                            ValueView::Num(f) => {
                                let (n, d) = f64_to_rat(f);
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
            let (bytes, _) = buf_get_raw_bytes(target)?;
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
        // Buf/Blob `read-*` (2 args: offset and byte order) and `subbuf`: the
        // rows' implementation (`method_table::blob_read`).
        "read-num32" | "read-num64" | "read-uint8" | "read-int8" | "read-uint16" | "read-int16"
        | "read-uint32" | "read-int32" | "read-uint64" | "read-int64" | "read-uint128"
        | "read-int128" => crate::builtins::method_table::blob_read::read(
            method,
            target,
            &[arg1.clone(), arg2.clone()],
        ),
        "subbuf" => {
            crate::builtins::method_table::blob_read::subbuf(target, &[arg1.clone(), arg2.clone()])
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
