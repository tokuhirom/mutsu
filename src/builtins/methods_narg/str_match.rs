#![allow(clippy::result_large_err)]

use crate::value::{RuntimeError, Value};

pub(crate) fn is_str_or_match_receiver(target: &Value) -> bool {
    matches!(target, Value::Str(_))
        || matches!(target, Value::Instance { class_name, .. } if class_name == "Match")
}

/// Separate `(needle, …)`-style positional args from the `:i`/`:ignorecase`/
/// `:m`/`:ignoremark` named markings shared by `contains` / `starts-with` /
/// `ends-with` / `substr-eq`. Returns `None` if an *unexpected* named arg is
/// present (the interpreter owns those semantics). `ignore_mark` being requested
/// also returns `None` from the callers below — `strip_marks` decomposition is
/// left to the interpreter so the native fast paths stay purely ASCII/`to_lowercase`.
fn split_string_match_args(args: &[Value]) -> Option<(Vec<&Value>, bool, bool)> {
    let mut positional: Vec<&Value> = Vec::new();
    let mut ignore_case = false;
    let mut ignore_mark = false;
    for arg in args {
        if let Value::Pair(key, value) = arg {
            match key.as_str() {
                "i" | "ignorecase" => ignore_case = value.truthy(),
                "m" | "ignoremark" => ignore_mark = value.truthy(),
                _ => return None,
            }
        } else {
            positional.push(arg);
        }
    }
    Some((positional, ignore_case, ignore_mark))
}

/// `.starts-with($needle, :i?)` / `.ends-with($needle, :i?)` on a `Str` receiver —
/// the case-insensitive named forms. The plain `starts-with($needle)` form keeps its
/// `native_method_1arg` arm; this handles only the forms that carry a marking Pair
/// (which pushes them past the arity-keyed native dispatch). Mirrors
/// `Interpreter::dispatch_prefix_suffix_check`.
///
/// Returns `None` (fall through) for: non-Str receivers, a Package needle,
/// `:m`/`:ignoremark` (strip_marks → interpreter), unknown named args, and the bare
/// single-needle form.
pub(crate) fn native_prefix_suffix_with_options(
    target: &Value,
    args: &[Value],
    is_prefix: bool,
) -> Option<Result<Value, RuntimeError>> {
    if !is_str_or_match_receiver(target) {
        return None;
    }
    let (positional, ignore_case, ignore_mark) = split_string_match_args(args)?;
    if ignore_mark {
        return None;
    }
    let needle_val: &Value = positional.first().copied()?;
    // Bare single needle (no markings) stays on the 1-arg native arm.
    if positional.len() == args.len() {
        return None;
    }
    if let Value::Package(_) = needle_val {
        return None;
    }
    let text = target.to_string_value();
    let needle = needle_val.to_string_value();
    let (t, n) = if ignore_case {
        (text.to_lowercase(), needle.to_lowercase())
    } else {
        (text, needle)
    };
    let ok = if is_prefix {
        t.starts_with(n.as_str())
    } else {
        t.ends_with(n.as_str())
    };
    Some(Ok(Value::Bool(ok)))
}

/// `.substr-eq($needle, $pos?, :i?)` on a `Str` receiver — the case-insensitive and/or
/// named forms. The plain `substr-eq($needle, Int $pos)` form keeps its
/// `native_method_2arg` arm; this handles the forms carrying a marking Pair (which push
/// them past the arity-keyed dispatch). Mirrors `Interpreter::dispatch_substr_eq`.
///
/// Returns `None` (fall through) for: non-Str receivers, a Package needle,
/// `:m`/`:ignoremark`, unknown named args, non-Int/Str positions (Whatever resolution),
/// out-of-range / negative positions (X::OutOfRange Failure), and the bare forms
/// already handled by the 1-/2-arg arms.
pub(crate) fn native_substr_eq_with_options(
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if !is_str_or_match_receiver(target) {
        return None;
    }
    let (positional, ignore_case, ignore_mark) = split_string_match_args(args)?;
    if ignore_mark {
        return None;
    }
    // Only the named forms reach here; the bare positional forms keep their
    // existing 1-/2-arg native arms.
    if positional.len() == args.len() {
        return None;
    }
    let needle_val: &Value = positional.first().copied()?;
    if let Value::Package(_) = needle_val {
        return None;
    }
    let start = match positional.get(1).copied() {
        Some(Value::Int(i)) => *i,
        Some(Value::Str(s)) => s.parse::<i64>().ok()?,
        Some(_) => return None,
        None => 0,
    };
    let text = target.to_string_value();
    let len = text.chars().count() as i64;
    if start < 0 || start > len {
        return None;
    }
    let needle = needle_val.to_string_value();
    let substr: String = text
        .chars()
        .skip(start as usize)
        .take(needle.chars().count())
        .collect();
    let eq = if ignore_case {
        substr.to_lowercase() == needle.to_lowercase()
    } else {
        substr == needle
    };
    Some(Ok(Value::Bool(eq)))
}
