use super::str_match::is_str_or_match_receiver;
use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};

pub(crate) fn fmt_joinable_target(target: &Value) -> bool {
    matches!(
        target.view(),
        ValueView::Array(..)
            | ValueView::Seq(..)
            | ValueView::Slip(..)
            | ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. }
    )
}

/// Extract key and value from a Pair or ValuePair.
pub(crate) fn pair_key_value(val: &Value) -> Option<(Value, Value)> {
    match val.view() {
        ValueView::Pair(k, v) => Some((Value::str(k.to_string()), v.clone())),
        ValueView::ValuePair(k, v) => Some((k.clone(), v.clone())),
        _ => None,
    }
}

/// Format a single value or a pair for `.fmt()`.
/// If the value is a Pair, format with two args (key, value).
/// Otherwise, format as a single arg.
pub(crate) fn fmt_single_or_pair(fmt: &str, item: &Value) -> String {
    if let Some((k, v)) = pair_key_value(item) {
        runtime::format_sprintf_args(fmt, &[k, v])
    } else {
        runtime::format_sprintf(fmt, Some(item))
    }
}

pub(crate) fn contains_value_recursive(hay: &str, needle: &Value) -> Value {
    match needle.view() {
        ValueView::Junction { kind, values } => {
            let mapped = values
                .iter()
                .map(|v| contains_value_recursive(hay, v))
                .collect::<Vec<_>>();
            Value::junction(kind, mapped)
        }
        _ => Value::truth(hay.contains(&needle.to_string_value())),
    }
}

fn contains_value_recursive_ci(hay_lc: &str, needle: &Value) -> Value {
    match needle.view() {
        ValueView::Junction { kind, values } => {
            let mapped = values
                .iter()
                .map(|v| contains_value_recursive_ci(hay_lc, v))
                .collect::<Vec<_>>();
            Value::junction(kind, mapped)
        }
        _ => Value::truth(hay_lc.contains(&needle.to_string_value().to_lowercase())),
    }
}

/// `.contains($needle, $pos?, :i/:ignorecase/:m/:ignoremark?)` on a `Str` receiver —
/// the forms that carry a start position and/or the case-/mark-insensitive named
/// markings. These never reach the arity-keyed `native_method_*arg` dispatch (a Pair
/// or a 3rd arg pushes them past it), so they previously bounced to the interpreter.
/// Mirrors `Interpreter::dispatch_contains` (runtime/methods_string.rs) exactly: named
/// `i`/`ignorecase`/`m`/`ignoremark` all fold to a lowercase compare, and the start
/// position is taken from the second positional (Int/Num/Str-parsed).
///
/// Returns `None` (fall through to the interpreter) for: non-Str receivers, a Package
/// (type-object) needle, a `BigInt` position (overflow → X::OutOfRange handled by the
/// interpreter), and out-of-range / negative positions (X::OutOfRange Failure). The
/// plain single-needle form (`contains($needle)`) keeps its `native_method_1arg` arm.
pub(crate) fn native_contains_with_options(
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if !is_str_or_match_receiver(target) {
        return None;
    }
    let mut positional: Vec<&Value> = Vec::new();
    let mut ignore_case = false;
    for arg in args {
        if let ValueView::Pair(key, value) = arg.view() {
            match key.as_str() {
                "i" | "ignorecase" => ignore_case = value.truthy(),
                // `:ignoremark` needs NFD mark-stripping (the interpreter's
                // strip logic); let dispatch_contains own it.
                "m" | "ignoremark" if value.truthy() => return None,
                "m" | "ignoremark" => {}
                // An unexpected named arg: let the interpreter own the semantics.
                _ => return None,
            }
        } else {
            positional.push(arg);
        }
    }
    // Only the positioned / named forms are handled here; the bare single needle
    // (`contains($needle)`) stays on the existing 1-arg native arm.
    let needle: &Value = positional.first().copied()?;
    if positional.len() == 1 && args.len() == 1 {
        return None;
    }
    if let ValueView::Package(_) = needle.view() {
        return None;
    }
    // A Regex needle needs the regex engine (&mut self); let the interpreter's
    // dispatch_contains own it rather than searching for the regex's gist.
    if let ValueView::Regex(..) = needle.view() {
        return None;
    }
    let start = match positional.get(1).copied().map(Value::view) {
        Some(ValueView::Int(i)) => i,
        Some(ValueView::Num(f)) => f as i64,
        Some(ValueView::Str(s)) => s.parse::<i64>().ok()?,
        Some(_) => return None,
        None => 0,
    };
    let text = target.to_string_value();
    let len = text.chars().count() as i64;
    if start < 0 || start > len {
        return None;
    }
    let hay: String = text.chars().skip(start as usize).collect();
    let result = if ignore_case {
        contains_value_recursive_ci(&hay.to_lowercase(), needle)
    } else {
        contains_value_recursive(&hay, needle)
    };
    Some(Ok(result))
}
